module Main where


import Data.List (groupBy)
import System.Environment (getArgs)
import System.Process (system, runInteractiveCommand)
import System.Exit (ExitCode(..))
import System.IO (hGetContents, hFlush, stdout)
import System.Directory (doesDirectoryExist, removeDirectoryRecursive)
import System.FilePath (takeDirectory)
import Control.Monad.Instances ()
import Control.Monad.Error


main :: IO ()
main = do
  input <- getArgs
  result <- runErrorT (mainError input)
  either putStr (const (return ())) result

mainError :: [String] -> ErrorT String IO ()
mainError input = do
  (package, force) <- ErrorT (return (parseArgs input))
  info@(PackageInfo _ _ _ libraryDir) <- ErrorT (selectPackageVersion package)
  ErrorT (unregisterPackage info force)
  lift (removeDirectory "package directory" (takeDirectory libraryDir))

(<|) :: a -> [a] -> [a]
x <| xs = xs++[x]

usageInfo :: String
usageInfo =
  "Version: 0.1.4\n\
  \Usage: cabal-uninstall {pkg-id} [--force]\n\
  \use sudo if the package is installed globally"

ambiguousErrorInfo :: String
ambiguousErrorInfo =
  "There are multiple versions of this package that cannot be distinguished"

internalErrorInfo :: String
internalErrorInfo =
  "Internal error: please contact Jan Christiansen (j.christiansen@monoid-it.de)"

parseArgs :: [String] -> Either String (String, Bool)
parseArgs [package]           = Right (package, False)
parseArgs [package,"--force"] = Right (package, True)
parseArgs _                   = Left usageInfo


data PackageInfo = PackageInfo Database String String String

instance Show PackageInfo where
  show (PackageInfo db name version _) = name ++ "-" ++ version ++ " (" ++ show db ++ " package)"

instance Eq PackageInfo where
  PackageInfo db1 n1 v1 _ == PackageInfo db2 n2 v2 _ =
    db1==db2 && n1==n2 && v1==v2


data Database = Global | User
  deriving Eq

instance Show Database where
  show Global = "global"
  show User   = "user"


allPackageInfos :: String -> IO (Either String [PackageInfo])
allPackageInfos package = do
  userPackageInfos <- packageInfos User package
  globalPackageInfos <- packageInfos Global package
  case (userPackageInfos, globalPackageInfos) of
       (Left user, Left global)   -> return (Left (user ++ global))
       (Left _, Right global)     -> return (Right global)
       (Right user, Left _)       -> return (Right user)
       (Right user, Right global) -> return (Right (user ++ global))

parameter :: Database -> String
parameter Global = "--global"
parameter User   = "--user"

packageInfos :: Database -> String -> IO (Either String [PackageInfo])
packageInfos database package = do
  let fields = "name,version,library-dirs"
  let command = "ghc-pkg field " ++ parameter database ++ " " ++ package ++ " " ++ fields
  (_, hout, herr, _) <- runInteractiveCommand command
  result <- hGetContents hout
  case result of
       [] -> do
             err <- hGetContents herr
             return (Left (show database ++ " database: " ++ err))
       _  -> return (parseOutput database (words result))


type Parser a = [String] -> Either String (a, [String])


parseOutput :: Database -> [String] -> Either String [PackageInfo]
parseOutput database input = do
  (packageInfo, rest) <- parseMany (parseVersion database) input
  if null rest
     then return packageInfo
     else fail internalErrorInfo

parseMany :: Parser a -> Parser [a]
parseMany _        []    = return ([], [])
parseMany parseOne input = do
  (x, rest) <- parseOne input
  (xs, rest') <- parseMany parseOne rest
  return (x:xs, rest')

parseVersion :: Database -> Parser PackageInfo
parseVersion database ("name:":name:"version:":version:"library-dirs:":libraryDirs:rest) =
  return (PackageInfo database name version libraryDirs, rest)
parseVersion _ _ = Left internalErrorInfo

selectPackageVersion :: String -> IO (Either String PackageInfo)
selectPackageVersion package = do
  eInfos <- allPackageInfos package
  case eInfos of
       Right []     -> return (Left internalErrorInfo)
       Right [info] -> return (Right info)
       Right infos  -> selectInfo infos
       Left err     -> return (Left err)

ambiguous :: [PackageInfo] -> Bool
ambiguous infos = all singleton (groupBy (==) infos)
 where
  singleton [_] = False
  singleton _   = True

selectInfo :: [PackageInfo] -> IO (Either String PackageInfo)
selectInfo infos
  | ambiguous infos = do
    mapM_ print infos
    return (Left ambiguousErrorInfo)
  | otherwise       = do
  putStr ("There are multiple versions of this package, please select one:\n"
          ++ unlines (zipWith line [(1::Int)..] (map show infos))
          ++ "\nPlease select a number\n")
  n <- getLine
  case reads n of
       [(i, "")] -> selectInfo' i
       _         -> selectInfo infos
 where
  line n packagePath = show n ++ ": " ++ packagePath
  selectInfo' i
    | i < 1 || i > noOfVersions+1 = selectInfo infos
    | otherwise                   = return (Right (infos!!(i-1)))
  noOfVersions = length infos

removeDirectory :: String -> FilePath -> IO ()
removeDirectory description directory = do
  b <- doesDirectoryExist directory
  if b
     then guardedAction ("Delete the " ++ description ++ " " ++ directory ++ "?")
                        (removeDirectoryRecursive directory)
                        (return ())
     else putStrLn ("The " ++ description ++ " " ++ directory ++ " does not exist")

unregisterPackage :: PackageInfo -> Bool -> IO (Either String ())
unregisterPackage packageInfo@(PackageInfo db name version _) force = do
  let command = "ghc-pkg unregister " ++ parameter db ++ useForce force ++ name ++ "-" ++ version
  guardedAction ("Unregister the package " ++ show packageInfo ++ "?")
                (do exitcode <- system command
                    return (errorcode exitcode))
                (return (Right ()))
 where
  useForce True  = " --force "
  useForce False = " "
  errorcode exitcode@(ExitFailure _) = Left (show exitcode)
  errorcode _                        = Right ()

guardedAction :: String -> IO a -> IO a -> IO a
guardedAction question thenAction elseAction = do
  putStr (question ++ " (yes/no)")
  hFlush stdout
  choice <- getLine
  case choice of
       "yes" -> thenAction
       _     -> elseAction
