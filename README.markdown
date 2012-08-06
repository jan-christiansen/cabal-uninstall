cabal-uninstall
===============


Introduction
------------

Very simple Haskell script to uninstall cabal packages.


Usage
-----

Invoke

    cabal-uninstall <package-name> [--force]

to delete the folder of a package named `<package-name>` and
unregister the packge. The parameter `--force` is passed to `ghc-pkg
unregister` to unregister a package even if it breaks other
packages. If the package is installed globally you have to provide
root privileges to `cabal-uninstall`.
