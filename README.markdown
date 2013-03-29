cabal-uninstall
===============

A very simple Haskell script that invokes `ghc-pkg` to uninstall cabal
packages.


Usage
-----

Invoke

    cabal-uninstall {pkg-id} [--force]

to unregister a package nameed `{pkg-id}` and delete the corresponding
package folder. If there are several packages with the name `{pkg-id}`
the script asks the user to select one. If `cabal-uninstall` is called
with the parameter `--force`, then this parameter is passed to
`ghc-pkg unregister` to unregister a package even if it breaks other
packages. You might have to provide root privileges to
`cabal-uninstall` to delete the folder.
