# My personal website

This repository contains the source files used to create my personal website with Haskell and Pandoc.

## Building the generator & generator the website

A Nix development shell is available through `nix develop`. Compiling the generator and building the website is done with `nix build .#generator` and `nix build` respectively.

The generator code is documented with Haddock, which can be rendered using `cabal haddock --haddock-executables`.

The code can be formatted by running `cabal-fmt -i generator.cabal`, `fourmolu -e -i $(find . -name '*.hs')` and `nix fmt`.

## License

The code is licensed under the MIT license, see [LICENSE](LICENSE). The content on the website comes with its own license; [see the website](https://www.hjdskes.nl/license).
