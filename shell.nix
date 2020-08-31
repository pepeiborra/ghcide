{ sources ? import nix/sources.nix,
  nixpkgs ? import sources.nixpkgs {},
  compiler ? "default",
  hoogle ? false
 }:
with nixpkgs;

let defaultCompiler = "ghc" + lib.replaceStrings ["."] [""] haskellPackages.ghc.version;
    haskellPackagesForProject = p:
        if compiler == "default" || compiler == defaultCompiler
            then if hoogle
                then haskellPackages.ghcWithHoogle p
                else haskellPackages.ghcWithPackages p
            # for all other compilers there is no Nix cache so dont bother building deps
            else if hoogle
                then  haskell.packages.${compiler}.ghcWithHoogle (_: [])
                else haskell.packages.${compiler}.ghcWithPackages (_: []);

   compilerWithPackages = haskellPackagesForProject(p:
        with p;
        [ aeson
          alex
          async
          base16-bytestring
          blaze-builder
          blaze-markup
          Chart
          Chart-diagrams
          conduit-extra
          conduit-parse
          cryptohash-sha1
          data-default
          data-default-class
          data-default-instances-containers
          data-default-instances-dlist
          data-default-instances-old-locale
          diagrams
          diagrams-svg
          Diff
          extra
          floskell
          fuzzy
          generic-deriving
          gitrev
          Glob
          happy
          haskell-lsp
          haskell-lsp-types
          hslogger
          hspec
          lens
          lsp-test
          megaparsec
          network
          optparse-simple
          QuickCheck
          parsers
          parser-combinators
          prettyprinter
          prettyprinter-ansi-terminal
          primes
          psqueues
          regex-tdfa
          rope-utf16-splay
          safe-exceptions
          shake
          sorted-list
          tasty
          tasty-golden
          tasty-hunit
          tasty-rerun
          temporary
          text
          typed-process
          unordered-containers
          xml
          yaml
          zlib
         ]);
in
stdenv.mkDerivation {
  name = "haskell-language-server";
  buildInputs = [
    gmp
    zlib
    ncurses

    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.ormolu
    haskellPackages.stylish-haskell

    compilerWithPackages

  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
