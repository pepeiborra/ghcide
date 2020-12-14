{ sources ? import ./sources.nix }:
let
  overlay = selfPkgs: pkgs:
    let sharedOverrides = {
        overrides = _self: super: {
            mkDerivation = args: super.mkDerivation (args //
                {
                    # skip running tests for Hackage packages
                    doCheck =
                        # but not for ghcide
                        args.version == "0";
                    # relax upper bounds
                    jailbreak = args.pname != "jailbreak-cabal";
                });
            };
            };
        gitignoreSource = (import sources.gitignore { inherit (pkgs) lib; }).gitignoreSource;
        extend = haskellPackages:
          (haskellPackages.override sharedOverrides).extend (pkgs.haskell.lib.packageSourceOverrides {
            ghcide = gitignoreSource ../.;
            hie-compat = gitignoreSource ../hie-compat;
            shake-bench = gitignoreSource ../shake-bench;
          });
        callGhcPackage = pkgs.buildPackages.newScope {
                      haskellLib = pkgs.haskell.lib;
                      overrides = pkgs.haskell.packageOverrides;
                      };
        in
        {
        inherit gitignoreSource;
        ourHaskell = pkgs.haskell // {
            compiler = pkgs.haskell.compiler // {
              ghc884s = selfPkgs.callPackage "${<nixpkgs>}/pkgs/development/compilers/ghc/8.8.4.nix" {
                      # aarch64 ghc865Binary gets SEGVs due to haskell#15449 or similar
                            bootPkgs = selfPkgs.haskell.packages.ghc865Binary;
                            inherit (selfPkgs.buildPackages.python3Packages) sphinx;
                            buildLlvmPackages = selfPkgs.buildPackages.llvmPackages_7;
                            llvmPackages = pkgs.llvmPackages_7;
                            enableShared = false;
                            };
                        };
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8101 = extend pkgs.haskell.packages.ghc8101;
                ghc8102 = extend pkgs.haskell.packages.ghc8102;
                ghc884s = callGhcPackage "${<nixpkgs>}/pkgs/development/haskell-modules" {
                        buildHaskellPackages = selfPkgs.buildPackages.ourHaskell.packages.ghc884s;
                        ghc = selfPkgs.buildPackages.ourHaskell.compiler.ghc884s;
                        compilerConfig = callGhcPackage "${<nixpkgs>}/pkgs/development/haskell-modules/configuration-ghc-8.8.x.nix" { };
                                        };

            };
        };
        };

in import sources.nixpkgs
{ overlays = [ overlay ] ; config = {allowBroken = true;}; }
