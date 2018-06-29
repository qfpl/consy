{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./consy.nix;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  
  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: {
      criterion = self.callHackage "criterion" "1.3.0.0" {};
      inspection-testing = self.callHackage "inspection-testing" "0.2.0.1" {};
      weigh = self.callHackage "weigh" "0.0.12" {};
    };
  };
  
  drv = modifiedHaskellPackages.callPackage f {};

in

  drv
