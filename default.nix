let

snack-src = (import <nixpkgs> {}).fetchFromGitHub {
  repo = "snack";
  owner = "nmattia";
  rev = "97d41baf2b0a2696079852e3b7ad38783f04d9bd";
  sha256 = "1kyx9bpmn8s1axiaxbvqgxbjvzs1y71iilaz19iycvf3bqyx74fd";
};

in

{ pkgs ? import (import "${snack-src}/nix/nixpkgs") {}
, snack ? import
  ("${snack-src}/snack-lib")
  { lib = pkgs.lib;
    haskellPackages = pkgs.haskellPackages;
    makeWrapper = pkgs.makeWrapper;
    rsync = pkgs.rsync;
    stdenv = pkgs.stdenv;
    symlinkJoin = pkgs.symlinkJoin;
    writeScriptBin = pkgs.writeScriptBin;
    writeText = pkgs.writeText;
    runCommand = pkgs.runCommand;
    callPackage = pkgs.callPackage;
  }
}:

snack.inferSnackBuild ./snack.nix
