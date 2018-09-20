let

snack-src = (import <nixpkgs> {}).fetchFromGitHub {
  repo = "snack";
  owner = "ruhatch";
  rev = "5f41965d12eadf53403162e930e5c1387890c67f";
  sha256 = "10hv2an6g9zhfjwg86p97vbm27b123l0fc5x8xqj278dmlg79035";
};

in

{ pkgs ? import (import "${snack-src}/nix/nixpkgs") {}
, snack ? import "${snack-src}/snack-lib" { inherit pkgs; }
}:

snack.mkPackage (import ./snack.nix)
