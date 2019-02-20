let

snack-src = (import <nixpkgs> {}).fetchFromGitHub {
  repo = "snack";
  owner = "ruhatch";
  rev = "57f8d198f4bf657a9a770a2a85a3baad47a95766";
  sha256 = "15v8i6s7h8k2szpijda9m3zl5x0fpz6vk15z5wbqyhr59c0vv4bk";
};

in

{ pkgs ? import (import "${snack-src}/nix/nixpkgs") {}
, snack ? import "${snack-src}/snack-lib" { inherit pkgs; }
}:

snack.mkPackage (import ./snack.nix)
