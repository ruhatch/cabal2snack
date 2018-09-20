let
  cabal2snack =
    { src = ./src;
      ghcOpts = [
        "-Wall"
        "-Werror"
      ];
      dependencies = [
        "base"
        "cabal2nix"
        "distribution-nixpkgs"
        "language-nix"
        "text"
      ];
    };
in
{ src = ./cabal2snack;
  main = "Main";
  dependencies = [ "base" ];
  packages = [ cabal2snack ];
}
