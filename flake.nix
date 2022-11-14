{
  description = "plutarch-quickcheck";

  inputs = {
    nixpkgs.follows = "plutarch/nixpkgs";
    nixpkgs-latest.url = "github:NixOS/nixpkgs";

    haskell-nix-extra-hackage.follows = "plutarch/haskell-nix-extra-hackage";
    haskell-nix.follows = "plutarch/haskell-nix";
    iohk-nix.follows = "plutarch/iohk-nix";
    haskell-language-server.follows = "plutarch/haskell-language-server";

    # Plutarch and its friends
    plutarch = {
      url = "github:Plutonomicon/plutarch-plutus?ref=master";

      inputs.emanote.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
      inputs.nixpkgs.follows =
        "plutarch/haskell-nix/nixpkgs-unstable";
    };

    liqwid-nix = {
      url = "github:Liqwid-Labs/liqwid-nix?ref=main";
      inputs.nixpkgs-latest.follows = "nixpkgs-latest";
    };
  };

  outputs = inputs@{ liqwid-nix, ... }:
    (liqwid-nix.buildProject
      {
        inherit inputs;
        src = ./.;
      }
      [
        liqwid-nix.plutarchProject
        liqwid-nix.haskellProject
        liqwid-nix.addBuildChecks
        (liqwid-nix.enableFormatCheck [
          "-XTemplateHaskell"
          "-XTypeApplications"
          "-XPatternSynonyms"
        ])
        liqwid-nix.enableCabalFormatCheck
        liqwid-nix.enableNixFormatCheck
      ]
    ).toFlake;
}
