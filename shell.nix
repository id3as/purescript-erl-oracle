let
  pinnedNixHash = "02336c5c5f719cd6bd4cfc5a091a1ccee6f06b1d";

  pinnedNix =
    builtins.fetchGit {
      name = "nixpkgs-pinned";
      url = "https://github.com/NixOS/nixpkgs.git";
      rev = "${pinnedNixHash}";
    };

  erlangReleases =
    builtins.fetchGit {
      name = "nixpkgs-nixerl";
      url = "https://github.com/id3as/nixpkgs-nixerl.git";
      rev = "b0af6d4884ae1c18cad6afe7436333f7ad72fb06";
    };

  purerlReleases =
    builtins.fetchGit {
      url = "https://github.com/purerl/nixpkgs-purerl.git";
      ref = "master";
      rev = "9fe13b1c145035808d3e9d37825362743fe42992";
    };

  easy-ps = import
    (nixpkgs.pkgs.fetchFromGitHub {
      ## Temporarily on Fabrizio's fork to get spago-next
      owner = "f-f";
      repo = "easy-purescript-nix";
      rev = "2e62b746859e396e541bdd63dbd10b2f231027d4";
      sha256 = "sha256-qQpWKE40wKkxb4y2+z0n4lF/OFrCsEU3Gwiugl3H+xc=";
    }) { pkgs = nixpkgs; };

  nixpkgs =
    import pinnedNix {
      overlays = [
        (import erlangReleases)
        (import purerlReleases)
      ];
    };

  erlang = nixpkgs.nixerl.erlang-26-0;

in

with nixpkgs;

let
    inherit (stdenv.lib) optionals;
    oracle-cli = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/976fa3369d722e76f37c77493d99829540d43845.tar.gz") {};

in

mkShell {
  buildInputs = with pkgs; [

    erlang.erlang
    erlang.rebar3.rebar3
    erlang.erlang-ls

    # Purescript
    easy-ps.purs-0_15_7
    easy-ps.spago-next
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    purerl.purerl-0-0-19

    # Kind of essential
    oracle-cli.oci-cli
  ];
}
