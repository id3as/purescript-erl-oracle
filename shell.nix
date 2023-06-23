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
      rev = "8ec38d6e474e65b73f54ee6aa725ada171eb884e";
      sha256 = "sha256-Z5vFKw7hWyv4W+rDkTCyfifHXtfa7E9KsvMJFoN5Ko8=";
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
in

mkShell {
  buildInputs = with pkgs; [

    erlang.erlang
    erlang.rebar3.rebar3
    erlang.erlang-ls

    # Purescript
    easy-ps.purs-0_15_9-2 # This is a prerelease because it's the first ARM build
    easy-ps.spago-next
    easy-ps.purescript-language-server
    easy-ps.purs-tidy
    purerl.purerl-0-0-19

    # Kind of essential
    oci-cli
  ];
}
