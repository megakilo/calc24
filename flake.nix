{
  description = "calc24";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
          };
        in
        {
          devShells.default =
            pkgs.mkShell {
              buildInputs = with pkgs; [
                clang
                git
                pypy3
                (haskellPackages.ghcWithPackages (pkgs: [ cabal-install ]))
                go
                cargo
                rustc
                rustfmt
                zig
                zls
              ];

              shellHook = ''
                export PS1='[\[\033[1;32m\]calc24\[\033[0m\]:\[\033[1;34m\]\w\[\033[0m\]]\$ '
              '';

              RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
            };
        }
      );
}
