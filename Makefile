
.PHONY: build update-build update-nixpkgs

build: update-build
	stack test --ghc-options="-Wall -Werror"

update-build:
	nix-shell update-build-shell.nix --run ./update-build.sh
