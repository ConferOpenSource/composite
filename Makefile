
.PHONY: update-build update-nixpkgs composite-aeson/composite-aeson.cabal composite-base/composite-base.cabal composite-ekg/composite-ekg.cabal composite-opaleye/composite-opaleye.cabal

update-build: composite-aeson/package.nix composite-base/package.nix composite-ekg/package.nix composite-opaleye/package.nix

composite-aeson/package.nix: composite-aeson/composite-aeson.cabal
	rm -f composite-aeson/package.nix
	cd composite-aeson && nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

composite-aeson/composite-aeson.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack composite-aeson'

composite-base/package.nix: composite-base/composite-base.cabal
	rm -f composite-base/package.nix
	cd composite-base && nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

composite-base/composite-base.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack composite-base'

composite-ekg/package.nix: composite-ekg/composite-ekg.cabal
	rm -f composite-ekg/package.nix
	cd composite-ekg && nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

composite-ekg/composite-ekg.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack composite-ekg'

composite-opaleye/package.nix: composite-opaleye/composite-opaleye.cabal
	rm -f composite-opaleye/package.nix
	cd composite-opaleye && nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

composite-opaleye/composite-opaleye.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack composite-opaleye'
