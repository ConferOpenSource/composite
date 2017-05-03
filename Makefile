
.PHONY: \
	build update-build update-nixpkgs                     \
	composite-aeson/composite-aeson.cabal                 \
	composite-aeson-refined/composite-aeson-refined.cabal \
	composite-base/composite-base.cabal                   \
	composite-ekg/composite-ekg.cabal                     \
	composite-opaleye/composite-opaleye.cabal             \
	composite-reflex/composite-reflex.cabal

build: update-build
	stack test --ghc-options="-Wall -Werror"

update-build: \
  composite-aeson/package.nix \
  composite-aeson-refined/package.nix \
  composite-base/package.nix \
  composite-ekg/package.nix \
  composite-opaleye/package.nix \
  composite-reflex/package.nix

composite-aeson/package.nix: composite-aeson/composite-aeson.cabal
	rm -f composite-aeson/package.nix
	cd composite-aeson && nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

composite-aeson/composite-aeson.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack composite-aeson'

composite-aeson-refined/package.nix: composite-aeson-refined/composite-aeson-refined.cabal
	rm -f composite-aeson-refined/package.nix
	cd composite-aeson-refined && nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

composite-aeson-refined/composite-aeson-refined.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack composite-aeson-refined'

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

composite-reflex/package.nix: composite-reflex/composite-reflex.cabal
	rm -f composite-reflex/package.nix
	cd composite-reflex && nix-shell -p cabal2nix --run 'cabal2nix .' > package.nix

composite-reflex/composite-reflex.cabal:
	nix-shell -p haskellPackages.hpack --run 'hpack composite-reflex'
