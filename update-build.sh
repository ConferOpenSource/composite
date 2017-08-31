for target in composite-{aeson{,-refined},base,ekg,opaleye,reflex} example; do
  ( cd $target && hpack && cabal2nix . > package.nix )
done
