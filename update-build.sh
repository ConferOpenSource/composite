for target in {composite-{aeson{,-path,-refined},base,binary,ekg,hashable,opaleye,reflex,swagger},example}; do
  ( cd $target && hpack --force && cabal2nix . > package.nix )
done
