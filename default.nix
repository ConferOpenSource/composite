let
  packages = {
    aeson         = import ./composite-aeson/package.nix;
    aeson-refined = import ./composite-aeson-refined/package.nix;
    base          = import ./composite-base/package.nix;
    ekg           = import ./composite-ekg/package.nix;
    opaleye       = import ./composite-opaleye/package.nix;
    reflex        = import ./composite-reflex/package.nix;
  };
in
  packages // {
    overrides = self: super: {
      composite-aeson         = self.callPackage packages.aeson {};
      composite-aeson-refined = self.callPackage packages.aeson-refined {};
      composite-base          = self.callPackage packages.base {};
      composite-ekg           = self.callPackage packages.ekg {};
      composite-opaleye       = self.callPackage packages.opaleye {};
      composite-reflex        = self.callPackage packages.reflex {};
    };
  }
