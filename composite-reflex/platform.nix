{ reflex-platform
, base-platform ? reflex-platform.ghcjs
, ...
}:

with {
  inherit (reflex-platform.nixpkgs) fetchgitPrivate fetchFromGitHub;
  inherit (reflex-platform.nixpkgs.haskell.lib) dontCheck;
  inherit (builtins) fromJSON readFile;
};

let
  composite = ./..;
in

base-platform.override {
  overrides = self: super: {
    composite-aeson        = self.callPackage (composite + /composite-aeson/package.nix) {};
    composite-base         = self.callPackage (composite + /composite-base/package.nix) {};
    # http-api-data          = dontCheck (self.callPackage ./http-api-data.nix {});
    # frost-shared           = self.callPackage ../shared/package.nix {};
    # servant                = dontCheck (self.callPackage ../servant.nix {});
    # servant-reflex         = self.callPackage ./servant-reflex.nix {};
  };
}

