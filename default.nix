{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "ca.srid.Taut";
  android.displayName = "Taut";
  ios.bundleIdentifier = "ca.srid.Taut";
  ios.bundleName = "Taut";

  packages = let
    obelisk-oauth = hackGet ./dep/obelisk-oauth;
  in {
    clay = pkgs.fetchFromGitHub {
      owner = "sebastiaanvisser";
      repo = "clay";
      rev = "54dc9eaf0abd180ef9e35d97313062d99a02ee75";
      sha256 = "0y38hyd2gvr7lrbxkrjwg4h0077a54m7gxlvm9s4kk0995z1ncax";
    };
    obelisk-oauth-common = obelisk-oauth + "/common";
    obelisk-oauth-backend = obelisk-oauth + "/backend";
    pagination = hackGet ./dep/pagination; # https://github.com/mrkkrp/pagination/issues/6
  };

  overrides = self: super: with pkgs.haskell.lib; let
    beam = hackGet ./dep/beam;
    direct-sqlite-src = pkgs.fetchFromGitHub {
      owner = "IreneKnapp";
      repo = "direct-sqlite";
      rev = "8e3da41c46b5de19942cc7bf421c3deb5117ba7a";
      sha256 = "0ffk5j1db2y1drn0przh4jw9gc3vygwd987wl1g1m3dw7ry4dxy6";
    };
    vector-sized-src = pkgs.fetchFromGitHub {
      owner = "expipiplus1";
      repo = "vector-sized";
      rev = "5f8773ee029e61c461e457ab58bdcd1a8e4065e4";
      sha256 = "1hafl49ggdb659p6sfhssanrn0pibwk804wpf6n19465zss64bna";
    };
  in
  {
    clay = dontCheck super.clay;
    mmark = dontCheck super.mmark;
    megaparsec = dontCheck super.megaparsec;
    beam-core = dontCheck (self.callCabal2nix "beam-core" (beam + /beam-core) {});
    beam-migrate = self.callCabal2nix "beam-migrate" (beam + /beam-migrate) {};
    beam-sqlite = self.callCabal2nix "beam-sqlite" (beam + /beam-sqlite) {};
    direct-sqlite = self.callCabal2nix "direct-sqlite" direct-sqlite-src {};
    vector-sized = doJailbreak (self.callCabal2nix "vector-sized" vector-sized-src {});
    indexed-list-literals = doJailbreak super.indexed-list-literals;
    email-validate = doJailbreak super.email-validate;
    pagination = dontCheck super.pagination;
    zip = dontCheck super.zip;
  };
})
