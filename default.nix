{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ pkgs, ... }: {
  android.applicationId = "ca.srid.Taut";
  android.displayName = "Taut";
  ios.bundleIdentifier = "ca.srid.Taut";
  ios.bundleName = "Taut";

  overrides = self: super: let
    beam = pkgs.fetchFromGitHub {
      owner = "tathougies";
      repo = "beam";
      rev = "a3b5e0763843fed48c7eef53fa7d08cfe710342d";
      sha256 = "15w4rqwj3wpssah6664pwvlh6lvv7pv4xa9v81kj3p1vx74lx9ps";
    };
  in
  {
    beam-core = self.callCabal2nix "beam-core" "${beam}/beam-core" {};
    beam-migrate = self.callCabal2nix "beam-migrate" "${beam}/beam-migrate" {};
    beam-sqlite = self.callCabal2nix "beam-sqlite" "${beam}/beam-sqlite" {};
  };
})
