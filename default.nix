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
      rev = "011999d65680debdf3d9a7c7efaf5679dc64368b";
      sha256 = "1g4pznzvl3iwnk02mlcwklrzc9frhahk7irrwggaf4395g95r6z4";
    };
  in
  {
    beam-core = self.callCabal2nix "beam-core" "${beam}/beam-core" {};
    beam-migrate = self.callCabal2nix "beam-migrate" "${beam}/beam-migrate" {};
    beam-sqlite = self.callCabal2nix "beam-sqlite" "${beam}/beam-sqlite" {};
  };
})
