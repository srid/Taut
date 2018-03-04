{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };

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
