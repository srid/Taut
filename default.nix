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

    diagrams-reflex = pkgs.fetchFromGitHub {
      owner = "diagrams";
      repo = "diagrams-reflex";
      rev = "4121332f3ddfcc900898a8a57d2e47a8ea6be974";
      sha256 = "0ia3lnhg127vf15ynlfw6mw01nh0j0y5q5sjxd3mi3v8aa4dr2wm";
    };

    reflex-dom-contrib = pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      rev = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3";
      sha256 = "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
    };
  in
  {
    beam-core = self.callCabal2nix "beam-core" "${beam}/beam-core" {};
    beam-migrate = self.callCabal2nix "beam-migrate" "${beam}/beam-migrate" {};
    beam-sqlite = self.callCabal2nix "beam-sqlite" "${beam}/beam-sqlite" {};
    diagrams-reflex = self.callPackage "${diagrams-reflex}" {};
    reflex-dom-contrib = self.callCabal2nix "reflex-dom-contrib" "${reflex-dom-contrib}" {};
  };
})
