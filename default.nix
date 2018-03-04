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
      rev = "1673f0762f9beca04923518bca1ba5c519fd56a3";
      sha256 = "0l1zs64n736gkrz3bs0ws6lq4kjig32przdmnjigk40rs3mly32k";
    };
  in
  {
    beam-core = self.callCabal2nix "beam-core" "${beam}/beam-core" {};
    beam-migrate = self.callCabal2nix "beam-migrate" "${beam}/beam-migrate" {};
    beam-sqlite = self.callCabal2nix "beam-sqlite" "${beam}/beam-sqlite" {};
  };
})
