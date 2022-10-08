final: prev: {

  haskellPackages = prev.haskellPackages.extend (hsFinal: hsPrev: {
    generator = hsPrev.callCabal2nix "generator" ./. { };
  });

  website = prev.stdenv.mkDerivation {
    name = "website";
    src = prev.pkgs.nix-gitignore.gitignoreSourcePure [
      ./.gitignore
      ".git"
      ".github"
    ] ./.;
    phases = "buildPhase installPhase";
    buildInputs = [ prev.pkgs.haskellPackages.generator ];

    buildPhase = ''
      generator
    '';
    installPhase = ''
      cp -r docs/* $out/
    '';
  };

}
