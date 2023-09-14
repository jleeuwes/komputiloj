{ boltons, lib, stdenv, makeWrapper,
    bash, openssh, nixos-rebuild, nix, nixops, wachtwoord, tipctl, coreutils, ...}:
with boltons;
let deps = [ bash coreutils openssh nixos-rebuild nix nixops wachtwoord tipctl ];
in
stdenv.mkDerivation rec {
    pname = "komputiloj";
    version = "0.1";
    
    # buildInputs = deps;
    nativeBuildInputs = [ makeWrapper ];

    unpackPhase = ":";
    
    installPhase = ''
        mkdir -p $out/bin
        cp ${./komputiloj} $out/bin/komputiloj
        chmod +x $out/bin/komputiloj
        # https://github.com/deepfire/nixos-wiki/blob/master/Nix%20Runtime%20Environment%20Wrapper.page
        wrapProgram $out/bin/komputiloj \
            --set AMBIENT_PATH '$PATH' \
            --set PATH ${lib.makeBinPath deps }
    '';
}
