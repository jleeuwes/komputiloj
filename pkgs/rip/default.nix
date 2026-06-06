{
    boltons, lib, stdenv, makeWrapper,
    coreutils, util-linux, gawk,
    cddiscid, cdparanoia, lame,
    ...
}:
with boltons;

stdenv.mkDerivation rec {
    pname = "rip";
    version = "0.2";
    
    buildInputs = [ coreutils util-linux gawk cddiscid cdparanoia lame ];
    nativeBuildInputs = [ makeWrapper ];
    
    unpackPhase = ":";
    
    installPhase = ''
        mkdir -p $out/bin
        cp ${./rip} $out/bin/rip
        chmod +x $out/bin/rip
        # https://github.com/deepfire/nixos-wiki/blob/master/Nix%20Runtime%20Environment%20Wrapper.page
        wrapProgram $out/bin/rip \
            --set PATH ${lib.makeBinPath [ coreutils util-linux gawk cddiscid cdparanoia lame ]}
    '';
}
