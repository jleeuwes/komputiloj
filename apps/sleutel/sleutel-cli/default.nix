{ boltons, lib, stdenv, makeWrapper, coreutils, mkpasswd, ...}:
with boltons;
stdenv.mkDerivation rec {
    pname = "sleutel";
    version = "0.1";
    
    buildInputs = [ coreutils mkpasswd ];
    nativeBuildInputs = [ makeWrapper ];

    unpackPhase = ":";
    
    installPhase = ''
        mkdir -p $out/bin
        cp ${./sleutel} $out/bin/sleutel
        chmod +x $out/bin/sleutel
        wrapProgram $out/bin/sleutel \
            --set PATH ${lib.makeBinPath [ coreutils mkpasswd ]}
    '';
}
