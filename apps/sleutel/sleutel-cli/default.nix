{ boltons, lib, stdenv, makeWrapper, coreutils, mkpasswd, systemd, ...}:
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
        # the /run/wrappers/bin is for sudo
        wrapProgram $out/bin/sleutel \
            --set PATH ${lib.makeBinPath [ coreutils mkpasswd ]}:/run/wrappers/bin \
            --set CMD_SYSTEMCTL ${systemd}/bin/systemctl
    '';
}
