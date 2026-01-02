{ lib, stdenv, makeWrapper, pandoc, w3m, ... }:

stdenv.mkDerivation rec {
    pname = "mdview";
    version = "0.1";

    buildInputs = [ pandoc w3m ];
    nativeBuildInputs = [ makeWrapper ];

    src = ./mdview.sh;
    dontUnpack = true;
    installPhase = ''
        runHook preInstal
        mkdir -p $out/bin
        install $src $out/bin/mdview
        wrapProgram $out/bin/mdview \
            --set PATH ${lib.makeBinPath [ pandoc w3m ]}
        runHook postInstall
    '';
}
