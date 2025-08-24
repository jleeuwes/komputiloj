{ lib, stdenv, makeWrapper, mozlz4a, jq, ... }:

stdenv.mkDerivation rec {
    pname = "moz-recover-as-bookmarks";
    version = "0.0";

    # src = ./.;

    buildInputs = [ mozlz4a jq ];
    nativeBuildInputs = [ makeWrapper ];

    unpackPhase = ":";

    explicit_dep = ./moz-recover-as-bookmarks;
    installPhase = ''
        mkdir -p $out/bin
        cp ${./moz-recover-as-bookmarks} $out/bin/moz-recover-as-bookmarks
        chmod +x $out/bin/moz-recover-as-bookmarks
        wrapProgram $out/bin/moz-recover-as-bookmarks \
            --set PATH ${lib.makeBinPath [ mozlz4a jq ]}
    '';

}
