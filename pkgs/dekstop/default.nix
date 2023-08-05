{
  lib, stdenv, makeWrapper,
  bash, xfce, brightnessctl, alsaUtils, dmenu,
  ...
}:
let
    deps = [ bash xfce.xfce4-terminal brightnessctl alsaUtils dmenu ];
    DEPS_PATH = lib.makeBinPath deps;
    XFCE_TERMINAL_PATH = lib.makeBinPath [xfce.xfce4-terminal];
in stdenv.mkDerivation rec {
    pname = "dekstop";
    version = "0.1";

    buildInputs = deps;
    nativeBuildInputs = [ makeWrapper ];

    unpackPhase = ":";

    installPhase = ''
        mkdir -p $out/bin
        cp ${./bin/dekstop} $out/bin/dekstop
        chmod +x $out/bin/dekstop
        wrapProgram $out/bin/dekstop \
            --set XFCE_TERMINAL_PATH ${XFCE_TERMINAL_PATH} \
            --set DEPS_PATH ${DEPS_PATH}
    '';
}
