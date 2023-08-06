{
  komputiloj,
  lib, stdenv, makeWrapper,
  bash, coreutils, gnused, gawk, gnugrep,
  xfce, brightnessctl, alsaUtils, dmenu, xmobar, gnupg, xsel,
  xrandr, wpa_supplicant,
  ...
}:
let
    deps = [
        bash coreutils gnused gawk gnugrep
        xfce.xfce4-terminal brightnessctl alsaUtils dmenu
        xmobar gnupg xsel komputiloj.packages.wachtwoord xrandr wpa_supplicant
    ];
in stdenv.mkDerivation rec {
    pname = "dekstop";
    version = "0.1";

    src = ./.;

    nativeBuildInputs = [ makeWrapper ];

    installPhase = ''
        mkdir -p $out/bin
        cp bin/* $out/bin
        chmod +x $out/bin/*
        wrapProgram $out/bin/dekstop \
            --set XFCE_TERMINAL_PATH ${lib.makeBinPath [xfce.xfce4-terminal]} \
            --set DEPS_PATH $out/bin:${lib.makeBinPath deps} \
            --set XMOBARRC ${./xmobarrc}
    '';
}
