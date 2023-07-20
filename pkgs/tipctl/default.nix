{ lib, stdenv, fetchurl, makeWrapper, php, ... }:

# Based on https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/php-packages/composer/default.nix

stdenv.mkDerivation rec {
    pname = "tipctl";
    version = "v6.24.0"; # newest version is 6.30.0 but it has no phar release

    src = fetchurl {
        url = "https://github.com/transip/tipctl/releases/download/${version}/tipctl.phar";
        hash = "sha256-l+pk/Q/+26e/lIUD55W+yssbHYqHbp+/yOkTECY+GCY=";
    };

    dontUnpack = true;

    nativeBuildInputs = [ makeWrapper ];

    installPhase = ''
        runHook preInstal
        mkdir -p $out/bin
        install -D $src $out/libexec/tipctl/tipctl.phar
        makeWrapper ${php}/bin/php $out/bin/tipctl \
            --add-flags --define \
            --add-flags display_errors=stderr \
            --add-flags "$out/libexec/tipctl/tipctl.phar"
        runHook postInstall
    '';

    meta = with lib; {
        homepage = "https://github.com/transip/tipctl";
        description = "TransIP Control (tipctl) is a tool that connects to the TransIP API from your terminal.";
        # license = licenses.apache2;
        maintainers = [ ];
    };
}
