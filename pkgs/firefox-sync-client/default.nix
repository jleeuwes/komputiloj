{
  lib,
  buildGoModule,
  fetchFromGitHub,
  ...
}:

buildGoModule rec {
  pname = "firefox-sync-client";
  version = "1.9.0-fastly";

  src = fetchFromGitHub {
    owner = "stv0g";
    repo = "firefox-sync-client";
    rev = "dddd3831d42ad78b7c6546a1ebc8e80d6f11d52c";
    hash = "sha256-50XDqAnBgofXzZaZqFHbjD40IBRDvhIxLcFWbss61xE=";
  };

  vendorHash = "sha256-NQKF5LugGh2wNWf6M3uUhS2YOTuv2/K56gWUv5ACwEU=";

  meta = {
    description = "Commandline-utility to list/view/edit/delete entries in a firefox-sync account";
    homepage = "https://github.com/Mikescher/firefox-sync-client";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ ambroisie ];
    mainProgram = "ffsclient";
  };
}
