{ lib, stdenv, fetchFromGitHub, rclone, makeWrapper, ... }:

stdenv.mkDerivation rec {
  pname = "git-annex-remote-rclone";
  version = "v0.7+1";
  rev = "0dc3baac40c5c7795fd1c6dac302dfe93f9bd5dd";

  src = fetchFromGitHub {
    inherit rev;
    owner = "DanielDent";
    repo = "git-annex-remote-rclone";
	sha256 = "1v2jmpapkxf9xzqs0chc2j5x3pw4rjrv2bvadhfspzl3n4myj63l";
  };

  nativeBuildInputs = [ makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp git-annex-remote-rclone $out/bin
    wrapProgram "$out/bin/git-annex-remote-rclone" \
      --prefix PATH ":" "${lib.makeBinPath [ rclone ]}"
  '';

  meta = with lib; {
    homepage = "https://github.com/DanielDent/git-annex-remote-rclone";
    description = "Use rclone supported cloud storage providers with git-annex";
    license = licenses.gpl3;
    maintainers = [ maintainers.montag451 ];
  };
}
