{ lib, stdenv, makeWrapper, gnupg, apacheHttpd, mkpasswd, coreutils }:
stdenv.mkDerivation rec {
	pname = "wachtwoord";
	version = "0.1";

	buildInputs = [ gnupg apacheHttpd mkpasswd ];
	nativeBuildInputs = [ makeWrapper ];

	unpackPhase = ":";
	installPhase = ''
		mkdir -p $out/bin
		cp ${./wachtwoord} $out/bin/wachtwoord
		chmod +x $out/bin/wachtwoord
		# https://github.com/deepfire/nixos-wiki/blob/master/Nix%20Runtime%20Environment%20Wrapper.page
		wrapProgram $out/bin/wachtwoord \
			--set PATH ${lib.makeBinPath [ gnupg apacheHttpd mkpasswd coreutils ]}
	'';
}
