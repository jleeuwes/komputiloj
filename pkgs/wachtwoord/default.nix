{ boltons, lib, stdenv, makeWrapper, age, apacheHttpd, mkpasswd, coreutils, wl-clipboard, ...}:
with boltons;

stdenv.mkDerivation rec {
	pname = "wachtwoord";
	version = "0.1";
	
	buildInputs = [ age apacheHttpd mkpasswd wl-clipboard ];
	nativeBuildInputs = [ makeWrapper ];

	unpackPhase = ":";
	
	explicit_dep_because_stripTabs_confuses_nix = ./wachtwoord;
	installPhase = stripTabs ''
		mkdir -p $out/bin
		cp ${./wachtwoord} $out/bin/wachtwoord
		chmod +x $out/bin/wachtwoord
		# https://github.com/deepfire/nixos-wiki/blob/master/Nix%20Runtime%20Environment%20Wrapper.page
		wrapProgram $out/bin/wachtwoord \
			--set DEPS_PATH ${lib.makeBinPath [ age apacheHttpd mkpasswd coreutils wl-clipboard ]}
	'';
}
