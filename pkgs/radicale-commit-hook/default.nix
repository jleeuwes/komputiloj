{ utilecoj, lib, stdenv, makeWrapper, git, openssh, mailutils, coreutils, ... }:
with utilecoj;

stdenv.mkDerivation rec {
	pname = "radicale-commit-hook";
	version = "0.1";

	buildInputs = [ git ];
	nativeBuildInputs = [ makeWrapper ];

	unpackPhase = ":";
	explicit_dep_because_stripTabs_confuses_nix = ./hook;
	installPhase = stripTabs ''
		mkdir -p $out/bin
		cp ${./hook} $out/bin/hook
		chmod +x $out/bin/hook
		# https://github.com/deepfire/nixos-wiki/blob/master/Nix%20Runtime%20Environment%20Wrapper.page
		wrapProgram $out/bin/hook \
			--set PATH ${lib.makeBinPath [ git openssh mailutils coreutils ]}
	'';
}
