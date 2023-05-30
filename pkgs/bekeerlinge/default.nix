{ stdenv, fetchgit }:

stdenv.mkDerivation rec {
	name = "bekeerlinge";
	# version = "0.1";
	rev = "0ad0c619c5fa1fced78557d0ef27ed1c85982c4f";

	src = fetchgit { # fetchFromGitea {
		# domain = "thee.radstand.nl";
		# owner = "jeroen";
		# repo = "bekeerlinge";
		# private = true;
		url = "gitea@thee.radstand.nl:jeroen/bekeerlinge.git";
		inherit rev;
		sha256 = "1111111111111111111111111111111111111111111111111111";
	};

	installPhase = stripTabs ''
		cp * $out
	'';
}
