# Inspiration taken from https://tech.ingolf-wagner.de/nixos/krops/

let
	krops = builtins.fetchGit {
		url = "https://cgit.krebsco.de/krops/";
		ref = "refs/tags/v1.24.1";
		rev = "c2fa48550f2bb46009b9cecdb9ac838dc402ce19";
	};
	lib = import "${krops}/lib";
	pkgs = import "${krops}/pkgs" {};

	source = name: lib.evalSource [
		{
			nixpkgs.git = {
				ref = "origin/nixos-20.09";
				url = https://github.com/NixOs/nixpkgs;
			};
			modules.file = toString ./modules;
			nixos-config.file = toString (./. + "/servers/${name}.nix"); # https://nixos.wiki/wiki/Nix_Expression_Language#Coercing_a_relative_path_with_interpolated_variables_to_an_absolute_path_.28for_imports.29
			"jeroen_rsa.pub".file = toString ../scarif/home/jeroen/.ssh/id_rsa.pub;
		}
	];

	gently = pkgs.krops.writeDeploy "deploy-gently" {
		source = source "gently";
		target = "root@gently.radstand.nl";
	};

in {
	gently = gently;
	all = pkgs.writeScript "deploy-all-servers" (
		lib.concatStringsSep "\n" [ gently ]
	);
}
