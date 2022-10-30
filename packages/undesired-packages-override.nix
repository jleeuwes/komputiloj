prefix:
with builtins;
with (import ../util.nix);
let
	traceMe = msg: trace ((__curPos.file) + ": " + msg);
	callOutPkgs = pkgs: x: traceMe (
		"Applying overrides to: " +
		toString (map (nm: prefix + nm) (attrNames pkgs))
	) x;
	apply = f: pkgs: callOutPkgs pkgs (mapAttrs (name: value: value.overrideAttrs f) pkgs);
	applyWithName = f: pkgs: callOutPkgs pkgs (mapAttrs (name: value: value.overrideAttrs (f name)) pkgs);
	
	describePkgAttr = name: pkg: "attr " + name + " (" + pkg.pname + " " + pkg.version + ")";
	markInsecure = vulnDesc: pkg: {
		meta = pkg.meta // {
			knownVulnerabilities = (pkg.meta.knownVulnerabilities or []) ++ [ vulnDesc ];
		};
	};
	markInsecureIf = f: vulnDesc: pkg: if (f pkg)
		then markInsecure vulnDesc pkg
		else pkg;
in
pkgs:
let
	openssl_packages = filterAttrs (a: substring 0 7 a.name == "openssl") pkgs;
	# NOTE: we can't check the version of the packages outside the override
	# (i.e. we can't define vulnerable_openssl_packages and map markInsecure on that)
	# because then nix(pkg) somehow tries to check against the version in the final package set,
	# leading to infinite recursion.
	openssl_vulnerable = pkg: true; # compareVersions pkg.version "3" >= 0;
in
	apply (markInsecureIf openssl_vulnerable "UNDISCLOSED") openssl_packages
