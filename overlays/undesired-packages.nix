{ boltons, ... }:
with boltons;
let
	traceMe = msg: trace ((__curPos.file) + ": " + msg);
	callOutPkgs = pkgs: x: traceMe ("Applying overrides to: " + toString (attrNames pkgs)) x;
	apply = f: pkgs: callOutPkgs pkgs (mapAttrs (name: value: value.overrideAttrs f) pkgs);
	
	markInsecure = vulnDesc: pkg: {
		meta = pkg.meta // {
			knownVulnerabilities = (pkg.meta.knownVulnerabilities or []) ++ [ vulnDesc ];
		};
	};
	markInsecureIf = f: vulnDesc: pkg: if (f pkg)
		then markInsecure vulnDesc pkg
		else pkg;
in
self: super:
let
	openssl_packages = filterAttrs (a: substring 0 7 a.name == "openssl") super;
	# NOTE: we can't check the version of the packages outside the override
	# (i.e. we can't define vulnerable_openssl_packages and map markInsecure on that)
	# because then nix(pkg) somehow tries to check against the version in the final package set,
	# leading to infinite recursion.
	# openssl_vulnerable = pkg: compareVersions pkg.version "3" >= 0
	# 	&& compareVersions pkg.version "3.0.7" < 0;
	openssl_vulnerable = pkg: true;
in
	apply (markInsecureIf openssl_vulnerable "UNDISCLOSED") openssl_packages
