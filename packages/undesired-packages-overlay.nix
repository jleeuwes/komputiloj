let
	overridePkgSet = import ./undesired-packages-override.nix;
self: super: overridePkgSet "" super
