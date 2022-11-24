{
	value = pkgs: let inherit (pkgs) callPackage; in {
		wachtwoord = pkgs.callPackage ./pkgs/wachtwoord {};
		git-annex-remote-rclone = pkgs.callPackage ./pkgs/git-annex-remote-rclone {};
	};
}
