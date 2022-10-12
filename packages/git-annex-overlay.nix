# https://nixos.wiki/wiki/Overlays#Overriding_a_version
self: super: {
	git-annex-remote-rclone = super.git-annex-remote-rclone.overrideAttrs (old: rec {
		version = "v0.7+1";
		rev = "0dc3baac40c5c7795fd1c6dac302dfe93f9bd5dd";
		src = super.fetchFromGitHub {
			inherit rev;
			owner = "DanielDent";
			repo = "git-annex-remote-rclone";
			sha256 = "1v2jmpapkxf9xzqs0chc2j5x3pw4rjrv2bvadhfspzl3n4myj63l";
		};
	});
}
