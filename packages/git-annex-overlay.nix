# https://nixos.wiki/wiki/Overlays#Overriding_a_version
self: super: {
	git-annex-remote-rclone = super.git-annex-remote-rclone.overrideAttrs (old: rec {
		version = "frankenstein";
		rev = "79f1a1bbda88238de822f21828cdf3df957dff92";
		src = super.fetchFromGitHub {
			inherit rev;
			owner = "jleeuwes";
			repo = "git-annex-remote-rclone";
			sha256 = "0c41mbfckwvrb45mhpggs43v6x9js05iyls7vjblpq3d5scmvcwi";
		};
	});
}
