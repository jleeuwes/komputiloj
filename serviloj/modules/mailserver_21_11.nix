let release = "nixos-21.11";
in {
	imports = [
		(builtins.fetchTarball {
			url = "https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/archive/${release}/nixos-mailserver-${release}.tar.gz";
			sha256 = "1i56llz037x416bw698v8j6arvv622qc0vsycd20lx3yx8n77n44";
		})
	];
}
