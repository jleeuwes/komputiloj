# This is not a Nix concept, but a roll-your-own pinning system that replaces channels.
# Entries are used as a kind of channels in our configuration.nix.
# One entry, which must be named nixpkgs, is special:
# nixos-rebuild is hardcoded to load <nixpkgs/nixos>, which in turn loads our configuration.nix,
# so by the time configuration.nix is evaluated, the 'main' nixpkgs is already chosen.
# We work around this with some scripting that bootstraps a minimal NIX_PATH from the nixpkgs entry in this file.
#
# Some resources that might come in handy and/or inspired this stuff:
# - https://github.com/NixOS/nixpkgs/issues/62832
# - https://github.com/NixOS/nixpkgs/issues/35411#issuecomment-368172579
# - https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH
# - https://nix.dev/reference/pinning-nixpkgs
# 
# TODO: implement a script that can update these 'channels' based on channel_url.
#       Maybe useful: https://unix.stackexchange.com/questions/45325/get-urls-redirect-target-with-curl
rec {
	nixos_18_09 = rec {
		channel_url = "https://channels.nixos.org/nixos-18.09";
		current_url = "https://releases.nixos.org/nixos/18.09/nixos-18.09.2574.a7e559a5504/nixexprs.tar.xz";
		# This needs to be obtained with nix-prefetch-url --unpack - you can't use the hash listed
		current_sha = "0f3y936zqblzvl76gpd9awamfyxxqck3y6z82hsq50d3bmb779zx";
		unpacked = builtins.fetchTarball {
			url = current_url; # "https://releases.nixos.org/nixos/18.09/nixos-18.09.2574.a7e559a5504/nixexprs.tar.xz";
			sha256 = current_sha; # "0f3y936zqblzvl76gpd9awamfyxxqck3y6z82hsq50d3bmb779zx";
		};
	};
	nixpkgs = rec {
		channel_url = "https://channels.nixos.org/nixos-20.09";
		current_url = "https://releases.nixos.org/nixos/20.09/nixos-20.09.4407.1c1f5649bb9/nixexprs.tar.xz";
		# This needs to be obtained with nix-prefetch-url --unpack - you can't use the hash listed
		current_sha = "17k7lac5gkfs8fz0ynw6wbgbqx1qsgzgdilggq7rqylm9b4nn6iw";
		unpacked = builtins.fetchTarball {
			url = current_url;
			sha256 = current_sha;
		};
	};
	unstable = rec {
		channel_url = "https://channels.nixos.org/nixos-unstable";
		current_url = "https://releases.nixos.org/nixos/unstable/nixos-22.11pre396557.5857574d459/nixexprs.tar.xz";
		current_sha = "16l0k5w5xb5d3jiiqc4jdhxn3szbsccs3i2lr7gbwdqafq6lnxwh";
		unpacked = builtins.fetchTarball {
			url = current_url;
			sha256 = current_sha;
		};
	};
}
