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
# 
# Currently, to update:
#
# 1. Go to the channel_url
# 2. Put the url to which you are redirected in current_url, appending /nixexprs.tar.xz
# 3. Call `nix-prefetch-url --unpack` with the current_url
#    and put the resulting hash in current_sha.
#    WARNING: if you forget to update the hash, nix will keep using the old unpacked tarball, as long as it is available.
#
# TODO: we might need to do some trickery to make sure the actively used channels are not gc'ed:
# https://discourse.nixos.org/t/pinned-nixpkgs-keeps-getting-garbage-collected/12912/6
rec {
	nixos_18_09 = rec {
		channel_url = "https://channels.nixos.org/nixos-18.09";
		current_url = "https://releases.nixos.org/nixos/18.09/nixos-18.09.2574.a7e559a5504/nixexprs.tar.xz";
		current_sha = "0f3y936zqblzvl76gpd9awamfyxxqck3y6z82hsq50d3bmb779zx";
		unpacked = builtins.fetchTarball {
			url = current_url; # "https://releases.nixos.org/nixos/18.09/nixos-18.09.2574.a7e559a5504/nixexprs.tar.xz";
			sha256 = current_sha; # "0f3y936zqblzvl76gpd9awamfyxxqck3y6z82hsq50d3bmb779zx";
		};
	};
	nixpkgs = rec {
		channel_url = "https://channels.nixos.org/nixos-21.11";
		current_url = "https://releases.nixos.org/nixos/21.11/nixos-21.11.337975.eabc3821918/nixexprs.tar.xz";
		current_sha = "1fq3zz7qfavksdbqvicns7hg61q3hhbxs2ibm818gy629hwkvsvm";
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