{ lib, legacyPackages, ... }:
let
    esc = lib.strings.escapeShellArg;
    channel_url = "https://channels.nixos.org/nixos-25.11";
in {
    inherit channel_url;
    lock = import ./lock.nix;
    updateScript = let
        nix-instantiate = "${legacyPackages.nix}/bin/nix-instantiate";
        nix-prefetch-url = "${legacyPackages.nix}/bin/nix-prefetch-url";
        curl = "${legacyPackages.curl}/bin/curl";
    in ''
        channel_url=${esc channel_url}
        current_overview_url=$(${curl} --head --silent --write-out "%{redirect_url}\n" --output /dev/null "$channel_url")
        if [[ -z "$current_overview_url" ]]; then
        	echo "Could not determine current url for channel $channel_url" >&2
        	echo "Likely cause: a typo in the channel name." >&2
        	echo "Less likely cause: a change in nixos.org redirect structure." >&2
        	exit 2
        fi
        current_url="$current_overview_url"/nixexprs.tar.xz
        current_sha=$(${nix-prefetch-url} --unpack "$current_url")
        cat > ${esc (toString ./lock.nix)} <<-EOF
        	# GENERATED
        	with builtins;
        	rec {
        	    url = "$current_url";
        	    sha256 = "$current_sha";
        	    nix_path = fetchTarball {
        	        inherit url sha256;
        	    };
        	}
        EOF
    '';
}
