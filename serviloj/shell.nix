with import <nixpkgs> {};

stdenv.mkDerivation rec {
	# Not sure what the use of this name is:
	name = "serviloj-deployment-shell";

	buildInputs = [
		nixops
	];

	# We probably want some pinning as used by
	# https://www.ryantm.com/blog/nixops-without-sharing/ or
	# https://github.com/nh2/nixops-tutorial
	# but I don't understand NIX_PATH/channels/nixpkgs enough at this moment;
	# So for now we'll just use the channel we also use on our local machine.

	shellHook = ''
		export NIXOPS_STATE="./state.nixops"

		function create_deployment() {
			if [ `nixops list | fgrep -c "$1"` -eq 0 ]; then
				(set -x; nixops create "$1".nix --deployment "$1")
			fi
		}

		create_deployment serviloj
	'';
}
