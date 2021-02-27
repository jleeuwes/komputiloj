#!/usr/bin/env bash

set -Eeu -o pipefail

# This version of nixops must be checked out and build using nix-build
NIXOPS_CMD=~/code/nixops/result/bin/nixops

# We probably want some pinning as used by
# https://www.ryantm.com/blog/nixops-without-sharing/ or
# https://github.com/nh2/nixops-tutorial
# but I don't understand NIX_PATH/channels/nixpkgs enough at this moment;
# So for now we'll just use the channel we also use on our local machine.

export NIXOPS_STATE="./state.nixops"

function create_deployment() {
	if [ `"$NIXOPS_CMD" list | fgrep -c "$1"` -eq 0 ]; then
		(set -x; "$NIXOPS_CMD" create "$1".nix --deployment "$1")
	fi
}

create_deployment serviloj

exec "$NIXOPS_CMD" "$@"
