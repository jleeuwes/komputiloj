# First set up our pinned NIX_PATH based on our sources.d:
source ../_pinned_NIX_PATH.sh
# And make sure we have the nixops command defined by our nixops source:
NIXOPS_DRV=$(nix-instantiate -E 'with import (<komputiloj> + /sources.nix); import nixops.unpacked')
NIXOPS_BUILT=$(nix-store --realise -- "$NIXOPS_DRV")
NIXOPS_CMD="$NIXOPS_BUILT"/bin/nixops

export NIXOPS_STATE=~/komputiloj/serviloj/state.nixops
export NIXOPS_DEPLOYMENT=serviloj

# Now make sure nothing else depends on NIX_PATH by making it empty (which would break such dependencies):
export NIX_PATH=
