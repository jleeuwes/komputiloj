# First set up NIX_PATH to safely (?) inject KOMPUTILOJ_PATH into the next nix expression:
export NIX_PATH="komputiloj=$KOMPUTILOJ_PATH"
# Make sure we have the nixops command defined by our nixops source:
NIXOPS_DRV=$(nix-instantiate -E 'with import (<komputiloj> + /sources.nix); import nixops.unpacked')
NIXOPS_BUILT=$(nix-store --realise -- "$NIXOPS_DRV")
NIXOPS_CMD="$NIXOPS_BUILT"/bin/nixops

export NIXOPS_STATE="$KOMPUTILOJ_PATH"/serviloj/state.nixops
export NIXOPS_DEPLOYMENT=serviloj

# Now make sure nothing else can depend on NIX_PATH by making it empty:
export NIX_PATH=
