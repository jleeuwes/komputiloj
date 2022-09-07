# This version of nixops must be checked out and build using nix-build
NIXOPS_CMD=~/code/nixops/result/bin/nixops
export NIXOPS_STATE=~/komputiloj/serviloj/state.nixops
export NIXOPS_DEPLOYMENT=serviloj
export NIX_PATH= # less room for error; serviloj.nix no longer depends on NIX_PATH
