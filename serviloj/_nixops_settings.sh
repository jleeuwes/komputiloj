# This version of nixops must be checked out and build using nix-build
NIXOPS_CMD=~/code/nixops/result/bin/nixops
export NIXOPS_STATE=~/komputiloj/serviloj/state.nixops
export NIXOPS_DEPLOYMENT=serviloj
# Use our pinned version of nixpkgs:
export NIX_PATH=nixpkgs=../nixpkgs
