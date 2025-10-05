{ platform, ... }:
with builtins;
let
source = import ./.;
self = {
    # TODO do we need outPath?
    outPath = source.nix_path;

    all = platform.lib.makeAll self;
    
    pins.nixos-mailserver = {
        gitlab_repo = readFile ./gitlab_repo;
        git_branch = readFile ./git_branch;
    };

    nixosModules = {
        mailserver = source.value;
    };
    
}; in self
