with builtins;
let
    source = import ./.;
in {
    # TODO do we need outPath?
    outPath = source.nix_path;
    
    pins.nixos-mailserver = {
        gitlab_repo = readFile ./gitlab_repo;
        git_branch = readFile ./git_branch;
    };

    nixosModules = {
        mailserver = source.value;
    };
    
}
