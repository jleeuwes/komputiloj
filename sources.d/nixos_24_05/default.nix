# GENERATED
with builtins;
rec {
    nix_path = fetchTarball {
        url    = "https://releases.nixos.org/nixos/24.05/nixos-24.05.4736.68e7dce0a653/nixexprs.tar.xz";
        sha256 = "0f998fpannr1xzhygj63dzn6y4wfhrrknd5xiakblv1m80wq1c7w";
    };
    value = import nix_path;
}
