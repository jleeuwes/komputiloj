sources_path=/home/jeroen/komputiloj/sources.nix
nixpkgs_path=$(nix-instantiate --eval "$sources_path" -A nixpkgs.unpacked | egrep -o '[^"]+')
export NIX_PATH="sources=$sources_path:nixpkgs=$nixpkgs_path:nixos-config=/etc/nixos/configuration.nix"
