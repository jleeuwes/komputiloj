komputiloj_path=/home/jeroen/komputiloj
nixpkgs_path=$(nix-instantiate --eval "$komputiloj_path"/sources.nix -A nixpkgs.unpacked | egrep -o '[^"]+')
export NIX_PATH="komputiloj=$komputiloj_path:nixpkgs=$nixpkgs_path:nixos-config=/etc/nixos/configuration.nix"
