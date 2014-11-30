{}:

let
  nixpkgs = import <nixpkgs> {};
in {
  dmenu = nixpkgs.dmenu.override { enableXft = true; };
}
