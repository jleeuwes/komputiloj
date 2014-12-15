{}:

let
  nixpkgs = import <nixpkgs> {};
in {
  dmenu = nixpkgs.dmenu.override { enableXft = true; };
  
  ledger = nixpkgs.lib.overrideDerivation nixpkgs.ledger3 (attrs: {
    name = "ledger-3.1";
    src = nixpkgs.fetchgit {
      url = "git://github.com/ledger/ledger.git";
      rev = "720c03b139";
      sha256 = "1l5y4k830jyw7n1nnhssci3qahq091fj5cxcr77znk20nclz851s";
    };
  });
}
