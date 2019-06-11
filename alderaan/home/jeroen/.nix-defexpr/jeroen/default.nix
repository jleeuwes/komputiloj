{}:

let
  nixpkgs = import <nixpkgs> {};
  rereDsl = import "/home/jeroen/code/rere/rere-dsl" {};
  haskellEnv = haskellPackages: with haskellPackages; [ hint alsaPcm
  vector hinotify stm async uuid caseInsensitive parsec ]; # rereDsl ];
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

  ghc = nixpkgs.haskellPackages.ghcWithPackages haskellEnv;
}
