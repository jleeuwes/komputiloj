{
    value = { nixpkgs, utilecoj, ... }: {
        users = import ./users.d { inherit utilecoj; };
        packages = let
            callPackage = pkg: nixpkgs.callPackage pkg { inherit utilecoj; };
        in {
            wachtwoord = callPackage ./pkgs/wachtwoord;
            git-annex-remote-rclone = callPackage ./pkgs/git-annex-remote-rclone;
            radicale-commit-hook = callPackage ./pkgs/radicale-commit-hook;
        };
    };
}
