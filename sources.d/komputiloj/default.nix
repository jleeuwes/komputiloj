{
    value = { pkgs, utilecoj, ... }: {
        users = import ./users.d { inherit utilecoj; };
        pkgs = let
            callPackage = pkg: pkgs.callPackage pkg { inherit utilecoj; };
        in {
            wachtwoord = callPackage ./pkgs/wachtwoord;
            git-annex-remote-rclone = callPackage ./pkgs/git-annex-remote-rclone;
            radicale-commit-hook = callPackage ./pkgs/radicale-commit-hook;
        };
    };
}
