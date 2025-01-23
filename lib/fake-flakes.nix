with builtins; {
    info_from_nixpkgs = nix_path: let
        versionSuffix = readFile (nix_path + "/.version-suffix");
        versionParts = filter (part: part != []) (split "\\." versionSuffix);
        number = elemAt versionParts 1;
        shortRevision = elemAt versionParts 2;
        fullRevision = readFile (nix_path + "/.git-revision");
        in {
            rev = fullRevision;
            shortRev = shortRevision;
            lastModifiedDate = number;
        };
}
