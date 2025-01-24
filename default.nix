let boltons = import ./lib/boltons.nix;
in with boltons;
let
    sources = importDir ./sources.d;
    default_nixos = "nixos_24_11"; # defines default nixos used by all of komputiloj
    default_nixos_source = getAttr default_nixos sources;
    default_mailserver_source = sources.mailserver_24_11; # defined here to not forget updating nixos and mailserver together
    callPackageWith = capsules.nixpkgsCurrent.lib.callPackageWith;
    fake_flakes = import ./lib/fake-flakes.nix;
    komputiloj_capsule = {
        users = importDir ./users.d;

        domains = importDir ./domains.d;

        commands = importDirAndApply ./commands.d capsules_and_boltons;

        packages = let
            callPackage = pkg: callPackageWith capsules.nixpkgsCurrent.packages pkg capsules_and_boltons;
        in rec {
            wachtwoord = callPackage ./pkgs/wachtwoord;
            radicale-commit-hook = callPackage ./pkgs/radicale-commit-hook;
            tipctl = callPackage ./pkgs/tipctl;
            dekstop = callPackage ./pkgs/dekstop;
            rip = callPackage ./pkgs/rip;
            # git-annex-remote-rclone is not here but in nixpkgsFuture,
            # because we will remove our copy of the package when the
            # desired version is included in nixpkgs
        };

        lib = {
            # TODO this is a 'metapackage', not a lib, because its system-dependent!
            writeCommand = args: let
                # TODO somehow enhance writeShellApplication to produce a clean PATH inside the script
                # Currently it is too easy to rely on some undeclared dependency.
                scriptDir = capsules.nixpkgsCurrent.packages.writeShellApplication {
                    name = "script";
                    runtimeInputs = args.runtimeInputs or [];
                    text = args.text;
                };
            in capsules.nixpkgsCurrent.packages.concatScript args.name [ "${scriptDir}/bin/script" ];
        };

        modules = importDirAndApply ./modules.d capsules_and_boltons;

        overlays = rec {
            undesired-packages = import ./overlays/undesired-packages.nix { inherit boltons; };
        };
        
        machines = importDirAndApply ./machines.d capsules_and_boltons;
    };
    real_capsules = {
        komputiloj = komputiloj_capsule;
        privata = sources.komputiloj-privata.value capsules_and_boltons;
        hello-infra = sources.hello-infra.value capsules_and_boltons;
        gorinchemindialoog = sources.gorinchemindialoog.value;
        wolk = (import ./apps/wolk) capsules_and_boltons;
        thee = (import ./apps/thee) capsules_and_boltons;
    };
    all_capsule = let
        cs = attrValues (real_capsules // fake_capsules);
    in {
        # special capsule which aggregates stuff from all other capsules
        users = mergeAttrsets (catAttrs "users" cs);
        lib = mergeAttrsets (catAttrs "lib" cs);
        domains = mergeAttrsets (catAttrs "domains" cs);
    };
    fake_capsules = rec {
        nixpkgsCurrent = let
            nixpkgs = default_nixos_source.value.outputs { self = nixpkgs; }
                // {
                    # If you import the outPath, it should result in 'old' nixpkgs. See https://stackoverflow.com/a/74465514
                    outPath = default_nixos_source.nix_path;
                    
                    # Fake some stuff to make lib/flake-version-info.nix return the same values
                    # as before we used the flake. Because we're not _really_ using flakes (yet),
                    # the flake version info otherwise gives a weird store path
                    # (like nixos-system-ferrix-24.05.19700101.dirty).
                    # See https://nix.dev/manual/nix/2.24/command-ref/new-cli/nix3-flake.html#flake-references
                    # for some of these 'magic' attrs that the flakes system usually sets.
                    inherit
                        (fake_flakes.info_from_nixpkgs default_nixos_source.nix_path)
                        rev shortRev lastModifiedDate;
                };
        in rec {
            outPath = nixpkgs.outPath;
            
            # TODO move closer to flakes by having packages.x86_64-linux
            packages = nixpkgs.legacyPackages.x86_64-linux;

            inherit (nixpkgs) lib;
            
            # TODO call this nixosModules
            modules = nixpkgs.nixosModules // {
                # Is it wise to put extra stuff in this capsule?
                # We do it because the mailserver is closely linked to the NixOS
                # (i.e. nixpkgs) version.
                mailserver = default_mailserver_source.value;
            };
        };
        nixpkgsFuture = let
            override = old: new: trace ("🕑🕐🕛 nixpkgsFuture: Replacing " + old.name + " with " + new.name) new;
        in {
            # NOTE: Each time we use something from unstable, we should pin
            # that exact source. That way, we can someday move away from the
            # unstable version when it hits the stable channels.  If we would
            # take packages directly from one perpetually updated unstable
            # source, we will never catch up.
        };
        nextcloud = {
            packages = {
                nextcloud = nixpkgsCurrent.packages.nextcloud29;
                apps = sources.nextcloud_29_apps.value nixpkgsCurrent.packages;
            };
        };
    };
    capsules = real_capsules // fake_capsules // { all = all_capsule; };
    capsules_and_boltons = capsules // { inherit boltons; };
in {
    boltons = boltons;
    default_nixos = default_nixos; # extracted by komputiloj script
    sources = sources;
    capsules = capsules;
    apps.thee = import ./apps/thee {
        inherit boltons;
        inherit capsules;
    };
    commands = let
        meta_commands = {};
        capsule_commands = capsuleName: capsule: mapNames (commandName: "${capsuleName}.${commandName}") (capsule.commands or {});
        per_capsule_commands = attrValues (mapAttrs capsule_commands capsules);
    # We flatten the commands so we can easily pull one out with getAttr
    in mergeAttrsets ([ meta_commands capsules.komputiloj.commands ] ++ per_capsule_commands );
}
