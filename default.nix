let boltons = import ./lib/boltons.nix;
in with boltons;
let
    sources = importDir ./sources.d;
    default_nixos = "nixos_25_05"; # defines default nixos used by all of komputiloj
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
            moz-recover-as-bookmarks = callPackage ./pkgs/moz-recover-as-bookmarks;
            sleutel = callPackage ./pkgs/sleutel;
            radicale-commit-hook = callPackage ./pkgs/radicale-commit-hook;
            tipctl = callPackage ./pkgs/tipctl;
            dekstop = callPackage ./pkgs/dekstop;
            rip = callPackage ./pkgs/rip;
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
        
        machines = importDirAndApply ./machines.d (capsules_and_boltons // {
            inherit (new_capsules) nixos_25_05 mailserver_25_05;
        });
    };
    real_capsules = {
        komputiloj = komputiloj_capsule;
        privata = sources.komputiloj-privata.value capsules_and_boltons;
        hello-infra = sources.hello-infra.value (capsules_and_boltons // {
            inherit (new_capsules) nixos_25_05;
        });
        gorinchemindialoog = sources.gorinchemindialoog.value;
        sleutel = (import ./apps/sleutel) capsules_and_boltons;
        wolk = (import ./apps/wolk) capsules_and_boltons;
        thee = (import ./apps/thee) capsules_and_boltons;
        notie = (import ./apps/notie) capsules_and_boltons;
    };
    all_capsule = let
        cs = attrValues (real_capsules // fake_capsules);
    in {
        # special capsule which aggregates stuff from all other capsules
        # TODO how do new capsules fit into this, if at all?
        users = mergeAttrsets (catAttrs "users" cs);
        lib = mergeAttrsets (catAttrs "lib" cs);
        domains = mergeAttrsets (catAttrs "domains" cs);
    };
    new_capsules = {
        boltons = {
            lib = boltons;
        };
        platform = {
            localSystem = builtins.currentSystem; # IMPURE. Make this a pin?
        };
        nixos_25_05 = import ./sources.d/nixos_25_05/capsule.nix {
            inherit (new_capsules) platform boltons;
        };
        mailserver_25_05 = import ./sources.d/mailserver_25_05/capsule.nix;
    };
    fake_capsules = rec {
        # TODO get rid of this backwards compatible abomination
        nixpkgsCurrent = let nixpkgs = getAttr default_nixos new_capsules; in
            nixpkgs // {
                # TODO move closer to flakes by getting rid of x64_64-linux default
                #      (i.e. the part before the // operator)
                packages = nixpkgs.legacyPackages.x86_64-linux // nixpkgs.legacyPackages;

                # TODO remove this and use nixosModules everywhere instead
                modules = nixpkgs.nixosModules;
            };
        nixpkgsFuture = let
            override = old: new: trace ("🕑🕐🕛 nixpkgsFuture: Replacing " + old.name + " with " + new.name) new;
            unstable = sources.nixos_unstable.value.outputs { self = unstable; }
                // {
                    outPath = sources.nixos_unstable.nix_path;
                    inherit
                        (fake_flakes.info_from_nixpkgs sources.nixos_unstable.nix_path)
                        rev shortRev lastModifiedDate;
                };
        in {
            # NOTE: Each time we use something from unstable, we should pin
            # that exact source. That way, we can someday move away from the
            # unstable version when it hits the stable channels.  If we would
            # take packages directly from one perpetually updated unstable
            # source, we will never catch up.

            packages.aarch64-linux = let
                callPackage = pkg: callPackageWith capsules.nixpkgsCurrent.packages.aarch64-linux pkg;
            in {
                ebusd = override
                    nixpkgsCurrent.packages.aarch64-linux.ebusd
                    (callPackage ./pkgs/ebusd {});
            };
            packages.x86_64-linux = let
                callPackage = pkg: callPackageWith capsules.nixpkgsCurrent.packages.x86_64-linux pkg;
            in {
                silverbullet = override
                    nixpkgsCurrent.packages.x86_64-linux.silverbullet
                    (callPackage ./pkgs/silverbullet {});
                ollama = override
                    nixpkgsCurrent.packages.x86_64-linux.ollama
                    unstable.legacyPackages.x86_64-linux.ollama;

            };
        };
        nextcloud = {
            packages = let
                callPackage = pkg: callPackageWith capsules.nixpkgsCurrent.packages.x86_64-linux pkg;
            in {
                # nextcloud = nixpkgsCurrent.packages.nextcloud31;
                # nextcloud = callPackage ./pkgs/nextcloud31-systemtags {}; #  { nextcloud = nixpkgsCurrent.packages.nextcloud31; };
                nextcloud = (import ./pkgs/nextcloud31-systemtags) { nextcloud31 = nixpkgsCurrent.packages.nextcloud31; };
                apps = sources.nextcloud_31_apps.value nixpkgsCurrent.packages;
            };
        };
        raspberry-pi-nix = (sources.flake-compat.value {
            # warning: raspberry-pi-nix will use its own version of nixpkgs as
            # defined in its flake.lock file because of how flake-compat works
            # TODO switch to flakes ourselves so we can upgrade nixpkgs,
            # or, if that takes too long, hack on flake-compat so we can pass
            # our own nixpkgs
            src = sources.raspberry-pi-nix.nix_path;
        }).outputs;
    };
    sloppy_capsules = real_capsules // fake_capsules // { all = all_capsule; };
    capsules = new_capsules // sloppy_capsules;
    # TODO get rid of passing capsules_and_boltons everywhere.
    # work towards an explicit dependency order
    capsules_and_boltons = sloppy_capsules // { inherit boltons; };
in {
    boltons = boltons;
    default_nixos = default_nixos; # extracted by komputiloj script
    sources = sources;
    capsules = capsules;
    # waarom staat dit hier?
    # apps.thee = import ./apps/thee {
    #     inherit boltons;
    #     inherit capsules;
    # };
    commands = let
        meta_commands = {};
        capsule_commands = capsuleName: capsule: mapNames (commandName: "${capsuleName}.${commandName}") (capsule.commands or {});
        per_capsule_commands = attrValues (mapAttrs capsule_commands capsules);
    # We flatten the commands so we can easily pull one out with getAttr
    in mergeAttrsets ([ meta_commands capsules.komputiloj.commands ] ++ per_capsule_commands );
}
