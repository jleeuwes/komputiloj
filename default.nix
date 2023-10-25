# TODO this file should become the anchor point for all komputiloj operations
let boltons = import ./lib/boltons.nix;
in with boltons;
let
    sources = importDir ./sources.d;
    default_nixos = "nixos_23_05"; # defines default nixos used by all of komputiloj
    default_nixos_source = getAttr default_nixos sources;
    default_mailserver_source = sources.mailserver_23_05; # defined here to not forget updating nixos and mailserver together
    nixpkgsLib = import (default_nixos_source.nix_path + "/lib");
    callPackageWith = nixpkgsLib.callPackageWith;
    komputiloj_capsule = {
        users = importDir ./users.d;

        commands = importDirAndApply ./commands.d capsules_and_boltons;

        packages = let
            callPackage = pkg: callPackageWith capsules.nixpkgsCurrent.packages pkg capsules_and_boltons;
        in rec {
            wachtwoord = callPackage ./pkgs/wachtwoord;
            radicale-commit-hook = callPackage ./pkgs/radicale-commit-hook;
            tipctl = callPackage ./pkgs/tipctl;
            dekstop = callPackage ./pkgs/dekstop;
            # git-annex-remote-rclone is not here but in nixpkgsFuture,
            # because we will remove our copy of the package when the
            # desired version is included in nixpkgs
        };

        lib = {
            writeCommand = args: let
                scriptDir = capsules.nixpkgsCurrent.lib.writeShellApplication {
                    name = "script";
                    runtimeInputs = args.runtimeInputs or [];
                    text = args.text;
                };
            in capsules.nixpkgsCurrent.lib.concatScript args.name [ "${scriptDir}/bin/script" ];
        };

        modules = importDirAndApply ./modules.d capsules_and_boltons;

        overlays = rec {
            undesired-packages = import ./overlays/undesired-packages.nix { inherit boltons; };
            rclone-deterministic-obscure = import ./overlays/rclone-deterministic-obscure.nix { inherit boltons; };
        };
        
        machines = importDirAndApply ./machines.d capsules_and_boltons;
    };
    real_capsules = {
        komputiloj = komputiloj_capsule;
        hello-infra = sources.hello-infra.value capsules_and_boltons;
        gorinchemindialoog = sources.gorinchemindialoog.value;
        wolk = (import ./apps/wolk) capsules_and_boltons;
    };
    all_capsule = let
        cs = attrValues (real_capsules // fake_capsules);
    in {
        # special capsule which aggregates stuff from all other capsules
        users = mergeAttrsets (catAttrs "users" cs);
        lib = mergeAttrsets (catAttrs "lib" cs);
    };
    fake_capsules = rec {
        nixpkgsCurrent = let
            # TODO use pkgs/top-level/default.nix instead of default.nix
            # (which calls pkgs/top-level/impure.nix)
            nixpkgs = default_nixos_source.value {
                overlays = [
                    capsules.komputiloj.overlays.rclone-deterministic-obscure
                    # TODO move overlays that should be global from machines to here
                ];
            };
            nixpkgsModule = {
                config.nixpkgs.pkgs = nixpkgs;
            ];
        in rec {
            nixPath = default_nixos_source.nix_path;

            # NOTE: This is actually very wrong:
            # nixpkgs {} is impure and selects the architecture of the current
            # system. So these packages won't be good for other hosts than
            # scarif. Currently we get away with it because gently and scarif
            # have the same architecture (x86_64).
            packages = nixpkgs; # TODO remove libs and other non-package stuff

            lib = nixpkgs.lib // {
                # We need to distribute callPackageWith to imported files. Is this really the best way to do that?
                inherit callPackageWith;

                # Prepare a function to make a nixosSystem with the nixpkgs we are defining here,
                # including overlays, while still allowing per-machine nixpkgs.overlays.
                # TODO use something less impure (like we want to do with nixpkgs, see above)
                nixosSystem = let evalConfig = import (default_nixos_source.nix_path + "/nixos/lib/eval-config.nix");
                              in attrs: evalConfig (attrs // { modules = attrs.modules ++ [ nixpkgsModule ]; });

                writeShellApplication = packages.writeShellApplication;
                writeShellScript = packages.writeShellScript;
                concatScript = packages.concatScript;
            };

            modules = {
                # Is it wise to put extra stuff in this capsule?
                # We do it because the mailserver is closely linked to the NixOS
                # (i.e. nixpkgs) version.
                mailserver = default_mailserver_source.value;
            };
        };
        # TODO rename this to nixpkgsFiddled or something and use this instead of overlays as much as possible.
        nixpkgsFuture = let
            override = old: new: trace ("🕑🕐🕛 nixpkgsFuture: Replacing " + old.name + " with " + new.name) new;
            override' = icon: old: f: let new = f old; in trace (icon + " Replacing " + old.name + " with " + new.name) new;
            replaceWithNewerVersion = override' "🕑🕐🕛";
            replaceWithPatchedVersion = override' "🩹🩹🩹";
        in {
            # NOTE: Each time we use something from unstable, we should pin
            # that exact source. That way, we can someday move away from the
            # unstable version when it hits the stable channels.  If we would
            # take packages directly from one perpetually updated unstable
            # source, we will never catch up.
            packages = {
                git-annex-remote-rclone = replaceWithNewerVersion
                    nixpkgsCurrent.packages.git-annex-remote-rclone
                    (original: callPackageWith nixpkgsCurrent.packages ./pkgs/git-annex-remote-rclone {});
                rcloneWithMountableOnTheFlyRemotes = replaceWithPatchedVersion
                    nixpkgsCurrent.packages.rclone
                    (original: original.overrideAttrs (oldAttrs: {
                        version = oldAttrs.version + "-with-deterministic-obscure";
                        patches = [ ./rclone-deterministic-obscure.patch ];
                    }));
            };
        };
        nextcloud = {
            packages = {
                nextcloud = nixpkgsCurrent.packages.nextcloud26;
                apps = sources.nextcloud_26_apps.value nixpkgsCurrent.packages;
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
