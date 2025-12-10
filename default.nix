let boltons = import ./lib/boltons.nix;
in with boltons;
let
    sources = importDir ./sources.d;
    default_nixos = "nixos_25_05"; # defines default nixos used by all of komputiloj
    callPackageWith = capsules.nixpkgsCurrent.lib.callPackageWith;
    fake_flakes = import ./lib/fake-flakes.nix;
    komputiloj_capsule = {
        
        # capsules (other than komputiloj) should be defined here eventually,
        # making new_capsules and the separate result attrset of default.nix unnecessary
        # Currently this is used for the 'all' construct.
        capsules = new_capsules;

        all = new_capsules.platform.lib.makeAll komputiloj_capsule;

        users = importDir ./users.d;

        domains = importDir ./domains.d;

        commands = importDirAndApply ./commands.d (capsules_and_boltons // {
            inherit (new_capsules) nixos_25_05 command-platform;
        });

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
            # TODO get rid of this, this is a 'metapackage', not a lib, because its system-dependent!
            writeCommand = warn "DON'T USE komputiloj.lib.writeCommand. Use the packageBuilder from command-platform." (args: let
                # TODO somehow enhance writeShellApplication to produce a clean PATH inside the script
                # Currently it is too easy to rely on some undeclared dependency.
                scriptDir = capsules.nixpkgsCurrent.packages.writeShellApplication {
                    name = "script";
                    runtimeInputs = args.runtimeInputs or [];
                    text = args.text;
                };
            in capsules.nixpkgsCurrent.packages.concatScript args.name [ "${scriptDir}/bin/script" ]);
        };

        modules = importDirAndApply ./modules.d {
            # NEVER pass new_capsules as a whole
            # The capsules we inherit in each output (modules, packages, ...)
            # will be the dependencies of the proper komputiloj capsule.
            inherit (new_capsules) boltons komputiloj-privata hello-infra nixos_25_11;

            # pass the capsule that we are constructing to itself.
            # TODO we would like to get rid of such magic
            komputiloj = komputiloj_capsule;
        };

        overlays = rec {
            undesired-packages = import ./overlays/undesired-packages.nix { inherit boltons; };
        };
        
        machines = importDirAndApply ./machines.d (capsules_and_boltons // {
            inherit (new_capsules) komputiloj-bootstrap nixos_25_05 mailserver_25_05 hello-infra;
        });
    };
    real_capsules = {
        komputiloj = komputiloj_capsule;
        privata = new_capsules.komputiloj-privata; # alias, TODO get rid of it
        gorinchemindialoog = sources.gorinchemindialoog.value;
        sleutel = (import ./apps/sleutel) capsules_and_boltons;
        wolk = (import ./apps/wolk) capsules_and_boltons;
        thee = (import ./apps/thee) capsules_and_boltons;
        notie = (import ./apps/notie) capsules_and_boltons;
    };
    all_capsule = let
        cs = attrValues (named (real_capsules // fake_capsules // new_capsules));
        prefixWith = pre: s: "${pre}:${s}";
        flatten = aspect:
            mergeAttrsets (map (capsule: mapNames (prefixWith capsule.name) (aspect capsule)) cs);
    in {
        # special capsule which aggregates stuff from all other capsules
        # TODO: Put the things that use this capsule into a new capsule.
        #       Let that capsules depend on a proper komputiloj capsule (work in progress)
        #       that exports all other capsules.
        #       The flattening that happens here should be applied to all capsules
        #       by way of some helper function that each capsule applies to itself.
        users = flatten (capsule: capsule.users or {});
        domains = flatten (capsule: capsule.domains or {});
    };
    new_capsules = {
        # Keep STRICT dependency order here and ONLY take from new_capsules!
        boltons = {
            lib = boltons;
        };
        platform = {
            localSystem = builtins.currentSystem; # IMPURE. Make this a pin?
            emulatedSystems = attrNames (readDir /run/binfmt); # IMPURE

            lib.makeAll = capsule:
                let
                    prefixAttrs = pre: mapNames (nm: "${pre}:${nm}");
                    subcapsules = attrValues (named (capsule.capsules or {}));
                in {

                    # TODO: I'm not sure how much generic manipulation I want to do on capsules.
                    # Things like applying `named` to all attrsets seems like a tempting convenience,
                    # but if we do that, it becomes important when we apply such a `wrapCapsule` function
                    # when constructing a capsule. If passing self within a capsule, do we want that
                    # to be the 'raw' capsule or the capsule with `wrapCapsule` applied?
                    # Do we want that to matter (much)?
                    # Maybe we're stepping into the framework versus library trap here.
                    # Let's do it like this for now:
                    #
                    # 1. Don't do any manipulation. Only provide a function to construct the `all` attr.
                    # 2. Use convention for that (like this whole capsule business already is)
                    #    and be robust/lenient where possible.
                    # 3. Maybe provide a function to apply to the whole capsule
                    #    that _checks_ our conventions without doing any manipulation.
                    
                    # CONVENTION: a capsule's all should have the same structure as a capsule (except it should not have all itself)
                    # TODO: warn about missing all
                    capsules = mergeAttrsets ([capsule.capsules or {}] ++ map (subcapsule: prefixAttrs subcapsule.name (subcapsule.all.capsules or {})) subcapsules);
                    commands = mergeAttrsets ([capsule.commands or {}] ++ map (subcapsule: prefixAttrs subcapsule.name (subcapsule.all.commands or {})) subcapsules);
                    pins     = mergeAttrsets ([capsule.pins     or {}] ++ map (subcapsule: prefixAttrs subcapsule.name (subcapsule.all.pins     or {})) subcapsules);
                };
        };
        flake-compat = {
            all = new_capsules.platform.lib.makeAll new_capsules.flake-compat;

            lib.flake-compat = sources.flake-compat.value;

            pins = {
                flake-compat = {
                    # TODO use an actual pin instead of a source
                    opaque = "sorry";
                };
            };
        };
        nixos_25_05 = import ./capsules/nixos_25_05 {
            inherit (new_capsules) platform boltons;
        };
        nixos_25_11 = import ./capsules/nixos_25_11 {
            inherit (new_capsules) platform boltons;
        };
        mailserver_25_05 = import ./sources.d/mailserver_25_05/capsule.nix {
            # TODO make the pin updatable so it can replace the source update mechanism and this can be moved to capsules
            inherit (new_capsules) platform;
        };
        command-platform = rec {
            # extra local scope because it's stupid that we would always need command-platform
            # *and* platform to do command-platform.native.${platform.localSystem}
            local = {
                packageBuilders = {
                    writeCommand = args: let
                        # TODO somehow enhance writeShellApplication to produce a clean PATH inside the script
                        # Currently it is too easy to rely on some undeclared dependency.
                        scriptDir =
                        new_capsules.nixos_25_05.native.${new_capsules.platform.localSystem}.packageBuilders.writeShellApplication {
                            name = "script";
                            runtimeInputs = args.runtimeInputs or [];
                            text = args.text;
                        };
                    in
                    new_capsules.nixos_25_05.portable.packageBuilders.concatScript args.name [ "${scriptDir}/bin/script" ];
                };
            };

            native.${new_capsules.platform.localSystem} = local;
        };
        komputiloj-privata = sources.komputiloj-privata.value {
            inherit (new_capsules) boltons;
        };
        komputiloj-bootstrap = import ./capsules/komputiloj-bootstrap {
            inherit (new_capsules) boltons;
        };
        hello-infra = sources.hello-infra.value {
            inherit (new_capsules) boltons platform flake-compat nixos_25_05 command-platform komputiloj-bootstrap;
        };
    };
    fake_capsules = rec {
        # TODO get rid of this backwards compatible abomination
        nixpkgsCurrent = let nixpkgs = getAttr default_nixos new_capsules; in
            nixpkgs // {
                packages = nixpkgs.native.x86_64-linux.legacyPackages;

                # TODO remove this and use nixosModules everywhere instead
                modules = nixpkgs.nixosModules;

                pins = {};
            };
        nixpkgsFuture = let
            override = old: new: trace ("🕑🕐🕛 nixpkgsFuture: Replacing " + old.name + " with " + new.name) new;
        in {
            # NOTE: Each time we use something from unstable, we should pin
            # that exact source. That way, we can someday move away from the
            # unstable version when it hits the stable channels.  If we would
            # take packages directly from one perpetually updated unstable
            # source, we will never catch up.
            
            # TODO these are flake-like outputs, not our multiarch capsule
            # outputs. This nixpkgsFuture mess should be removed anyway
            # so I guess we can live with that for now?

            packages.aarch64-linux = let
                callPackage = pkg: callPackageWith nixpkgsCurrent.qemu.aarch64-linux.legacyPackages pkg;
            in {
                ebusd = override
                    nixpkgsCurrent.qemu.aarch64-linux.legacyPackages.ebusd
                    (callPackage ./pkgs/ebusd {});
            };
            packages.x86_64-linux = let
                callPackage = pkg: callPackageWith nixpkgsCurrent.native.x86_64-linux.legacyPackages pkg;
            in {
                silverbullet = override
                    nixpkgsCurrent.native.x86_64-linux.legacyPackages.silverbullet
                    (callPackage ./pkgs/silverbullet {});
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
    };
    sloppy_capsules = real_capsules // fake_capsules // { all = all_capsule; };
    capsules = new_capsules // sloppy_capsules;
    # TODO get rid of passing capsules_and_boltons everywhere.
    # work towards an explicit dependency order
    capsules_and_boltons = sloppy_capsules // {
        # make boltons.lib already work in sloppy capsules
        boltons = boltons // { lib = boltons; };
    };
in komputiloj_capsule
