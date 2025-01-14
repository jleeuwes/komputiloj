{ komputiloj, hello-infra, nixpkgsCurrent, nixpkgsFuture, ... }:
rec {
    targetHost = "scarif.radstand.nl"; # TODO configure duckdns
    sshPublicKeys = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFN+m0J0mjJBDho4cTqt9OlnbMUtYuj6OacT7VWi/ahC";
    nixosSystem = nixpkgsCurrent.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ mainModule ];
    };

    mainModule = { config, pkgs, lib, ... }: {
        # # Add the --option extra-builtins-file to nix
        # # using a magic spell from https://elvishjerricco.github.io/2018/06/24/secure-declarative-key-management.html
        # (Doesn't work: warning: ignoring the user-specified setting 'extra-builtins-file', because it is a restricted setting and you are not a trusted user)
        # nix.extraOptions = ''
        #     plugin-files = ${pkgs.nix-plugins.override { nix = config.nix.package; }}/lib/nix/plugins/libnix-extra-builtins.so
        # '';
        
        # We need to construct our own NIX_PATH, because the default becomes very weird with our scheme.
        # Also, make sure you don't have a ~/.nix-defexpr because that gets added
        # too (see /etc/set-environment).
        nix.nixPath = [
            # Don't do ${<nixpkgs>}, because that makes a weird non-working copy of
            # our sources.nix in the nix-store. With toString the literal path is
            # used instead.
            "nixpkgs=${nixpkgsCurrent.nixPath}"
            # I don't think we need these during normal operation:
            # "komputiloj=${builtins.toString <komputiloj>}"
            # "nixos-config=${builtins.toString <nixos-config>}"
        ];

        nixpkgs.config = {
            # Selectively allow some unfree packages
            # - https://nixos.org/nixpkgs/manual/#sec-allow-unfree
            allowUnfreePredicate = pkg:
                builtins.elem (lib.getName pkg) [
                    "steam-run"
                    "steam-original"
                    "android-studio"
                    "symbola" # non-commercial, no modifications,
                              # no redistribution, "a single instantiation and no
                              # network installation"
                ];

            # Selectively allow some packages with known vulnerabilities
            # - https://nixos.org/manual/nixpkgs/stable/#sec-allow-insecure
            permittedInsecurePackages = [
                "python2.7-urllib3-1.26.2" # used by... something? TODO get rid of it
                "python2.7-pyjwt-1.7.1"    # TODO get rid of this
            ];
        };
        nixpkgs.overlays = [
            komputiloj.overlays.undesired-packages
        ];

        imports =
            [
                ./hardware-configuration.nix
                komputiloj.modules.ssh-client-config
                komputiloj.modules.librewolf
            ];
        

        # Make sure ~/bin is added to PATH:
        environment.homeBinInPath = true;

        # Use the systemd-boot EFI boot loader.
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        # Make /tmp in-memory:
        boot.tmp.useTmpfs = true;

        # Enable read-write NTFS support:
        boot.supportedFilesystems = [ "ntfs" ];

        # Here I was looking into creating a ramfs mount with /tmp properties
        # (i.e. anyone can write) for temporarily decripted keys.
        # However, this is discouraged as size limits are not enforced?
        # Anyway, we don't have swap on this system so we just use /tmp.
        # boot.specialFileSystems = {
        #     "/run/stuff" = {
        #         fsType = "ramfs";
        #         options = [ "nosuid" "nodev" "mode=01777" "size=100M" ];/swap
        # };

        networking = {
            hostName = "scarif";
            wireless = {
                enable = true;  # Enables wireless support via wpa_supplicant.
                interfaces = [ "wlp4s0" ];
                # Netwerken staan in /etc/wpa_supplicant.conf vanwege passphrases
            };
            nameservers = [ "1.1.1.1" "8.8.8.8" ];
            hosts = {
                # "ip.ad.dr.es" = [ "domain.name" ];
            };
        };

        # Select internationalisation properties.
        console = {
            font = "Lat2-Terminus16";
            keyMap = "us";
        };
        i18n = {
            defaultLocale = "nl_NL.UTF-8";
        };

        # Set your time zone.
        time.timeZone = "Europe/Amsterdam";

        fonts = {
            enableDefaultPackages = true; # <- dit lijkt niet echt iets uit te maken t.o.v.  weglaten
            packages = [
                # nerdfonts is HUGE and nixos-rebuild hangs without progress information
                # (withFont does not help)
                # (also it is a useless font for emoji - it has nice icons but only
                # custom ones, not stuff actually defined in unicode)
                # (pkgs.nerdfonts.override { withFont = "SpaceMono"; })
                # noto is veelbelovend maar sommige emoji zijn kleur (niet zo erg) en ENORM (wel erg)
                # pkgs.noto-fonts-emoji
                # pkgs.noto-fonts
                # We weten nu trouwens zeker dat er een of ander fallback-systeem aan
                # het werk is, want ook als je andere fonts kiest en noto-fonts-emoji is
                # aanwezig krijg je emoji.
                # WE HEBBEN EEN WINNAAR!
                # symbola heeft mooie zwartwit-emoji op normale grootte.
                # woooooooot \o/
                # UPDATE 30 oktober: dit is kapot, ik zie nu kleur-emoji
                # UPDATE 2024: upstream is verdwenen
                # pkgs.symbola

                pkgs.dejavu_fonts
                pkgs.ubuntu_font_family
            ];
        };

        # List packages installed in system profile. To search by name, run:
        # $ nix-env -qaP | grep wget
        environment.systemPackages = let
            # this incomprehensible magic incantation was conjured by TheMsDosNerd at https://www.reddit.com/r/NixOS/comments/8cq4ic/problem_installing_python_package/
            myPythonPackages = pythonPackages: with pythonPackages; [
                    # flask
                    # flask-api
            ]; in with pkgs; [
            
            # enhance nix-build
            nix-output-monitor

            # for running precompiled games:
            steam-run-native

            ocrad

            # usefull programs:
            mkpasswd
            unicode-paracode
            gitFull vim file subversionClient pciutils pmount squashfsTools
            parted gparted
            wget rtorrent
            sshpass
            gnupg paperkey qrencode zbar pwgen
            komputiloj.packages.wachtwoord
            inetutils # for ftp for the nas
            openssl
            nmap
            tree

            pamix pavucontrol alsaUtils
            zstd # for unpacking arch packages
            glxinfo
            xorg.xdpyinfo
            xorg.xev
            
            # Install scanimage (saneBackends) and scanadf (saneFrontends),
            # among other scanning tools.
            # I can't get ADF scanning to work. 'Best' command line is:
            #     scanimage -d pixma:04A91824_214FE1 --batch=scan%02d.png --format tiff --batch-start 1 --batch-double --batch-count 3 --source "Automatic Document Feeder"
            # But it gives "scanimage: sane_read: Operation was cancelled"
            # - this is probably related to https://gitlab.com/sane-project/backends/-/merge_requests/213
            sane-backends sane-frontends
            
            # programming:
            (python3.withPackages myPythonPackages)
            dejsonlz4 # for reading firefox jsonlz4 files
            remarshal # for yaml2json etc
            jq # json manipulation
            # jetbrains.idea-community
            openjdk11 maven visualvm
            # love_11
            # arduino
            haskellPackages.ghc
            
            # unstable.android-studio
            # apktool dex1jar

            # LaTeX and PDF:
            # (Not sure if rubber uses the chosen texlive distribution)
            # texlive.combined.scheme-medium
            # rubber
            poppler_utils
            qpdf pdftk

            firefox thunderbird # librewolf is configured through programs.librewolf
            # isync # for e-mail backups (eigenlijk mbsync)
            chromium
            geany
            zathura # pdf viewer
            # mindmapping-tools:
            vym freemind
            vlc ffmpeg
            beets

            inkscape gimp exiftool
            imagemagick scrot
            libreoffice antiword
            audacity
            
            pstree
            
            # Belgian eID (it looks in /run/current-system/sw/ by default for some things so it's easier to have it installed system-wide):
            # eid-mw

            # other package managers (to be used for non-reproducable things only):
            # nodejs #npm

            zip unzip
            par2cmdline # for error-correcting archives
            llvmPackages.bintools # to get ar (to extract .deb files)
            sqlite
        ];

        programs.librewolf = {
            enable = true;

            languagePacks = [ "nl" "en-US" ];

            policies = {
                # https://mozilla.github.io/policy-templates/
                ExtensionSettings = {
                    "*" = {
                        blocked_install_message = "Dit moet via Nix.";
                        installation_mode = "blocked";
                    };
                    "gdpr@cavi.au.dk" = {
                        # TODO figure out how to do updates and/or pinning
                        install_url = "https://addons.mozilla.org/firefox/downloads/latest/consent-o-matic/latest.xpi";
                        installation_mode = "force_installed";
                    };
                    # ublock origin is included with librewolf,
                    # but they use Extensions for that, and apparently
                    # having an ExtensionSettings policy overrides that
                    # completely. So we need to have ublock here.
                    "uBlock0@raymondhill.net" = {
                        install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
                        installation_mode = "force_installed";
                    };
                };
            };
        };

        programs.gnupg.agent = {
            enable = true;
            enableSSHSupport = true;
            pinentryPackage = pkgs.pinentry-gtk2;
        };

        # Generate setuid wrappers for pmount:
        security.wrappers = {
            pmount = {
                source  = "${pkgs.pmount}/bin/pmount";
                setuid = true;
                owner = "root";
                group = "root";
            };
            pumount = {
                source = "${pkgs.pmount}/bin/pumount";
                setuid = true;
                owner = "root";
                group = "root";
            };
        };

        # List services that you want to enable:
        

        # Set this to true if you want to cast with chromium,
        # in combination with enabling
        # chrome://flags/#load-media-router-component-extension and
        # chrome://flags/#cast-media-route-provider
        # Note that cupsd spams the log if this is true, so disable if not used.
        # (See https://github.com/NixOS/nixpkgs/issues/195090)
        services.avahi.enable = false;

        # Enable the OpenSSH daemon.
        services.openssh.enable = true;
        programs.ssh = {
            knownHosts = {
                "[u362967.your-storagebox.de]:23" = {
                    publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";
                };
            };
        };

        # Enable CUPS and printing.
        # Add the Canon TR7500 printer using:
        #   lpadmin -p kantoor -v ipp://192.168.1.44:631/ipp/print -E -m everywhere
        # (Don't forget to make the IP fixed in the router config!)
        # Then further configure it at http://localhost:631/printers/kantoor
        # 
        # I used https://wiki.debian.org/CUPSDriverlessPrinting to get this stuff
        # working; I had to use the IP address in the URI instead of using the
        # dnssd://... detected by the CUPS web interface or the name-based ipp://...
        # detected by ippfind
        services.printing.enable = true;
        
        # enable gvfs to have ftp support etcetera in thunar
        # (doesn't work; maybe use
        # https://github.com/NixOS/nixpkgs/blob/release-19.09/nixos/modules/services/x11/desktop-managers/xfce.nix
        # ?)
        # services.gvfs.enable = true;
        # services.gvfs.package = pkgs.xfce.gvfs;
        
        # Enable sound.
        sound.enable = true;
        sound.extraConfig = ''
            pcm.pulse {
              type pulse
              hint.description "PulseAudio device for audacity"
            }
            ctl.pulse {
              type pulse
            }
        '';
        hardware.pulseaudio.enable = true;
        # Some NixOS packages can be built with explicit PulseAudio support which is disabled by default. This support can be enabled in all applicable packages by setting:
        nixpkgs.config.pulseaudio = true;

        hardware.bluetooth = {
            enable = true;
        };

        # Screen locker
        services.physlock = {
            enable = true;
            # nog niet beschikbaar (eens updaten!)
            # allowAnyUser = true; # of moeten we rechten via systemd regelen?
        };

        # Enable adb group and udev rules and such:
        programs.adb.enable = true;

        # For 32-bit games:
        hardware.opengl.driSupport32Bit = true;
        hardware.opengl.driSupport = true; # is actually the default
        
        users.users.root = {
			openssh.authorizedKeys.keys = [
			    # Always have a key here, otherwise we can't deploy.
				komputiloj.users.jeroen.sshKeys.ferrix
			];
            
        };
        # Define a user account. Don't forget to set a password with ‘passwd’.
        users.extraUsers.jeroen = {
            uid = komputiloj.users.jeroen.linux.uid;
            isNormalUser = true;
            description = komputiloj.users.jeroen.fullName;
            extraGroups = [ "wheel" "network-manager" "dialout" "adbusers" "video" "audio"
                "lp" # for scanning with Canon
            ];
        };
        users.extraUsers.speel = {
            # A dedicated user account to play untrusted game binaries.
            uid = 1001; # clashes with gorinchemindialoog.
                        # Fortunately, it's unlikely that we will have speel on our
                        # servers or gorinchemindialoog on this laptop.
            isNormalUser = true;
            description = "Speel Spelletjes";
            extraGroups = [ "video" "audio" ];
        };

        # The NixOS release to be compatible with for stateful data such as databases.
        system.stateVersion = "16.09";
    };
}
