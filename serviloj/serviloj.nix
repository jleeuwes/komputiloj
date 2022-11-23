let
	sources            = import ../sources.nix;
	util               = import ../util.nix;
	nixpkgs            = sources.nixos_22_05.value {};
	mailserver         = sources.mailserver_22_05.value;
	nextcloud_apps     = sources.nextcloud_25_apps.value;
	gorinchemindialoog = import ./gorinchemindialoog/serviloj.nix;
in {
	# Inspiration taken from https://github.com/nh2/nixops-tutorial/blob/master/example-nginx-deployment.nix

	network.description = "Our humble all-encompassing serviloj deployment";
	network.nixpkgs = nixpkgs;

	# TODO quota
	# TODO monitoring:
	# - laat backupscript zijn laatste backup vermelden ergens op de server,
	#   zodat de server ons kan herinneren als er te lang geen backup is gemaakt
	# - ...
	#
	
	gently2 = { config, nodes, lib, pkgs, ... }: {
		imports = [
			./modules/hetzner_vps.nix
			mailserver
		];

		nixpkgs.overlays = [
			(import (../packages/undesired-packages-overlay.nix))
		];

		deployment.targetHost = "gently.radstand.nl";
		deployment.provisionSSHKey = false;
		# deployment.hasFastConnection = true; # helps to deploy when DNS is borked on the server
		
		deployment.keys = {
			# TODO somehow get rid of implicit dependency on wachtwoord in PATH;
			# Maybe we should package komputiloj and make it depend on
			# wachtwoord and nixops?
			"luks-storage" = {
				keyCommand = [ "wachtwoord" "cat" "-n" "secrets/luks-storage@hetzner" ];
			};
			"nextcloud-admin" = {
				keyCommand = [ "wachtwoord" "cat" "-n" "secrets/admin@wolk.radstand.nl" ];
				user = "nextcloud";
				group = "nextcloud";
				permissions = "ug=r,o=";
			};
			"account-gorinchemindialoog" = {
				destDir = "/run/keys/persist";
				keyCommand = [ "wachtwoord" "hash" "-n" "secrets/gorinchemindialoog@radstand.nl" ];
			};
			"account-gorinchemindialoog-bcrypt" = {
				destDir = "/run/keys/persist";
				keyCommand = [ "wachtwoord" "hash-with-bcrypt" "-n" "secrets/gorinchemindialoog@radstand.nl" ];
			};
		};
		
		# The rest is a configuration just like nixos/configuration.nix
		
		# WARNING this setting is ignored.
		# Instead, nixops determines the stateVersion at first deploy based on the NixOS version it encounters.
		# Our deploy script stores this state on gently now to keep the correct stateVersion.
		# system.stateVersion = lib.mkForce "20.09";

		# TODO shouldn't 'storage-mounted' be a target?
		systemd.services.mount-storage = {
			serviceConfig = {
				Type = "oneshot";
				RemainAfterExit = true;
			};
			# onFailure = [ "failure-mailer@%n.service" ]; # not useful; postfix depends on this service
			wants = [ "luks-storage-key.service" ];
			after    = [ "luks-storage-key.service" ];
			wantedBy = [ "multi-user.target" ];
			requiredBy = [
				"nextcloud-cron.service"
				"nextcloud-setup.service"
				"nextcloud-update-plugins.service"
				"phpfpm-nextcloud.service"
				"gitea.service"
				"btrbk-storage.service"
				"postfix.service"
				"dovecot2.service"
			];
			before = [
				"nextcloud-cron.service"
				"nextcloud-setup.service"
				"nextcloud-update-plugins.service"
				"phpfpm-nextcloud.service"
				"gitea.service"
				"btrbk-storage.service"
				"postfix.service"
				"dovecot2.service"
			];
			path = [ pkgs.cryptsetup pkgs.utillinux pkgs.unixtools.mount pkgs.unixtools.umount ];
			script = ''
				if mountpoint -q /mnt/storage; then
					echo "Storage already mounted. Done here."
					exit 0
				fi
				if [ -b /dev/mapper/storage ]; then
					echo "LUKS mapping already opened."
				else
					echo "Opening LUKS mapping..."
					cryptsetup open UUID=6c8d5be7-ae46-4e51-a270-fd5bdce46f3b storage --type luks --key-file /run/keys/luks-storage
				fi
				echo "Mounting..."
				mkdir -p /mnt/storage
				mount /dev/mapper/storage /mnt/storage
				echo "Done here."
			'';
			preStop = ''
				if mountpoint -q /mnt/storage; then
					umount /mnt/storage
				fi
				cryptsetup close storage
			'';
		};
		
		systemd.services.make-spare-keys = {
			# Makes copies of keys on our storage volume in case we need to
			# restore them without having access to our deployment tooling.
			# Only semi-secrets (such as hashed passwords) should be persisted this way.
			# Restoring is manual at the moment: just copy the spare keys to /run/keys/persist
			serviceConfig.Type = "simple";
			onFailure = [ "failure-mailer@%n.service" ];
			startAt = "*:5,20,35,55";
			requisite = [ "mount-storage.service" ];
			script = ''
				SPAREDIR=/mnt/storage/live/komputiloj/spare-keys
				if [[ ! -d "$SPAREDIR" ]]; then
					# Don't use mkdir -p so we don't put spare-keys on the root
					# fs in the (rare) case that the storage volume is not
					# correctly mounted.
					mkdir -m 0750 -- "$SPAREDIR"
					chown root:keys -- "$SPAREDIR"
				fi
				cp -a /run/keys/persist/* -- "$SPAREDIR"
			'';
		};

		systemd.services.dagelijks-rapport = {
			serviceConfig.Type = "simple";
			onFailure = [ "failure-mailer@%n.service" ];
			startAt = "03:00 Europe/Amsterdam"; # avoid 02:00-03:00 because of DST ambiguity
			script = ''
				vandaag=$(LC_TIME=nl_NL.UTF8 date '+%Y-%m-%d (%a)')
				schijven=$(df -h | fgrep -v tmp)
				gebruik=$(find / -mindepth 1 -maxdepth 1 -a -not -name mnt | xargs du -hs | sort -hr)
				${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s "[gently] overzicht voor $vandaag" jeroen@lwstn.eu <<-EOF
					Hoi,

					Zo staat het met de schijfruimte:

					$schijven

					En dit is de grootte per dir in / (zonder mnt):

					$gebruik

					Groetjes!
				EOF
			'';
		};
		systemd.services.check-disk-usage = {
			serviceConfig.Type = "simple";
			onFailure = [ "failure-mailer@%n.service" ];
			startAt = "*:0,15,30,45";
			script = ''
				if problems=$(df -h | egrep '(100|9[0-9])%'); then
					${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s '[gently] bijna vol!' jeroen@lwstn.eu <<-EOF
						Hoi,

						De volgende schijven zijn bijna vol:

						$problems

						Succes ermee!
					EOF
				fi
			'';
		};
		systemd.services.setup-persistent-homedirs = {
			serviceConfig.Type = "oneshot";
			onFailure = [ "failure-mailer@%n.service" ];
			wantedBy = [ "multi-user.target" ];
			script = ''
				ln -sfT /mnt/storage/live/home/gorinchemindialoog /home/gorinchemindialoog
			'';
		};
		systemd.services.gorinchemindialoog-autocommit = {
			serviceConfig = {
				Type = "simple";
				User = "gorinchemindialoog";
			};
			onFailure = [ "failure-mailer@%n.service" ];
			startAt = "04:00";
			requisite = [ "mount-storage.service" ];
			path = [ pkgs.gitMinimal pkgs.openssh ];
			script = ''
				# The git working tree with the actual website files:
				export GIT_WORK_TREE=/mnt/storage/live/sftp/gorinchemindialoog/home/gorinchemindialoog/Website/Live
				# The git administration dir (normally .git):
				export GIT_DIR=/mnt/storage/live/home/gorinchemindialoog/website.git

				git add --all
				stamp=$(LANG=nl_NL.UTF-8 TZ=Europe/Amsterdam date)
				if git diff --staged --quiet --exit-code; then
					echo Nothing to commit.
				else
					git commit -m "Autocommit $stamp"
				fi
				git push
			'';
			# One-time setup (as gorinchemindialoog, with storage mounted and prepare-chroots done):
			# 1. Run: ssh-keygen
			# 2. Add the SSH key to gidbot on thee.radstand.nl
			# 3. Export the environment variables in the script above.
			# 4. Run: mkdir -p $GIT_WORK_TREE && git init && git remote add gitea@thee.radstand.nl:gorinchemindialoog/website.git && git fetch origin && git checkout main
			# 5. Run: git config --global user.email gorinchemindialoog@radstand.nl ; git config --global user.name 'Gorinchem in Dialoog'
		};

		systemd.services."failure-mailer@" = {
			serviceConfig.Type = "simple";
			scriptArgs = "%I";
			script = ''
				unit="$1"
				${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s "[gently] probleem met $unit" jeroen@lwstn.eu <<-EOF
					Hoi,

					Ja, dus $unit heeft een ongelukje gehad:

					$(systemctl status -- $unit)

					Succes ermee!
				EOF
				
			'';
		};
		
		services.btrbk = {
			instances = {
				storage = {
					settings = {
						timestamp_format = "long-iso"; # safe from the caveat at https://digint.ch/btrbk/doc/btrbk.conf.5.html#_reference_time as long as we don't use btrbk for backups
						snapshot_preserve_min = "24h"; # for manual snapshots? not sure, but we need to set it to something other than "all"
						snapshot_preserve = "24h 7d 5w 12m *y";
						preserve_day_of_week = "monday";
						preserve_hour_of_day = "0";
						volume."/mnt/storage" = {
							snapshot_dir = "snapshots";
							subvolume."live/*" = {};
						};
					};
					onCalendar = "hourly";
				};
			};
		};

		services.btrfs.autoScrub = {
			enable = true;
			fileSystems = [ "/mnt/storage" ];
			interval = "weekly";
		};

		services.openssh = {
			enable = true;
			knownHosts = {
				"thee.radstand.nl" = {
					# Needed for gorinchemindialoog autocommit.
					# I guess this key is regenerated on gitea install, so we'll have to update this if rebuilding.
					publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHmMPh91t1reE1ddLcFYyddQs0hx4v41KcaNBS2UVnEA";
				};
			};
			extraConfig = ''
				Match Group sftp_only
					ChrootDirectory /mnt/storage/live/sftp/%u
					ForceCommand internal-sftp
					AllowTcpForwarding no
					X11Forwarding no
			'';
		};

		users = {
			users.root = {
				# Password file must be present and readable by the system
				# during boot, without storage being mounted yet,
				# otherwise we're locking ourselves out.
				passwordFile = "/root/password";
				openssh.authorizedKeys.keyFiles = [
					../scarif/home/jeroen/.ssh/id_rsa.pub
				];
			};

			# Make sure users have the same uid on all our machines.
			# Add users here that don't have a fixed uid in nixpkgs/nixos.
			# Warning: changing uids here after a user has been created has no effect!
			# (I think - the note here was about containers.)
			# You have to rm /var/lib/nixos/uid-map and userdel the user.
			users.nextcloud = {
				uid = 70000;
				group = "nextcloud";
				extraGroups = [ "keys" ];
			};
			groups.nextcloud = {
				gid = 70000;
			};
			users.gitea = {
				uid = 70001;
				group = "gitea";
			};
			groups.gitea = {
				gid = 70001;
			};
			users.vmail = {
				uid = 70002;
				group = "vmail";
				isSystemUser = true;
			};
			groups.vmail = {
				gid = 70002;
			};
			groups.sftp_only = {
				gid = 2001;
			};
			users.gorinchemindialoog = {
				isNormalUser = true;
				createHome = false;
				home = "/home/gorinchemindialoog"; # must exist both inside and outside the sftp_only chroot
				uid = 1001;
				passwordFile = "/run/keys/persist/account-gorinchemindialoog";
				extraGroups = [ "sftp_only" ];
			};
		};

		networking = {
			hostName = "gently";
			domain = "radstand.nl";
			interfaces.ens3 = {
				useDHCP = true;
			};
			firewall = {
				allowPing = true;
				allowedTCPPorts = [
					# NOTE: most services add their ports automatically,
					# so most of this is just documentation.
					22  # SSH
					80  # HTTP - only allowed for Let's Encrypt challenges
					443 # HTTPS
					143 # IMAP
					993 # IMAPS
					25  # SMTP
					465 # SMTP submission over TLS
					587 # SMTP submission
				];
			};
		};

		services.fail2ban.enable = true;

		security.acme = {
			acceptTerms = true;
			defaults = {
				email = "jeroen@lwstn.eu";
			};
		};

		services.nginx = {
			enable = true;
			recommendedGzipSettings = true;
			recommendedOptimisation = true;
			recommendedProxySettings = true;
			recommendedTlsSettings = true;

			virtualHosts = {
				"wolk.radstand.nl" = {
					forceSSL = true;
					enableACME = true;
				};
				"thee.radstand.nl" = {
					forceSSL = true;
					enableACME = true;
					locations."/" = {
						proxyPass = "http://localhost:3000";
					};
				};
				# TODO strict transport security (not critical for gorinchemindialoog)
				# TODO compare access logs
				"www2.gorinchemindialoog.nl" = {
					forceSSL = true;
					enableACME = true;
					root = "/mnt/storage/live/sftp/gorinchemindialoog/home/gorinchemindialoog/Website/Live";
					# TODO redirect www to non-www: globalRedirect = "gorinchemindialoog.nl";
					extraConfig = ''
						disable_symlinks if_not_owner;
						add_header Cache-Control "no-cache";
					'';
				};
			};
		};

		services.nextcloud = {
			enable = true;

			package = pkgs.nextcloud25;

			home = "/mnt/storage/live/nextcloud/rootdir";

			autoUpdateApps = {
				enable = true;
			};

			hostName = "wolk.radstand.nl";
			https = true; # no idea how this relates to config.overwriteProtocol

			maxUploadSize = "512M";

			config = {
				adminuser = "admin";
				adminpassFile = "/run/keys/nextcloud-admin";
				
				dbtype = "sqlite"; # let's start simple
				
				overwriteProtocol = "https";
			};

			extraApps = let apps = nextcloud_apps pkgs; in {
				inherit (apps) files_linkeditor calendar;
			};
		};
		
		services.gitea = {
			enable = true;

			database.type = "sqlite3";

			rootUrl = "https://thee.radstand.nl/";
			domain = "thee.radstand.nl";
			cookieSecure = true;

			log.level = "Info";

			# NOTE: after changing the stateDir, regenerate gitea's authorized_keys file through the admin webinterface.
			stateDir = "/mnt/storage/live/gitea/rootdir";

			disableRegistration = true;

			# mailerPasswordFile = ...;
			settings = {
				mailer = {
					ENABLED = true;
					FROM = "thee@radstand.nl";
					# https://docs.gitea.io/en-us/config-cheat-sheet/#mailer-mailer
					HOST = "localhost:25";
					SKIP_VERIFY = true; # this is okay, as long as it's localhost
					# https://github.com/NixOS/nixpkgs/issues/103446
					# MAILER_TYPE = "sendmail"; # not sure which of...
					# PROTOCOL = "sendmail";    # ...these two we need
					# SENDMAIL_PATH = "${pkgs.system-sendmail}/bin/sendmail";
				};
				other = {
					SHOW_FOOTER_VERSION = false;
				};
			};
		};

		mailserver = {
			enable = true;
			
			# We won't get nameservers from DHCP if this if true (the default)!
			# See https://discourse.nixos.org/t/how-to-use-a-nameserver-with-a-static-networking-configuration/10932/3
			# and https://gitlab.com/simple-nixos-mailserver/nixos-mailserver/-/issues/206
			# Maybe configure nameservers ourselves and make a PR for the docs
			# at https://nixos-mailserver.readthedocs.io/en/latest/options.html
			localDnsResolver = false;
	
			# TODO get rid of nginx welcome page on mail2.radstand.nl
			fqdn = "mail2.radstand.nl";
			sendingFqdn = "gently.radstand.nl";
			domains = [ "gorinchemindialoog.nl" "radstand.nl" ];

			loginAccounts = {
				"gorinchemindialoog@radstand.nl" = {
					name = "gorinchemindialoog";
					hashedPasswordFile = "/run/keys/persist/account-gorinchemindialoog-bcrypt";
				};
			};
			forwards = util.mapNames (name : name + "@gorinchemindialoog.nl") gorinchemindialoog.forwards // {
				# catch-all:
				"@radstand.nl" = "jeroen@lwstn.eu";
			};

			indexDir = "/var/mail-indexes";
			mailDirectory = "/mnt/storage/live/mail/vmail"; # TODO make relevant service depend on this mount!
			sieveDirectory = "/mnt/storage/live/mail/sieve"; # TODO not sure if this is persistent state
			vmailGroupName = "vmail";
			vmailUserName = "vmail";
			vmailUID = 70002;

			certificateScheme = 3; # let's hope this uses the regular letsencrypt infrastructure of NixOS so it doesn't clash with nginx
		};

		environment.systemPackages = with pkgs; [
			screen
			netcat
			vim
			cryptsetup btrfs-progs parted
			mailutils
			gitMinimal
			gitAndTools.git-annex
		];
	};
}
