{ boltons, nixpkgsCurrent, komputiloj, privata, gorinchemindialoog, hello-infra,
  sleutel, wolk, thee, notie, ...  }:
with boltons;
let
	nixpkgs            = nixpkgsCurrent.packages;
	hello              = hello-infra;
	inherit (nixpkgs.lib.strings) escapeShellArgs;
in rec {
	# Inspiration taken from https://github.com/nh2/nixops-tutorial/blob/master/example-nginx-deployment.nix

	# TODO quota
	# TODO monitoring:
	# - laat backupscript zijn laatste backup vermelden ergens op de server,
	#   zodat de server ons kan herinneren als er te lang geen backup is gemaakt
	# - ...
	
	targetHost = "gently.radstand.nl";
	inherit (privata.machines.gently) masterAgeKey;
	sshPublicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHmMPh91t1reE1ddLcFYyddQs0hx4v41KcaNBS2UVnEA";
	# TODO move the secrets from here to below (nixos config)
	# (the capsule secrets remain, because those also provide information about updating a secret)
	secrets = {
		luks-storage-key = {
			encryptedContent = privata.secrets.luks-storage-key.encryptedContent;
		};
		bigstorage1-git-annex-hello-creds = {
			encryptedContent = hello.secrets.bigstorage1-git-annex-hello-creds.encryptedContent;
			user = "git-annex";
			group = "git-annex";
			permissions = "u=r,go=";
		};
	};
	
	nixosSystem = nixpkgsCurrent.lib.nixosSystem {
		system = "x86_64-linux";
		modules = [
			mainModule
		];
	};

	mainModule = { config, lib, pkgs, ... }:
	let
		makeJob = s: s // {
			mailOnFailure = true;
			startAt = if s ? startAt then checkStart s.startAt else [];
		};
		makeJobWithStorage = s@{requisite ? [], after ? [], ...}: s // {
			# If the volume is not mounted, this service fails.
			needsStorageVolume = "requisite";
			mailOnFailure = true;
			startAt = if s ? startAt then checkStart s.startAt else [];
		};
		makeService = s: s // {
			mailOnFailure = true;
			startAt = if s ? startAt then checkStart s.startAt else [];
		};
		makeServiceWithStorage = s@{requires ? [], after ? [], ...}: s // {
			# If the volume is not mounted, this service will try to mount it.
			needsStorageVolume = "requires";
			mailOnFailure = true;
			startAt = if s ? startAt then checkStart s.startAt else [];
		};
		checkStart = time:
			if match ".*(02:[0-9][0-9]|03:00).*Europe/Amsterdam.*" time != null
			then trace "WARNING: Time spec ${time} will behave weirdly during DST transitions!" time
			else time;
	in {
		imports = [
			komputiloj.modules.hetzner_vps
			komputiloj.modules.systemd-failure-mailer
			komputiloj.modules.storage-volume
			komputiloj.modules.ssh-client-config
			komputiloj.modules.secrets
			nixpkgsCurrent.modules.mailserver
			hello.modules."70004-backup"
			hello.modules."70004-autocommit"
			hello.modules."70004-ingest-data"
			hello.modules."70004-known-host"
			sleutel.modules.all_in_one
			wolk.modules.all_in_one
			notie.modules.all_in_one
			thee.modules.all_in_one
		];

		secrets = secrets; # SEE ABOVE

		nixpkgs.overlays = [
			komputiloj.overlays.undesired-packages
		];

		# Get rid of dependency on whole nixpkgs source tree (400MB)
		nix.registry = lib.mkForce {};

		
		# WARNING this setting is ignored by nixops.
		# Instead, nixops determines the stateVersion at first deploy based on the NixOS version it encounters.
		# Our deploy script stores this state on gently now to keep the correct stateVersion.
		system.stateVersion = lib.mkForce "20.09";
		
		i18n = {
			supportedLocales = [
				"C.UTF-8/UTF-8"
				"en_US.UTF-8/UTF-8"
				"nl_NL.UTF-8/UTF-8"
			];
		};

		systemd = {
			services = {
				# TODO shouldn't 'storage-mounted' be a target?

				btrbk-storage = {
					mailOnFailure = true;
				};
				
				dagelijks-rapport = makeJob {
					serviceConfig.Type = "simple";
					startAt = "05:00 Europe/Amsterdam";
					path = [ pkgs.btrfs-progs ];
					script = stripTabs ''
						vandaag=$(LANG=nl_NL.UTF8 date '+%Y-%m-%d (%a)')
						schijven=$(df -h | fgrep -v tmp)
						btrfs=$(btrfs filesystem usage /mnt/storage)
						gebruik=$(find / -mindepth 1 -maxdepth 1 -a -not -name mnt | xargs du -hs | sort -hr)
						${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s "[gently] overzicht voor $vandaag" jeroen@lwstn.eu <<-EOF
							Hoi,

							Zo staat het met de schijfruimte volgens df:

							$schijven

							Dit is de grootte per dir in / (zonder mnt):

							$gebruik

							Zo staat het met de schijfruimte volgens btrfs:

							$btrfs

							Groetjes!
						EOF
					'';
				};
				check-disk-usage = makeJob {
					serviceConfig.Type = "simple";
					startAt = "*:0";
					path = [ pkgs.btrfs-progs ];
					script = stripTabs ''
						if problems=$(df -h | fgrep '100%'); then
							${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s '[gently] vol!' jeroen@lwstn.eu <<-EOF
								Hoi,

								De volgende schijven zijn vol:

								$problems

								Succes ermee!
							EOF
						elif problems=$(df -h | egrep '9[5-9]%'); then
							${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s '[gently] bijna vol!' jeroen@lwstn.eu <<-EOF
								Hoi,

								De volgende schijven zijn bijna vol:

								$problems

								Succes ermee!
							EOF
						fi
						# General rule of thumb: you want 5GB worth of UNALLOCATED space on EACH device to allow BTRFS to work properly.
						# -- https://old.reddit.com/r/btrfs/comments/xxlju2/how_full_is_too_full/
						if problems=$(btrfs fi usage /mnt/storage | sed -E 's/\.[0-9]+//g' | egrep 'unallocated:\s*([0-9]B|MiB|[321]GiB)'); then
							${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s '[gently] BTRFS raakt vol!' jeroen@lwstn.eu <<-EOF
								Hoi,

								Potentiële problemen op BTRFS:

								$problems

								Ik balance elke nacht. Dat zou normaal moeten helpen.
								Als deze mail blijft komen, dan is onze storage echt aan het vollopen
								en moeten er dingen worden weggegooid
								of moet het volume worden vergroot.

								Meer info hier:

								* https://archive.kernel.org/oldwiki/btrfs.wiki.kernel.org/index.php/Problem_FAQ.html#I_get_.22No_space_left_on_device.22_errors.2C_but_df_says_I.27ve_got_lots_of_space
								* https://old.reddit.com/r/btrfs/comments/15a1pw2/unallocated_vs_free_space/
								* https://old.reddit.com/r/btrfs/comments/xxlju2/how_full_is_too_full/

								Succes ermee!
							EOF
						fi
					'';
				};
				btrfs-balance-storage = makeJobWithStorage {
					serviceConfig.Type = "simple";
					startAt = "01:00 Europe/Amsterdam";
					path = [ pkgs.btrfs-progs ];
					# Start with a bunch of low numbers to handle cases where
					# space is very limited. Then go faster up to 75% to
					# actually reclaim significant allocated space if possible.
					script = stripTabs ''
						for percent in 0 1 2 3 4 5 6 7 8 9 10 25 50 75; do
							printf 'Balancing blocks with usage below %d%%...\n' "$percent"
							btrfs balance start -dusage="$percent" /mnt/storage
						done
					'';
				};
				setup-persistent-homedirs = makeService {
					serviceConfig.Type = "oneshot";
					wantedBy = [ "multi-user.target" ];
					script = stripTabs ''
						ln -sfT /mnt/storage/live/home/gorinchemindialoog /home/gorinchemindialoog
					'';
				};
				gorinchemindialoog-publish = makeJobWithStorage {
					serviceConfig = {
						Type = "simple";
						User = "gorinchemindialoog";
					};
					startAt = "*:*:00,30";
					path = [ pkgs.rsync ];
					script = stripTabs ''
						rsync -a --delete \
							/mnt/storage/live/sftp/gorinchemindialoog/home/gorinchemindialoog/Website/Live/ \
							/srv/http/gorinchemindialoog.nl/
					'';
				};
				gorinchemindialoog-autocommit = makeJobWithStorage {
					serviceConfig = {
						Type = "simple";
						User = "gorinchemindialoog";
					};
					startAt = "04:00 Europe/Amsterdam";
					path = [ pkgs.gitMinimal pkgs.openssh ];
					script = stripTabs ''
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

				git-annex-assist-hello = {
					mailOnFailure = true;
					needsStorageVolume = "requires";
					serviceConfig = {
						Type = "simple";
						User = "git-annex";
					};
					startAt = "*:*:00";
					wantedBy = [ "multi-user.target" ];
					wants = [ "secret-bigstorage1-git-annex-hello-creds.service" ];
					after = [ "secret-bigstorage1-git-annex-hello-creds.service" ];
					path = [
						# basics:
						pkgs.gitMinimal pkgs.git-annex pkgs.openssh
						# needed for decryption:
						pkgs.gnupg
					];
					script = stripTabs ''
						git config --global user.name git-annex
						git config --global user.email git-annex@radstand.nl

						cd /mnt/storage/live/git-annex/rootdir/Hello

						# We don't want to store plaintext credentials in the
						# .git dir, so we set annex.cachecreds to false:
						git config --global annex.cachecreds false
						# We do *pretend* to have cached credentials by
						# symlinking a secret into the place where the cached
						# credentials usually reside, as a way to provide the
						# credentials to git-annex:
						mkdir -p .git/annex/creds
						ln -sTf /run/keys/bigstorage1-git-annex-hello-creds \
							.git/annex/creds/3ba01384-b195-4696-a200-732ed3b89647
						if ! fgrep 3ba01384-b195-4696-a200-732ed3b89647 .git/config > /dev/null; then
							git annex enableremote 3ba01384-b195-4696-a200-732ed3b89647
						fi

						# TODO manage wanted content:
						# - git annex get * in all dirs that have some WANTED marker file
						#   (or some recursive variant of the marker file in a parent)
						# - git annex drop * in all other dirs
						# (wantedcontent should be set to manual)
						
						# TODO manage non-present files (put a filename.NIET_HIER
						#      which should not be checked in,
						#      empty or maybe with whereis information in it)
						
						# TODO manage empty dirs (put .gitkeep in them?)
						
						git annex assist --explain

						# Make the wolk-exposed subdir group-writeable
						find Hello -type d,f -exec chmod g+w {} \; |& {
							grep -Ev 'chmod: changing permissions of .*: Operation not permitted' || true
						}
						echo "Ownership count:"
						find Hello -type d,f -exec stat --format %U {} + | sort | uniq --count
					'';
					# Eerste setup git-annex:
					# 1. Run: ssh-keygen
					# 2. Add the SSH key to users.trajanus and run thee.activate

					# # Nieuwe setup van een git-annex (niet getest):
					# cd /mnt/storage/live/git-annex/rootdir
					# mkdir Hello
					# chown git-annex:git-annex Hello
					# chmod g+rx,o= Hello
					# sudo -u git-annex bash
					# cd Hello
					# git init -b main
					# git remote add origin gitea@thee.radstand.nl:hello/Hello
					# git config --global user.name git-annex
					# git config --global user.email git-annex@radstand.nl
					# git annex init
					# git commit -m 'Initial empty commit' --allow-empty
					# git annex adjust --unlock-present
					# mkdir Hello # this is the subdir we share on wolk, to hide the .git dir
					# chmod g+rwxs Hello
					# touch Hello/.gitkeep
				};
			};
		};
		
		services.btrbk = {
			instances = {
				storage = {
					settings = {
						timestamp_format = "long-iso"; # safe from the caveat at https://digint.ch/btrbk/doc/btrbk.conf.5.html#_reference_time as long as we don't use btrbk for backups
						# We should preserve at least the latest 2 snapshots,
						# otherwise our backup script can fail if it runs during
						# btrbk's hourly run.
						# There's no way to specify latest 2.
						# 2h tends to preserve 3 snapshots, but that's good
						# enough.
						snapshot_preserve_min = "latest";
						snapshot_preserve = "2h 14d";
						preserve_day_of_week = "monday";
						preserve_hour_of_day = "0";
						volume."/mnt/storage" = {
							subvolume."live/*" = {
								snapshot_dir = "snapshots";
							};
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
			# Do NOT disable this, or we lose the ability to deploy.
			enable = true;
			# NOTE: extraConfig is put in a heredoc, so $ needs to be escaped!
			# TODO: maybe make a PR to fix that?
			extraConfig = stripTabs ''
				Match Group sftp_only
					ChrootDirectory /mnt/storage/live/sftp/%u
					ForceCommand internal-sftp
					DisableForwarding yes
					PermitTTY no

				Match User git-annex
					ForceCommand git-annex-shell -c "\$SSH_ORIGINAL_COMMAND"
					DisableForwarding yes
					PermitTTY no
				
				# match nobody to make sure any config that might come after
				# is not placed arbitrarily in the last Match block
				Match User xxxxxxxxxxxxxxxxxxxx
			'';
		};
		programs.screen = {
			enable = true;
			screenrc = stripTabs ''
				defscrollback 1000
			'';
		};

		programs.git = {
			enable = true;
			config = {
				# Limit memory usage of git client (used for autocommit tasks).
				# See https://discourse.gitea.io/t/remote-aborting-due-to-possible-repository-corruption/6617
				# and https://www.reddit.com/r/Gitea/comments/tb0ns6/errors_on_push/
				# and https://git-scm.com/docs/git-config
				pack.threads = 1;
				pack.windowMemory = "100m";
			};
		};

		users = {
			users.root = {
				# Password file must be present and readable by the system
				# during boot, without storage being mounted yet,
				# otherwise we're locking ourselves out.
				hashedPasswordFile = "/root/password";
				openssh.authorizedKeys.keys = [
					# Always have a key here, otherwise we can't deploy.
					komputiloj.users.jeroen.sshKeys.scarif
					komputiloj.users.jeroen.sshKeys.ferrix
				];
			};

			# Make sure users have the same uid on all our machines.
			# Add users here that don't have a fixed uid in nixpkgs/nixos.
			# Warning: changing uids here after a user has been created has no effect!
			# (I think - the note here was about containers.)
			# You have to rm /var/lib/nixos/uid-map and userdel the user.
			users.vmail = {
				uid = 70002;
				group = "vmail";
				isSystemUser = true;
			};
			groups.vmail = {
				gid = 70002;
			};
			users.radicale = {
				uid = komputiloj.users.radicale.linux.uid;
				group = "radicale";
				extraGroups = [ "sleutel" ];
				isSystemUser = true;
				home = "/mnt/storage/live/home/radicale";
				createHome = false;
			};
			groups.radicale = {
				gid = komputiloj.users.radicale.linux.uid;
			};
			users."70004" = {
				name = hello.users."70004".name;
				group = hello.users."70004".name;
				uid = hello.users."70004".linux.uid;
				isSystemUser = true;
				home = "/mnt/storage/live/home/${hello.users."70004".name}";
				createHome = false;
			};
			groups."70004" = {
				name = hello.users."70004".name;
				gid = hello.users."70004".linux.uid;
			};
			groups.sftp_only = {
				gid = 2001;
			};
			users.gorinchemindialoog = {
				isNormalUser = true;
				createHome = false;
				home = "/home/gorinchemindialoog"; # must exist both inside and outside the sftp_only chroot
				uid = gorinchemindialoog.users.gorinchemindialoog.linux.uid;
				passwordManagedBySleutel = true;
				extraGroups = [ "sftp_only" ];
			};
			users.git-annex = {
				# TODO rename the linux user to trajanus?
				uid = komputiloj.users.trajanus.linux.uid;
				group = "git-annex";
				extraGroups = [ "keys" ];
				isSystemUser = true;
				# Home is set such that we can with relative path:
				#   git clone git-annex@gently:Hello
				# This scheme looks nice but should not be opened up to untrusted users as-is,
				# because you can still pass arbitrary full paths and maybe other weirdness
				# (empty string, ~, ...?)
				# Also, we would need to somehow restrict access to specific repos.
				home = "/mnt/storage/live/git-annex/rootdir";
				createHome = false;
				shell = pkgs.bashInteractive; # needed for forced command
				openssh.authorizedKeys.keys = [
					# We already have restrictions in sshd_config,
					# but there is no full equivalent to restrict in sshd_config,
					# so we add restrict here just as an extra layer of security.
					"restrict ${komputiloj.users.jeroen.sshKeys.scarif}"
				];
			};
			groups.git-annex = {
				gid = komputiloj.users.trajanus.linux.uid;
			};
			users.nextcloud.extraGroups = [ "git-annex" ];
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
					5232 # Support old radicale URL
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
				# TODO requests without SNI get gorinchemindialoog.nl (I think);
				# this is nice for gorinchemindialoog.nl but looks a bit arbitrary. Should we do something about this?
				# TODO Run https://www.ssllabs.com/ssltest/index.html
				# NOTE forceSSL does not add HTST. If you do add HTST, be
				# careful with the default server, otherwise every subdomain
				# might end up with HTST enabled.

				"knol.radstand.nl" = {
					forceSSL = true;
					enableACME = true;
					locations."/" = {
						proxyPass = "http://localhost:5231/";
						extraConfig = stripTabs ''
							proxy_set_header  X-Script-Name "";
							proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
							proxy_pass_header Authorization;
						'';
					};
				};
				"radicale.radstand.nl" = {
					# Compatibility with the old radicale on https://radicale.radstand.nl:5232
					# We listen with SSL on port 5232 and proxy this to radicale.
					# We also listen on port 80 without SSL for Let's Encrypt challenges.
					listen = [{
						addr = "0.0.0.0";
						port = 80;
					} {
						addr = "[::0]";
						port = 80;
					} {
						addr = "0.0.0.0";
						port = 5232;
						ssl = true;
					} {
						addr = "[::0]";
						port = 5232;
						ssl = true;
					}];
					enableACME = true;
					# We need to configure some things manually when we have `listen` blocks:
					extraConfig = stripTabs ''
						ssl_certificate /var/lib/acme/radicale.radstand.nl/fullchain.pem;
						ssl_certificate_key /var/lib/acme/radicale.radstand.nl/key.pem;
						ssl_trusted_certificate /var/lib/acme/radicale.radstand.nl/chain.pem;
					'';
					# forceSSL = true; # TODO try this
					locations."/" = {
						proxyPass = "http://localhost:5231/";
						extraConfig = stripTabs ''
							proxy_set_header  X-Script-Name "";
							proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
							proxy_pass_header Authorization;
						'';
					};
				};
				# TODO compare access logs
				"www.gorinchemindialoog.nl" = {
					addSSL = true;
					enableACME = true;
					globalRedirect = "gorinchemindialoog.nl";
				};
				"gorinchemindialoog.nl" = {
					forceSSL = true;
					enableACME = true;
					root = "/srv/http/gorinchemindialoog.nl";
					extraConfig = stripTabs ''
						disable_symlinks if_not_owner;
						add_header Cache-Control "no-cache";
						error_page 404 /404.html;
					'';
				};
				"www.radstand.nl" = {
					addSSL = true;
					enableACME = true;
					globalRedirect = "radstand.nl";
				};
				"radstand.nl" = {
					forceSSL = true;
					enableACME = true;
					root = "/mnt/storage/live/http-hodgepodge/radstand.nl";
					locations = {
						"${hello.nginxLocations.liedjes.location}" =
							hello.nginxLocations.liedjes.config;
					};
					extraConfig = stripTabs ''
						disable_symlinks if_not_owner from=$document_root/dump;
						add_header Cache-Control "no-cache";
						index index.html;
					'';
					default = true;
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
	
			# TODO get rid of nginx welcome page on mail.radstand.nl
			fqdn = "mail.radstand.nl";
			sendingFqdn = "gently.radstand.nl";
			domains = [ "gorinchemindialoog.nl" "radstand.nl" ];

			loginAccounts = {
				"info@gorinchemindialoog.nl" = {
					# name = "gorinchemindialoog"; # not sure what this does
					hashedPasswordFile = "/mnt/storage/live/sleutel/rootdir/users/gorinchemindialoog/password/password.bcrypt";
				};
			};
			forwards = mapNames (name : name + "@gorinchemindialoog.nl") gorinchemindialoog.forwards // {
				# catch-all: (let op: dit stuurt ALLES door, niet alleen
				# onbekende accounts):
				# "@radstand.nl" = "jeroen@lwstn.eu";
			};

			indexDir = "/var/mail-indexes";
			mailDirectory = "/mnt/storage/live/mail/vmail"; # TODO make relevant service depend on this mount!
			sieveDirectory = "/mnt/storage/live/mail/sieve"; # TODO not sure if this is persistent state
			vmailGroupName = "vmail";
			vmailUserName = "vmail";
			vmailUID = 70002;

			certificateScheme = "acme-nginx"; # let's hope this uses the regular letsencrypt infrastructure of NixOS so it doesn't clash with nginx
		};
		systemd.services.postfix = {
			needsStorageVolume = "requires";
		};
		systemd.services.dovecot2 = {
			needsStorageVolume = "requires";
		};

		services.radicale = {
			enable = true;
			settings = {
				server = {
					hosts = "0.0.0.0:5231"; # nginx should use ipv4 internally
				};
				auth = {
					type = "htpasswd";
					htpasswd_filename = "/mnt/storage/live/sleutel/rootdir/apps/knol/auth";
					htpasswd_encryption = "bcrypt";
				};
				storage = {
					filesystem_folder = "/mnt/storage/live/radicale/collections";
					# Warning: this hook cannot handle usernames containing ' or \
					hook = "${komputiloj.packages.radicale-commit-hook}/bin/hook '%(user)s'";
				};
			};
		};
		systemd.services.radicale = {
			needsStorageVolume = "requires";
		};

		environment.systemPackages = with pkgs; [
			screen
			netcat
			vim
			cryptsetup btrfs-progs parted
			mailutils
			gitMinimal
			gitAndTools.git-annex
			btdu # btrfs disk usage profiler
			zip unzip

			thee.packages.forgejo-cli

			sqlite
		];
	};
}
