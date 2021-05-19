{
	# Inspiration taken from https://github.com/nh2/nixops-tutorial/blob/master/example-nginx-deployment.nix

	network.description = "Our humble all-encompassing serviloj deployment";

	# TODO quota
	# TODO mail
	# TODO monitoring
	
	gently = { config, nodes, lib, pkgs, ... }: {
		imports = [ ./modules/tilaa_vps.nix ];

		deployment.targetHost = "gently.radstand.nl";
		deployment.provisionSSHKey = false;
		
		deployment.keys = {
		};
		
		# The rest is a configuration just like nixos/configuration.nix
		
		# WARNING this setting is not used
		# Instead, nixops determines the stateVersion at first deploy based on the NixOS version it encounters.
		# TODO change our deployment scripts so they keep the state db, maybe on the server itself?
		system.stateVersion = "20.03";
		
		services.openssh.enable = true;
		users.users = {
			root = {
				passwordFile = "/root/password"; # must be present on the machine
				openssh.authorizedKeys.keyFiles = [
					../scarif/home/jeroen/.ssh/id_rsa.pub
				];
				openssh.authorizedKeys.keys = [
					''
					ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDafdp3tAbIxh12qC4aBaYcI19vZ3IwqZu3wl1GiZscKVpWmDoqhKyjyZ8Ep5odXbL8+6D4FI1UOqBvX872I0eeQuekIppSi07xFJxc6+zFTNXUN1K4+IoiLyQ5yDGDndxplXWgL751r3IB2Ozv+YE5YPm7kjzxYmNq9a+x2SsKr5KNiM2uySDbGrYhdEnqwwp8qz2I9ikNoSCmHuXGR16HLptJHLxTCRlzyxXb7zocN5owQ/kXzl9Ks0nNyiCcs5q4CdSzfzHBUgoe7Pkqvunnp4hRSsDk0ZSCdLtaJm280sRrE1Ff5+FgHIdj+CxvdiRdftK4SJ5GpZMjl1ocYAxkrzK4YwT9Ha9TAuMGq4g8pgEi50unyj+OH3Oc9oZ9plfbT10jJ4uIP+RfJpD3OKrFjlLsno2ZBuE77SXuij4/duVuCghviVqkm1lD24WMR89aIJceWREn1eqEJuUz5HnlrB6uOrXXp/hnN6MA5FztMj0Dw9cMjLvNCcFxISTQxf8= root@gently2
					''
				];
			};

			# Make sure users have the same uid on all our machines.
			# Add users here that don't have a fixed uid in nixpkgs/nixos.
			# also exist on the machine (actually, in our whole deployment), with a fixed uid.
			# TODO also fix groups
			# Warning: changing uids here after a user has been created has no effect!
			# (I think - the note here was about containers.)
			# You have to rm /var/lib/nixos/uid-map and userdel the user.
			nextcloud = {
				uid = 70000;
				extraGroups = [ "keys" ];
			};
		};

		networking = {
			hostName = "gently";
			hosts = {
				"10.0.0.2" = [ "wilder.radstand.nl" "wilder" ];
			};
			domain = "radstand.nl";
			interfaces.ens4.ipv4 = {
				addresses = [{
					# TODO ipconfig says broadcast is 0.0.0.0 - is that okay?
					address = "10.0.0.1";
					prefixLength = 24;
				}];
			};
			firewall = {
				allowPing = true;
				# we allow port 22 globally and for ens3
				# in case the interface name changes and we were to be locked out
				allowedTCPPorts = [ 22 ];
				interfaces.ens3 = {
					# port 80 is only allowed for Let's Encrypt challenges
					allowedTCPPorts = [ 22 80 443 ];
				};
				interfaces.ens4 = {
					allowedTCPPorts = [ 1234 ];
				};
				
				# Make sure we don't allow traffic from IPs that shouldn't come
				# through the corresponding interface.
				# This prevents non-10.* traffic from coming in over ens4,
				# as long as the mask (prefix) is set correctly on that interface (see above).
				# It doesn't prevent 10.* traffic from the internet (I think),
				# which is what the extraCommands below are for.
				#
				# Note that this is usually true by default, but I want to be explicit.
				#
				# Also, note that I'm being a bit paranoid here, because ISPs and Tilaa shouldn't route 10.*
				# over the internet anyway. But let's call it defense in depth :P
				checkReversePath = true;

				extraCommands = ''
					# Cleanup
					ip46tables -D INPUT -j protect-wendimoor 2> /dev/null || true
					ip46tables -D FORWARD -j protect-wendimoor 2> /dev/null || true
					ip46tables -F protect-wendimoor 2> /dev/null || true
					ip46tables -X protect-wendimoor 2> /dev/null || true

					ip46tables -F log-refuse-wendimoor 2> /dev/null || true
					ip46tables -X log-refuse-wendimoor 2> /dev/null || true

					# Set up log+refuse chain
					ip46tables -N log-refuse-wendimoor
					ip46tables -A log-refuse-wendimoor -j LOG --log-prefix "foreign packet in wendimoor: " --log-level 4 # TODO actively do something with these warnings
					ip46tables -A log-refuse-wendimoor -j DROP
					
					# Set up the actual protection
					ip46tables -N protect-wendimoor
					# Drop any packet from ens3 (the internet) that claims to be from 10.* (wendimoor, ens4)
					iptables -A protect-wendimoor -i ens3 -s 10.0.0.0/8 -j log-refuse-wendimoor
					# Drop any packet from ens3 (the internet) that wants to reach 10.* (wendimoor)
					iptables -A protect-wendimoor -i ens3 -d 10.0.0.0/8 -j log-refuse-wendimoor
					# Don't allow any IPv6 in wendimoor (we don't need it so let's just remove the possibility):
					ip6tables -A protect-wendimoor -i ens4 -j log-refuse-wendimoor

					# Put our checks before NixOS firewall checks
					ip46tables -I INPUT -j protect-wendimoor
					ip46tables -I FORWARD -j protect-wendimoor
				'';
			};
		};

		services.fail2ban.enable = true;

		security.acme = {
			acceptTerms = true;
			# I'm guessing the rest of the cert config is autogenerated
			# due to the nginx config.
			certs."wolk.radstand.nl".email = "jeroen@lwstn.eu";
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
			};
		};

		environment.systemPackages = with pkgs; [
			screen
			netcat
			vim
		];
	};

	# will supersede gently
	gently2 = { config, nodes, lib, pkgs, ... }: {
		imports = [ ./modules/hetzner_vps.nix ];

		deployment.targetHost = "gently2.radstand.nl";
		deployment.provisionSSHKey = false;
		
		deployment.keys = {
			"luks-storage" = {
				keyCommand = [ "wachtwoord" "get-exact" "secrets/luks-storage@hetzner" ];
			};
			"nextcloud-admin" = {
				keyCommand = [ "wachtwoord" "get-exact" "secrets/admin@wolk.radstand.nl" ];
				user = "nextcloud";
				group = "nextcloud";
				permissions = "ug=r,o=";
			};
		};
		
		# The rest is a configuration just like nixos/configuration.nix
		
		# WARNING this setting is not used
		# Instead, nixops determines the stateVersion at first deploy based on the NixOS version it encounters.
		# TODO change our deployment scripts so they keep the state db, maybe on the server itself?
		# See also https://github.com/NixOS/nixops/issues/1340 (mkForce doesn't work)
		# TODO try and see what the actually used stateVersion is by putting it
		# in some file in /etc
		system.stateVersion = lib.mkForce "20.09";

		systemd.services.mount-storage = {
			serviceConfig = {
				Type = "oneshot";
				RemainAfterExit = true;
			};
			requires = [ "luks-storage-key.service" ];
			after    = [ "luks-storage-key.service" ];
			wantedBy = [ "multi-user.target" ];
			requiredBy = [
				"nextcloud-cron.service"
				"nextcloud-setup.service"
				"nextcloud-update-plugins.service"
				"phpfpm-nextcloud.service"
			];
			before = [
				"nextcloud-cron.service"
				"nextcloud-setup.service"
				"nextcloud-update-plugins.service"
				"phpfpm-nextcloud.service"
			];
			path = [ pkgs.cryptsetup pkgs.utillinux pkgs.unixtools.mount pkgs.unixtools.umount ];
			script = ''
				cryptsetup open UUID=6c8d5be7-ae46-4e51-a270-fd5bdce46f3b storage --type luks --key-file /run/keys/luks-storage
				mkdir -p /mnt/storage
				mount /dev/mapper/storage /mnt/storage
			'';
			postStop = ''
				if mountpoint -q /mnt/storage; then
					umount /mnt/storage
				fi
				cryptsetup close storage
			'';
		};

		services.openssh.enable = true;
		users = {
			users.root = {
				passwordFile = "/root/password"; # must be present on the machine
				openssh.authorizedKeys.keyFiles = [
					../scarif/home/jeroen/.ssh/id_rsa.pub
				];
			};

			# Make sure users have the same uid on all our machines.
			# Add users here that don't have a fixed uid in nixpkgs/nixos.
			# also exist on the machine (actually, in our whole deployment), with a fixed uid.
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
		};

		networking = {
			hostName = "gently2";
			domain = "radstand.nl";
			interfaces.ens3 = {
				useDHCP = true;
			};
			firewall = {
				allowPing = true;
				# port 80 is only allowed for Let's Encrypt challenges
				allowedTCPPorts = [ 22 80 443 ];
			};
		};

		services.fail2ban.enable = true;

		security.acme = {
			acceptTerms = true;
			# I'm guessing the rest of the cert config is autogenerated
			# due to the nginx config.
			certs."wolk.radstand.nl".email = "jeroen@lwstn.eu";
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
			};
		};

		services.nextcloud = {
			enable = true;

			package = pkgs.nextcloud20;

			home = "/mnt/storage/live/nextcloud";

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
		};

		environment.systemPackages = with pkgs; [
			screen
			netcat
			vim
			cryptsetup btrfs-progs parted
		];
	};
}
