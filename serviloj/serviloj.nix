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
			"nextcloud-admin" = {
				keyCommand = [ "wachtwoord" "cat" "secrets/admin@wolk.radstand.nl" ];
				user = "nextcloud";
				group = "nextcloud";
				permissions = "ug=r,o=";
			};
			"onedrive-js-token" = {
				keyCommand = [ "wachtwoord" "cat" "secrets/oauth-token:ACCOUNT_NAME_OBFUSCATED@onedrive.com" ];
				user = "js";
				group = "js";
				permissions = "u=r,go=";
			};
		};
		
		# The rest is a configuration just like nixos/configuration.nix
		
		# I set the stateVersion here because I don't want to use the nixops state file.
		# stateVersion starts with 19.09 because that it the version nixos-infect installs.
		# You should only update this if you think there is no (important) state created yet.
		# NOTE: nixops determines the stateVersion based on /etc/os-release
		# on the target machine, so I'm not sure if this setting here has any effect.
		system.stateVersion = "20.03";
		
		services.openssh.enable = true;
		users.groups = {
			js = {
				gid = 1000;
			};
		};
		users.users = {
			root = {
				passwordFile = "/root/password"; # must be present on the machine
				openssh.authorizedKeys.keyFiles = [
					../scarif/home/jeroen/.ssh/id_rsa.pub
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

			js = {
				uid = 1000;
				group = "js";
				extraGroups = [ "keys" ];
				useDefaultShell = true;
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

		services.nextcloud = {
			# TODO take relevant php/nextcloud config from current
			# manual installation

			enable = true;

			package = pkgs.nextcloud20;

			home = "/srv/nextcloud";

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
			rclone
		];
	};

	# systemd.services = {
	# 	onedrive-js = {
	# 		after = ""; # TODO
	# 		preStart = ''
	# 			
	# 		'';
	# 	};
	# };
}
