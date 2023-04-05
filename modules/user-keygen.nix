{ config, lib, pkgs,...}:

with lib;
with (import ../utilecoj.nix);

let
	# keygen = {
	userOptions = {
		options.openssh.keygen = {
		enabled = mkOption {
			type = types.bool;
			default = false;
			description = lib.mkDoc (stripTabs ''
				Whether to generate an SSH keypair without a passphrase for this
				user.
			'');
		};
		};
	};

	# userOptions = {
	# 	options.openssh.keygen = mkOption {
	# 		type = types.attrsOf keygen;
	# 		default = {};
	# 	};
	# };

	keygenUsers =
		map (user: user.name)
		(filter (user: user.openssh.keygen.enabled)
		(attrValues config.users.users));
in
{
	options = {
		users.users = mkOption {
			type = types.attrsOf (types.submodule userOptions);
			# example = {
			# 	jeroen = {
			# 		createHome = true;
			# 		keygen.enabled = true;
			# 	};
			# };
		};
	};

	config = {
		system.activationScripts.keygen = {
			deps = [ "users" ];
			text = stripTabs ''
				# keygenUsers=( ${strings.escapeShellArgs keygenUsers} )
				for keygenUser in ${strings.escapeShellArgs keygenUsers}; do
					echo "TODO $keygenUser"
					# ${pkgs.util-linux}/bin/runuser -u "$keygenUser" \
					# 	${pkgs.openssh}/bin/ssh-keygen -N ""
				done
			'';
		};
	};
}
