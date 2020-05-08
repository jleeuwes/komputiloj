# Common configuration to import into every container.
# Maybe make this module export a function that creates/finishes
# a container so we can also put stuff like autoStart and privateNetwork here
# (we can't do that with imports because autoStart is defined outside the config
# attribute)
{ ... }: {
	imports = [ ./machinelike.nix ];
	config = {
		# nixpkgs is afraid we're locking ourselves out if don't
		# give a root password of if we set it to !
		# But this isn't a problem because this is a container.
		# We get around that with this fake deactivated password.
		users.users.root.hashedPassword = "!this_is_fine";
	};
}

