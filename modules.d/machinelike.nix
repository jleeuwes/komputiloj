{ ... }:
# Common configuration to import into every machinelike entity,
# which is either a (nixops) machine or a container.
{ ... }: {
	config = {
		users.mutableUsers = false;
		boot.tmp.cleanOnBoot = true;
		networking.usePredictableInterfaceNames = true;
	};
}
