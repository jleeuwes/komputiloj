# Common configuration to import into every machinelike entity,
# which is either a (nixops) machine or a container.
{ ... }: {
	config = {
		users.mutableUsers = false;
		boot.cleanTmpDir = true;
		networking.usePredictableInterfaceNames = true;
	};
}
