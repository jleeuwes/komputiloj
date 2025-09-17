{ komputiloj, ... }:
{ modulesPath, ... }: {
	imports = [
		(modulesPath + "profiles/qemu-guest")
		komputiloj.modules.machinelike
	];

	config = {
		boot.loader.grub.device = "/dev/vda";
		fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };

		networking.useDHCP = true;
	};
}
