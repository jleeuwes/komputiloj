{ ... }: {
	# I don't like the way we link to <nixpkgs> here
	# instead of using the pkgs from configuration.nix or serviloj.nix;
	# TODO increase Nix-fu and then come up with something cleaner
	imports = [
		<nixpkgs/nixos/modules/profiles/qemu-guest.nix>
		./machinelike.nix
	];

	config = {
		boot.loader.grub.device = "/dev/vda";
		fileSystems."/" = { device = "/dev/vda1"; fsType = "ext4"; };

		networking.useDHCP = true;
	};
}
