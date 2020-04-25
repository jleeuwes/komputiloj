# This networking stuff was populated by nixos-infect
# with the networking details gathered from the active system.
# We don't need this because of https://support.tilaa.com/hc/en-us/articles/228650427-How-to-configure-your-server-to-use-DHCP
# but maybe it might come in handy sometime.
networking = {
	nameservers = [
		"46.19.33.38"
		"46.19.33.111"
	];
	defaultGateway = "46.19.33.1";
	defaultGateway6 = "fe80::fffe";
	dhcpcd.enable = false;
	usePredictableInterfaceNames = lib.mkForce true;
	interfaces = {
		ens3 = {
			ipv4.addresses = [
				{ address="46.19.33.88"; prefixLength=24; }
			];
			ipv6.addresses = [
				{ address="2a02:2770::21a:4aff:fef1:b128"; prefixLength=64; }
				{ address="fe80::21a:4aff:fef1:b128"; prefixLength=64; }
			];
			ipv4.routes = [ { address = "46.19.33.1"; prefixLength = 32; } ];
			ipv6.routes = [ { address = "fe80::fffe"; prefixLength = 32; } ];
		};
	};
};
# This is from nixos-infect as well
services.udev.extraRules = ''
	ATTR{address}=="00:1a:4a:f1:b1:28", NAME="ens3"
'';

