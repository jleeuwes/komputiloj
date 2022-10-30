let sources = import (<komputiloj> + /sources.nix);
in
{ config, pkgs, lib, ... }:

{
	# # Add the --option extra-builtins-file to nix
	# # using a magic spell from https://elvishjerricco.github.io/2018/06/24/secure-declarative-key-management.html
	# (Doesn't work: warning: ignoring the user-specified setting 'extra-builtins-file', because it is a restricted setting and you are not a trusted user)
	# nix.extraOptions = ''
	# 	plugin-files = ${pkgs.nix-plugins.override { nix = config.nix.package; }}/lib/nix/plugins/libnix-extra-builtins.so
	# '';
	
	# We need to construct our own NIX_PATH, because the default becomes very weird with our scheme.
	# Also, make sure you don't have a ~/.nix-defexpr because that gets added
	# too (see /etc/set-environment).
	nix.nixPath = [
		# First, reconstruct the NIX_PATH defined by with-pinned-NIX_PATH.
		# This assumes no extra paths are added there in the future.
		# Don't do ${<komputiloj>}, because that makes a weird non-working copy of
		# our sources.nix in the nix-store. With toString the literal path is
		# used instead.
		"komputiloj=${builtins.toString <komputiloj>}"
		"nixpkgs=${sources.nixpkgs.unpacked}"
		"nixos-config=${builtins.toString <nixos-config>}"
	];

	nixpkgs.config = {
		# Selectively allow some unfree packages
		# - https://nixos.org/nixpkgs/manual/#sec-allow-unfree
		allowUnfreePredicate = pkg:
			builtins.elem (lib.getName pkg) [
				"steam-original"
				"android-studio"
			];

		# Selectively allow some packages with known vulnerabilities
		# - https://nixos.org/manual/nixpkgs/stable/#sec-allow-insecure
		permittedInsecurePackages = [
			"python2.7-urllib3-1.26.2" # used by... something? TODO get rid of it
			"python2.7-pyjwt-1.7.1"    # TODO get rid of this
		];
	};
	nixpkgs.overlays =
		let
			overrideUndesiredPackages = import (<komputiloj> + /packages/undesired-packages-override.nix);
			mergeUndesiredPackages = prefix: pkgs: pkgs // overrideUndesiredPackages prefix pkgs;
		in [
			(import (<komputiloj> + /packages/git-annex-overlay.nix))
			(self: super: overrideUndesiredPackages "" super)
			(self: super: {
				# NOTE: a lot of fiddling around to mark undesired packages inside our sub-packagesets.
				# Please not that if we use a package from unstable, its dependencies seem to come from
				# the combines packageset, not from unstable.
				# So all this fiddling around might not be super useful.
				unstable = mergeUndesiredPackages "unstable." (
					import sources.unstable.unpacked {
						config = config.nixpkgs.config;
					}
				);
				nixos_18_09 = mergeUndesiredPackages "nixos_18_09." (
					import sources.nixos_18_09.unpacked {
						config = config.nixpkgs.config;
					}
				);
			})
		];

	imports =
		[ # Include the results of the hardware scan.
			./hardware-configuration.nix
		];
	

	# Make sure ~/bin is added to PATH:
	environment.homeBinInPath = true;

	# Use the systemd-boot EFI boot loader.
	boot.loader.systemd-boot.enable = true;
	boot.loader.efi.canTouchEfiVariables = true;

	# Make /tmp in-memory:
	boot.tmpOnTmpfs = true;

	# Enable read-write NTFS support:
	boot.supportedFilesystems = [ "ntfs" ];

	# Here I was looking into creating a ramfs mount with /tmp properties
	# (i.e. anyone can write) for temporarily decripted keys.
	# However, this is discouraged as size limits are not enforced?
	# Anyway, we don't have swap on this system so we just use /tmp.
	# boot.specialFileSystems = {
	# 	"/run/stuff" = {
	# 		fsType = "ramfs";
	# 		options = [ "nosuid" "nodev" "mode=01777" "size=100M" ];/swap
	# };

	networking.hostName = "scarif";
	networking.wireless = {
		enable = true;  # Enables wireless support via wpa_supplicant.
		interfaces = [ "wlp4s0" ];
	# Netwerken staan in /etc/wpa_supplicant.conf vanwege passphrases
	};
	networking.nameservers = [ "1.1.1.1" "8.8.8.8" ];

	# Select internationalisation properties.
	console = {
		font = "Lat2-Terminus16";
		keyMap = "us";
	};
	i18n = {
		defaultLocale = "nl_NL.UTF-8";
	};

	# Set your time zone.
	time.timeZone = "Europe/Amsterdam";

	fonts = {
		enableDefaultFonts = true; # <- dit lijkt niet echt iets uit te maken t.o.v.  weglaten
		fonts = [
			# nerdfonts is HUGE and nixos-rebuild hangs without progress information
			# (withFont does not help)
			# (also it is a useless font for emoji - it has nice icons but only
			# custom ones, not stuff actually defined in unicode)
			# (pkgs.nerdfonts.override { withFont = "SpaceMono"; })
			# noto is veelbelovend maar sommige emoji zijn kleur (niet zo erg) en ENORM (wel erg)
			# pkgs.noto-fonts-emoji
			# pkgs.noto-fonts
			# We weten nu trouwens zeker dat er een of ander fallback-systeem aan
			# het werk is, want ook als je andere fonts kiest en noto-fonts-emoji is
			# aanwezig krijg je emoji.
			# WE HEBBEN EEN WINNAAR!
			# symbola heeft mooie zwartwit-emoji op normale grootte.
			# woooooooot \o/
			# (is unfree tegenwoordig dus we gebruiken een oudere versie)
			pkgs.nixos_18_09.symbola

			pkgs.dejavu_fonts
			pkgs.ubuntu_font_family
		];
	};

	# List packages installed in system profile. To search by name, run:
	# $ nix-env -qaP | grep wget
	environment.systemPackages = let
		# this incomprehensible magic incantation was conjured by TheMsDosNerd at https://www.reddit.com/r/NixOS/comments/8cq4ic/problem_installing_python_package/
		myPythonPackages = pythonPackages: with pythonPackages; [
				# flask
				# flask-api
		]; in with pkgs; [
		mkpasswd

		# for running precompiled games:
		steam-run-native

		ocrad

		# prettiness ( more inspiration at https://gist.github.com/taohansen/d15e1fe4674a286cb9bcd8e3378a9f23 and https://stackoverflow.com/questions/38576616/how-to-install-gtk-themes-under-nixos-without-hacky-scripts )
		# gtk-engine-murrine arc-theme arc-icon-theme elementary-icon-theme
		# gtk
		hicolor-icon-theme xfce.xfce4-icon-theme tango-icon-theme
		# usefull programs:
		gitFull vim file subversionClient pciutils pmount squashfsTools
		parted gparted
		wget rtorrent
		unstable.git-annex git-annex-remote-rclone rclone
		sshpass
		gnupg paperkey qrencode zbar pwgen
		inetutils # for ftp for the nas
		openssl
		nmap
		tree

		pamix pavucontrol alsaUtils
		zstd # for unpacking arch packages
		glxinfo
		xorg.xdpyinfo
		xorg.xev
		
		# Install scanimage (saneBackends) and scanadf (saneFrontends),
		# among other scanning tools.
		# I can't get ADF scanning to work. 'Best' command line is:
		#     scanimage -d pixma:04A91824_214FE1 --batch=scan%02d.png --format tiff --batch-start 1 --batch-double --batch-count 3 --source "Automatic Document Feeder"
		# But it gives "scanimage: sane_read: Operation was cancelled"
		# - this is probably related to https://gitlab.com/sane-project/backends/-/merge_requests/213
		sane-backends sane-frontends
		
		# programming:
		(python3.withPackages myPythonPackages)
		dejsonlz4 # for reading firefox jsonlz4 files
		remarshal # for yaml2json etc
		jq # json manipulation
		# unstable.android-studio
		jetbrains.idea-community
		openjdk11 maven visualvm
		love_11
		arduino
		haskellPackages.ghc

		# LaTeX and PDF:
		# (Not sure if rubber uses the chosen texlive distribution)
		texlive.combined.scheme-medium
		rubber
		poppler_utils
		qpdf pdftk

		firefox thunderbird
		isync # for e-mail backups (eigenlijk mbsync)
		chromium
		geany
		zathura # pdf viewer
		# gnome stuff (won't work because dconf is missing):
		# gnome3.gedit
		# xfce stuff:
		# support stuff (needed for generating thumbnails for instance - maar het werkt voor geen zak)
		xfce.exo xfce.xfconf # xfce.xfce4settings
		shared-mime-info xfce.tumbler # <- these two in particular seemed to do the trick!
		# mindmapping-tools:
		vym freemind
		vlc ffmpeg

		inkscape gimp exiftool
		imagemagick scrot
		libreoffice antiword
		audacity
		
		hsetroot # program to help my xmonad config set the background in an xfce-terminal compatible way
		# xorg.xbacklight # doesn't work anymore - https://github.com/NixOS/nixpkgs/issues/55520#issuecomment-470501591
		brightnessctl
		unclutter-xfixes # hides the mouse if unused
		dmenu xsel # some helpers for menus
		xmobar # status bar
		
		pstree
		
		# actual programs:
		xfce.xfce4-terminal xfce.thunar xfce.ristretto
		# Belgian eID (it looks in /run/current-system/sw/ by default for some things so it's easier to have it installed system-wide):
		eid-mw

		# other package managers (to be used for non-reproducable things only):
		nodejs #npm

		zip unzip
		par2cmdline # for error-correcting archives
		llvmPackages.bintools # to get ar (to extract .deb files)
		sqlite
	];

	programs.gnupg.agent = {
		enable = true;
		enableSSHSupport = true;
		pinentryFlavor = "gtk2";
	};

	# Generate setuid wrappers for pmount:
	security.wrappers = {
		pmount = {
			source  = "${pkgs.pmount}/bin/pmount";
			setuid = true;
			owner = "root";
			group = "root";
		};
		pumount = {
			source = "${pkgs.pmount}/bin/pumount";
			setuid = true;
			owner = "root";
			group = "root";
		};
	};

	# List services that you want to enable:
	

	# Needed for ChromeCast to work in chromium,
	# in combination with enabling
	# chrome://flags/#load-media-router-component-extension and
	# chrome://flags/#cast-media-route-provider
	services.avahi.enable = true;

	# Enable the OpenSSH daemon.
	# services.openssh.enable = true;

	# Enable CUPS and printing.
	# Add the Canon TR7500 printer using:
	#   lpadmin -p kantoor -v ipp://192.168.1.44:631/ipp/print -E -m everywhere
	# (Don't forget to make the IP fixed in the router config!)
	# Then further configure it at http://localhost:631/printers/kantoor
	# 
	# I used https://wiki.debian.org/CUPSDriverlessPrinting to get this stuff
	# working; I had to use the IP address in the URI instead of using the
	# dnssd://... detected by the CUPS web interface or the name-based ipp://...
	# detected by ippfind
	services.printing.enable = true;
	
	# enable gvfs to have ftp support etcetera in thunar
	# (doesn't work; maybe use
	# https://github.com/NixOS/nixpkgs/blob/release-19.09/nixos/modules/services/x11/desktop-managers/xfce.nix
	# ?)
	# services.gvfs.enable = true;
	# services.gvfs.package = pkgs.xfce.gvfs;
	
	# Enable sound.
	sound.enable = true;
	sound.extraConfig = ''
pcm.pulse {
  type pulse
  hint.description "PulseAudio device for audacity"
}
ctl.pulse {
  type pulse
}
'';
	hardware.pulseaudio.enable = true;
	# Some NixOS packages can be built with explicit PulseAudio support which is disabled by default. This support can be enabled in all applicable packages by setting:
	nixpkgs.config.pulseaudio = true;

	hardware.bluetooth = {
		enable = true;
	};

	# Screen locker
	services.physlock = {
		enable = true;
		# nog niet beschikbaar (eens updaten!)
		# allowAnyUser = true; # of moeten we rechten via systemd regelen?
	};

	# Enable the X11 windowing system.
	services.xserver = {
		enable = true;
		layout = "us";
		xkbOptions = "compose:ralt,eurosign:e";

		displayManager.lightdm.greeters.gtk.cursorTheme = {
			# This only sets the cursor on the greeter screen,
			# but it also install the cursor package,
			# which we use in a bunch of our user-local config to configure the
			# cursor theme and size *inside* an X session.
			name = "Vanilla-DMZ";
			package = pkgs.vanilla-dmz;
			size = 32;
		};

		# displayManager.sessionCommands = ''
		# 	# ( adapted from https://gist.github.com/taohansen/d15e1fe4674a286cb9bcd8e3378a9f23 )
		# 	# (lijkt allemaal voor geen flikker te werken)
		# 	# This allows GTK to load SVG icons.
		# 	export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
		# 	# Set GTK_PATH so that GTK+ can find the Xfce theme engine.
		# 	export GTK_PATH=${pkgs.gtk-engine-murrine}/lib/gtk-2.0
		# 	# Set GTK_DATA_PREFIX so that GTK+ can find the Xfce themes.
		# 	export GTK_DATA_PREFIX=${config.system.path}
		# 	# Launch xfce settings daemon.
		# 	${pkgs.xfce.xfce4-settings}/bin/xfsettingsd &
		# '';

		windowManager.xmonad.enable = true;
		windowManager.xmonad.enableContribAndExtras = true;
		desktopManager.xterm.enable = false;
		desktopManager.xfce.enable = true;
	
		# touchpad
		# (this was synaptics before, but I had to change to libinput
		# because xfce demands it, and it turns out to work better. Yay!)
		libinput.enable = true;
		libinput.touchpad = {
			scrollMethod = "twofinger";
			sendEventsMode = "disabled-on-external-mouse";
			tapping = false;
			tappingDragLock = false;
			disableWhileTyping = true;
		};
	};

	# en xfce voor de goodies
	# services.xserver.desktopManager.xfce.enable = true;
	# (TODO die vereist upower, dus staat nu uit;
	# als ik dingen mis moet ik de losse programma's maar installeren)
	

	# services.udev.extraRules = ''
	#   # https://wiki.archlinux.org/index.php/Touchpad_Synaptics#Disable_touchpad_on_mouse_detection
	#   # Turn off touchpad when mouse is connected (hardcoded for user jeroen)
	#   # ACTION=="add", ATTRS{bInterfaceProtocol}=="02", ATTRS{bInterfaceClass}=="03", ATTRS{bInterfaceSubClass}=="01", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/jeroen/.Xauthority", RUN+="${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=1"
	#   ACTION=="add", SUBSYSTEM=="input", KERNEL=="mouse[0-9]*", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/jeroen/.Xauthority", RUN+="${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=1"
	#   ACTION=="remove", SUBSYSTEM=="input", KERNEL=="mouse[0-9]*", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/jeroen/.Xauthority", RUN+="${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=0"
	# '';
	# Enable adb group and udev rules and such:
	programs.adb.enable = true;

	# Enable the KDE Desktop Environment.
	# services.xserver.displayManager.kdm.enable = true;
	# services.xserver.desktopManager.kde4.enable = true;

	# For 32-bit games:
	hardware.opengl.driSupport32Bit = true;
	hardware.opengl.driSupport = true; # is actually the default

	# Define a user account. Don't forget to set a password with ‘passwd’.
	users.extraUsers.jeroen = {
		uid = 1000;
		isNormalUser = true;
		description = "Jeroen Leeuwestein";
		extraGroups = [ "wheel" "network-manager" "dialout" "adbusers" "video" "audio"
			"lp" # for scanning with Canon
		];
	};
	users.extraUsers.speel = {
		# A dedicated user account to play untrusted game binaries.
		uid = 1001;
		isNormalUser = true;
		description = "Speel Spelletjes";
		extraGroups = [ "video" "audio" ];
	};

	# The NixOS release to be compatible with for stateful data such as databases.
	system.stateVersion = "16.09";

}
