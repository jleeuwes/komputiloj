# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config = {
    packageOverrides = pkgs: {
      # You need to first add the nixos-unstable channel as 'unstable' using nix-channel
      unstable = import <unstable> {
        config = config.nixpkgs.config;
      };
      # You need to first add a nixos-18.09 channel called nixos_18_09 using nix-channel
      nixos_18_09 = import <nixos_18_09> {
        config = config.nixpkgs.config;
      };
    };
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  hardware.pulseaudio.enable = true;
  hardware.acpilight.enable = true; # doesn't work
  hardware.brightnessctl.enable = true;

  # also doesn't work:
  environment.etc."X11/xorg.conf.d/05-backlight.conf".text = ''
    Section "Device"
      Identifier "Intel Graphics"
      Driver "intel"
      Option "Backlight" "intel_backlight"
    EndSection
  '';

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Make /tmp in-memory:
  boot.tmpOnTmpfs = true;

  networking.hostName = "scarif";
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # Netwerken staan in /etc/wpa_supplicant.conf vanwege passphrases
  networking.nameservers = [ "1.1.1.1" "8.8.8.8" ];

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
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
	];
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = let
    # this incomprehensible magic incantation was conjured by TheMsDosNerd at https://www.reddit.com/r/NixOS/comments/8cq4ic/problem_installing_python_package/
    myPythonPackages = pythonPackages: with pythonPackages; [
        flask
        flask-api
    ]; in with pkgs; [
    # prettiness ( more inspiration at https://gist.github.com/taohansen/d15e1fe4674a286cb9bcd8e3378a9f23 and https://stackoverflow.com/questions/38576616/how-to-install-gtk-themes-under-nixos-without-hacky-scripts )
    # gtk-engine-murrine arc-theme arc-icon-theme elementary-icon-theme
    # gtk
    hicolor_icon_theme xfce.xfce4icontheme tango-icon-theme
    # usefull programs:
    gitFull vim file subversionClient pciutils pmount parted squashfsTools wget
    unstable.gitAndTools.git-annex
    sshpass
    gnupg paperkey qrencode zbar
    telnet # for ftp for the nas
    
    # programming:
    (python3.withPackages myPythonPackages)

    # LaTeX:
    # (Not sure if rubber uses the chosen texlive distribution)
    texlive.combined.scheme-medium
    rubber

    firefox thunderbird
    geany
    zathura # pdf viewer
    # gnome stuff (won't work because dconf is missing):
    # gnome3.gedit
    # xfce stuff:
    # support stuff (needed for generating thumbnails for instance - maar het werkt voor geen zak)
    xfce.exo xfce.xfconf # xfce.xfce4settings
    shared_mime_info xfce.tumbler # <- these two in particular seemed to do the trick!
    
    hsetroot # program to help my xmonad config set the background in an xfce-terminal compatible way
    # xorg.xbacklight # doesn't work anymore - https://github.com/NixOS/nixpkgs/issues/55520#issuecomment-470501591
    unclutter-xfixes # hides the mouse if unused
    dmenu xsel # some helpers for menus
    
    # actual programs:
    xfce.terminal xfce.thunar xfce.ristretto
    # Belgian eID (it looks in /run/current-system/sw/ by default for some things so it's easier to have it installed system-wide):
    eid-mw
  ];

  # Generate setuid wrappers for pmount:
  security.wrappers = {
    pmount.source  = "${pkgs.pmount}/bin/pmount";
    pumount.source = "${pkgs.pmount}/bin/pumount";
  };

  # List services that you want to enable:
  
  # I suspect UPower hard-shutdowns my laptop if one battery is empty,
  # so let's try with it disabled:
  services.upower.enable = false;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  
  # Enable sound.
  sound.enable = true;

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
    xkbOptions = "compose:ralt eurosign:e";

    displayManager.sessionCommands = ''
      # # ( adapted from https://gist.github.com/taohansen/d15e1fe4674a286cb9bcd8e3378a9f23 )
      # # (lijkt allemaal voor geen flikker te werken)
      # # This allows GTK to load SVG icons.
      # export GDK_PIXBUF_MODULE_FILE=$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)
      # # Set GTK_PATH so that GTK+ can find the Xfce theme engine.
      # export GTK_PATH=${pkgs.gtk-engine-murrine}/lib/gtk-2.0
      # # Set GTK_DATA_PREFIX so that GTK+ can find the Xfce themes.
      # export GTK_DATA_PREFIX=${config.system.path}
      # # Launch xfce settings daemon.
      # ${pkgs.xfce.xfce4settings}/bin/xfsettingsd &
    '';

    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
  
    # touchpad
    synaptics = {
      enable = true;
      horizEdgeScroll = false;
      horizTwoFingerScroll = true;
      vertEdgeScroll = false;
      vertTwoFingerScroll = true;
      tapButtons = false;
      maxSpeed = "2";
      accelFactor = "0.04";
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
    extraGroups = [ "wheel" "network-manager" "dialout" "adbusers" "video" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
