# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  nixpkgs.config = {
    # allowUnfree = true;
    packageOverrides = pkgs: {
      # You need to first add the nixos-unstable channel as 'unstable' using nix-channel
      unstable = import <unstable> {
        config = config.nixpkgs.config;
      };
    };
  };

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

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

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    git vim file subversionClient pciutils pmount parted
    # xfce stuff:
    xfce.terminal xfce.thunar xfce.ristretto
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

  # Screen locker
  services.physlock = {
    enable = true;
    # nog niet beschikbaar (eens updaten!)
    # allowAnyUser = true; # of moeten we rechten via systemd regelen?
  };

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Xmonad :)
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  # en xfce voor de goodies
  # services.xserver.desktopManager.xfce.enable = true;
  # (TODO die vereist upower, dus staat nu uit;
  # als ik dingen mis moet ik de losse programma's maar installeren)
  
  # touchpad
  services.xserver.synaptics = {
    enable = true;
    horizEdgeScroll = false;
    horizTwoFingerScroll = true;
    vertEdgeScroll = false;
    vertTwoFingerScroll = true;
    tapButtons = false;
    maxSpeed = "2";
    accelFactor = "0.04";
  };

  services.udev.extraRules = ''
    # https://wiki.archlinux.org/index.php/Touchpad_Synaptics#Disable_touchpad_on_mouse_detection
    # Turn off touchpad when mouse is connected (hardcoded for user jeroen)
    # ACTION=="add", ATTRS{bInterfaceProtocol}=="02", ATTRS{bInterfaceClass}=="03", ATTRS{bInterfaceSubClass}=="01", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/jeroen/.Xauthority", RUN+="${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=1"
    ACTION=="add", SUBSYSTEM=="input", KERNEL=="mouse[0-9]*", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/jeroen/.Xauthority", RUN+="${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=1"
    ACTION=="remove", SUBSYSTEM=="input", KERNEL=="mouse[0-9]*", ENV{DISPLAY}=":0", ENV{XAUTHORITY}="/home/jeroen/.Xauthority", RUN+="${pkgs.xorg.xf86inputsynaptics}/bin/synclient TouchpadOff=0"
  '';
  # Enable adb group and udev rules and such:
  programs.adb.enable = true;
  # Use newer rules for OnePlus 5T support:
  services.udev.packages = [ pkgs.unstable.android-udev-rules ];

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
    extraGroups = [ "wheel" "network-manager" "dialout" "adbusers" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
