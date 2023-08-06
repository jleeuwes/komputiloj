{ komputiloj, ...}:
{ config, options, lib, pkgs, ...}:
with builtins;
{
    config = {
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

            windowManager.xmonad = {
                enable = true;
                enableContribAndExtras = true;
                config = readFile ./xmonad.hs;
            };

            desktopManager = {
                xterm.enable = false;
                xfce.enable = true;
            };

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

        environment.systemPackages = with pkgs; [
            # prettiness ( more inspiration at https://gist.github.com/taohansen/d15e1fe4674a286cb9bcd8e3378a9f23 and https://stackoverflow.com/questions/38576616/how-to-install-gtk-themes-under-nixos-without-hacky-scripts )
            # gtk-engine-murrine arc-theme arc-icon-theme elementary-icon-theme
            # gtk
            hicolor-icon-theme xfce.xfce4-icon-theme tango-icon-theme
            
            # gnome stuff (won't work because dconf is missing):
            # gnome3.gedit
            
            # xfce stuff:
            # support stuff (needed for generating thumbnails for instance - maar het werkt voor geen zak)
            xfce.exo xfce.xfconf # xfce.xfce4settings
            shared-mime-info xfce.tumbler # <- these two in particular seemed to do the trick!

            # hsetroot # program to help my xmonad config set the background in an xfce-terminal compatible way
            
            # xorg.xbacklight # doesn't work anymore - https://github.com/NixOS/nixpkgs/issues/55520#issuecomment-470501591
            # brightnessctl
            unclutter-xfixes # hides the mouse if unused
            
            komputiloj.packages.dekstop # must be in PATH for our xmonad config!
        
            xsel

            # actual programs:
            # xfce.xfce4-terminal
            xfce.thunar xfce.ristretto
        ];
    };
}
