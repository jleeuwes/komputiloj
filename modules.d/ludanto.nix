{ ... }:
# proper module starts here
{ pkgs, config, lib, ... }:
let
    user = "ludanto"; # NO SPECIAL CHARACTERS!
in {

    _file = __curPos.file;
    
    users.extraUsers."${user}" = {
        # A dedicated user account to play untrusted game binaries.
        uid = 1001; # clashes with gorinchemindialoog.
                    # Fortunately, it's unlikely that we will have games on our
                    # servers or gorinchemindialoog on a laptop.
        isNormalUser = true;
        description = "Ludanto de Videoludoj";
        extraGroups = [ "video" "audio" ];
    };

    systemd.services."getty@tty6" = {
        serviceConfig = {
            ExecStart = [
                # https://wiki.archlinux.org/title/Getty#Automatic_login_to_virtual_console
                ""
                "${pkgs.util-linux}/bin/agetty -o '-p -f -- \\\\u' --noclear --autologin ${user} tty6 $TERM"
            ];
        };

        # https://wiki.archlinux.org/title/Getty#Automatic_login_to_virtual_console
        overrideStrategy = "asDropin";

        restartIfChanged = false;
    };

    system.activationScripts = {
        # https://wiki.archlinux.org/title/Xinit#Autostart_X_at_login
        "fork-shirt-up-for-${user}" = {
            deps = [ "users" ];
            text = ''
                cd ~${user}
                
                # autostart wayland:
                cat <<'EOF' > .bash_profile
                if [[ -z $DISPLAY && $XDG_VTNR = 6 ]]; then
                    exec ${pkgs.kdePackages.plasma-workspace}/bin/startplasma-wayland
                fi
                EOF

                # disable wifi controls:
                ${pkgs.gnused}/bin/sed -Ei 's/^(extraItems=.*)kde.plasma.networkmanagement,?/\1/' \
                    .config/plasma-org.kde.plasma.desktop-appletsrc
                
                # disable locking:
                cat <<'EOF' > .config/kscreenlockerrc
                [Daemon]
                Autolock=false
                LockOnResume=false
                Timeout=0
                EOF

                # kwalletrc?
            '';
        };
    };

    # systemd.services."games@tty8" = {
    #     serviceConfig = {
    #         ExecStart = "${pkgs.kdePackages.plasma-workspace}/bin/startplasma-wayland";
    #         Type = "idle";
    #         User = user;
    #         Group = "users";
    #         Restart = "always";
    #         RestartSec = 5;
    #         # UtmpIdentifier=%I
    #         StandardInput = "tty";
    #         StandardOutput = "tty";
    #         TTYPath = "/dev/tty8";
    #         TTYReset = "yes";
    #         TTYVHangup = "yes";
    #         TTYVTDisallocate = "yes";
    #         # IgnoreSIGPIPE=no
    #         # SendSIGHUP=yes
    #         # ImportCredential=agetty.*
    #         # ImportCredential=login.*
    #         Environment = "TTY=tty8";
    #     };

    #     wantedBy = [ "graphical.target" ];
    #     after = [ "systemd-user-sessions.service" ];
    #     conflicts = [ "getty@tty8.service" ];

    #     restartIfChanged = false;
    # };
}
