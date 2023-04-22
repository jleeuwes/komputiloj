{ config, lib, pkgs, ...}:
with lib;
let
    serviceOptions = { config, ...}: {
        options.mailOnFailure = mkOption {
            type = types.bool;
            default = false;
            description = lib.mkDoc ''
                Whether to send out a mail if this service fails.
            '';
        };

        config = {
            onFailure = mkIf config.mailOnFailure [ "failure-mailer@%n.service" ];
        };
    };
in {
    options = {
        systemd.services = mkOption {
            type = types.attrsOf (types.submodule serviceOptions);
        };
    };

    config = {
        systemd.services."failure-mailer@" = {
            serviceConfig.Type = "simple";
            scriptArgs = "%I";
            script = ''
                unit=$(printf '%s\n' "$1" | sed -E 's/\//-/g') # for some reason, - becomes /, so we need to translate back
                ${pkgs.mailutils}/bin/mail -aFrom:systeem@radstand.nl -s "[gently] probleem met $unit" jeroen@lwstn.eu <<-EOF
                    Hoi,

                    Er was een probleem met $unit:

                    $(systemctl status -- $unit)

                    Succes ermee!
                EOF
            '';
        };
    };
}
