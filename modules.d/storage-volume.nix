{ boltons, ... }:
{ config, options, lib, pkgs, ...}:
with lib;
let
    serviceOptions = { config, ...}: {
        options.needsStorageVolume = mkOption {
            type = types.nullOr (types.enum [ "requires" "requisite" ]);
            default = null;
            description = lib.mkDoc ''
                Whether this service needs our encrypted storage volume.
            '';
        };

        config = {
            requires = mkIf (config.needsStorageVolume == "requires") [ "mount-storage.service" ];
            requisite = mkIf (config.needsStorageVolume == "requisite") [ "mount-storage.service" ];
            after = mkIf (config.needsStorageVolume != null) [ "mount-storage.service" ];
        };
    };
in {
    imports = [
        # TODO depends on systemd-failure-mailer
    ];

    options = {
        systemd.services = mkOption {
            type = types.attrsOf (types.submodule serviceOptions);
        };
    };

    config = {
        systemd.services.mount-storage = {
            serviceConfig = {
                Type = "oneshot";
                RemainAfterExit = true;
            };
            mailOnFailure = false; # not useful to set this to true; postfix depends on this service
            wants = [ "luks-storage-key.service" ];
            after    = [ "luks-storage-key.service" ];
            wantedBy = [ "multi-user.target" ];
            # TODO move requiredBy and before to the respective services
            requiredBy = [
                "btrbk-storage.service" # TODO make requisite
            ];
            before = [
                "btrbk-storage.service"
            ];
            path = [ pkgs.cryptsetup pkgs.utillinux pkgs.unixtools.mount pkgs.unixtools.umount ];
            script = ''
                if mountpoint -q /mnt/storage; then
                    echo "Storage already mounted. Done here."
                    exit 0
                fi
                if [ -b /dev/mapper/storage ]; then
                    echo "LUKS mapping already opened."
                else
                    echo "Opening LUKS mapping..."
                    cryptsetup open UUID=6c8d5be7-ae46-4e51-a270-fd5bdce46f3b storage --type luks --key-file /run/keys/luks-storage
                fi
                echo "Mounting..."
                mkdir -p /mnt/storage
                mount /dev/mapper/storage /mnt/storage
                echo "Done here."
            '';
            preStop = ''
                if mountpoint -q /mnt/storage; then
                    umount /mnt/storage
                fi
                cryptsetup close storage
            '';
        };
    };
}
