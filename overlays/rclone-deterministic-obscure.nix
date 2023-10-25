{ boltons, ... }:
with boltons;
self: super: {
    rclone = trace "🩹 ${__curPos.file}: Patching rclone" (super.rclone.overrideAttrs (oldAttrs: {
        version = oldAttrs.version + "-with-deterministic-obscure";
        patches = [ ./rclone-deterministic-obscure.patch ];
    }));
}
