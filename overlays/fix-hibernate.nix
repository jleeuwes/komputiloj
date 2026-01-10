# Applies the workaround from https://bugs.kde.org/show_bug.cgi?id=510992
# Might interfere with regular sleep but we don't use that.
self: super:
{
    kdePackages = super.kdePackages.overrideScope (kdeSelf: kdeSuper: {
        powerdevil = kdeSuper.powerdevil.overrideAttrs ({ postInstall ? "", ...}: {
            postInstall = postInstall + ''
                chmod a-x $out/libexec/wakeupsourcehelper
            '';
        });
    });
}
