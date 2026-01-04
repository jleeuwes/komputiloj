{ boltons, command-platform, komputiloj, nixos, all, ... }:
with boltons;
let
    escape = nixos.lib.strings.escapeShellArg;
    updateVersioDomain = command-platform.local.packageBuilders.writeCommand {
        name = "update-versio-domain";
        # no runtimeInputs because PATH is (currently) inherited from calling script
        text = readFile ./update-versio-domain.sh;
    };
    updateTransipDomain = command-platform.local.packageBuilders.writeCommand {
        name = "update-transip-domain";
        # no runtimeInputs because PATH is (currently) inherited from calling script
        text = readFile ./update-transip-domain.sh;
    };
    providerScripts = {
        versio = updateVersioDomain;
        transip = updateTransipDomain;
    };
    recordsFile = domain: nixos.portable.packageBuilders.writeTextFile {
        name = "records";
        text = domain.records;
    };
in command-platform.local.packageBuilders.writeCommand {
    name = "update-domains";
    runtimeInputs = [
        nixos.local.legacyPackages.curl
        nixos.local.legacyPackages.gnugrep
        nixos.local.legacyPackages.coreutils
        nixos.local.legacyPackages.jq
        komputiloj.packages.wachtwoord
        komputiloj.packages.tipctl
    ];
    text = ''
        TRANSIP_USER=jeroenleeuwestein
        TRANSIP_PRIVKEY=$(wachtwoord cat -n \
            "$KOMPUTILOJ_PATH"/secrets/privkey@api.transip.nl)
        VERSIO_USER=jeroen@lwstn.eu:$(wachtwoord cat -n \
            "$KOMPUTILOJ_PATH"/secrets/jeroen@lwstn.eu@versio.nl)
        export TRANSIP_USER TRANSIP_PRIVKEY VERSIO_USER

        ${unlines (map
            (domain:
                "printf 'Updating domain %s at %s\\n' ${escape domain.name} ${escape domain.provider} >&2"
                + "\n" +
                "${providerScripts.${domain.provider}} ${escape domain.name} ${recordsFile domain}"
            )
            (attrValues all.domains))}
    '';
}
