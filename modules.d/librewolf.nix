{ ... }:
# proper module starts here
{ pkgs, config, lib, ... }:

let
  package = pkgs.librewolf;

  # The firefox version is in src.json in the librewolf package,
  # but we can't get to it here.
  # TODO: make this less hacky.
  firefoxVersion = {
    "128.0-2" = "128.0";
    "128.0.3-2" = "128.0.3";
    "130.0-1" = "130.0";
    "131.0.2-1" = "131.0.2";
    "134.0-1" = "134.0";
    "134.0.1-1" = "134.0.1";
    "136.0-1" = "136.0";
    "136.0-2" = "136.0";
    "136.0.1-1" = "136.0.1";
    "137.0.2-1" = "137.0.2";
    "138.0.1-2" = "138.0.1";
  }."${pkgs.librewolf.version}"
  or (throw "Unknown librewolf version ${pkgs.librewolf.version}");

  cfg = config.programs.librewolf;

  policyFormat = pkgs.formats.json { };

  organisationInfo = ''
    When this option is in use, Librewolf will inform you that "your browser
    is managed by your organisation". That message appears because NixOS
    installs what you have declared here such that it cannot be overridden
    through the user interface. It does not mean that someone else has been
    given control of your browser, unless of course they also control your
    NixOS configuration.
  '';

  # /etc/librewolf/policies/policies.json,
  # if it exists, completely overrides Librewolf's policies.
  # So we merge Librewolf's policies into our own policies,
  # from which we generate policies.json.
  policiesFileFromDistribution = "${package}/lib/librewolf/distribution/policies.json";
  policiesFromDistribution = {
    _file = policiesFileFromDistribution;
    programs.librewolf.policies =
      # TODO fix https://nix.dev/manual/nix/2.23/language/import-from-derivation ?
      (builtins.fromJSON (builtins.readFile policiesFileFromDistribution))
      .policies;
  };

in
{

  imports = [
    policiesFromDistribution
  ];

  _file = __curPos.file;

  options.programs.librewolf = {
    enable = lib.mkEnableOption "the Librewolf web browser";

    wrapperConfig = lib.mkOption {
      type = lib.types.attrs;
      default = {};
      description = "Arguments to pass to Librewolf wrapper";
    };

    policies = lib.mkOption {
      type = policyFormat.type;
      default = { };
      description = ''
        Group policies to install.

        See [Mozilla's documentation](https://mozilla.github.io/policy-templates/)
        for a list of available options.

        This can be used to install extensions declaratively! Check out the
        documentation of the `ExtensionSettings` policy for details.

        ${organisationInfo}
      '';
    };

    languagePacks = lib.mkOption {
      # Available languages can be found in https://releases.mozilla.org/pub/firefox/releases/${firefoxVersion}/linux-x86_64/xpi/
      type = lib.types.listOf (lib.types.enum ([
        "ach"
        "af"
        "an"
        "ar"
        "ast"
        "az"
        "be"
        "bg"
        "bn"
        "br"
        "bs"
        "ca-valencia"
        "ca"
        "cak"
        "cs"
        "cy"
        "da"
        "de"
        "dsb"
        "el"
        "en-CA"
        "en-GB"
        "en-US"
        "eo"
        "es-AR"
        "es-CL"
        "es-ES"
        "es-MX"
        "et"
        "eu"
        "fa"
        "ff"
        "fi"
        "fr"
        "fy-NL"
        "ga-IE"
        "gd"
        "gl"
        "gn"
        "gu-IN"
        "he"
        "hi-IN"
        "hr"
        "hsb"
        "hu"
        "hy-AM"
        "ia"
        "id"
        "is"
        "it"
        "ja"
        "ka"
        "kab"
        "kk"
        "km"
        "kn"
        "ko"
        "lij"
        "lt"
        "lv"
        "mk"
        "mr"
        "ms"
        "my"
        "nb-NO"
        "ne-NP"
        "nl"
        "nn-NO"
        "oc"
        "pa-IN"
        "pl"
        "pt-BR"
        "pt-PT"
        "rm"
        "ro"
        "ru"
        "sco"
        "si"
        "sk"
        "sl"
        "son"
        "sq"
        "sr"
        "sv-SE"
        "szl"
        "ta"
        "te"
        "th"
        "tl"
        "tr"
        "trs"
        "uk"
        "ur"
        "uz"
        "vi"
        "xh"
        "zh-CN"
        "zh-TW"
      ]));
      default = [ ];
      description = ''
        The language packs to install.
      '';
    };

  };

  config = let
  in lib.mkIf cfg.enable {

    environment.systemPackages = [
      (package.override (old: {
        cfg = (old.cfg or {}) // cfg.wrapperConfig;
      }))
    ];

    environment.etc =
      let
        policiesJSON = policyFormat.generate "librewolf-policies.json" { inherit (cfg) policies; };
      in
      lib.mkIf (cfg.policies != { }) {
        "librewolf/policies/policies.json".source = "${policiesJSON}";
      };

    programs.librewolf.policies = {
      DisableAppUpdate = true;
      ExtensionSettings = builtins.listToAttrs (builtins.map
        (lang: lib.attrsets.nameValuePair
          "langpack-${lang}@firefox.mozilla.org"
          {
            installation_mode = "normal_installed";
            install_url = "https://releases.mozilla.org/pub/firefox/releases/${firefoxVersion}/linux-x86_64/xpi/${lang}.xpi";
          }
        )
        cfg.languagePacks);
    };
  };

  meta.maintainers = [ {
     name = "Jeroen Leeuwestein";

     # Optional, but at least one of email, matrix or githubId must be given
     email = "jeroen@lwstn.eu";
     github = "jleeuwes";
     githubId = 7975192;
  } ];
}
