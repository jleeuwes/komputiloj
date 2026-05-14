{ ... }:
# proper module starts here
# AF_ALG is a regular source of exploits and not really necessary: https://news.ycombinator.com/item?id=47956312
# So we disable it using instructions from https://news.ycombinator.com/item?id=47957409
{ ... }:
{

    _file = __curPos.file;

    boot = {
        blacklistedKernelModules = [ "af_alg" "algif_hash" "algif_skcipher" "algif_rng" "algif_aead" ];
        extraModprobeConfig = ''
            install af_alg /bin/false
            install algif_hash /bin/false
            install algif_skcipher /bin/false
            install algif_rng /bin/false
            install algif_aead /bin/false
        '';
    };
}
