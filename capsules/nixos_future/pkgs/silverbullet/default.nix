{ lib
, stdenv
, fetchurl
, deno
, makeWrapper
, nixosTests
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "silverbullet";
  version = "0.10.4";

  srcs = [
    (fetchurl {
        url = "https://github.com/silverbulletmd/silverbullet/releases/download/${finalAttrs.version}/silverbullet.js";
        hash = "sha256-ko1zXfvn0rVY+lp9zTZ71BL41h7AOazooBVeqELP3Ps=";
    })
    # the vendor dir is prepared by running:
    # deno install --vendor --entrypoint silverbullet.js
    ./vendor
  ];
  sourceRoot = ".";

  nativeBuildInputs = [ makeWrapper ];

  unpackPhase = ''
    srcs_array=($srcs)
    cp ${"$"}{srcs_array[0]} silverbullet.js
    cp -a ${"$"}{srcs_array[1]} vendor
  '';

  buildPhase = ":";

  installPhase = ''
    runHook preInstall
    mkdir -p $out/{bin,lib}
    cp silverbullet.js $out/lib
    cp -a vendor $out/lib
    makeWrapper ${lib.getExe deno} $out/bin/silverbullet \
        --set DENO_NO_UPDATE_CHECK 1 \
        --chdir ${placeholder "out"}/lib \
        --add-flags "run --cached-only --vendor -A --unstable-kv --unstable-worker-options silverbullet.js"
    runHook postInstall
  '';

  passthru.tests = {
    inherit (nixosTests) silverbullet;
  };

  meta = {
    changelog = "https://github.com/silverbulletmd/silverbullet/blob/${finalAttrs.version}/website/CHANGELOG.md";
    description = "Open-source, self-hosted, offline-capable Personal Knowledge Management (PKM) web application";
    homepage = "https://silverbullet.md";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ aorith ];
    mainProgram = "silverbullet";
    inherit (deno.meta) platforms;
  };
})
