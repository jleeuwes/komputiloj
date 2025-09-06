{ nextcloud31 }:
nextcloud31.overrideAttrs (prev: {
    patches = (prev.patches or []) ++ [ ./778ae6a4875cc0789f76dbf67f934ff3d3987f89.patch ];
})
