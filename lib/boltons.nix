# boltons: utilities and stuff missing from builtins.
with builtins;
builtins // rec {
    
    dedup_strings = list_of_strings: attrNames (listToAttrs (map (s: { name = s;
                    value = "whatever"; }) list_of_strings));
    
    # Given an attrset of attrsets, make sure each attrsets has a name attribute.
    # If an attrset already has a name attribute, it is kept as-is.
    # Otherwise, name is set to the name to which the attrset is assigned in the
    # outer attrset.
    named = mapAttrs (name: value: { name = name; } // value);

    attrsToList = (attrs: map (key: {name = key; value = getAttr key attrs;}) (attrNames attrs));
    
    mapNames = (f: attrs: listToAttrs (map (a: {name = f a.name; value = a.value;}) (attrsToList attrs)));
    mapValues = f: mapAttrs (name: value: f value);

    filterAttrs = (f: attrs: listToAttrs (filter f (attrsToList attrs)));
    
    # Merges a list of attrsets with //
    mergeAttrsets = foldl' (a: b: mergeAttrs a b) {};
    mergeAttrs = a: b: let
        duplicates = attrNames (intersectAttrs a b);
        loc = nm: set: let
            l = unsafeGetAttrPos nm set;
        in if l == null then "<unknown>" else "${l.file}:${toString l.line}";
        mkError = nm: "mergeAttrs: attribute \"${nm}\" is defined in both ${loc nm a} and ${loc nm b}";
    in if duplicates == []
    then (a // b)
    else throw (mkError (head duplicates));

    # Make an attrset containing an attribute for each .nix file and each
    # directory in the given path.
    # The name of each attribute is the file name (without .nix) or directory
    # name. The value is the imported file or directory.
    importDir = path: let
        entries = readDirPerType path;
        importDir = nm: {
            name = nm;
            value = import (path + "/${nm}");
        };
        importFile = nm: {
            name = replaceRegex "\.nix$" "" nm;
            value = import (path + "/${nm}");
        };
        importedDirs = map importDir entries.directories;
        importedFiles = map importFile
            (filter (matches ".*\.nix") entries.regulars);
    in listToAttrs (importedFiles ++ importedDirs);

    importDirAndApply = path: arg: mapValues (f: f arg) (importDir path);

    readDirPerType = path: let
        entries = attrsToList (readDir path);
        only' = f: map (e: e.name) (filter (e: f e.value) entries);
        only = type: only' (t: t == type);
    in {
        directories = only "directory";
        regulars = only "regular";
        symlinks = only "symlink";
        others   = only' (type: !elem type [ "directory" "regular" "symlink" ]);

        # Putting regular files and symlinks on one pile was misguided:
        # 1. we assume symlinks to be files, but they can be directories.
        # 2. symlink handling is confusing in nix, see https://github.com/NixOS/nix/issues/2109
        files    = warn "Don't use readDirPerType's files result. Use regulars and/or symlinks directly"
            (only' (type: elem type [ "regular" "symlink" ]));
    };

    lines = input:
        filter (element: typeOf element == "string")
        (split "\n" input);
    
    unlines = lns: let
        parts = map (ln: "${ln}\n") lns;
        in concatStringsSep "" parts;
    unwords = concatStringsSep " ";

    mapLines = f: input: unlines (map f (lines input));

    matches = regex: str: match regex str != null;

    replaceRegex = regex: substitution: input: let
        splits = split regex input;
        mapped = map (s: if typeOf s == "string" then s else substitution) splits;
        in concatStringsSep "" mapped;

    charsToString = concatStringsSep "";

    # Untested, might be useful later
    stringToChars = input: let
        splits = split "(.)" input;
        groups = filter (elem: typeOf elem == "list") splits;
        in concatLists groups;
    
    # Untested, might be useful later
    mapChars = f: input: let
        chars = stringToChars input;
        mappedChars = map f chars;
        in charsToString mappedChars;

    # Determines the tab-based indentation of the first line of input,
    # then strips that amount of tabs at te start of each line.
    # Also strips any tabs after the last line to support a less indented
    # multiline string ending:
    #
    # {
    #   str = ''
    #       bla
    #       bla
    #   ''; # the tabs before '' are removed
    # }
    stripTabs = input: let
        indentation = head (match "^(\t*).*$" input);
        regex = "^" + indentation;
        inputWithoutLastHangingLine = replaceRegex "\n\t+$" "" input;
        in mapLines (replaceRegex regex "") inputWithoutLastHangingLine;
    
    traceThisString = input: trace input input;
}
