# boltons: utilities and stuff missing from builtins.
with builtins;
builtins // rec {

    attrsToList = (attrs: map (key: {name = key; value = getAttr key attrs;}) (attrNames attrs));
    
    mapNames = (f: attrs: listToAttrs (map (a: {name = f a.name; value = a.value;}) (attrsToList attrs)));

    filterAttrs = (f: attrs: listToAttrs (filter f (attrsToList attrs)));
    
    # Merges a list of attrsets with //
    mergeAttrs = foldl' (a: b: a // b) {};

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
            (filter (matches ".*\.nix") entries.files);
    in listToAttrs (importedFiles ++ importedDirs);

    readDirPerType = path: let
        entries = attrsToList (readDir path);
        only' = f: map (e: e.name) (filter (e: f e.value) entries);
        only = type: only' (t: t == type);
    in {
        directories = only "directory";
        regulars = only "regular";
        symlinks = only "symlink";
        files    = only' (type: elem type [ "regular" "symlink" ]);
        others   = only' (type: !elem type [ "directory" "regular" "symlink" ]);
    };

    lines = input:
        filter (element: typeOf element == "string")
        (split "\n" input);
    
    unlines = concatStringsSep "\n";

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
    stripTabs = input: let
        indentation = head (match "^(\t*).*$" input);
        regex = "^" + indentation;
        in mapLines (replaceRegex regex "") input;
    
    traceThisString = input: trace input input;
}
