with builtins;
rec {
	# mapNames = (f: attrs:
	# 	listToAttrs (map (key: {name = f key; value = getAttr key attrs;}) (attrNames attrs))
	# );

	attrsToList = (attrs: map (key: {name = key; value = getAttr key attrs;}) (attrNames attrs));
	
	mapNames = (f: attrs: listToAttrs (map (a: {name = f a.name; value = a.value;}) (attrsToList attrs)));

	filterAttrs = (f: attrs: listToAttrs (filter f (attrsToList attrs)));
	
	dirnamesIn = dir: attrNames (filterAttrs (d: d.value == "directory") (readDir dir));
	# deprecated snake case alias:
    dirnames_in = dirnamesIn;

    importDir = path: let
        sourceDirs = dirnamesIn path;
        source = sourceDir: import (path + "/${sourceDir}");
        importedPairs = map (sourceDir: {name = sourceDir; value = source sourceDir;}) sourceDirs;
    in
        listToAttrs importedPairs;

	lines = input:
		filter (element: typeOf element == "string")
		(split "\n" input);
	
	unlines = concatStringsSep "\n";

	mapLines = f: input: unlines (map f (lines input));

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
