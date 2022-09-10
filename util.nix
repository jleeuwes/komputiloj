with builtins;
rec {
	# mapNames = (f: attrs:
	# 	listToAttrs (map (key: {name = f key; value = getAttr key attrs;}) (attrNames attrs))
	# );

	attrsToList = (attrs: map (key: {name = key; value = getAttr key attrs;}) (attrNames attrs));
	
	mapNames = (f: attrs: listToAttrs (map (a: {name = f a.name; value = a.value;}) (attrsToList attrs)));

	filterAttrs = (f: attrs: listToAttrs (filter f (attrsToList attrs)));
}
