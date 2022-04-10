rec {
	# mapNames = (f: attrs:
	# 	builtins.listToAttrs (map (key: {name = f key; value = builtins.getAttr key attrs;}) (builtins.attrNames attrs))
	# );

	attrsToList = (attrs: map (key: {name = key; value = builtins.getAttr key attrs;}) (builtins.attrNames attrs));
	
	mapNames = (f: attrs: builtins.listToAttrs (map (a: {name = f a.name; value = a.value;}) (attrsToList attrs)));
}
