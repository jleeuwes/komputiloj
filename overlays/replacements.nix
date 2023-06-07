{ boltons }:
with boltons;
replacements: self: super:
let
	traceMe = msg: trace ((__curPos.file) + ": " + msg);
	traceOverride = old: new: traceMe ("Replacing " + old.name + " with " + new.name);
in mapAttrs (name: value: traceOverride (getAttr name super) value value)
replacements
