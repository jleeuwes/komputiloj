+# untested stuff to turn our package function into a callPackage compatible
+# function:
+# let makeCallPackageCompatible = f:
+#     let
+#         lazyargs = f {
+#             buildTools = {};
+#             # others
+#         };
+#         argnames = builtins.attrNames lazyargs;
+#         argsDesc = map (arg: {
+#             name = arg;
+#             value = false; # whether the arg has a default
+#         }) argnames;
+#         # setFunctionArgs taken from nixpkgs' lib/trivial.nix
+#         # if we really want to do this we should probably reuse that
+#         setFunctionArgs = f: args: {
+#           __functor = self: f;
+#           __functionArgs = args;
+#         };
+#         fun = args: f {
+#             buildTools = args;
+#             # others
+#         };
+#         in setFunctionArgs fun argsDesc;
+# in

