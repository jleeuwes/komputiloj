# This capsule is just a hack to get to a non-cyclic dependency order.
# It's not really a source but we don't have a proper place for capsules yet.
# Thinks in here must be present in a scenario in which our constellation of
# machines is not yet set up, so they can't go in komputiloj-privata.
{ boltons }:
with boltons.lib;
{
    users = importDir ./users.d;
}
