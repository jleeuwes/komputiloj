# This capsule is just a hack to get to a non-cyclic dependency order.
# Things in here must be present in a scenario in which our constellation of
# machines is not yet set up, so they can't go in komputiloj-privata.
{ boltons }:
with boltons.lib;
{
    users = importDir ./users.d;
    machines = {
        gently = {
            ssh.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHmMPh91t1reE1ddLcFYyddQs0hx4v41KcaNBS2UVnEA";
        };
        scarif = {
            ssh.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFN+m0J0mjJBDho4cTqt9OlnbMUtYuj6OacT7VWi/ahC";
        };
        ferrix = {
            ssh.publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGMnMxp885h+2wrTdDDM2e9nTTnR1n/JWeezGGrXOBZY";
        };
    };
}
