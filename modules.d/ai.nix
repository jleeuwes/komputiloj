{ nixos_25_11, ... }:
# proper module starts here
{ pkgs, config, ... }:
{
    services.ollama = {
        enable = true;
        # acceleration = "rocm";
        package = nixos_25_11.native.${pkgs.system}.legacyPackages.ollama;
    };

    # warnings = if config.hardware.amdgpu.opencl.enable
    # hardware.amdgpu.opencl.enable = true;
    #
}
