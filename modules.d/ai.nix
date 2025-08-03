{ nixpkgsFuture, ... }:
# proper module starts here
{ pkgs, config, ... }:
{
    services.ollama = {
        enable = true;
        acceleration = "rocm";
    };

    # warnings = if config.hardware.amdgpu.opencl.enable
    # hardware.amdgpu.opencl.enable = true;
    #
}
