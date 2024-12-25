{ ... }:
# proper module starts here
# Automatically initializes Apple SuperDrive
# See https://www.cmos.blog/use-apples-usb-superdrive-with-linux/
{ pkgs, config, lib, ... }:
{

    _file = __curPos.file;
    
    services.udev.extraRules = ''
        ACTION=="add", ATTRS{idProduct}=="1500", ATTRS{idVendor}=="05ac", DRIVERS=="usb", RUN+="${pkgs.sg3_utils}/bin/sg_raw /dev/$kernel EA 00 00 00 00 00 01"
    '';
}
