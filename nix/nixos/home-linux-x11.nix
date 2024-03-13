{ config, pkgs, ... }: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "amey";
  home.homeDirectory = "/home/${config.home.username}";

  home.packages = [
    pkgs.betterlockscreen
    pkgs.xss-lock
  ];

  services.xidlehook = {
    enable = true;
    not-when-audio = true;
    timers = [
      {
        # Lock after 10 minutes
        delay = 600;
        command = "${pkgs.betterlockscreen}/bin/betterlockscreen -l blur";
      }
      {
        # Suspend after 20 minutes
        delay = 1200;
        command = "systemctl suspend";
      }
    ];
  };
}
