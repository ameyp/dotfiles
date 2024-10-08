{ config, pkgs, ... }: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "amey";
  home.homeDirectory = "/home/${config.home.username}";

  home.packages = with pkgs; [
    hyprlock
    swaybg
    wofi
    xss-lock
  ];

  home.file.".config/hypr/hyprlock.conf".text = ''
    background {
      monitor =
      path = ${./wallpapers/artorias.png}
      blur_passes = 2
      blur_size = 7
      noise = 0.0117
    }

    input-field {
      monitor =
      size = 250, 50
      position = 0, -20
      halign = center
      valign = center

      outline_thickness = 3
      dots_size = 0.33
      dots_spacing = 0.15
      dots_rounding = -1
      outer_color = rgb(151515)
      inner_color = rgb(200, 200, 200)
      font_color = rgb(10, 10, 10)
      fade_on_empty = true
      fade_timeout = 3000
      placeholder_text = <i>Password</i>
      hide_input = false
      rounding = -1
      check_color = rgb(204, 136, 34)
      fail_color = rgb(204, 34, 34)
      fail_text = <i>$FAIL <b>($ATTEMPTS)</b></i>
      fail_transition = 300
    }

    label {
      monitor =
      text = Current time: $TIME
      color = rgba(200, 200, 200, 1.0)
      font_size = 30

      position = 0, 80
      halign = center
      valign = center
    }
  '';

  gtk = {
    enable = true;

    cursorTheme = {
      package = pkgs.simp1e-cursors;
      name = "Simp1e-Dark";
      size = 16;
    };

    theme = {
      package = pkgs.flat-remix-gtk;
      name = "Flat-Remix-GTK-Grey-Darkest";
    };

    iconTheme = {
      package = pkgs.adwaita-icon-theme;
      name = "Adwaita";
    };

    font = {
      name = "Sans";
      size = 11;
    };
  };

  services.dunst = {
    enable = true;
  };

  programs.waybar = {
    enable = true;
    style = ./waybar.css;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 32;
        modules-left = [ "hyprland/workspaces" "hyprland/submap" "wlr/taskbar" ];
        modules-center = [ "hyprland/window" ];
        modules-right = [ "pulseaudio" "battery" "temperature" "clock" "tray" ];
        battery = {
          bat = "BAT0";
          interval = 60;
          states = {
            warning = 30;
            critical = 15;
          };
          format = "{capacity}% {icon}";
          format-icons = ["" "" "" "" ""];
          max-length = 30;
        };
        "hyprland/workspaces" = {
          disable-scroll = true;
          all-outputs = true;
        };
        "pulseaudio" = {
          format = "{volume}% {icon}";
          format-bluetooth = "{volume}% {icon}";
          format-muted = "";
          format-icons = {
            headphone = "";
            default = ["" ""];
          };
          scroll-step = 1;
          on-click = "pavucontrol";
          ignored-sinks = [
            "GA104 High Definition Audio Controller Digital Stereo (HDMI)"
          ];
        };
        "clock" = {
          format = "{:%H:%M}";
          format-alt = "{:%A, %B %d, %Y (%R)}";
          tooltip-format = "<tt><small>{calendar}</small></tt>";
          calendar = {
            mode = "year";
            mode-mon-col = 3;
            weeks-pos = "right";
            on-scroll = 1;
            on-click-right = "mode";
            format = {
              months = "<span color='#ffead3'><b>{}</b></span>";
              days = "<span color='#ecc6d9'><b>{}</b></span>";
              weeks = "<span color='#99ffdd'><b>W{}</b></span>";
              weekdays = "<span color='#ffcc66'><b>{}</b></span>";
              today = "<span color='#ff6699'><b><u>{}</u></b></span>";
            };
          };
          actions = {
            on-click-right = "mode";
            on-scroll-up = "shift_up";
            on-scroll-down = "shift_down";
          };
        };
      };
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    settings = {
      "$mod" = "SUPER";
      monitor = [
        "DP-3,2560x1440@120,0x0,1"
        "LVDS-1,2560x1440@60,0x0,1.6"
        "Virtual-1,2560x1440@120,0x0,1"
        "eDP-1,1920x1080@144,0x0,1"
        # This monitor showed up recently, seems like an Nvidia driver issue.
        "Unknown-1,disable"
      ];
      "exec-once" = [
        "swaybg -m fill -i ./wallpapers/artorias.png"
        "waybar"
      ];
      general = {
        gaps_in = 4;
        gaps_out = 8;
      };
      decoration = {
        rounding = 2;
      };
      bind =
        [
          "$mod, C, exec, kitty"
          "$mod, E, exec, emacsclient -c"
          "$mod, Q, exec, hyprctl reload"
          "$mod, P, exec, wofi --show=run"
          "$mod_SHIFT, Q, exit"
          "$mod, F, fakefullscreen"
          "$mod_SHIFT, F, fullscreen"
          "$mod, W, killactive"
          # Focus
          "$mod, TAB, cyclenext"
          "$mod_SHIFT, TAB, cyclenext, prev"
          # Move windows
          "$mod, UP, movewindow, u"
          "$mod, DOWN, movewindow, d"
          "$mod, LEFT, movewindow, l"
          "$mod, RIGHT, movewindow, r"
          ", Print, exec, grimblast copy area"
        ]
        ++ (
          # workspaces
          # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
          builtins.concatLists (builtins.genList (
            x: let
              ws = let
                c = (x + 1) / 10;
              in
                builtins.toString (x + 1 - (c * 10));
            in [
              "$mod, ${ws}, workspace, ${toString (x + 1)}"
              "$mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
            ]
          )
            10)
        );
    };
  };
}
