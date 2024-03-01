{ config, pkgs, lib, sops-nix, ... }: let
  peerVpnToJail = "veth1";
  peerJail = "veth2";
  peerHost = "veth-mullvad-br";
  peerVpnToHost = "veth-mullvad";
  bridge = "v-net-0";
  netNsVpn = "mullvad";
  netNsJail = "downloaders";
  mullvadWgInterface = "wg-mullvad";
  hostAddress = "192.168.100.10";
  mullvadAddress = "192.168.100.11";
  downloadersAddress = "192.168.100.12";
  peerVpnIp = "10.10.0.2";
  peerJailIp = "10.10.0.3";
in {
  # TODO this is just for sops, remove for deploying.
  services.openssh.enable = true;

  # All secrets
  sops.secrets = {
    # I couldn't figure out how to get json format working, it complained about the key being ''
    # for device and settings.
    # Even the binary format has its own issues, each file must be edited with
    # sops --input-type binary --output-type binary secrets/<whatever>
    # because the creator is a stubborn ass: https://github.com/getsops/sops/issues/813
    "mullvad-account.json" = {
      sopsFile = ./secrets/mullvad-account.txt;
      path = "/etc/mullvad-vpn/account-history.json";
      format = "binary";
    };
    "mullvad-device.json" = {
      sopsFile = ./secrets/mullvad-device.json;
      path = "/etc/mullvad-vpn/device.json";
      format = "binary";
    };
    # settings.json is not sent as a secret because mullvad daemon doesn't seem to respect it
    # and then has connection issues.
  };

  # This sets up the link between the mullvad container and the downloaders container.
  systemd.services."downloaders-vpn-link" = {
    description = "Create network namespaces";
    before = [ "network.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = with pkgs; writers.writeBash "downloaders-vpn-link-up" ''
        set -euo pipefail

        # Create a link between them. We will later set up NAT routing on the VPN side
        # of this link with forwarding to the VPN interface, and the jail side of the
        # link will only be able to ingress/egress traffic over this peer link.
        #
        # Both interfaces in this link are moved to their respective containers,
        # and deleted when the container is deleted.
        ${iproute}/bin/ip link add ${peerVpnToJail} type veth peer name ${peerJail}
      '';
      ExecStop = with pkgs; writers.writeBash "netns-service-down" ''
        # Don't 'set -euo pipefail' here because some of the links might have been deleted already.

        ${iproute}/bin/ip link del ${peerVpnToJail}
        ${iproute}/bin/ip link del ${peerJail}
      '';
    };
  };

  networking.nat = {
    enable = true;
    # Instead of enabling NAT for all internal interfaces, only enable it for the mullvad container.
    # This way, the other containers don't automatically get internet access.
    internalInterfaces = ["ve-mullvad"];
    externalInterface = "enp7s0";
    enableIPv6 = false;
  };

  containers.mullvad = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = hostAddress;
    localAddress = mullvadAddress;
    interfaces = [ peerVpnToJail ];

    bindMounts = {
      # Not mounted as read-only because the daemon wants to modify them.
      "/etc/mullvad-vpn/account-history.json".isReadOnly = false;
      "/etc/mullvad-vpn/device.json".isReadOnly = false;
    };

    config = { config, pkgs, ... }: {
      boot.kernel.sysctl = {
        "net.ipv4.ip_forward" = true;
      };

      services.mullvad-vpn = {
        enable = true;
      };

      systemd.services.mullvad-daemon = {
        serviceConfig = {
          # Necessary because of https://github.com/mullvad/mullvadvpn-app/issues/3651
          Environment = "TALPID_NET_CLS_MOUNT_DIR=/var/cache/mullvad-vpn/cgroup_net_cls";
        };
      };

      systemd.services.mullvad-connect = {
        after = [ "mullvad-daemon.service" ];
        requires = [ "mullvad-daemon.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = with pkgs; writers.writeBash "mullvad-connect" ''
            set -euo pipefail

            # Initialize the peer connection to the downloaders container.
            ${iproute}/bin/ip addr add ${peerVpnIp}/24 dev ${peerVpnToJail}
            ${iproute}/bin/ip link set dev ${peerVpnToJail} up

            # Set up forwarding
            ${iptables}/bin/iptables -A FORWARD -i ${mullvadWgInterface} \
              -o ${peerVpnToJail} -m state --state ESTABLISHED,RELATED -j ACCEPT
            ${iptables}/bin/iptables -A FORWARD -i ${peerVpnToJail} -o ${mullvadWgInterface} -j ACCEPT
            ${iptables}/bin/iptables -t nat -A POSTROUTING -o ${mullvadWgInterface} -j MASQUERADE

            # Configure mullvad settings
            ${mullvad-vpn}/bin/mullvad relay set tunnel-protocol wireguard
            ${mullvad-vpn}/bin/mullvad relay set location ch
            ${mullvad-vpn}/bin/mullvad lockdown-mode set on
            ${mullvad-vpn}/bin/mullvad auto-connect set on

            # Connect to the endpoint. Don't wait with -w because it looks like
            # NixOS waits to set the peer veth on the host up till after all
            # services in the container are ready.
            ${mullvad-vpn}/bin/mullvad connect
          '';
          ExecStop = with pkgs; writers.writeBash "mullvad-disconnect" ''
            set -euo pipefail

            # Stop mullvad
            ${mullvad-vpn}/bin/mullvad disconnect

            # Remove iptables rules
            ${iptables}/bin/iptables -D FORWARD -i ${mullvadWgInterface} \
              -o ${peerVpnToJail} -m state --state ESTABLISHED,RELATED -j ACCEPT
            ${iptables}/bin/iptables -D FORWARD -i ${peerVpnToJail} -o ${mullvadWgInterface} -j ACCEPT
            ${iptables}/bin/iptables -t nat -D POSTROUTING -o ${mullvadWgInterface} -j MASQUERADE
          '';
        };
      };

      system.stateVersion = "23.11";

      networking = {
        firewall = {
          enable = true;
        };

        # Bug https://github.com/NixOS/nixpkgs/issues/162686 is not applicable here
        # because I actually want the container to be able to communicate with my
        # lan gateway, at least of DNS. This might change later when I move the
        # DNS resolver to nixos.
        useHostResolvConf = lib.mkForce true;
      };
    };
  };

  containers.downloaders = {
    autoStart = true;
    privateNetwork = true;
    hostAddress = hostAddress;
    localAddress = downloadersAddress;
    interfaces = [ peerJail ];

    config = { config, pkgs, ... }: {
      system.stateVersion = "23.11";

      systemd.services.peer-link-setup = {
        requires = [ "network-online.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          RemainAfterExit = true;
          ExecStart = with pkgs; writers.writeBash "mullvad-setup" ''
            set -euo pipefail

            # Set up the link to the vpn container
            ${iproute}/bin/ip addr add ${peerJailIp}/24 dev ${peerJail}
            ${iproute}/bin/ip link set dev ${peerJail} up

            # Delete the default route
            ${iproute}/bin/ip route del default
            # Add a default route via the peer link
            ${iproute}/bin/ip route add default via ${peerVpnIp}
            # Add a LAN route via the host link
            ${iproute}/bin/ip route add 192.168.1.0/24 via 192.168.100.10
          '';
        };
      };

      networking = {
        firewall = {
          enable = true;
        };

        useHostResolvConf = lib.mkForce true;
      };
    };
  };

  systemd.services."container@mullvad" = {
    bindsTo = [ "downloaders-vpn-link.service" ];
    after = [ "downloaders-vpn-link.service" ];
    requires = [ "network-online.target" ];
  };

  systemd.services."container@downloaders" = {
    bindsTo = [ "downloaders-vpn-link.service" ];
    after = [ "downloaders-vpn-link.service" ];
    requires = [ "network-online.target" ];
  };

  systemd.services."downloaders-vpn-link-reset.service" = {
    bindsTo = [ "container@mullvad.service" "container@downloaders.service" ];
    after = [ "downloaders-vpn-link.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.coreutils-full}/bin/echo 'Dummy, nothing to do here.'";
      ExecStop = pkgs.writers.writeBash "mullvad-setup" ''
        set -euo pipefail

        systemctl stop downloaders-vpn-link
      '';
    };
  };
}
