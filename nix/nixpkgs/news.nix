# https://github.com/nix-community/home-manager/issues/2033#issuecomment-1848326144
{ config, pkgs, ... }:
{
  # disabledModules = [ "misc/news.nix" ];
  config = {
    news.display = "silent";
    news.json = lib.mkForce { };
    news.entries = lib.mkForce [ ];
  };
}
