{
  description = "flake for nixOS";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-unstable";
    };
    hyprland = {
      url = "github:hyprwm/Hyprland";
    };
  };

  outputs = { self, nixpkgs, ... } @ inputs: {
    nixosConfigurations = {
      hyperv = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit inputs; };
        modules = [
          ./configuration.nix
        ];
      };
    };
  };
}
