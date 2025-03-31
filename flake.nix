{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-24.11-darwin";
    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-24.11";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    home-manager = {
      url = "github:nix-community/home-manager/release-24.11"; # Or your desired release
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nix-darwin, nixpkgs, home-manager, ... }@inputs:
  {
    darwinConfigurations."schmapple" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = { inherit inputs; };

      modules = [
        ./hosts/schmapple/configuration.nix
        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = { inherit inputs; system = "aarch64-darwin"; };
          users.users.stevenschaefer = {
            home = "/Users/stevenschaefer";
          };
          home-manager.users.stevenschaefer = import ./hosts/schmapple/home.nix;
        }
      ];
    };
  };
}
