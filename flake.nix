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

    zen-browser.url = "github:0xc000022070/zen-browser-flake";

    # Homebrew package manager support
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";

    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };

    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };

    homebrew-bundle = {
      url = "github:homebrew/homebrew-bundle";
      flake = false;
    };

    homebrew-emacs-plus = {
      url = "github:d12frosted/homebrew-emacs-plus";
      flake = false;
    };
  };

  outputs = { self, nix-darwin, nixpkgs, home-manager, nix-homebrew, homebrew-core,
              homebrew-cask, homebrew-bundle, homebrew-emacs-plus, zen-browser, ... }@inputs:
  {
    darwinConfigurations."schmapple" = nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      specialArgs = { inherit inputs; };

      modules = [
	      ./configuration.nix
        ./hosts/schmapple/configuration.nix

        home-manager.darwinModules.home-manager {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = { inherit inputs; system = "aarch64-darwin"; };
          users.users.stevenschaefer = {
            home = "/Users/stevenschaefer";
          };
          home-manager.users.stevenschaefer = {
	          imports = [
	            ./home.nix
	            ./hosts/schmapple/home.nix
	          ];
	        };
        }

        nix-homebrew.darwinModules.nix-homebrew
          {
            nix-homebrew = {
              enable = true;
              enableRosetta = true;
              user = "stevenschaefer";
              taps = {
                "homebrew/homebrew-core" = homebrew-core;
                "homebrew/homebrew-cask" = homebrew-cask;
                "homebrew/homebrew-bundle" = homebrew-bundle;
                "d12frosted/homebrew-emacs-plus" = homebrew-emacs-plus;
              };
              mutableTaps = false;
            };
          }
      ];
    };

   nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; };

      modules = [
        ./configuration.nix
        ./nixos/configuration.nix
	      home-manager.nixosModules.home-manager {
	        home-manager.useGlobalPkgs = true;
	        home-manager.useUserPackages = true;
          home-manager.extraSpecialArgs = { inherit inputs; system = "x86_64-linux"; };
	        home-manager.users.steven = {
	          imports = [
	            ./home.nix
	            ./hosts/nixos/home.nix
	          ];
          };
	      }
      ];
   };
  };
}
