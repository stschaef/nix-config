# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page

# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, inputs, ... }:
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    systemd-boot.configurationLimit = 1;
  };

  fonts.fontconfig.enable = true;

  services.tailscale.enable = true;

  networking = {
    hostName = "cheeseplease"; # Define your hostname.
    networkmanager.enable = true;
    wireless.enable = false;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
      trustedInterfaces = [ "tailscale0" ];
      allowedUDPPorts = [ 41641 ]; # Tailscale
    };
    interfaces.enp3s0.wakeOnLan.enable = true;
  };
  hardware.enableRedistributableFirmware = true;


  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      workstation = true;
    };
  };


  time.timeZone = "America/Detroit";

  i18n = {
    defaultLocale = "en_US.UTF-8";

    extraLocaleSettings = {
      LC_ADDRESS = "en_US.UTF-8";
      LC_IDENTIFICATION = "en_US.UTF-8";
      LC_MEASUREMENT = "en_US.UTF-8";
      LC_MONETARY = "en_US.UTF-8";
      LC_NAME = "en_US.UTF-8";
      LC_NUMERIC = "en_US.UTF-8";
      LC_PAPER = "en_US.UTF-8";
      LC_TELEPHONE = "en_US.UTF-8";
      LC_TIME = "en_US.UTF-8";
    };
  };

  services.xserver = {
    enable = true;
    displayManager = {
      gdm.enable = true;
    };
    desktopManager.gnome.enable = true;
    xkb = {
      layout = "us";
      variant = "";
      options = "caps:swapescape";
    };
  };

  services.displayManager.defaultSession = "gnome";

  console.useXkbConfig = true;

  # AMD GPU support (for Sapphire card)
  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      # AMD GPU video acceleration
      rocmPackages.clr.icd
      amdvlk
    ];
    extraPackages32 = with pkgs; [
      driversi686Linux.amdvlk
    ];
  };

  # AMD GPU driver
  services.xserver.videoDrivers = [ "amdgpu" ];

  # NVMe storage mount
  fileSystems."/mnt/nvme" = {
    device = "/dev/nvme0n1p1";  # Adjust if partition is different
    fsType = "ext4";
    options = [ "defaults" ];
  };

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = true;
      PermitRootLogin = "no";
      AllowUsers = [ "steven" ];
    };
    # Listen on all interfaces
    listenAddresses = [
      { addr = "0.0.0.0"; port = 22; }
    ];
  };


  # Enable sound with pipewire.
  hardware.pulseaudio.enable = false;

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.users.steven = {
    isNormalUser = true;
    description = "Steven Schaefer";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
    ];
  };

  # Enable automatic login for the user.
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "steven";

  # Workaround for GNOME autologin: https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd.services."getty@tty1".enable = false;
  systemd.services."autovt@tty1".enable = false;

  # Install firefox.
  programs.firefox.enable = true;

  environment.systemPackages = import ./packages.nix { inherit pkgs inputs; };

  users.defaultUserShell = pkgs.zsh;

  system.stateVersion = "25.11";

}
