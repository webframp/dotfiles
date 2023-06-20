{ inputs, lib, pkgs, config, outputs, ... }:

{
  nix = {
    package = lib.mkDefault pkgs.nix;
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      warn-dirty = false;
    };
  };

  programs.home-manager.enable = true;

  home = {
    username = lib.mkDefault "sme";
    homeDirectory = lib.mkDefault "/home/${config.home.username}";
    stateVersion = lib.mkDefault "23.05";
  };

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableSyntaxHighlighting = true;
    autocd = true;
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      ignoreSpace = true;
    };
    # https://nix-community.github.io/home-manager/options.html#opt-programs.zsh.plugins
    # plugins = []
    # or maybe: https://nix-community.github.io/home-manager/options.html#opt-programs.zsh.zplug.enable
  };

  programs.fzf = { enable = true; };

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.exa = { enable = true; };

  home.file.".gemrc".text = "gem: --no-ri --no-rdoc";
}
