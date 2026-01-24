# ABOUTME: Minimal bootstrap config for fresh linux systems without existing nix management
# ABOUTME: Provides base zsh setup and essential tools as a starting point
{...}: {
  imports = [./global];

  home.shellAliases = {
    yay = "home-manager switch --flake .#sme@generic";
  };
}
