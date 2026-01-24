# ABOUTME: Reusable home-manager modules for this flake
# ABOUTME: Import these modules in home configurations to share common settings
{
  bat = import ./bat.nix;
  delta = import ./delta.nix;
  fzf = import ./fzf.nix;
  zsh = import ./zsh.nix;
}
