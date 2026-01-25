# ABOUTME: Reusable home-manager modules for this flake
# ABOUTME: Import these modules in home configurations to share common settings
{
  bat = import ./bat.nix;
  delta = import ./delta.nix;
  direnv = import ./direnv.nix;
  fzf = import ./fzf.nix;
  tmux = import ./tmux.nix;
  zsh = import ./zsh.nix;
}
