# ABOUTME: Reusable home-manager modules for this flake
# ABOUTME: Import these modules in home configurations to share common settings
{
  bat = import ./bat.nix;
  claudeCode = import ./claude-code.nix;
  delta = import ./delta.nix;
  doom = import ./doom.nix;
  direnv = import ./direnv.nix;
  fzf = import ./fzf.nix;
  git = import ./git.nix;
  kiroCli = import ./kiro-cli.nix;
  mail = import ./mail.nix;
  pi = import ./pi.nix;
  tmux = import ./tmux.nix;
  zsh = import ./zsh.nix;
}
