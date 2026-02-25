# ABOUTME: Single source of truth for home-manager packages across all platforms
# ABOUTME: Platform-specific packages handled via lib.optionals conditionals
{
  lib,
  pkgs,
  ...
}:

with pkgs;
with nodePackages_latest;
with tflint-plugins;

  # Common packages (all platforms)
  [
    adr-tools
    alejandra
    ansible
    aws-cdk
    aws-doctor
    aws-vault
    awscli2
    awslogs

    bash-language-server
    cachix
    cdk8s-cli
    claude-code
    cmake
    coder
    cspell
    csvkit

    delta
    devbox
    dig
    direnv
    dockfmt

    editorconfig-checker
    editorconfig-core-c
    exiftool

    fd
    ffmpeg
    file
    fontconfig
    fzf

    gemini-cli
    gh
    git
    git-extras
    git-lfs
    glab
    gnumake
    gnupg
    go
    google-cloud-sdk
    gopls
    gore
    gomodifytags
    gotools
    gotests

    html-tidy
    htop
    hugo
    hyperfine

    imagemagick
    inetutils
    ispell

    jq
    jsbeautifier

    keychain
    kind
    kubectl
    kubernetes-helm
    kustomize

    mob
    multimarkdown

    nixd
    nix-index
    nodejs_20
    nomad
    nox

    openssl

    pandoc
    (pass.withExtensions
      (ext: with ext; [pass-genphrase pass-otp pass-update]))
    pass-git-helper
    patchelf
    perl
    pipx
    podman
    podman-compose
    prettier
    pry
    python3

    rain
    ripgrep
    ruff

    shellcheck
    shfmt
    sqlite
    ssm-session-manager-plugin
    stylelint

    terraform
    terraform-docs
    terraform-ls
    tflint
    tflint-ruleset-aws
    tfsec
    tmux
    typescript-language-server

    unzip
    urlscan
    uv

    vale
    vault
    vscode-langservers-extracted

    wget
    wordnet

    yaml-language-server
    yarn
    yq-go
    yt-dlp

    zbar
    zip
    zoxide
    zstd
  ]
  # Darwin-specific packages
  ++ lib.optionals stdenv.isDarwin [
    clang # macOS uses clang; Linux uses gcc
    coreutils-prefixed # prefixed to avoid conflicts with macOS builtins
    glibtool # macOS version of libtool
    gnused # GNU sed (macOS sed is BSD)
    graphviz # full graphviz with GUI support
    mas # Mac App Store CLI
    pinentry_mac
    pngpaste # paste images from clipboard
  ]
  # Linux-specific packages
  ++ lib.optionals stdenv.isLinux [
    coreutils

    # Wails (Go desktop framework) dependencies
    gtk3
    pkg-config
    wails
    webkitgtk_4_1
    csharpier
    dotnet-sdk
    gcc # Linux uses gcc; macOS uses clang
    glibcLocales
    graphviz-nox # graphviz without X dependencies
    libtool
    pinentry-curses
    vanilla-dmz # cursor theme
    xorg.xvfb
    xvfb-run
  ]
