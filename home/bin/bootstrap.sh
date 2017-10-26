#!/bin/bash

# basic bootstrapper shell script for Arch Linux
pacman -S curl base-devel --noconfirm --needed

need_cmd curl
need_cmd makepkg

# AUR helpers
mkdir /tmp/aurhelp
pushd /tmp/aurhelp
curl https://aur.archlinux.org/cgit/aur.git/snapshot/package-query.tar.gz|tar xzf -
pushd package-query
makepkg -si --noconfirm
popd
curl https://aur.archlinux.org/cgit/aur.git/snapshot/yaourt.tar.gz| tar xzf -
pushd yaourt
makepkg -si --noconfirm
popd
# leave /tmp/aurhelp
popd

# shell setup
pacman -S zsh exa

# extra packages
need_cmd yaourt
yaourt --noconfirm --needed -S haveged clipit xsel xclip rxvt-unicode direnv exa ncurses5-compat-libs lastpass-cli keybase_bin openssh chromium pidgin pidgin-sipe dmenu yegonesh homeshick-git

systemctl start haveged
systemctl enable haveged

# fonts
yaourt --noconfirm --needed -S ttf-iosevka ttf-ms-fonts

# PulseAudio and video tools
yaourt --noconfirm --needed -S pulseaudio pulseaudio-alsa pulseaudio-gconf paprefs pamixer pavucontrol
yaourt --noconfirm --needed -S guvcview

# ArchHaskell and packages

# User specific setup
useradd -m -g users -G wheel sme
sudo -u sme sh -c "cd $HOME && sh-c $(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
# relink settings
need_cmd homeshick
sudo -u sme sh -c 'homeshick clone webframp/dotfiles'
sudo -u sme sh -c 'homeshick link dotfiles --force'

need_cmd() {
	if ! command -v "$1" > /dev/null 2>&1; then
		exit_with "Required command '$1' not found on PATH " 127
	fi
}

exit_with() {
	warn "$1"
	exit "${2:-10}"
}
