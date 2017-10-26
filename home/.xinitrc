#!/bin/sh

# user files
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -I$HOME -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# color calibration
# TODO: do some detection with hwinfo maybe?
# xcalib .colorprofile/macbook.icc &
# cursor icon
xsetroot -cursor_name left_ptr
# set wallpaper
nitrogen --restore
# set keyboard repeat rate
xset r rate 250 50
# 1:1 mouse movement
xset m 1/1
# TODO:
# rofi # dmenu replacement
# dunst # dunstrc for theme
# Ensure sound is started
systemctl --user start pulseaudio
# Seed a few basic sounds for later use
for FILE in $HOME/.audio/*; do
    pactl upload-sample $FILE $(basename ${FILE%.*})
done
# clipboard history manager
clipit&
# finally start the WM
exec dbus-launch --sh-syntax --exit-with-session xmonad
