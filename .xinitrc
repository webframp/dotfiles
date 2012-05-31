# Source system-wide xinit scripts
if [ -d /etc/X11/xinit/xinitrc.d/ ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# set proper color profile
xcalib $HOME/.colorprofiles/colorlcd.icc
# system tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype percent --width 5 --transparent true --tint 0x3f3f3f --height 12 &
# set the cursor
xsetroot -cursor_name left_ptr
# clipboard manager
parcellite &
batti &
autocutsel -fork &
autocutsel -selection PRIMARY -fork &
# run terminal daemon
urxvtd &
# load defaults and remap capslock
xrdb $HOME/.Xdefaults
xmodmap $HOME/.Xmodmap
# set background
eval `cat $HOME/.fehbg`
# gimmah some soundz
pulseaudio --start
# turn on keyboard backlight
echo 100 | sudo tee /sys/class/leds/smc::kbd_backlight/brightness
# best wm evah
xmonad
