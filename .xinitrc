#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

# exec gnome-session
# exec startkde
# exec startxfce4

setxkbmap -option "" -layout "us,ua" \
     -option grp:shift_toggle \
     -option grp_led:scroll :2

xset r rate 200 30

xset -dpms;xset s off

unclutter -grab &

exec pidgin&
exec xmobar&
exec xmonad
