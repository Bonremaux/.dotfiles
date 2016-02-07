#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export PATH=$PATH:~/bin
export PATH=$PATH:~/.cabal/bin
export PATH=$PATH:/opt/cuda/bin
export GLPATH=/usr/lib
export TERM=xterm-256color
export EDITOR=vim

xsetroot -cursor_name left_ptr

setxkbmap -option "" -layout "us,ua" \
     -option grp:shift_toggle \
     -option grp_led:scroll :2

xset r rate 200 30

xset -dpms;xset s off

unclutter -grab &

feh --bg-fill ~/dotfiles/DSC_0553.JPG &

#exec dbus-launch --sh-syntax --exit-with-session
exec pidgin&
exec xmonad
