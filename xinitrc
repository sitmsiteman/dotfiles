#!/bin/sh

[ -f $HOME/.Xresources ] && xrdb -merge $HOME/.Xresources

xset s on
xset +dpms
xset dpms 300 450 600
xset b 0
xset r rate 250 40

xsetroot -cursor_name left_ptr
xsetroot -solid 'rgb:00/22/44'

setxkbmap -layout kr -variant kr104

export LANG=ko_KR.UTF-8
export LC_ALL=ko_KR.UTF-8

ibus-daemon -drRx

export PATH="$PATH:$HOME/bin:$HOME/go/bin"

xss-lock --transfer-sleep-lock i3lock &
xbindkeys &
udiskie &
cbatticon &
volumeicon &
telegram-desktop &
dunst &

# exec fvwm
exec i3
