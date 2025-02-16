#!/bin/sh

[ -f $HOME/.Xresources ] && xrdb -merge $HOME/.Xresources

xset s on
xset +dpms
xset dpms 900 1200 1500
xset b 0
xset r rate 250 40

setxkbmap -layout kr -variant kr104

export GTK_IM_MODULE=ibus
export QT_IM_MODULE=ibus
export XMODOFIERS=@im=ibus

ibus-daemon -drx

export LANG=ko_KR.UTF-8
export LC_ALL=ko_KR.UTF-8

exec fvwm
