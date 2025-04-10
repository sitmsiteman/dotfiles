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

export QT_IM_MODULE=fcitx
export GTK_IM_MODULE=fcitx	
export XMODIFIERS=@im=fcitx

fcitx5 -d

export PATH="$PATH:$HOME/bin:$HOME/go/bin"

pkill xss-lock
xcompmgr &

[ -f $HOME/.fehbg ] && $HOME/.fehbg

stalonetray -geometry 1x1-0+100 -t --grow-gravity E &
xclock -geometry 80x80-0+0 &
xss-lock --transfer-sleep-lock i3lock &
xbindkeys &
udiskie &
cbatticon &
volumeicon &
telegram-desktop &
dunst &
xterm -geometry +0+0 &

exec fvwm
