#!/bin/sh

[ -f $HOME/.Xresources ] && xrdb -merge $HOME/.Xresources

if [ -x /usr/local/bin/dbus-launch -a -z "${DBUS_SESSION_BUS_ADDRESS}" ]; then
        eval `dbus-launch --sh-syntax --exit-with-session`
fi

. $HOME/.profile

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
export IBUS_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx

fcitx5 -d

pkill xss-lock
xcompmgr &

[ -f $HOME/.fehbg ] && $HOME/.fehbg

xclock -geometry 80x80-0+0 &
xss-lock --transfer-sleep-lock i3lock &
xbatt -geometry +0-0 &
xbindkeys &
telegram-desktop &
xterm -geometry 80x24+0+0 &
dunst &

exec fvwm
