#!/bin/bash -l
#
# I blatantly stole this from Ami when setting up 
# ratpoison. Lots of stuff is commented out for now.
# In addition, I'm marking the stuff that I blatantly stole
# from him so that I remember that I have no idea what it is. ;)
#

# I'm not sure if I should need this at the beginning, but it doesn't
# seem to get loaded by default:
xrdb </home/craigcitro/.Xdefaults

# I love a reasonable repeat rate
xset r rate 200 40

# stolen
#if [ $(ps axuwww|grep " $PPID[ ]"|grep gdm|wc -l) -gt 0 ]; then
#  xauth -f ~/.Xauthority merge \
#    $(cat /proc/$(ps --ppid $(ps axuwww|grep gdm-session-worke[r]| \
#        awk '{print $2}') | grep ratpoison | \
#      awk '{print $1}')/environ|tr '\000' '\012'|sed -n -e 's/XAUTHORITY=//p')
#fi
export XAUTHORITY=~/.Xauthority

# fix the nvidia monitor flicker
/usr/bin/nvidia-settings -a [gpu:0]/GPUPowerMizerMode=1
# in theory, this would have worked, but it seems that I'm not the 
# only one running into this problem:
#  https://groups.google.com/a/google.com/group/goobuntu-discuss/browse_frm/thread/875e891504d8d708?pli=1
# /usr/bin/nvidia-settings -l

# stolen
/usr/bin/xsetroot -solid black

# stolen
#/usr/bin/gnome-settings-daemon &

# stolen
~/ext/xsettingsd/xsettingsd &

# stolen
# /usr/lib/gnome-volume-manager/gnome-volume-manager --sm-disable
/usr/bin/xmodmap ~/.xmodmaprc

# stolen
#/usr/bin/gnome-screensaver &
# For some reason, gnome-screensaver wouldn't work for me. I'd always
# start up and get the following message:
# WARNING **: Failed to get session presence proxy: Could not get owner of name 'org.gnome.SessionManager': no such name
# I didn't try hard to diagnose it. Instead, I switched to xscreensaver:
# make sure to kill existing copies of xscreensaver
xscreensaver-command -exit
# (2010 Sep 27) currently, goobuntu is still sticking something in my
# way that starts gnome-screensaver; here's a hacky fix:
killall gnome-screensaver
# run a new one
xscreensaver &

# stolen
/usr/bin/xset -b

# set up my mouse button appropriately:
xinput set-button-map $(xinput --list | grep "Evoluent" | sed -e "s/.*id=\([0-9]*\).*/\1/g") 1 2 3 4 5 6 7 3 8

# start the WM
exec /usr/bin/ratpoison
