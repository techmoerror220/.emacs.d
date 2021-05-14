#!/bin/sh

# Source .profile for common environment vars
. ~/.profile

# Disable access control for the current user
xhost +SI:localuser:$USER

# Make Java applications aware this is a non-reparenting window manager.
export _JAVA_AWT_WM_NONREPARENTING=1

# Set themes, etc
# gnome-settings-daemon &

# Set fallback cursor
xsetroot -cursor_name left_ptr

## Over-rides
#
xset b off &
#numlockx off                    # Turn off numlock

# Set keyboard repeat rate
xset r rate 180 40

# Set capslock as ctrl. DGM had the happy idea of disabling it and then I got the Spanish layout on my keyboard!
setxkbmap -layout us -option ctrl:nocaps
# setxkbmap -layout us
# setxkbmap -layout es -option ctrl:nocaps
# setxkbmap -layout us,us -variant ,dvorak -option "lv3:rwin_switch,grp:alt_space_toggle"
# setxkbmap -layout us -variant dvorak

# DGM trying to get the volume working
# Jan 1, 2021. Disabled, as I'm trying DW's config
# amixer -c 1 set Master unmute
# amixer -c 1 set Master 100%
# amixer -c 1 set Mic 5db

# Programs to start upon startup
# xfce4-power-manager &
syncthing -no-browser &         # Syncthing
## nm-applet &                     # Network Manager systemtray app that gives you information about your connections (disabled to try and activate it from the init.el)
udiskie --tray &                # Disk mount utility (option for showing a system tray)

# Others from exwm-tutorial.pdf (DGM on 30 Dic 2019 comments out the next two lines in an attempt to use alsa instead of pulseaudio)
# Jan 1, 2021. Disabled, as I'm trying DW's config
pulseaudio --kill               # Kill pulseaudio
pulseaudio --start              # Start pulseaudio
## volti &                         # Volume managers. Not working though.
## pasystray &                  # Pulseaudio volume control from tray (disabled to try and activate it from the init.el)

# DGM. command to use nice icons
# in the future, I should try and connect it to the SESSION_MANAGER, I think.
# xfsettingsd &

# Run the screen composition
compton &

################ redshift
## redshift -x
## redshift -O 3500 -P -m randr

## File set to Fuengirola location.
# /home/dgm/.config/latlon.sh tries to automate the location process, but it doesn't work. See https://wiki.archlinux.org/index.php/redshift
redshift -c /home/dgm/.config/redshift.conf &

## Set screen DPI
xrdb ~/.emacs.d/exwm/Xresources

# launch emacs upon startup
# commented out by DGM on 22/12/2020 to see if i can simply start with <exec emacs> as told in https://github.com/ch11ng/exwm/issues/299
exec dbus-launch --exit-with-session emacs -mm --debug-init --use-exwm
#exec emacs
