# Set numlock
if [ -x /usr/bin/numlockx ]; then
    /usr/bin/numlockx on
fi

dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK

xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

exec i3
