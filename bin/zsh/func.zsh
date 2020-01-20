sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
    zle end-of-line
}

emacs-edit() {
    i3-msg '[title=".*Doom.*$"] scratchpad show' > /dev/null
    emacsclient -a -c "$@" > /dev/null
    i3-msg '[title=".*Doom.*$"] scratchpad show' > /dev/null
}

emacs-magit() {
    i3-msg '[title=".*Doom.*$"] scratchpad show' > /dev/null
    emacsclient --eval "(call-interactively #'magit-status)"
}

enable-proxy() {
    export http_proxy=http://127.0.0.1:8989
    export https_proxy=http://127.0.0.1:8989
    export no_proxy="localhost, 127.0.0.0/8, ::1"
}

enable-lennon-bloom-env() {
    export BLOOM_VERBOSE=1
    export ROSDISTRO_INDEX_URL=https://raw.githubusercontent.com/lennonwoo/rosdistro/master/index-v4.yaml
}

vbox-init() {
    xrandr --newmode "1920x1080"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
    xrandr --addmode Virtual-1 1920x1080
    xrandr --output Virtual-1 --mode 1920x1080
    VBoxClient-all
}
