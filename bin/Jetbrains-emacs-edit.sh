i3-msg '[title=".*Doom.*$"] scratchpad show' > /dev/null
emacsclient -a -c "$@" > /dev/null
i3-msg '[title=".*Doom.*$"] scratchpad show' > /dev/null
