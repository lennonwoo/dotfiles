i3-msg '[title=".*Doom.*$"] scratchpad show' > /dev/null
emacsclient -a -c --eval "(call-interactively #'magit-status)" > /dev/null
