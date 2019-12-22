# Misc
alias c="clear"
alias d="date"
alias vi="nvim"
alias svi="sudo nvim"
alias e="emacs-edit"
alias em="emacs-magit"
alias ls='ls --color=auto'
alias ll='ls -lh --color=auto'
alias la='ls -h -all --color=auto'
alias ss="proxychains"
alias sss="sudo proxychains"
alias wi="sudo wifi-menu"
alias wifi="sudo wifi-menu"
alias sy="systemctl"
alias syst="systemctl status"
alias systss="systemctl status shadowsocks -l"
alias ta="tmux attach"
alias agcc="arm-none-linux-gnueabi-gcc"
alias mackup="python ~/bin/mackup.py"
alias handle_clipboard="python3 ~/bin/handle_clipboard"
alias p='handle_clipboard'

if [ -n "$(uname -a | grep Ubuntu)" ]; then
    alias xout="xrandr --output HDMI-1-1 --mode 1920x1080 --left-of eDP-1-1 --rotate normal"
    alias xoff="xrandr --output VGA1 --off --output HDMI-1-1 --off --output LVDS1 --left-of HDMI-1-1 --auto"
    alias ap="sudo create_ap wlp3s0 enp2s0 arch-wifi 86167606"
    alias ap2="sudo create_ap wlp3s0 wlp3s0 arch-wifi 86167606"
fi


# avoid annoying beep
# if [ -n "$(uname -a | grep Arch)" ]; then
alias xout="xrandr --output HDMI1 --mode 1920x1080 --left-of eDP1 --rotate normal"
alias xoff="xrandr --output VGA1 --off --output HDMI1 --off --output LVDS1 --left-of HDMI1 --auto"
alias ap="sudo create_ap wlp3s0 enp2s0 arch-wifi 86167606"
alias ap2="sudo create_ap wlp3s0 wlp3s0 arch-wifi 86167606"
# fi
alias google-chrome="google-chrome-stable"

# Git
alias gs="git status"
alias gp="git push"
alias gl="git log --oneline --graph"
alias gc="git clone"

# Net related
alias nmdetail="nmcli device show"
alias nmstat="nmcli device status"
alias nmcon="nmcli device connect"
alias nmdiscon="nmcli device disconnect"
alias nmwifi="nmcli device wifi connect"

# ArchLinux
alias pacupg='sudo pacman -Syu'
alias pacin='sudo pacman -S'
alias pacrem='sudo pacman -Rns'
alias paclocs='pacman -Qs'
alias yain='yaourt -S'
alias paclist="pacman -Q"
# alias yain="proxychains yaourt -S"
alias yarem='yaourt -Rns'

# Pyenv
alias pl="pyenv local"
alias pg="pyenv global"
alias pv="pyenv versions"
alias pn="pyenv virtualenv"

# Directory jump
alias j="z"
alias ji="z -i"
alias jh='z --'
alias jb='z -b'
alias ..="cd .."
