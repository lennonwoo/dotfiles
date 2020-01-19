# Chinese
export LANG=en_US.UTF-8
export LC_CTYPE=zh_CN.UTF-8
export XIM="fcitx"
export XIM_PROGRAM="fcitx"
export XMODIFIERS="@im=fcitx"
export GTK_IM_MODULE="fcitx"
export QT_IM_MODULE="fcitx"


# PATH
export PATH=$PATH:/home/lennon/bin/
export CXXFLAGS="-pthread"
export CFLAGS="-pthread"
export SUBLIME_NEOVINTAGEOUS_DEBUG=1

# Go
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/Code/MOOC/mit-6.824/6.824:$HOME/Code/go

# RBENV
export PATH="$HOME/.rbenv/bin:$PATH"
export PATH=~/.rbenv/shims:$PATH
export PATH=~/.local/bin:$PATH
export RBENV_ROOT="$HOME/.rbenv"
export RUBY_BUILD_MIRROR_URL="$HOME/.rbenv/cache/"
export RUBY_BUILD_CACHE_PATH="$HOME/.rbenv/cache/"
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

export PATH=$PATH:/usr/local/cuda-9.0/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/cuda-9.0/lib64
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib/nvidia-384

# Archlinux
if [ -d ~/.virtualenvs ]; then
    export WORKON_HOME=$HOME/.virtualenvs
    export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
    source /home/lennon/.local/bin/virtualenvwrapper.sh
fi

export VISUAL="nvim"

# Ubuntu only
# if [ -n "$(uname -a | grep Ubuntu)" ]; then
#    source /opt/ros/kinetic/setup.zsh
# fi

# avoid annoying beep
if [ -n "$(uname -a | grep arch)" ]; then
    xset b off
    # http/https proxy with privoxy
fi

# export PATH=$PATH:~/dev/i7-dev/bloom/scripts
