if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export PATH=$HOME/.local/bin:/usr/lib/jvm/java-20-openjdk/bin:/usr/local/bin:$HOME/.config/emacs/bin:/opt/flutter/bin:$PATH

export ZSH="$HOME/.oh-my-zsh"


ZSH_THEME="powerlevel10k/powerlevel10k"

HYPHEN_INSENSITIVE="true"

zstyle ':omz:update' mode auto 
zstyle ':omz:update' frequency 15

ENABLE_CORRECTION="true"

plugins=(git sudo aliases colored-man-pages ripgrep systemd zoxide fd zsh-autosuggestions)

export EDITOR="nvim"
export VISUAL="nvim"

export VI_MODE_SET_CURSOR=true
export XDG_SCREENSHOTS_DIR="/home/parzival/Pictures/Screenshots"
export JAVA_HOME="/usr/lib/jvm/java-17-openjdk"
export LD_LIBRARY_PATH="/usr/lib/jvm/java-17-openjdk/lib"
export CHROME_EXECUTABLE="vivaldi-stable"
export SDL_VIDEODRIVER="wayland"
export QT_QPA_PLATFORMTHEME="qt5ct"
source $ZSH/oh-my-zsh.sh

alias cd=z
alias ls="exa -al"
alias ll="exa -al"
alias reload="source ~/.zshrc"
alias vi=vim
alias tree="exa --tree"
alias config="git --git-dir=/home/parzival/.cfg --work-tree=/home/parzival/."
alias mirrors="rate-mirrors --allow-root --protocol https arch | sudo tee /etc/pacman.d/mirrorlist"
alias vim="nvim"
alias fixpacman="sudo rm /var/lib/pacman/db.lck"

cat $HOME/.cache/wal/sequences

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/parzival/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/parzival/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/home/parzival/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/home/parzival/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/parzival/mambaforge/etc/profile.d/mamba.sh" ]; then
    . "/home/parzival/mambaforge/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<
