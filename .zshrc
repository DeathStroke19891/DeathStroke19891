if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export PATH=$HOME/.local/bin:/usr/lib/jvm/java-20-openjdk/bin:/usr/local/bin:$HOME/.config/emacs/bin:$PATH

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="powerlevel10k/powerlevel10k"

HYPHEN_INSENSITIVE="true"

zstyle ':omz:update' mode auto 
zstyle ':omz:update' frequency 15

ENABLE_CORRECTION="true"

plugins=(git sudo aliases colored-man-pages ripgrep systemd zoxide fd zsh-autosuggestions)

export EDITOR="vim"
export VISUAL="vim"

export VI_MODE_SET_CURSOR=true
export XDG_SCREENSHOTS_DIR="/home/parzival/Pictures/Screenshots"
export JAVA_HOME="/usr/lib/jvm/java-20-openjdk"
export LD_LIBRARY_PATH="/usr/lib/jvm/java-20-openjdk/lib"
export CHROME_EXECUTABLE="vivaldi-stable"
export SDL_VIDEODRIVER="wayland, x11"
source $ZSH/oh-my-zsh.sh

alias cd=z
alias ls=exa
alias ll="exa -al"
alias reload="source ~/.zshrc"
alias vi=vim
alias tree="exa --tree"
alias config="git --git-dir=/home/parzival/.cfg --work-tree=/home/parzival/."
alias mirrors="rate-mirrors --allow-root --protocol https arch | sudo tee /etc/pacman.d/mirrorlist"

clear

[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
