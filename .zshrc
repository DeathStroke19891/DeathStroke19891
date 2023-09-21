if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export PATH=$HOME/.local/bin:/usr/lib/jvm/java-20-openjdk/bin:/usr/local/bin:$HOME/.config/emacs/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
zstyle ':omz:update' frequency 15

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

plugins=(git sudo aliases colored-man-pages ripgrep rust systemd zoxide fd zsh-autosuggestions)

export EDITOR="vim"
export VISUAL="vim"

export VI_MODE_SET_CURSOR=true
export XDG_SCREENSHOTS_DIR="/home/parzival/Pictures/Screenshots"
export JAVA_HOME="/usr/lib/jvm/java-20-openjdk"
export LD_LIBRARY_PATH="/usr/lib/jvm/java-20-openjdk/lib"
export CHROME_EXECUTABLE="vivaldi-stable"
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
