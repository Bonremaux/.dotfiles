
# history settings
setopt hist_ignore_all_dups inc_append_history
HISTFILE=~/.zhistory
HISTSIZE=4096
SAVEHIST=4096

# https://raw.githubusercontent.com/Dnomyar/dotfiles/master/.zshrc

# Open a file with the appropriate application
function open {
    while [ "$1" ] ; do
        xdg-open $1 &> /dev/null
        shift # shift décale les param
    done
}


function cd_ls () {
    \cd "$@" && ls --color=auto -laAh
}


# Alias
alias ls='ls -h --color=auto'
alias ll="ls --color=auto -laAh"
alias grep='grep -i --color=auto'
alias rm='rm --interactive --verbose'
alias mv='mv --interactive --verbose'
alias cp='cp -R --verbose'
alias ..='cd ..'
alias path='echo $PATH | tr -s ":" "\n"'
alias cl='cd_ls'
alias lstree="find . | sed 's/[^/]*\//| /g;s/| *\([^| ]\)/+--- \1/'"
alias df='df -h'


autoload -U colors
colors

PROMPT="%{$fg[yellow]%}%n %{$reset_color%}%c $ "

# enable colored output from ls, etc
export CLICOLOR=1

autoload -U compinit promptinit
compinit
promptinit
