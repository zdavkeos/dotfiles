# The following lines were added by compinstall

autoload -Uz compinit
autoload -U zmv
compinit

# matches case insensitive for lowercase
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# pasting with tabs doesn't perform completion
zstyle ':completion:*' insert-tab pending


HISTFILE=~/.histfile
HISTSIZE=2000
SAVEHIST=1000
setopt appendhistory 
setopt extendedglob
unsetopt beep nomatch notify
bindkey -e


# fix delete key
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char


function batt_level {
    local arrow
    local level

    # get charge level
    level=$(acpi -b | sed -rn 's/Battery 0: [a-Z]+, ([0-9]+)%.*/\1%%/p')
    
    # see if we are charing or discharging
    if [[ -n "$level" ]]; then
	arrow=$(acpi -b | grep -o Charging)
	if [[ -n "$arrow" ]]; then
	    arrow=" ⇧" # ↑
	else
	    arrow=" ⇩" # ↓
	fi
    fi

    echo $arrow$level
}

function prompt_char {
    git branch >/dev/null 2>/dev/null && echo ' ±' && return
    hg root >/dev/null 2>/dev/null && echo ' ☿' && return
    svn info >/dev/null 2>/dev/null && echo ' ʂ' && return
}

function simple_time {
    date +"%I:%M%p"
}

# when in a git repo, only look at local files instead of asking git
__git_files () { 
    _wanted files expl 'local files' _files     
}

## Prompt
export PROMPT='[%n@%m] %.%# '
#export PROMPT='\033[G[%n@%m] %.%# '
#setopt PROMPT_SUBST
#export RPROMPT='->$(simple_time) %!'
#export PROMPT='[%n@%m$(batt_level)] %.$(prompt_char)%# '
#export PROMPT='[%n@%m] %.$(prompt_char)%# '

#global exports
export LANGUAGE=en_US.UTF8
export EDITOR=emacsclient
export SVN_EDITOR='emacs'

export PATH="$HOME/.cargo/bin:$PATH"

# my aliases
alias emacsq='emacs -nw -Q'
alias e='emacsclient -c --no-wait'
alias claer='clear' # I always miss-type these two...
alias clera='clear'
alias l='ls'
alias la='ls -a'
alias ll='ls -lh'
alias cd..='cd ..'
alias py='python'
alias cls='clear'
alias givm='gvim'
