######################
# cc config

##################
## Basics

if [[ $(uname -s) == "Linux" ]]; then
  SYSTEM='Linux'
elif [[ $(uname -s) == "Darwin" ]]; then
  SYSTEM='Darwin'
else
  SYSTEM='unknown'
fi

###############################
## mac configuration



###############################
## ls configuration

if [[ "$SYSTEM" == "Darwin" ]]; then
  ## darwin/bsd ls formatting
  export LSCOLORS="Dxfxcxdxexegedabagacad"
  export BLOCKSIZE=1024
  alias l='ls -sFG' ## bsd ls
  alias la='ls -sFGa'
  alias ll='ls -lFG'
  alias lla='ls -lFGa'
else
  ## gnu ls formatting
  alias ls='ls --color=always'
  alias l='ls -BhFvs' ## gnu ls
  alias la='ls -hFvsA'
  alias ll='ls -lBhFv'
  alias lla='ls -lhFvA'
fi

###############################
## emacs-related

# We want everything to route through one central set of emacs
# commands.
export EMACS_SERVERNAME='craigcitro'
alias et='emacsclient -c -s ${EMACS_SERVERNAME} -t'
# Every EDITOR variable I can find.
export EDITOR="emacsclient -c -s ${EMACS_SERVERNAME} -t"
export VISUAL="emacsclient -c -s ${EMACS_SERVERNAME} -t"
export CVSEDITOR="$(which emacsclient) -c -s ${EMACS_SERVERNAME} -t"
export GIT_EDITOR="$(which emacsclient) -c -s ${EMACS_SERVERNAME} -t"
# (2011 Sep 13) Here's the old version:
# if [ "$(ps awx -U ${USER} | grep macs | grep daemon | grep ${EMACS_SERVERNAME})xxx" == "xxx" ]; then
# Amusingly, it's actually a bit more robust: a server can crash and fail to
# close the socket. However, the command below will correctly identify servers
# whose name wasn't specified at the command line.
remacs () {
  if [[ ! -S $(emacsclient -s ${EMACS_SERVERNAME} --eval '(print server-socket-dir (lambda (s) nil))' 2>/dev/null | awk -F\" '{print $2}')"/"${EMACS_SERVERNAME} ]]; then
    LOCKFILE="${HOME}/.lock-emacs-${EMACS_SERVERNAME}"
    if [[ -f "${LOCKFILE}" ]]; then
      echo "Emacs is currently starting up, skipping ..."
    else
      echo "Starting Emacs daemon with servername ${EMACS_SERVERNAME} ..."
      touch ${LOCKFILE}
      $(which emacs) --daemon="${EMACS_SERVERNAME}"
      rm -f "${LOCKFILE}"
    fi
  fi
}
if [[ "$SYSTEM" != "Linux" || ${DEVBOX:-0} != "0" ]]; then
  remacs
fi

###############################
## other unix default stuff

if [[ "$TERM" = "xterm" ]]; then
  export TERM=xterm-256color
fi
if [[ "$TERM" = "screen" ]]; then
  export TERM=screen-256color
fi

export PAGER='less -qFRX'
alias less='less -qFRX'

typeset -U path

# TODO(craigcitro): look at this
# fix man paths
# pathappend '/usr/local/share/man' MANPATH;
# pathappend '/usr/X11/share/man' MANPATH;
# pathappend '/Developer/usr/share/man' MANPATH;
# pathappend '/usr/share/man' MANPATH;
# pathappend '/usr/local/man' MANPATH;
# export MANPATH

################################
# Common commands
alias df='df -h'
alias scp='scp -p'
alias du='du -h -d 1'

# Maybe there's a better name for this?
alias reup="source $HOME/.zshrc"

# Silly commands for avoiding 'cd ../..': there's probably a better way, but
# these have done me right for a while.
alias u='cd ..'
alias uu='cd ../..'
alias uuu='cd ../../..'
alias uuuu='cd ../../../..'
alias uuuuu='cd ../../../../..'
alias uuuuuu='cd ../../../../../..'
alias uuuuuuu='cd ../../../../../../..'
alias uuuuuuuu='cd ../../../../../../../..'
alias uuuuuuuuu='cd ../../../../../../../../..'
alias pu='pushd ..'
alias puu='pushd ../..'
alias puuu='pushd ../../..'
alias puuuu='pushd ../../../..'
alias puuuuu='pushd ../../../../..'
alias puuuuuu='pushd ../../../../../..'
alias puuuuuuu='pushd ../../../../../../..'
alias puuuuuuuu='pushd ../../../../../../../..'
alias puuuuuuuuu='pushd ../../../../../../../../..'

#################################
# zsh keybindings

bindkey "^[[A" history-beginning-search-backward
bindkey "^[[B" history-beginning-search-forward
bindkey "^P" history-beginning-search-backward
bindkey "^N" history-beginning-search-forward

# This tweaks backward-kill-word to use only alphanumeric characters.
autoload -U select-word-style
select-word-style bash

# I don't want to share history among *live* shells.
unsetopt sharehistory

# how is this not enabled by default
autoload -z edit-command-line
zle -N edit-command-line
bindkey "^X^E" edit-command-line

# meh
unsetopt autocd

#################################
# prompt

# I solemnly swear I am up to no good.
autoload -Uz colors && colors
setopt prompt_subst
# I don't want the prompt to auto-refresh.
export NO_PROMPT_AUTO_REFRESH=1

autoload -Uz add-zsh-hook
typeset -aU precmd_functions

if [[ $SYSTEM == "Linux" ]]; then
  alias _date='date +%s%3N'
else
  alias _date='gdate +%s%3N'
fi

preexec.cc_set_timer() {
  CC_CMD_TIMER=$(($(_date)))
}
add-zsh-hook preexec preexec.cc_set_timer

preexec.cc_save_cmd() {
  CC_LAST_CMD=$1
}
add-zsh-hook preexec preexec.cc_save_cmd

maybe_print_execution_time_line() {
  if (( ${#CC_CMD_TIMER} <= 0 )); then
    return
  fi
  if (( ${#CC_LAST_CMD} <= 0 )); then
    return
  fi
  FORMATTED=" last: "
  DELTA=$(( $(_date) - $CC_CMD_TIMER ))
  HOURS=$(( DELTA / 3600000 )); DELTA=$(( DELTA % 3600000 ))
  MINUTES=$(( DELTA / 60000 )); DELTA=$(( DELTA % 60000 ))
  SECONDS=$(( DELTA / 1000 )); MILLIS=$(( DELTA % 1000 ))
  (( $HOURS > 0 )) && FORMATTED+="${HOURS}h"
  (( $MINUTES > 0 )) && FORMATTED+="${MINUTES}m"
  (( $SECONDS > 0 )) && FORMATTED+="${SECONDS}s"
  (( $MILLIS > 0 )) && FORMATTED+="${MILLIS}ms"
  CC_LAST_CMD=""
  print -P "%F{239}[%m] $(date '+%Y %b %d %a %H:%M:%S')${FORMATTED}%f"
}
precmd_functions+=(maybe_print_execution_time_line)

#
# git status in prompt
#
# the entry looks like: {branchname?!+}, with:
# * ? for untracked files
# * ! for # edited files
# * + for unpushed commits.
#
# i cache it in $CC_LAST_GIT_PROMPT and only update every $CC_GIT_TIMEOUT
# seconds; I track the time since the last via $CC_LAST_GIT_CHECK.
#
# i also clear the cache whenever I run `git*`, `cd*`, or `u*`.
cc_gitinfo() {
  local branch=""
  local new=""
  local ahead=""
  local changed=""
  git status --porcelain=v2 --branch 2>/dev/null | while read line; do
    if [[ $line == "# branch.head "* ]]; then
      words=("${(s/ /)line}")
      branch=$words[3]
    elif [[ -z $changed && ($line == 1* || $line == 2*) ]]; then
      changed="!"
    elif [[ $line == "# branch.ab +"* && $line != "# branch.ab +0"* ]]; then
      ahead="+"
    elif [[ $line == "?"* ]]; then
      new="?"
      break
    fi
  done
  if [[ -n $branch ]]; then
    echo "{$branch$new$changed$ahead}"
  fi
}

precmd.cc_git_prompt_info() {
  if [[ $SYSTEM == "Linux" && (-z $COO_DEVSPACE && -z $COO_HOMESPACE) ]]; then
    return
  fi
  CC_GIT_TIMEOUT=${CC_GIT_TIMEOUT:-8}
  local sec_elapsed=$(( $(date +%s) - ${CC_LAST_GIT_CHECK:-0} ))
  if [[ -n $CC_LAST_GIT_PROMPT ]] && (( $sec_elapsed < $CC_GIT_TIMEOUT )); then
    if [[ -n $CC_GIT_PROMPT_VERBOSE ]]; then
      echo "CC_GIT: reusing existing state"
    fi
    return
  fi
  if [[ -n $CC_GIT_PROMPT_VERBOSE ]]; then
    echo "CC_GIT: re-calculating git status"
  fi
  local b="$(cc_gitinfo)"
  if [[ ! -n ${b} ]]; then
    if [[ -n $CC_GIT_PROMPT_VERBOSE ]]; then
      echo "CC_GIT: no git repo"
    fi
    return
  fi
  local color="green"
  if [[ $b = *"!"* || $b = *"?"* ]]; then
    color="yellow"
  fi
  if [[ $b = *"+"* ]]; then
    color="red"
  fi
  export CC_LAST_GIT_PROMPT=" %F{$color}${b}%f"
  export CC_LAST_GIT_CHECK=$(date +%s)
  if [[ -n $CC_GIT_PROMPT_VERBOSE ]]; then
    echo "CC_GIT: setting timestamp $CC_LAST_GIT_CHECK"
  fi
}
add-zsh-hook precmd precmd.cc_git_prompt_info

preexec.cc_maybe_clear_git_prompt_cache() {
  if [[ "$1" == git* || "$1" == cd* || "$1" == u || "$1" == uu* || "$1" == et* ]]; then
    unset CC_LAST_GIT_PROMPT
    unset CC_LAST_GIT_CHECK
  fi
}
add-zsh-hook preexec preexec.cc_maybe_clear_git_prompt_cache

abbrev_string() {
  if [ ${#1} -gt 0 ]; then
    local len=${#1}
    local width=${2:-35}
    if [ $len -gt $width ]; then
      echo "${1[0,7]}...${1[$#1-($width-10),$#1]}"
    else
      echo "$1"
    fi
  else
    echo ""
  fi
}

prompt_pwd() {
  local dir=${PWD/#\/root/\~}
  dir=${dir/#\/Users\/craigcitro/\~}
  local abbrev=$(abbrev_string "${dir}")
  echo "${abbrev}"
}

export PS1='%F{032}$(prompt_pwd)%f$CC_LAST_GIT_PROMPT%(?.. %F{red}exit:%?%f) %# %F{green}'
preexec.cc_reset_term_color() {
  echo -ne "\e[0m"
}
add-zsh-hook preexec preexec.cc_reset_term_color

##################################
# assorted tools

if (( $+commands[fzf] )); then
  source <(fzf --zsh)
  # maybe this one's insane?
  export FZF_TMUX=1
  export FZF_DEFAULT_OPTS="--bind 'ctrl-x:execute-silent(echo -n {..} | pbcopy)'"
fi

# Enable direnv (if available).
if (( $+commands[direnv] )); then
  eval "$(direnv hook zsh)"
fi

