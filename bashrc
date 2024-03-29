## .bashrc

##################
## Basics

if [ $(uname -s) == "Linux" ]; then
  SYSTEM='Linux'
elif [ $(uname -s) == "Darwin" ]; then
  SYSTEM='Darwin'
else
  SYSTEM='unknown'
fi

# Stolen from:
#   http://www.linuxfromscratch.org/blfs/view/svn/postlfs/profile.html
# Functions to help us manage paths. Second argument is the name of the
# path variable to be modified (default: PATH)
pathremove () {
  local IFS=':'
  local NEWPATH
  local DIR
  local PATHVARIABLE=${2:-PATH}
  for DIR in ${!PATHVARIABLE} ; do
    if [ "$DIR" != "$1" ] ; then
      NEWPATH=${NEWPATH:+$NEWPATH:}$DIR
    fi
  done
  export $PATHVARIABLE="$NEWPATH"
}
export -f pathremove

pathprepend () {
  if [ -d $1 ]; then
    pathremove $1 $2
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="$1${!PATHVARIABLE:+:${!PATHVARIABLE}}"
  fi
}
export -f pathprepend

pathappend () {
  if [ -d $1 ]; then
    pathremove $1 $2
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="${!PATHVARIABLE:+${!PATHVARIABLE}:}$1"
  fi
}
export -f pathappend

maybesrc () {
  if [ -e $1 ]; then
    if [ ! -z "$CC_BASHRC_LOGGING" ]; then
      echo "Sourcing $1"
    fi
    source $1;
  fi
}
export -f maybesrc

# Path munging
# lean
pathappend $HOME/.elan/bin;
# For pip install --user:
pathprepend $HOME/.local/bin;

# Mac-specific
if [ "$SYSTEM" == "Darwin" ]; then
  if [ "$LD_LIBRARY_PATH" = "" ]; then
    LD_LIBRARY_PATH="/usr/lib"
  fi

  pathappend /usr/local/lib LD_LIBRARY_PATH;
  pathappend /usr/X11/lib LD_LIBRARY_PATH;

  pathprepend /usr/local/bin;
  pathappend /usr/texbin;
  # add git via git-osx-installer
  pathappend /usr/local/git/bin;
  pathappend /usr/local/git/share/man MANPATH;
  pathappend /usr/local/git/current/share/man MANPATH;

  # There's a chicken-and-egg problem here: we can't use `brew --prefix`
  # until `brew` is on our path.
  pathprepend /opt/homebrew/bin;
  if [[ $(command -v brew) ]]; then
    BREWPREFIX=$(brew --prefix)
    pathappend ${BREWPREFIX}/include CPATH;
    pathappend ${BREWPREFIX}/lib LD_LIBRARY_PATH;
    pathappend ${BREWPREFIX}/lib LIBRARY_PATH;
    pathprepend ${BREWPREFIX}/bin;
    pathappend ${BREWPREFIX}/sbin;
  else
    echo 'Consider installing homebrew.'
  fi

  export CPATH
  export LD_LIBRARY_PATH
  export LIBRARY_PATH
  export MANPATH
  export PATH
fi

# My additions to $PATH
pathprepend $HOME/ext/bin;
pathprepend $HOME/bin;
pathprepend $HOME/ext/share/man MANPATH;
export PATH
export MANPATH

# As Ami says, "Mmmm ... core files are so yummy!"
ulimit -c unlimited

###############################
## ls configuration

if [ "$SYSTEM" == "Darwin" ]; then
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
if [ ! -S $(emacsclient -s ${EMACS_SERVERNAME} --eval '(print server-socket-dir (lambda (s) nil))' 2>/dev/null | awk -F\" '{print $2}')"/"${EMACS_SERVERNAME} ]; then
  LOCKFILE="${HOME}/.lock-emacs-${EMACS_SERVERNAME}"
  if [ -f "${LOCKFILE}" ]; then
    echo "Emacs is currently starting up, skipping ..."
  else
    echo "Starting Emacs daemon with servername ${EMACS_SERVERNAME} ..."
    touch ${LOCKFILE}
    $(which emacs) --daemon="${EMACS_SERVERNAME}"
    rm -f "${LOCKFILE}"
  fi
fi

# This line is here in support of my .inputrc: I want to be able to use \C-s
# and \C-r for interactive history search, but I need to disable xterm's ^S =
# stop behavior to do so.
#
# It causes lots of warning messages whenever something that's *not* a shell
# sources my .bashrc; found a fix here:
#   http://www.perlmonks.org/?node_id=534691
if [ -t 0 ]; then
  stty stop ^^
fi

###############################
## other unix default stuff

if [ "$TERM" = "xterm" ]; then
  export TERM=xterm-256color
fi
if [ "$TERM" = "screen" ]; then
  export TERM=screen-256color
fi

export PAGER='less -qFRX'
alias less='less -qFRX'

# fix man paths
pathappend '/usr/local/share/man' MANPATH;
pathappend '/usr/X11/share/man' MANPATH;
pathappend '/Developer/usr/share/man' MANPATH;
pathappend '/usr/share/man' MANPATH;
pathappend '/usr/local/man' MANPATH;
export MANPATH

#######################
## Play with shopt
#######################
# correct spelling in cd
# shopt -s cdspell
# keep LINES and COLUMNS up to date
shopt -s checkwinsize
# include dot names in globs
shopt -s dotglob
# better globbing
shopt -s extglob
# no completing the empty line
shopt -s no_empty_cmd_completion
# don't overwrite history
shopt -s histappend
# really really large history
HISTFILESIZE=100000000
HISTSIZE=100000000
unset HISTTIMEFORMAT
alias gh='cat ${HOME}/.shell.log | grep'

###############################
## tab completion!
maybesrc '/etc/bash_completion'
[[ $(command -v brew) ]] && maybesrc '/opt/homebrew/share/bash-completion/bash_completion'
maybesrc '/usr/local/share/bash-completion/bash_completion'
maybesrc '/usr/share/git-core/git-completion.bash'
maybesrc '/usr/local/git/current/share/git-core/git-completion.bash'
maybesrc "${HOME}/ext/etc/profile.d/bash_completion.sh"
maybesrc '/usr/share/doc/tmux/examples/bash_completion_tmux.sh'

# I found this clever trick here:
#   http://www.macosxhints.com/article.php?story=20080317085050719
# It autocompletes from .ssh/known_hosts, which is pretty useful ...
#
# (2011 Aug 16) This was over-complicated; don't know if there's any
# need to be that cautious. Original version:
#   sed -e 's/^  *//' -e '/^#/d' -e 's/[, ].*//' -e '/\[/d' ~/.ssh/known_hosts
if [ "$SYSTEM" == "Darwin" ]; then
  complete -W "$(echo $(sed -e 's/[, ].*//' ~/.ssh/known_hosts | sort -u);)" ssh
  complete -W "$(echo $(sed -e 's/[, ].*//' ~/.ssh/known_hosts | sort -u);)" -f -d scp
fi

################################
# Common commands
alias c=clear
alias d=date
alias df='df -h'
alias scp='scp -p'
alias du='du -h -d 1'

# Maybe there's a better name for this?
alias reup="source $HOME/.bashrc"

# Silly commands for avoiding 'cd ../..': there's
# probably a better way, but this is a start.
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

# (2010 Oct 23) This isn't the perfect alias, but I type this *all*
# the time ...
alias lth='ll -t | head'

################################
# Local config
################################

################################
# R
export R_HISTSIZE=100000
export R_HISTFILE=$HOME'/.Rhistory'
export R_PROFILE_USER=$HOME'/.Rprofile'
alias R='R --no-save'

################################
# python-related aliases
export PYTHONSTARTUP=$HOME'/.pythonrc'

#############
## Go

GOPATH=$HOME'/ext/go'
export GOPATH
pathappend "${GOPATH}/bin"

################################################################
######################## Prompt stuff ##########################

# ANSI colors for displays
       BLACK_COLOR="\e[0;30m"
   DARK_GRAY_COLOR="\e[1;30m"
        BLUE_COLOR="\e[0;34m"
  LIGHT_BLUE_COLOR="\e[1;34m"
       GREEN_COLOR="\e[0;32m"
 LIGHT_GREEN_COLOR="\e[1;32m"
        CYAN_COLOR="\e[0;36m"
  LIGHT_CYAN_COLOR="\e[1;36m"
         RED_COLOR="\e[0;31m"
   LIGHT_RED_COLOR="\e[1;31m"
      PURPLE_COLOR="\e[0;35m"
LIGHT_PURPLE_COLOR="\e[1;35m"
       BROWN_COLOR="\e[0;33m"
      YELLOW_COLOR="\e[1;33m"
  LIGHT_GRAY_COLOR="\e[0;37m"
       WHITE_COLOR="\e[1;37m"
         END_COLOR="\e[0m"

### ANSI prompt colors
#  note that the \[ and \] are very important -- if they aren't
#  present, bash will incorrectly count line length and have
#  weird wrapping issues. (see
#    http://ubuntuforums.org/showthread.php?t=472369 )
#
       BLACK_PROMPT_COLOR="\[\e[0;30m\]"
   DARK_GRAY_PROMPT_COLOR="\[\e[1;30m\]"
        BLUE_PROMPT_COLOR="\[\e[0;34m\]"
  LIGHT_BLUE_PROMPT_COLOR="\[\e[1;34m\]"
       GREEN_PROMPT_COLOR="\[\e[0;32m\]"
 LIGHT_GREEN_PROMPT_COLOR="\[\e[1;32m\]"
        CYAN_PROMPT_COLOR="\[\e[0;36m\]"
  LIGHT_CYAN_PROMPT_COLOR="\[\e[1;36m\]"
         RED_PROMPT_COLOR="\[\e[0;31m\]"
   LIGHT_RED_PROMPT_COLOR="\[\e[1;31m\]"
      PURPLE_PROMPT_COLOR="\[\e[0;35m\]"
LIGHT_PURPLE_PROMPT_COLOR="\[\e[1;35m\]"
       BROWN_PROMPT_COLOR="\[\e[0;33m\]"
      YELLOW_PROMPT_COLOR="\[\e[1;33m\]"
  LIGHT_GRAY_PROMPT_COLOR="\[\e[0;37m\]"
       WHITE_PROMPT_COLOR="\[\e[1;37m\]"
     IPURPLE_PROMPT_COLOR="\[\e[0;95m\]"
#############################################################

case $HOSTNAME in
penguin|themini.*|cc7.lan)
  BRACKET_COLOR="$BLUE_PROMPT_COLOR"
  PROMPT_TEXT="\$(prompt_pwd)"
  PROMPT_TEXT_COLOR="$RED_PROMPT_COLOR"
  PROMPT_DOLLAR_COLOR="$CYAN_PROMPT_COLOR"
  ;;
teeny2*)
  BRACKET_COLOR="$CYAN_PROMPT_COLOR"
  PROMPT_TEXT="\$(prompt_pwd)"
  PROMPT_TEXT_COLOR="$RED_PROMPT_COLOR"
  PROMPT_DOLLAR_COLOR="$CYAN_PROMPT_COLOR"
  ;;
craigcitro-macbookpro**)
  BRACKET_COLOR="$RED_PROMPT_COLOR"
  PROMPT_TEXT="\$(prompt_pwd)"
  PROMPT_TEXT_COLOR="$PURPLE_PROMPT_COLOR"
  PROMPT_DOLLAR_COLOR="$CYAN_PROMPT_COLOR"
  ;;
*)
  BRACKET_COLOR="$RED_PROMPT_COLOR"
  PROMPT_TEXT="\h \$(prompt_pwd)"
  PROMPT_TEXT_COLOR="$PURPLE_PROMPT_COLOR"
  PROMPT_DOLLAR_COLOR="$GREEN_PROMPT_COLOR"
  ;;
esac
NORMAL_TEXT_COLOR="$LIGHT_GRAY_PROMPT_COLOR"

#################################
## History
##
## Changes the prompt and saves history information somewhere other
## than just the HISTFILE; this whole section was "deeply inspired" by
## Ami's .bashrc, i.e. I stole it and modified it.
export CC_SHELL_LOG="${HOME}/.shell.log"
function gh() {
  grep -a "$@" $CC_SHELL_LOG | cut -d' ' -f9- | sed -e 's/^ *//' | cut -c-2000;
}
export -f gh

function git_prompt_info () {
  local b="$(gitinfo)"
  if [[ -n ${b} ]]; then
    echo " ${b}"
  fi
}
export -f git_prompt_info

function show_last_cmd () {
  local cmd=$(history 1 | awk "length() < 5000 {print}")
  if [ ! -z "$cmd" -a "$(id -u)" -ne 0 ]; then
    echo $(date -R) $(pwd|tr " " "_") $(history 1)
  fi
}
export -f show_last_cmd

function abbrev_string () {
  if [ ${#1} -gt 0 ]; then
    local len=${#1}
    local width=${2:-35}
    if [ $len -gt $width ]; then
      echo "${1:0:7}...${1:len-($width-10)}"
    else
      echo "$1"
    fi
  else
    echo ""
  fi
}
export -f abbrev_string

function prompt_pwd () {
  local dir=${PWD/#\/home\/craigcitro/\~}
  dir=${dir/#\/Users\/craigcitro/\~}
  local abbrev=$(abbrev_string "${dir}")
  echo "${abbrev}"
}
export -f prompt_pwd

function restore_bash_history () {
  # Can't use history -r - or history -r <(cut...) because history -r
  # is not stream-capable.
  history -c
  TMPHIST=$(mktemp -t cc.shell.log.XXXXXX) #
  cut -f9- -d' ' $CC_SHELL_LOG | tail -n 40000 > $TMPHIST
  history -r $TMPHIST
  rm -f $TMPHIST
}
export -f restore_bash_history

function restore_full_bash_history () {
  # Can't use history -r - or history -r <(cut...) because history -r
  # is not stream-capable.
  history -c
  TMPHIST=$(mktemp -t cc.shell.log.XXXXXX) #
  cut -f9- -d' ' $CC_SHELL_LOG > $TMPHIST
  history -r $TMPHIST
  rm -f $TMPHIST
}
export -f restore_bash_history

function save_last_exit() {
  LAST_STATUS=(${PIPESTATUS[@]});
}
export -f save_last_exit
function exit_status() {
  (( ${LAST_STATUS[@]/#/+} > 0 )) && (IFS=,; echo " exit:${LAST_STATUS[*]}");
}
export -f exit_status

export PROMPT_COMMAND=""

####################################################
# Google config
####################################################

maybesrc $HOME"/.bashrc.google"
maybesrc $HOME"/.bashrc.extra"

####################################################
# Final config
#
# Last bits of config -- in particular, anything
# that has to run after Google-specific config.
####################################################

###############
## Prompt

PROMPT_COMMAND="save_last_exit ; show_last_cmd >>$CC_SHELL_LOG ; $PROMPT_COMMAND"

export COLOR_PS1="$BRACKET_COLOR[$PROMPT_TEXT_COLOR$PROMPT_TEXT$BRACKET_COLOR]$GREEN_PROMPT_COLOR\$(git_prompt_info)$RED_PROMPT_COLOR\$(exit_status) $PROMPT_DOLLAR_COLOR\\$ $NORMAL_TEXT_COLOR"
export EMACS_PS1="$COLOR_PS1"
export MONOPS1="[\h \$(prompt_pwd)] \\$ "

# TODO(craigcitro): Make the prompt include \h if it's not a known host.

alias mono='export PS1=$MONOPS1'   # Means black [and white] mono color
alias color='export PS1=$COLOR_PS1' # Intended for color on black backgrounds
export PS1=$COLOR_PS1 # Intended for color on black backgrounds

# Colorize what I type.
export PS1="${PS1}\[\e[1;32m\]"
trap 'echo -ne "\e[0m"' DEBUG

unset BLACK_COLOR DARK_GRAY_COLOR BLUE_COLOR \
    LIGHT_BLUE_COLOR GREEN_COLOR LIGHT_GREEN_COLOR \
    CYAN_COLOR LIGHT_CYAN_COLOR RED_COLOR \
    LIGHT_RED_COLOR PURPLE_COLOR LIGHT_PURPLE_COLOR \
    BROWN_COLOR YELLOW_COLOR LIGHT_GRAY_COLOR WHITE_COLOR \
    BLACK_PROMPT_COLOR DARK_GRAY_PROMPT_COLOR BLUE_PROMPT_COLOR \
    LIGHT_BLUE_PROMPT_COLOR GREEN_PROMPT_COLOR \
    LIGHT_GREEN_PROMPT_COLOR CYAN_PROMPT_COLOR \
    LIGHT_CYAN_PROMPT_COLOR RED_PROMPT_COLOR \
    LIGHT_RED_PROMPT_COLOR PURPLE_PROMPT_COLOR \
    IPURPLE_PROMPT_COLOR \
    LIGHT_PURPLE_PROMPT_COLOR BROWN_PROMPT_COLOR \
    YELLOW_PROMPT_COLOR LIGHT_GRAY_PROMPT_COLOR WHITE_PROMPT_COLOR

if [[ "${SYSTEM}" == "Linux" ]]; then
  # Include start & end times. Inspired by https://stackoverflow.com/a/58140726/8755609.
  export BASHTIME="$HOME/tmp/bashtime/${USER}"
  mkdir -p "$BASHTIME"
  export PROMPT_START_TIME_FILE="${BASHTIME}/bash.${BASHPID}"
  function log_current_time() {
      # date -Ins |tr ',\012' '. '
      date +%s%3N > "$PROMPT_START_TIME_FILE"
  }
  function maybe_print_execution_time_line() {
      [[ -f "$PROMPT_START_TIME_FILE" ]] || return
      # date -Ins |tr ',\012' '. '
      read start < "$PROMPT_START_TIME_FILE"
      rm -f "$PROMPT_START_TIME_FILE"
      DELTA=$(( ($(date +%s%3N) - $start) ))
      HOURS=$(( DELTA / 3600000 )); DELTA=$(( DELTA % 3600000 ))
      MINUTES=$(( DELTA / 60000 )); DELTA=$(( DELTA % 60000 ))
      SECONDS=$(( DELTA / 1000 )); MILLIS=$(( DELTA % 1000 ))
      FORMATTED=""
      (( $HOURS > 0 )) && FORMATTED+="${HOURS}h"
      (( $MINUTES > 0 )) && FORMATTED+="${MINUTES}m"
      (( $SECONDS > 0 )) && FORMATTED+="${SECONDS}s"
      (( $MILLIS > 0 )) && FORMATTED+="${MILLIS}ms"
      echo "$(date -Ins |tr ',\012' '.') Execution time: ${FORMATTED}"
  }
  if [[ "${CC_BASHRC_INITIALIZED}" != "true" ]]; then
    export PS0="$PS0\$(log_current_time)"
    export PS1="\[\e[1;30m\]\$(maybe_print_execution_time_line)\[\e[0m\]\n${PS1}"
  fi
fi

# Done!
export CC_BASHRC_INITIALIZED="true"
