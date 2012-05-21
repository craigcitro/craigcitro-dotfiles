#!/bin/bash
##
## .bashrc
## Craig Citro
##
## ==================================================
## CHANGES:
##
## [2010.06.15] Wow, I never update these comments.
## 
## [2007.12.19] Removing all the fink-related stuff, because
## I'm at least temporarily not using fink.
##
## [2006.02.08] Stole ryan's .bashrc, finally editing it. To
## be fair, it was once mine. :)
##
## [2005.09.24] Tried to unify the code in my .bashrc_laptop 
## and start switch my laptop over to this script.  
## Still have some path problems.
##
## [2005.09.29] I like the append_path and prepend_path
## functions defined in /sw/bin/init.sh.  They prevent duplicates.
## I'm going to use them myself.
##
## ==================================================

##################
## Basics

if [ $(uname -s) == "Linux" ]; then
  SYSTEM='Linux'
elif [ $(uname -s) == "Darwin" ]; then
  SYSTEM='Darwin'
else
  SYSTEM='unknown'
fi

# if [ -f /etc/bashrc ] ;  then (source /etc/bashrc); fi

# Stolen from: 
#   http://www.linuxfromscratch.org/blfs/view/svn/postlfs/profile.html
# Functions to help us manage paths.  Second argument is the name of the
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

pathprepend () {
  if [ -d $1 ]; then
    pathremove $1 $2
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="$1${!PATHVARIABLE:+:${!PATHVARIABLE}}"
  fi
}

pathappend () {
  if [ -d $1 ]; then
    pathremove $1 $2
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="${!PATHVARIABLE:+${!PATHVARIABLE}:}$1"
  fi
}

# BE VERY CAREFUL MUNGING YOUR PATH, YOUNG PADAWAN! Don't forget
# the nsscache snafu from your first week at Google.
if [ "$SYSTEM" == "Darwin" ]; then
  if [ "$LD_LIBRARY_PATH" = "" ]; then
    LD_LIBRARY_PATH="/usr/lib"
  fi
  
  pathappend /usr/local/lib LD_LIBRARY_PATH;
  # Why is this here? It at least causes some confusing problems with
  # loading libpng.dylib in sage, because sage stupidly copies the
  # LD_LIBRARY_PATH into DYLD_LIBRARY_PATH. I wonder why I put it here
  # in the first place, though?
  pathappend /usr/X11/lib LD_LIBRARY_PATH;

  pathprepend /usr/local/bin; 
  pathappend /opt/subversion/bin;
  # add git via git-osx-installer
  pathappend /usr/local/git/bin;
  pathappend /usr/local/git/share/man MANPATH;
  
  export LD_LIBRARY_PATH
  export MANPATH
fi

# My additions to $PATH
pathprepend $HOME/ext/bin;
pathprepend $HOME/bin; 
export PATH

pathprepend $HOME/ext/share/man MANPATH;

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

if [ -d "/Applications/Emacs.app/" ]; then
  # are there other paths I have to set up? 
  pathprepend "/Applications/Emacs.app/Contents/MacOS/bin";
  pathprepend "/Applications/Emacs.app/Contents/MacOS";
  export PATH;
fi

# TMUX + Emacs
# (2011 Jun 29) I've gotten slightly annoyed at having *one* emacs session; I'd like
# to experiment with just a few, tied to my tmux sessions.
# (2011 Sep 13) ... and a name for the non-tmux one.
# (2011 Sep 25) Even better -- done with this "many servers" nonsense. Just record
# the TMUX_SESSION for use in various places.
if [ ${#TMUX} -gt 0 ]; then
  export TMUX_SESSION="$(tmux display -p \#S)"
else
  export TMUX_SESSION='craigcitro'
fi
export EMACS_SERVERNAME='craigcitro'

# We want everything to route through one central set of emacs
# commands ...
alias emacsdaemon='$(which emacs) --daemon'
alias emacs="emacsclient -c -s ${EMACS_SERVERNAME}"
alias e='emacs'
alias et='emacs -t'
# Every editor I can find ...
export EDITOR="emacsclient -c -s ${EMACS_SERVERNAME} -t"
export VISUAL="emacsclient -c -s ${EMACS_SERVERNAME} -t"
export CVSEDITOR="emacsclient -c -s ${EMACS_SERVERNAME} -t"
export GIT_EDITOR="$(which emacsclient) -c -s ${EMACS_SERVERNAME} -t"
# (2011 Sep 13) Here's the old version:
# if [ "$(ps awx -U ${USER} | grep macs | grep daemon | grep ${EMACS_SERVERNAME})xxx" == "xxx" ]; then
# Amusingly, it's actually a bit more robust: a server can crash and
# fail to close the socket. However, the command below will correctly
# identify servers whose name wasn't specified at the command line ...
if [ ! -S $(emacsclient -s ${EMACS_SERVERNAME} --eval '(print server-socket-dir (get-buffer "*Messages*"))' 2>/dev/null | sed -e 's/^"//' -e 's/"$//')"/"${EMACS_SERVERNAME} ]; then
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

# this line is here in support of my .inputrc: I want
# to be able to use \C-s and \C-r for interactive history
# search, but I need to disable xterm's ^S = stop behavior
# to do so.
#
# It causes lots of warning messages whenever something that's *not* a
# shell sources my .bashrc; found a fix here:
#  http://www.perlmonks.org/?node_id=534691
if [ -t 0 ]; then
  stty stop ^^
fi

###############################
## other unix default stuff

# color TERM types.
# i'm still no expert on this stuff -- but honestly, is there really
# a point in *not* using a color term type in this day and age?
if [ "$TERM" = "xterm" ]; then
  export TERM=xterm-256color
fi
if [ "$TERM" = "screen" ]; then
  export TERM=screen-256color
fi

# set up less. 
#  - at some point, i included the '-e' option (which quits less
#    when you hit EOF the *second* time). it annoyed me, but maybe
#    i'll change my mind at some point?
export PAGER='less -qFRX'
alias less='less -qFRX'

# fix man paths
pathappend '/usr/local/share/man' MANPATH;
pathappend '/usr/X11/share/man' MANPATH;
pathappend '/Developer/usr/share/man' MANPATH;
pathappend '/usr/share/man' MANPATH;
pathappend '/usr/local/man' MANPATH;
export MANPATH

# Try and save myself from trouble
alias rm="rm -i"
alias cp="cp -i"
alias mv="mv -i"

#######################
## Play with shopt
#######################
# correct spelling in cd
shopt -s cdspell
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
# This would uniq the history ... which kinda sucks.
#HISTCONTROL=ignoredups:ignorespace
# really really large history
HISTFILESIZE=100000000
HISTSIZE=100000000

###############################
## tab completion!
COMPLETE_BASE='/etc/bash_completion'
if [ -e $COMPLETE_BASE ]; then
  source $COMPLETE_BASE
fi

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
alias cut="cut -d' '"
alias d=date
# Why isn't this the default?
alias df='df -h'
alias g='grep -n --color=always -E'
alias gg='grep --color=always -E'
alias h=history
alias ht='history | tail -n 20'
alias ny=nyxmms2
alias p=echo
alias scp='scp -p'
alias sigh='echo You let out a good, long sigh of relief.'
# So I'm going to try using emacs as my only editor, just
# for kicks. 
# alias v=vim
# alias vw=view
alias v=et
alias vw=et

# (2011 Jul 16) I like using the dir stack -- but it's awfully
# primitive.
alias pd=pushd
alias po=popd
alias di='dirs -v'

# It's better to just overwrite du, and then we can re-specify
# --max-depth if needed.
if [ "$SYSTEM" == "Darwin" ]; then
  alias du='du -h -d 1'
else
  alias du='du -h --max-depth 1'
fi

# ps is in the same boat as du. I always use the same options, so just
# make those the default.
if [ "$SYSTEM" == "Darwin" ]; then
  # wide by default on OSX
  alias ps="ps -U $USER"
  alias psw="ps -U $USER"
else
  alias ps="ps w -u $USER"
  alias psw="ps ww -u $USER"
fi

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
alias pu='pd ..'
alias puu='pd ../..'
alias puuu='pd ../../..'
alias puuuu='pd ../../../..'
alias puuuuu='pd ../../../../..'
alias puuuuuu='pd ../../../../../..'
alias puuuuuuu='pd ../../../../../../..'
alias puuuuuuuu='pd ../../../../../../../..'
alias puuuuuuuuu='pd ../../../../../../../../..'

# (2010 Oct 23) This isn't the perfect alias, but I type this *all*
# the time ...
alias lth='ll -t | head'

# i find i use this alot:
alias mr='cd $(/bin/ls -BF1t | grep / | head -1)'
alias mrd='/bin/ls -BF1t | grep / | head -1'

################################
# Local config
################################

################################
# python-related aliases
alias py='python2.6'
alias py2='python2.6'
alias py3='python3.1'

PYTHONSTARTUP=$HOME'/.pythonrc'
export PYTHONSTARTUP

#############
## Haskell

pathappend $HOME'/ext/cabal/bin';
pathappend $HOME'/ext/cabal/share/man' MANPATH;

################################################################
######################## Prompt stuff ##########################

# ANSI colors for displays
       BLACK_COLOR="\033[0;30m"
   DARK_GRAY_COLOR="\033[1;30m"
        BLUE_COLOR="\033[0;34m"
  LIGHT_BLUE_COLOR="\033[1;34m"
       GREEN_COLOR="\033[0;32m"
 LIGHT_GREEN_COLOR="\033[1;32m"
        CYAN_COLOR="\033[0;36m"
  LIGHT_CYAN_COLOR="\033[1;36m"
         RED_COLOR="\033[0;31m"
   LIGHT_RED_COLOR="\033[1;31m"
      PURPLE_COLOR="\033[0;35m"
LIGHT_PURPLE_COLOR="\033[1;35m"
       BROWN_COLOR="\033[0;33m"
      YELLOW_COLOR="\033[1;33m"
  LIGHT_GRAY_COLOR="\033[0;37m"
       WHITE_COLOR="\033[1;37m"

### ANSI prompt colors
#  note that the \[ and \] are very important -- if they aren't
#  present, bash will incorrectly count line length and have 
#  weird wrapping issues. (see
#    http://ubuntuforums.org/showthread.php?t=472369 )
#
       BLACK_PROMPT_COLOR="\[\033[0;30m\]"
   DARK_GRAY_PROMPT_COLOR="\[\033[1;30m\]"
        BLUE_PROMPT_COLOR="\[\033[0;34m\]"
  LIGHT_BLUE_PROMPT_COLOR="\[\033[1;34m\]"
       GREEN_PROMPT_COLOR="\[\033[0;32m\]"
 LIGHT_GREEN_PROMPT_COLOR="\[\033[1;32m\]"
        CYAN_PROMPT_COLOR="\[\033[0;36m\]"
  LIGHT_CYAN_PROMPT_COLOR="\[\033[1;36m\]"
         RED_PROMPT_COLOR="\[\033[0;31m\]"
   LIGHT_RED_PROMPT_COLOR="\[\033[1;31m\]"
      PURPLE_PROMPT_COLOR="\[\033[0;35m\]"
LIGHT_PURPLE_PROMPT_COLOR="\[\033[1;35m\]"
       BROWN_PROMPT_COLOR="\[\033[0;33m\]"
      YELLOW_PROMPT_COLOR="\[\033[1;33m\]"
  LIGHT_GRAY_PROMPT_COLOR="\[\033[0;37m\]"
       WHITE_PROMPT_COLOR="\[\033[1;37m\]"
#############################################################

case $HOSTNAME in
craigcitro-macbookpro.local)
  BRACKET_COLOR="$PURPLE_PROMPT_COLOR"
  PROMPT_TEXT="\h \$(prompt_pwd)"
  PROMPT_TEXT_COLOR="$RED_PROMPT_COLOR"
  PROMPT_DOLLAR_COLOR="$CYAN_PROMPT_COLOR"
  ;;
*)
  BRACKET_COLOR="$RED_PROMPT_COLOR"
  PROMPT_TEXT="\h \$(prompt_pwd)"  
  PROMPT_TEXT_COLOR="$PURPLE_PROMPT_COLOR"
  PROMPT_DOLLAR_COLOR="$GREEN_PROMPT_COLOR"
  ;;
esac
PROMPT_GIT_COLOR="$GREEN_PROMPT_COLOR"
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
} #
export -f gh

function export_git_info() {
  if [ "$PWD" = ~ -o "$PWD" = "/home" ]; then
    export CC_GIT_BRANCH=""
    export CC_GIT_ROOT=""
    return
  fi
  if [ "${PWD##~}" = "$PWD" -a "${PWD##/home}" != "$PWD" ]; then
    return
  fi
  local b="$(git symbolic-ref HEAD 2>/dev/null)"
  if [ ${#b} -gt 0 ]; then
    export CC_GIT_BRANCH="${b##refs/heads/}"
    export CC_GIT_ROOT="$(git rev-parse --show-toplevel)"
    return
  fi
}
export -f export_git_info

function git_prompt_info () {
  if [ ${#CC_GIT_BRANCH} -gt 0 ]; then
    echo " {${CC_GIT_BRANCH}}"
  fi
}
export -f git_prompt_info

function show_last_cmd () {
  local cmd=$(history 1 | awk "length() < 5000 {print}")
  if [ ! -z "$cmd" -a "$(id -u)" -ne 0 ]; then
    echo $(date) $(pwd|tr " " "_") $(history 1)
  fi
}
export -f show_last_cmd

function abbrev_string () {
  if [ ${#1} -gt 0 ]; then
    local len=${#1}
    local width=${2:-35}
    if [ $len -gt $width ]; then
      echo "..."${1:len-($width-3)}
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

###################################################
## Unused config
###################################################

# Michael Sheldon asked me this: how can you make ^D *not*
# kill your shell, but still get passed through as EOF for
# any other program? Here's the answer:
# export IGNOREEOF=1
# ... which apparently doesn't completely work.

####################################################
# Google config
####################################################

function at_work () {
  local len=${#HOSTNAME}
  if [ ${HOSTNAME:len-10:len} = "google.com" -a ${HOSTNAME:0:10} = "craigcitro" ]; then
    echo ${HOSTNAME}
  fi
}
export -f at_work
if [ -n "$(at_work)" -a -e $HOME"/.bashrc.google" ]; then
  source $HOME"/.bashrc.google"
fi

####################################################
# Final config
#
# Last bits of config -- in particular, anything
# that has to run after Google-specific config.
####################################################

###############
## Prompt

PROMPT_COMMAND="save_last_exit ; export_git_info ; show_last_cmd >>$CC_SHELL_LOG ; $PROMPT_COMMAND"

export COLOR_PS1="$BRACKET_COLOR[$PROMPT_TEXT_COLOR$PROMPT_TEXT$BRACKET_COLOR]$PROMPT_GIT_COLOR\$(git_prompt_info)$RED_PROMPT_COLOR\$(exit_status) $PROMPT_DOLLAR_COLOR\\$ $NORMAL_TEXT_COLOR"
export EMACS_PS1="$COLOR_PS1"
export MONOPS1="[\h \$(prompt_pwd)] \\$ "

# TODO(craigcitro): Make the prompt include \h if it's not a known host.

alias mono='export PS1=$MONOPS1'   # Means black [and white] mono color
alias color='export PS1=$COLOR_PS1' # Intended for color on black backgrounds
export PS1=$COLOR_PS1 # Intended for color on black backgrounds

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
    LIGHT_PURPLE_PROMPT_COLOR BROWN_PROMPT_COLOR \
    YELLOW_PROMPT_COLOR LIGHT_GRAY_PROMPT_COLOR WHITE_PROMPT_COLOR 


# tmux-related
# Currently, tmux uses chdir(2) on creating a new shell, so can't do a cd without
# inheriting the "feature" of following symlinks. So here's my workaround: have tmux
# drop a shell var, then let the shell cd (and hence preserve paths). See:
#  http://fixunix.com/questions/15902-bash-checking-if-env-var-set.html
if [ ${#TMUX_DEFAULT_DIR} -gt 0 -a ! ${#CC_BASHRC_INITIALIZED} -gt 0 ]; then
  cd $TMUX_DEFAULT_DIR
fi
export CC_BASHRC_INITIALIZED="true"

