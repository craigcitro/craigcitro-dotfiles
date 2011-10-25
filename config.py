#!/usr/bin/env python
#
# config.py
#
# Basic script to do some machine config
#

import getopt
import os
import platform
import subprocess
import sys

HOMEDIR = os.path.expanduser('~')

BASIC_LINKS = [
    'Xdefaults',
    'bash_profile',
    'bashrc',
    'emacs',
    'emacs.d',
    'ghci',
    'gitconfig',
    'gitignore',
    'hgrc',
    'inputrc',
    'pdbrc',
    'pdbrc.py',
    'pythonrc',
    'screenrc',
    'tmux.conf',
    ]
MAC_LINKS = [
    'cmd-key-happy.lua',
    ]
XWINDOWS_LINKS = [
    'ratpoisonrc',
    'ratpoisonrc.ugly',
    'xmodmap-ctrl',
    'xsession',
    'xsettingsd',
    ]

SCRIPTS = [
    (['./create_vimrc.py']),
    (['mv', '.vimrc', HOMEDIR]),
    ]


def _SetupLinks(links_to_create):
    print
    print "Creating symbolic links ..."
    print
    repodir = os.getcwd()
    os.chdir(HOMEDIR)
    for f in links_to_create:
        filename = os.path.join(repodir, f)
        linkname = '.' + f
        print "--- Symlinking %s to %s" % (filename, linkname)
        if os.path.exists(linkname):
            print 'File %s already exists, skipping.' % (linkname,)
            continue
        elif os.path.lexists(linkname):
            print '    >> Removing broken symlink %s <<' % (linkname,)
            os.remove(linkname)
        try:
            subprocess.check_call(['ln', '-s', filename, linkname])
        except subprocess.CalledProcessError, e:
            print "ERROR creating symbolic link: " + str(e)
            break
    os.chdir(repodir)
    return


def _RunScripts(scripts):
    print
    print "Running scripts ..."
    print
    for s in scripts:
        print "=== Running: %s " % (' '.join(s),)
        try:
            subprocess.check_call(s)
        except subprocess.CalledProcessError, e:
            print "ERROR running script: " + str(e)
            break


if __name__ == '__main__':
    try:
        opts, args = getopt.getopt(sys.argv[1:], 'x', ['with-x'])
        opts = [x[0] for x in opts]
    except getopt.GetoptError, e:
        print "Error parsing arguments: " + str(e)
        exit(1)
    setup_x = False
    if ('-x' in opts) or ('--with-x' in opts):
        setup_x = True

    links_to_create = BASIC_LINKS
    if platform.system() == 'Darwin':
        links_to_create += MAC_LINKS
    elif platform.system() == 'Linux':
        if setup_x:
            links_to_create += XWINDOWS_LINKS

    _SetupLinks(links_to_create)
    _RunScripts(SCRIPTS)

    
