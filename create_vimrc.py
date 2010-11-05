#!/usr/bin/env python
#
# create_vimrc.py
# craigcitro
# 
# Given a stub vimrc.local file, this will:
#  * find the system-wide vimrc_example.vim
#  * append vimrc.local
#

import commands
import os

vimrc_header = '''
" 
" craigcitro's .vimrc file
" 
'''

vimrc_whining = '''
" I find it annoying that there's no "standard" way
" to source the system-wide vimrc example, but this
" does the trick.
if (exists('%s'))
  so %s
endif
'''

if __name__ == '__main__':
    #vim_version = subprocess.Popen('vim --version | head -n 1 | awk \'{ print $5; }\' | sed -e \'s/\.//\'', shell=True, stdout=subprocess.PIPE).stdout.read().rstrip()
    vim_version_long = commands.getoutput('vim --version')
    vim_version = ''.join(vim_version_long[:30].split()[4].split('.'))
    if vim_version:
        print "Found vim version %s"%vim_version 
    else:
        print "Failed to calculate vim_version! Aborting."
        exit(1)

    vimrc_filename = '/usr/share/vim/vim%s/vimrc_example.vim'%vim_version
    if os.path.exists(vimrc_filename):
        print "Using example file %s"%vimrc_filename
    else:
        print "Could not find system-wide vimrc! Aborting."
        exit(1)

    vimrc_local = 'vimrc.local'
    if os.path.exists(vimrc_local):
        print "Using local config %s"%vimrc_local
    else:
        print "Could not find local vimrc! Aborting."
        exit(1)

    if os.path.exists('.vimrc'):
        print ".vimrc already exists! Aborting."
        exit(1)
    try:
        out = open('.vimrc', 'w')
    except IOError:
        print "Error opening .vimrc for writing! Aborting."
        exit(1)

    print >>out, vimrc_header
    print >>out, "\" begin system-wide config"
    print >>out, vimrc_whining%(vimrc_filename, vimrc_filename)
    print >>out, "\" begin local config"
    for line in open(vimrc_local, "r"):
        print >>out, line.strip()
    print >>out, ""
    out.close()

# This was the start of some code to avoid setting and 
# re-setting variables ... but who cares.
#    vl_lines = open(vimrc_local, "r").readlines()
#    vl_vars_set = [ line.split()[1] for line in vl_lines 
#                    if line.strip().startswith("set ") ]
#
#    for line in open(vimrc_filename):
#        line = line.rstrip()
#        if not line:
#            print >>out, ""
#            continue
#        command_only = line.strip()
#        if line.startswith("set "):
#            var = line.split()[1]
#            if var in vl_vars_set:
#                continue
        
    
