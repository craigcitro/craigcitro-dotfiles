#!/usr/bin/env python
"""Basic script to do some machine config."""

import contextlib
import os
import socket
import subprocess

BASIC_LINKS = [
    # General
    'Xdefaults', 'bash_profile', 'bashrc', 'colordiffrc', 'gitconfig',
    'gitignore', 'hgrc', 'inputrc', 'screenrc', 'tmux.conf',
    # Editors
    'emacs', 'emacs.d', 'vimrc',
    # Languages
    'pdbrc', 'pdbrc.py', 'pythonrc', 'pylintrc',
    'Rprofile', 'ghci',
    ]


@contextlib.contextmanager
def Chdir(path):
  current_dir = os.getcwd()
  print ' +++ Changing directory to %s +++' % path
  os.chdir(path)
  yield
  print ' --- Returning to directory to %s ---' % current_dir
  print
  os.chdir(current_dir)


def _CreateLink(filename, linkname, indent='    '):
  """Link filename to linkname, returning True on success."""
  print '%s== Symlinking %s to %s' % (indent, filename, linkname)
  if os.path.exists(linkname):
    print '%sFile %s already exists, skipping.' % (indent, linkname)
    return True
  if os.path.lexists(linkname):
    print '%s>> Removing broken symlink %s <<' % (indent, linkname)
    os.remove(linkname)
  try:
    subprocess.check_call(['ln', '-s', filename, linkname])
  except subprocess.CalledProcessError, e:
    print '%sERROR creating symbolic link: %s' % (indent, e)
    return False
  return True


def _SetupLinks(links_to_create):
  print
  print 'Creating symbolic links ...'
  print
  repodir = os.getcwd()
  homedir = os.path.expanduser('~')
  # Link the dotfiles
  with Chdir(homedir):
    for f in links_to_create:
      filename = os.path.join(repodir, f)
      linkname = '.' + f
      if not _CreateLink(filename, linkname):
        break
  # Now the binaries
  binary_dest_dir = os.path.join(homedir, 'bin')
  if not os.path.exists(binary_dest_dir):
    os.mkdir(binary_dest_dir)
  with Chdir(binary_dest_dir):
    binary_src_dir = os.path.join(repodir, 'bin')
    for binary in os.listdir(binary_src_dir):
      filename = os.path.join(binary_src_dir, binary)
      if not _CreateLink(filename, binary):
        break
  # Don't forget a tmux colors file
  if 'google.com' in socket.gethostname():
    tmux_colors = 'tmux.green.conf'
  else:
    tmux_colors = 'tmux.blue.conf'
  tmux_colors = os.path.join(repodir, tmux_colors)
  with Chdir(homedir):
    _CreateLink(tmux_colors, '.tmux.colors.conf')


if __name__ == '__main__':
  _SetupLinks(BASIC_LINKS)
