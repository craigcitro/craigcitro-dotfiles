#!/usr/bin/env python
"""Basic script to do some machine config."""

import contextlib
import os
import subprocess

BASIC_LINKS = [
    # General
    'Xdefaults', 'bash_profile', 'bashrc', 'gitconfig', 'gitignore', 'hgrc',
    'inputrc', 'screenrc', 'tmux.conf',
    # Editors
    'emacs', 'emacs.d', 'vimrc',
    # Languages
    'Rprofile', 'ghci', 'pdbrc', 'pdbrc.py', 'pythonrc',
    ]


@contextlib.contextmanager
def Chdir(path):
  current_dir = os.getcwd()
  print ' +++ Changing directory to %s +++' % path
  os.chdir(path)
  yield
  print ' --- Returning to directory to %s ---' % current_dir
  os.chdir(current_dir)


def _CreateLink(filename, linkname):
  """Link filename to linkname, returning True on success."""
  print "== Symlinking %s to %s" % (filename, linkname)
  if os.path.exists(linkname):
    print 'File %s already exists, skipping.' % (linkname,)
    return True
  if os.path.lexists(linkname):
    print '    >> Removing broken symlink %s <<' % (linkname,)
    os.remove(linkname)
  try:
    subprocess.check_call(['ln', '-s', filename, linkname])
  except subprocess.CalledProcessError, e:
    print "ERROR creating symbolic link: " + str(e)
    return False
  return True


def _SetupLinks(links_to_create):
  print
  print "Creating symbolic links ..."
  print
  repodir = os.getcwd()
  homedir = os.path.expanduser('~')
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


if __name__ == '__main__':
  _SetupLinks(BASIC_LINKS)
  print "Don't forget to link something to .tmux.colors.conf."
