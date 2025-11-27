#!/usr/bin/env python3
"""Basic script to do some machine config."""

import contextlib
import os
import socket
import subprocess

BASIC_LINKS = [
    # General
    'Xdefaults',
    'bash_profile',
    'bashrc',
    'colordiffrc',
    'gitconfig',
    'gitignore',
    'hgrc',
    'inputrc',
    'screenrc',
    'tmux.conf',
    'zshrc',
    # Editors
    'emacs',
    'emacs.d',
    'vimrc',
    # Languages
    'pdbrc',
    'pythonrc',
    'Rprofile',
    'ghci',
]


@contextlib.contextmanager
def Chdir(path):
    current_dir = os.getcwd()
    print(f' +++ Changing directory to {path} +++')
    os.chdir(path)
    yield
    print(f' --- Returning to directory to {current_dir} ---')
    print()
    os.chdir(current_dir)


def _CreateLink(filename, linkname, indent='    '):
    """Link filename to linkname, returning True on success."""
    print(f'{indent}== Symlinking {filename} to {linkname}')
    if os.path.exists(linkname):
        print(f'{indent}File {linkname} already exists, skipping.')
        return True
    if os.path.lexists(linkname):
        print(f'{indent}>> Removing broken symlink {linkname} <<')
        os.remove(linkname)
    try:
        subprocess.check_call(['ln', '-s', filename, linkname])
    except subprocess.CalledProcessError as e:
        print(f'{indent}ERROR creating symbolic link: {e}')
        return False
    return True


def _SetupLinks(links_to_create):
    print()
    print('Creating symbolic links ...')
    print()
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
