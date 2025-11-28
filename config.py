#!/usr/bin/env python3
"""Basic script to do some machine config."""

import contextlib
import os
import platform
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

BREW_PACKAGES = [
    'bat',
    'colordiff',
    'direnv',
    'emacs',
    'fzf',
    'pstree',
    'python3',
    'ripgrep',
    'tmux',
]


def _setup_packages():
    if platform.system() == 'Darwin':
        subprocess.run(
            ['brew', 'install'] + BREW_PACKAGES,
            check=True,
            text=True,
        )

@contextlib.contextmanager
def chdir(path):
    current_dir = os.getcwd()
    print(f' +++ Changing directory to {path} +++')
    os.chdir(path)
    yield
    print(f' --- Returning to directory to {current_dir} ---')
    print()
    os.chdir(current_dir)


def _create_link(filename, linkname, indent='    '):
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


def _setup_links(links_to_create):
    print()
    print('Creating symbolic links ...')
    print()
    repodir = os.getcwd()
    homedir = os.path.expanduser('~')
    # Link the dotfiles
    with chdir(homedir):
        for f in links_to_create:
            filename = os.path.join(repodir, f)
            linkname = '.' + f
            if not _create_link(filename, linkname):
                break
    # Now the binaries
    binary_dest_dir = os.path.join(homedir, 'bin')
    if not os.path.exists(binary_dest_dir):
        os.mkdir(binary_dest_dir)
    with chdir(binary_dest_dir):
        binary_src_dir = os.path.join(repodir, 'bin')
        for binary in os.listdir(binary_src_dir):
            filename = os.path.join(binary_src_dir, binary)
            if not _create_link(filename, binary):
                break
    # Don't forget a tmux colors file
    tmux_colors = 'tmux.blue.conf'
    tmux_colors = os.path.join(repodir, tmux_colors)
    with chdir(homedir):
        _create_link(tmux_colors, '.tmux.colors.conf')


if __name__ == '__main__':
    _setup_packages()
    _setup_links(BASIC_LINKS)
