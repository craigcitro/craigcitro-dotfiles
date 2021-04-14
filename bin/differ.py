#!/usr/bin/env python3

import os
import subprocess
import sys
import tempfile


def main(argv):
    diff = os.environ.get(
        'DIFFER', 'git diff --no-index --color=always').split()
    with open(os.devnull, 'w') as f:
        exists = subprocess.call([diff[0], '--help'], stdout=f, stderr=f)
    if exists:
        print(f'Diff program {diff[0]} not found')
        return 1
    args = argv[1:]
    diff_opts = '-u' if args[0] == ':' else args.pop(0)
    with tempfile.NamedTemporaryFile() as out:
        while args:
            if len(args) < 2:
                break
            if args[0] == ':':
                args.pop(0)
            left, right = args[:2]
            args = args[2:]
            subprocess.call(
                diff + [diff_opts, left, right], stdout=out, stderr=out)

        out.seek(0)
        pager = os.environ.get('PAGER', 'less -qrFX')
        subprocess.call(pager.split() + [out.name])


if __name__ == '__main__':
    sys.exit(main(sys.argv))
