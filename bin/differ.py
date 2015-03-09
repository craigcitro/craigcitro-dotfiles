#!/usr/bin/env python

import os
import subprocess
import sys
import tempfile

def main(argv):
  diff = os.environ.get('DIFFER', 'colordiff')
  diff_opts = os.environ.get('CC_DIFFOPTS', '-u')
  args = argv[1:]
  with tempfile.NamedTemporaryFile() as out:
    while args:
      if len(args) < 2:
        break
      if args[0] == ':':
        args.pop(0)
      left, right = args[:2]
      args = args[2:]
      subprocess.call([diff, diff_opts, left, right], stdout=out, stderr=out)

    out.seek(0)
    pager = os.environ.get('PAGER', 'less -qFRX')
    subprocess.call(pager.split() + [out.name])


if __name__ == '__main__':
  main(sys.argv)
