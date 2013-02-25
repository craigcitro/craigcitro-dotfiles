#!/usr/bin/env python

import os
import subprocess
import sys
import tempfile

def main(argv):
  diff = os.environ.get('DIFFER', 'colordiff')
  diff_opts = argv[1]
  args = argv[2:]
  with tempfile.NamedTemporaryFile() as out:
    print 'out.name: ', out.name
    while args:
      if len(args) < 3:
        break
      _, left, right = args[:3]
      args = args[3:]
      subprocess.call([diff, diff_opts, left, right], stdout=out, stderr=out)

    out.seek(0)
    pager = os.environ.get('PAGER', 'less -qFRX')
    subprocess.call(pager.split() + [out.name])


if __name__ == '__main__':
  main(sys.argv)
