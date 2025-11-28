# TouchID for sudo

Setting this up on a mac takes several steps:

* `cd /etc/pam.d/`
* `cp sudo_local.template sudo_local`
* uncomment the line containing `pam_tid.so`
* `brew install pam-reattach`
* add a line before the previous one with `auth       optional       /opt/homebrew/lib/pam/pam_reattach.so`
* `System Settings → Privacy & Security → Input Monitoring` and add iterm2
