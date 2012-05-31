#!/bin/sh
notmuch new

# add additional tagging, mailing lists
notmuch tag +chef list:"<chef.lists.opscode.com>"
notmuch tag +chef-dev list:"<chef-dev.lists.opscode.com>"
notmuch tag +heavywater +goldstar list:"<goldstar.goldstar.github.com>"
notmuch tag +pdxfunc list:"<pdxfunc.googlegroups.com>"
notmuch tag +pdxpython list:"portland.python.org"
notmuch tag +pdxdevops list:"<pdxdevops.googlegroups.com>"
notmuch tag +pdxruby list"<pdxruby.googlegroups.com>"
notmuch tag +xmonad list:"xmonad.haskell.org"
notmuch tag +ptp-general list:"<ptp-general.googlegroups.com>" 
notmuch tag +ptp-ops list:"<ptp-ops.googlegroups.com>"
notmuch tag +inbox +unread -new -- tag:new
