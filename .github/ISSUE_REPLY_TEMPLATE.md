Hi there, thanks for using the Emacs Application Framework!

If you're a **new user** and are having trouble with your installation, please make sure you're following the [installation instruction](https://github.com/emacs-eaf/emacs-application-framework#install) carefully.

If you're an **existing user**, you just did a `git pull`, now for some reason your EAF is not working anymore! No worries, first run `M-x eaf-install-dependencies` or the `install-eaf.py` script to update your EAF to the latest apps and dependencies (in general, you should always do this after `git pull`), then have a look at [Mandatory Procedures to Keep Your EAF Up-To-Date](https://github.com/emacs-eaf/emacs-application-framework/discussions/527?sort=new) to see if anything posted there is relevant to your problem.

Finally, if all else fails, you are back here. Make sure that:
- you're following the BUG_REPORT template carefully
- your error is reproducible through a clean `emacs -q` with EAF installation only
- any relevant error message is pasted here
- add a GitHub label that represents the system you're having this problem, to help us triaging
