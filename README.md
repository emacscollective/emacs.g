Assimilate Emacs packages as Git submodules
===========================================

*For more information see the [announcement][init] and the [manual].*

About `borg.el`
---------------

[Borg] is a bare-bones package manager for Emacs packages.  It
provides only a few essential features and should be combined with
other tools such as Magit, `epkg`, `use-package`, and `auto-compile`.

Borg assimilates packages into the `~/.emacs.d` repository as Git
submodules.  An assimilated package is called a drone and a borg-based
`~/.emacs.d` repository is called a collective.

About this collective
---------------------

This particular collective is intended to be used to bootstrap private
configurations.  Fork your own copy and then start assimilating as you
please.

If you wish you can later merge changes from the upstream repository,
to get updates for the drones that have been assimilated in the base
configuration.  Very rarely additional drones might be assimilated or
the configuration of existing drones might be tweaked.

Or you can just update and further configure these drones as you would
update the drones you have assimilated yourself.

If you do base your own configuration on this collective and make it
publically available as source of inspiration for others, then please
do so by forking the upstream repository, which is available from
[Github].

You might also want to adjust this description.

[init]:    https://emacsair.me/2016/05/17/assimilate-emacs-packages-as-git-submodules
[Borg]:    https://gitlab.com/tarsius/borg
[manual]:  https://emacsmirror.net/manual/borg
[Github]:  https://github.com/emacscollective/emacs.g
