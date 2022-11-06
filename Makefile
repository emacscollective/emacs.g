-include lib/borg/borg.mk

ifndef BORG_DIR

help helpall::
	$(info )
	$(info Bootstrapping)
	$(info -------------)
	$(info make bootstrap-borg  = make borg and make targets available)
	@printf "\n"

bootstrap-borg:
	@git submodule--helper clone --name borg --path lib/borg \
	--url git@github.com:emacscollective/borg.git
	@cd lib/borg; git symbolic-ref HEAD refs/heads/master
	@cd lib/borg; git reset --hard HEAD

else

helpall::
	$(info Test and fix targets)
	$(info --------------------)
	$(info make codespell-dry   = run codespell, dry run)
	$(info make codespell-fix   = run codespell, write fixes)
	@printf "\n"

codespell-dry:
	@cd lib; codespell \
	  --ignore-words ~/.emacs.d/etc/codespell/ignore-words \
	  --exclude-file ~/.emacs.d/etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  ~/.emacs.d/etc/codespell/ignore-files | tr "\\n" ",")

codespell-fix:
	@cd lib; codespell --write-changes \
	  --ignore-words ~/.emacs.d/etc/codespell/ignore-words \
	  --exclude-file ~/.emacs.d/etc/codespell/ignore-lines \
	  --skip $(shell sed '/^\s*$$/d;/^\s*#.*$$/d;s/#.*//;s/\s//g' \
	  ~/.emacs.d/etc/codespell/ignore-files | tr "\\n" ",")

endif
