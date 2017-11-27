# Copyright (C) 2016-2017  Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# License: GPL v3 <https://www.gnu.org/licenses/gpl-3.0.txt>

EMACS ?= emacs

.PHONY: all help build build-init quick bootstrap
.FORCE:

all: build

help:
	$(info )
	$(info make [all|build]    = rebuild all drones and init files)
	$(info make quick          = rebuild most drones and init files)
	$(info make lib/DRONE      = rebuild DRONE)
	$(info make build-init     = rebuild init files)
	$(info make bootstrap      = bootstrap collective or new drones)
	@printf "\n"

build:
	@rm -f init.elc
	@$(EMACS) -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--funcall borg-batch-rebuild 2>&1

build-init:
	@rm -f init.elc
	@$(EMACS) -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--funcall borg-batch-rebuild-init 2>&1

quick:
	@rm -f init.elc
	@$(EMACS) -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--eval  '(borg-batch-rebuild t)' 2>&1

lib/%: .FORCE
	@$(EMACS) -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--eval  '(borg-build "$(@F)")' 2>&1

bootstrap:
	@printf "\n=== Running 'git submodule init' ===\n\n"
	@git submodule init
	@printf "\n=== Running 'bin/borg-bootstrap' ===\n"
	@bin/borg-bootstrap
	@printf "\n=== Running 'make build' ===\n\n"
	@make build
