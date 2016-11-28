# Copyright (C) 2016  Jonas Bernoulli
#
# Author: Jonas Bernoulli <jonas@bernoul.li>
# License: GPL v3 <https://www.gnu.org/licenses/gpl-3.0.txt>

.PHONY: all help build build-init quick bootstrap
.FORCE:

SUPPRESS_WARNINGS  = 2>&1 | grep -v
SUPPRESS_WARNINGS += -e "‘defgeneric’ is an obsolete macro"
SUPPRESS_WARNINGS += -e "‘defmethod’ is an obsolete macro"

all: build

help:
	$(info )
	$(info make [all|build]    = rebuild all drones and init.el)
	$(info make quick          = rebuild most drones and init.el)
	$(info make lib/DRONE      = rebuild DRONE)
	$(info make build-init     = rebuild init.el)
	$(info make bootstrap      = bootstrap collective)
	@printf "\n"

build:
	@rm -f init.elc
	@emacs -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--funcall borg-batch-rebuild $(SUPPRESS_WARNINGS)

build-init:
	@rm -f init.elc
	@emacs -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--funcall borg-batch-rebuild-init 2>&1

quick:
	@rm -f init.elc
	@emacs -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--eval  '(borg-batch-rebuild t)' $(SUPPRESS_WARNINGS)

lib/%: .FORCE
	@emacs -Q --batch -L lib/borg --load borg \
	--funcall borg-initialize \
	--eval  '(borg-build "$(@F)")' $(SUPPRESS_WARNINGS)

bootstrap:
	git submodule init
	git submodule update
	git submodule foreach 'git checkout master; git reset --hard $$sha1'
	make build
