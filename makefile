include esy-docker.mk

env = ESY_IMAGE=$(shell cat $$(pwd)/.docker/image.esy)

OS := $(shell uname)
ifeq ($(OS), Linux)
	dc = sudo docker-compose
	su-rm = sudo rm
	xargs-seperator = -d '\n'
else
	dc = docker-compose
	su-rm = rm
	xargs-seperator = -0
endif

.env: .docker/image.esy
	echo $(env) > .env
	echo "TZ=$${TZ:-America/Phoenix}" >> .env

.PHONY: docker-compose
docker-compose:
	$(dc) $(a)

.docker/image.dev: .env
	$(dc) up --detach dev
	$(dc) images --quiet dev > $(@)
	$(dc) stop

.PHONY: docker-images
docker-images: .env
	$(dc) build $(s)

.PHONY: start
start: .env
	$(dc) up $(s) || $(MAKE) stop s=$(s)

.PHONY: stop
stop: .env
	$(dc) stop $(s)

.PHONY: docker-clean
docker-clean:
	$(dc) down --volumes --rmi local || exit 0
	rm -f .env

.PHONY: docker-deeply-clean
docker-deeply-clean: 
	$(dc) down --volumes --rmi all || exit 0
	$(su-rm) -rf .docker 
	rm -f esy
	rm -f .env

.PHONY: esy-build-clean
esy-build-clean: 
	$(su-rm) -rf _esy

.PHONY: shell
shell: .env
	$(dc) exec $(s) bash

define ESY_FOR_HOST
#!/bin/bash

export $$(grep -v '^#' $(shell pwd)/.env | xargs $(xargs-seperator))
$(dc) --file="$(shell pwd)/docker-compose.yml" \
	exec --workdir="/app" dev esy "$$@"
endef

esy: .env
	@$(call EMIT,$(ESY_FOR_HOST)) > $(@)
	@chmod +x $(@)

_esy: esy
	./esy install

lib/%_t.ml: _esy
	$(MAKE) t_ml-of-atd

lib/%_j.ml: _esy
	$(MAKE) j_ml-of-atd

_esy/default/build/default/bin/main.exe: esy lib/%_t.ml lib/%_j.ml
	./esy

.PHONY: t_ml-of-atd
t_ml-of-atd:
	$(dc) exec dev find . -iname '*.atd' -exec esy atdgen -t '{}' ';' 

.PHONY: j_ml-of-atd
j_ml-of-atd:
	$(dc) exec dev find . -iname '*.atd' -exec esy atdgen -j '{}' ';' 

.PHONY: ml-of-atd
ml-of-atd: t_ml-of-atd j_ml-of-atd

.PHONY: rebuild
rebuild: _esy esy
	$(MAKE) ml-of-atd
	./esy

.PHONY: run
run: _esy/default/build/default/bin/main.exe
	$(dc) exec dev /app/_esy/default/build/default/bin/main.exe /app/config.json

# Build and run main.exe
.PHONY: brun
brun: rebuild run