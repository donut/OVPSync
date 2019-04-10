include esy-docker.mk

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

_env/docker-compose.env:
	@echo "### Environment config not setup. ###"
	@echo "Take a look at _env/README for instructions."
	@exit 1

.env: .docker/image.esy _env/docker-compose.env
	echo "ESY_IMAGE=$(shell cat .docker/image.esy)" > .env
	cat _env/docker-compose.env >> .env

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

atd_t_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_t.ml"; done | paste -sd " " -)
atd_j_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_j.ml"; done | paste -sd " " -)
atd_ml_files = $(atd_t_ml_files) $(atd_j_ml_files)

lib/%_t.ml: _esy
	$(MAKE) ml-of-atd

lib/%_j.ml: $(atd_t_ml_files)

_esy/default/build/default/bin/main.exe: _esy esy $(atd_ml_files)
	./esy

.PHONY: ml-of-atd
ml-of-atd: 
	$(dc) exec dev find . -type f -name '*.atd' \
		-exec esy atdgen -t '{}' ';'  \
		-exec esy atdgen -j '{}' ';' 

.PHONY: clean-ml-of-atd
clean-ml-of-atd:
	find -E bin lib -type f -iregex '.*\_[tj].mli?' -exec rm '{}' ';'

.PHONY: rebuild
rebuild: _esy esy
	$(MAKE) ml-of-atd
	./esy

.PHONY: run
run: _esy/default/build/default/bin/main.exe
	$(dc) exec dev /app/_esy/default/build/default/bin/main.exe \
		/app/_env/config.json

# Build and run main.exe
.PHONY: brun
brun: rebuild run