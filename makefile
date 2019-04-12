SHELL = /bin/bash


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


define newline


endef

EMIT = echo -e '$(subst $(newline),\n,$(1))'


_env/docker-compose.env:
	@echo "### Environment config not setup. ###"
	@echo "Take a look at _env/README for instructions."
	@exit 1

_env/mysql-dump.sql:
	# Create an empty file to avoid docker creating it as a directory when
	# setting up the binding.
	touch _env/mysql-dump.sql


_data/app:
	mkdir -p _data/app

_data/db/data:
	mkdir -p _data/db/data


.env: _env/docker-compose.env
	ln -s _env/docker-compose.env .env


.PHONY: docker-images
docker-images: .env
	$(dc) build $(s)

.PHONY: start
start: .env _env/mysql-dump.sql _data/app _data/db/data
	$(dc) up $(s) || $(MAKE) stop s=$(s)

.PHONY: stop
stop: .env
	$(dc) stop $(s)


.PHONY: tear-it-all-down
tear-it-all-down:
	@echo "### Tearing it all down... ###"
	$(MAKE) clean-docker-deeply
	$(MAKE) clean-data
	$(MAKE) clean-esy-build
	$(MAKE) clean-ml-of-atd
	rm -f .env

.PHONY: clean-docker-containers
clean-docker-containers:
	$(dc) down --volumes || exit 0
	rm -f .env

.PHONY: clean-docker-deeply
clean-docker-deeply:
	$(dc) down --volumes --rmi all || exit 0
	$(su-rm) -rf .docker 
	rm -f esy
	rm -f .env

.PHONY: clean-data
clean-data:
	rm -rf _data


.PHONY: shell
shell: .env
	$(dc) exec $(s) bash


define ESY_FOR_HOST
#!/bin/bash

export $$(grep -v '^#' $(shell pwd)/.env | xargs $(xargs-seperator))
$(dc) --file="$(shell pwd)/docker-compose.yml" \
	exec --workdir="/app" app esy "$$@"
endef

esy: .env
	@$(call EMIT,$(ESY_FOR_HOST)) > $(@)
	@chmod +x $(@)


_esy/default: esy
	./esy install

.PHONY: clean-esy-build
clean-esy-build: .env
	$(su-rm) -rf _esy/*


atd_t_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_t.ml"; done | paste -sd " " -)
atd_j_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_j.ml"; done | paste -sd " " -)
atd_ml_files = $(atd_t_ml_files) $(atd_j_ml_files)

lib/%_t.ml: _esy/default
	$(MAKE) ml-of-atd

lib/%_j.ml: $(atd_t_ml_files)

.PHONY: ml-of-atd
ml-of-atd: _esy/default
	$(dc) exec app find . -type f -name '*.atd' \
		-exec esy atdgen -t '{}' ';' \
		-exec esy atdgen -j '{}' ';' 

.PHONY: clean-ml-of-atd
clean-ml-of-atd:
	find -E bin lib -type f -iregex '.*\_[tj].mli?' -exec rm '{}' ';'


_esy/default/build/default/bin/main.exe: esy _esy/default $(atd_ml_files)
	./esy

.PHONY: main.exe
main.exe: _esy/default/build/default/bin/main.exe

.PHONY: rebuild
rebuild: _esy/default esy
	$(MAKE) ml-of-atd
	./esy

.PHONY: run
run: _esy/default/build/default/bin/main.exe
	$(dc) exec app /app/_esy/default/build/default/bin/main.exe \
		/app/_env/config.json

# Build and run main.exe
.PHONY: brun
brun: rebuild run