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

.PHONY: start-detached
start-detached: .env _env/mysql-dump.sql _data/app _data/db/data
	$(dc) up --detach $(s) 

.PHONY: restart
restart: .env _env/mysql-dump.sql _data/app _data/db/data
	$(dc) restart $(s) 

.PHONY: stop
stop: .env
	$(dc) stop $(s)


.PHONY: rebuild-app-service
rebuild-app-service: .env _env/mysql-dump.sql _data/app _data/db/data
	$(MAKE) docker-images s=app
	$(MAKE) restart s=app


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
	$(dc) exec $(s) sh -c "fish || bash || sh || exit 0"
	

define ESY_FOR_HOST
#!/bin/bash

export $$(grep -v '^#' $(shell pwd)/.env | xargs $(xargs-seperator))
$(dc) --file="$(shell pwd)/docker-compose.yml" \
	exec --workdir="/app" app esy "$$@"
endef

esy: .env
	@$(call EMIT,$(ESY_FOR_HOST)) > $(@)
	@chmod +x $(@)



app-make = $(dc) exec --workdir=/app app make


.PHONY: clean-esy-build
clean-esy-build: .env
	$(su-rm) -rf _esy
	$(app-make) $@


.PHONY: ml-of-atd
ml-of-atd: .env
	$(app-make) $@

.PHONY: clean-ml-of-atd
clean-ml-of-atd:
	$(app-make) $@


_esy := _esy/default/installation.json
$(_esy): esy.lock/index.json
	esy install


ml_files = $(shell for file in $$(find . -type f -iname "*.ml"); do echo "$$file"; done | paste -sd " " -)


.PHONY: test
test: $(_esy) $(ml_files)
	esy x dune runtest


main-exe := _esy/default/build/default/bin/main.exe
$(main-exe): $(_esy) $(ml_files)
	esy build

.PHONY: main.exe
main.exe: .env
	$(app-make) $@
	

.PHONY: rebuild
rebuild: .env
	$(app-make) $@
	esy

.PHONY: run
run: .env
	$(app-make) $@
	@$(MAKE) $(main-exe)

# Build and run main.exe
.PHONY: brun
brun: rebuild run


.PHONY: follow-db-logs
follow-db-logs: .env
	$(dc) exec db sh -c "tail -f /var/log/mysql/*.log"