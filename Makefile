SHELL = /bin/bash


.PHONY: PHONY
PHONY:


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


docker-images: PHONY .env
	$(dc) build $(s)

start: PHONY .env _env/mysql-dump.sql _data/app _data/db/data
	$(dc) up $(s) || $(MAKE) stop s=$(s)

start-detached: PHONY .env _env/mysql-dump.sql _data/app _data/db/data
	$(dc) up --detach $(s) 

restart: PHONY .env _env/mysql-dump.sql _data/app _data/db/data
	$(dc) restart $(s) 

stop: PHONY .env
	$(dc) stop $(s)


rebuild-app-service: PHONY .env _env/mysql-dump.sql _data/app _data/db/data
	$(MAKE) docker-images s=app
	$(MAKE) stop s=app
	$(MAKE) start-detached s=app


tear-it-all-down: PHONY
	@echo "### Tearing it all down... ###"
	$(MAKE) clean-docker-deeply
	$(MAKE) clean-data
	$(MAKE) clean-build
	$(MAKE) clean-ml-of-atd
	rm -f .env

clean-docker-containers: PHONY
	$(dc) down --volumes || exit 0
	rm -f .env

clean-docker-deeply: PHONY
	$(dc) down --volumes --rmi all || exit 0
	$(su-rm) -rf .docker 
	rm -f esy
	rm -f .env

clean-data: PHONY
	rm -rf _data


shell: PHONY .env
	$(dc) exec $(s) sh -c "fish || bash || sh; exit 0"
	


app-make = $(dc) exec --user=opam --workdir=/app/src app make


clean-build: PHONY .env
	$(app-make) $@


ml-of-atd: PHONY .env
	$(app-make) $@

clean-ml-of-atd: PHONY
	$(app-make) $@


test: PHONY .env
	$(app-make) $@

main.exe: PHONY .env
	$(app-make) $@
	

rebuild: PHONY .env
	$(app-make) $@

run: PHONY .env
	$(app-make) $@

# Build and run main.exe
brun: PHONY
	$(app-make) $@


follow-db-logs: PHONY .env
	$(dc) exec db sh -c "tail -f /var/log/mysql/*.log"
