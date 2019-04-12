SHELL = /bin/bash

_esy:
	esy install

.PHONY: clean-esy-build
clean-esy-build: 
	rm -rf _esy

atd_t_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_t.ml"; done | paste -sd " " -)
atd_j_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_j.ml"; done | paste -sd " " -)
atd_ml_files = $(atd_t_ml_files) $(atd_j_ml_files)

lib/%_t.ml: _esy
	$(MAKE) -s ml-of-atd

lib/%_j.ml: $(atd_t_ml_files)

.PHONY: ml-of-atd
ml-of-atd: _esy
	find . -type f -name '*.atd' \
		-exec esy atdgen -t '{}' ';' \
		-exec esy atdgen -j '{}' ';' 

.PHONY: clean-ml-of-atd
clean-ml-of-atd:
	find -E bin lib -type f -iregex '.*\_[tj].mli?' -exec rm '{}' ';'


_esy/default/build/default/bin/main.exe: _esy $(atd_ml_files)
	esy

.PHONY: main.exe
main.exe: _esy/default/build/default/bin/main.exe

.PHONY: rebuild
rebuild: _esy
	$(MAKE) ml-of-atd
	esy

.PHONY: run
run: _esy/default/build/default/bin/main.exe
	./_esy/default/build/default/bin/main.exe _env/config.json

# Build and run main.exe
.PHONY: brun
brun: rebuild run