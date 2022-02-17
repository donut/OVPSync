SHELL = /bin/bash

.PHONY: PHONY
PHONY:


env = eval $$(opam env) &&


_opam/.opam-switch/switch-state:
	$(env) opam switch create . --empty
	# The build process sometimes fails due to parallelism. This will repeat the
	# build without parallelization if it fails the first time.
	$(env) (opam switch import switch-export.opam \
					|| opam switch import --jobs=1 switch-export.opam)

dependencies: PHONY _opam/.opam-switch/switch-state

clean-local-opam-switch: PHONY
	rm -rf _opam

switch-export.opam: _opam/.opam-switch/switch-state
	$(env) opam switch export switch-export.opam


atd_ml_files = $(shell \
	for file in $$(find ./bin ./lib -type f -iname "*.atd"); do \
		echo "$$(expr "$$file" : '\./\(.*\)\.atd')_t.ml"; \
		echo "$$(expr "$$file" : '\./\(.*\)\.atd')_j.ml"; \
	done \
	| paste -sd " " -\
)

define atdgen
	$(eval type := $(shell echo -n "$$(expr "$2" : '.*\_\([tj]\).ml')"))
	$(env) atdgen -$(type) $1
endef

$(filter %_t.ml,$(atd_ml_files)): %_t.ml: %.atd switch-export.opam
	$(call atdgen,$<,$@)

$(filter %_j.ml,$(atd_ml_files)): %_j.ml: %.atd switch-export.opam
	$(call atdgen,$<,$@)


ml-of-atd: PHONY switch-export.opam $(atd_ml_files)

clean-ml-of-atd: PHONY
	find ./bin ./lib -type f -regex '^.*_[jt]\.mli?$$' -exec rm '{}' ';'


ml_files = $(shell \
	for file in $$(find . -type f \( -iname "?*.ml" -or -iname "?*.mli" \)); do \
		echo "$$file"; \
	done \
	| paste -sd " " - \
)

dune_files = $(shell find . -type f -iname "dune" | paste -sd " " -)

source_files = $(atd_ml_files) $(ml_files) $(dune_files)

test: PHONY switch-export.opam $(source_files)
	$(env) dune runtest


_build/default/bin/main.exe: switch-export.opam $(source_files)
	$(env) dune build bin/main.exe

main.exe: PHONY _build/default/bin/main.exe

clean-build:
	rm -rf _build

rebuild: PHONY switch-export.opam
	$(env) dune build bin/main.exe

run: PHONY main.exe
	_build/default/bin/main.exe ../_env/config.json

# Build and run main.exe
brun: PHONY rebuild run
