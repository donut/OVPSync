SHELL = /bin/bash

define newline


endef

EMIT = echo -e '$(subst $(newline),\n,$(1))'


# For some reason building on Ubuntu chokes on this `g_date_strftime` call
# in gdate.c. This tells it to ignore the relevant errors and continue.
define GDATE_C_PATCH
--- a/glib/glib/gdate.c
+++ b/glib/glib/gdate.c
@@ -2439,6 +2439,9 @@ win32_strftime_helper (const GDate     *d,
  *
  * Returns: number of characters written to the buffer, or 0 the buffer was too small
  */
+#pragma GCC diagnostic push
+#pragma GCC diagnostic ignored "-Wformat-nonliteral"
+
 gsize     
 g_date_strftime (gchar       *s, 
                  gsize        slen, 
@@ -2549,3 +2552,5 @@ g_date_strftime (gchar       *s,
   return retval;
 #edieqf
 }
+
+#pragma GCC diagnostic pop
endef


work/gdate.c.patch:
	mkdir -p work
	@$(call EMIT,$(GDATE_C_PATCH)) > $(@)


_esy := _esy/default/installation.json

$(_esy): work/gdate.c.patch package.json
	esy install --color=always
	find /root/.esy -name "gdate.c" -exec patch -N '{}' work/gdate.c.patch ';'

.PHONY: install-dependencies
install-dependencies: $(_esy)

.PHONY: build-dependencies
build-dependencies: $(_esy)
	esy build-dependencies --color=always

.PHONY: clean-esy-build
clean-esy-build: 
	rm -rf _esy


atd_t_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_t.ml"; done | paste -sd " " -)
atd_j_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_j.ml"; done | paste -sd " " -)
atd_ml_files = $(atd_t_ml_files) $(atd_j_ml_files)


define atdgen
	$(eval atd_file := $(shell echo -n "$$(expr "$2" : '\(.*\)\_[tj].ml').atd"))
	esy atdgen -$1 $(atd_file)
endef

$(atd_t_ml_files): %_t.ml: %.atd $(_esy)
	$(call atdgen,t,$@)

$(atd_j_ml_files): %_j.ml: %.atd $(_esy)
	$(call atdgen,j,$@)


.PHONY: ml-of-atd
ml-of-atd: $(_esy)
	find . -type f -name '*.atd' \
		-exec esy atdgen -t '{}' ';' \
		-exec esy atdgen -j '{}' ';' 

.PHONY: clean-ml-of-atd
clean-ml-of-atd:
	find bin lib -type f -regex '^.*_[jt]\.mli?$$' -exec rm '{}' ';'


ml_files = $(shell for file in $$(find . -type f \( -iname "?*.ml" -or -iname "?*.mli" \)); do echo "$$file"; done | paste -sd " " -)

.PHONY: test
test: $(_esy) $(atd_ml_files) $(ml_files)
	esy x dune runtest


_esy/default/build/default/bin/main.exe: $(_esy) $(atd_ml_files) $(ml_files)
	esy build --color=always

.PHONY: main.exe
main.exe: _esy/default/build/default/bin/main.exe

.PHONY: rebuild
rebuild: $(_esy)
	esy build --color=always

.PHONY: run
run: _esy/default/build/default/bin/main.exe
	./_esy/default/build/default/bin/main.exe _env/config.json

# Build and run main.exe
.PHONY: brun
brun: rebuild run