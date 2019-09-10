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
	esy install
	find /root/.esy -name "gdate.c" -exec patch -N '{}' work/gdate.c.patch ';'

.PHONY: clean-esy-build
clean-esy-build: 
	rm -rf _esy


atd_t_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_t.ml"; done | paste -sd " " -)
atd_j_ml_files = $(shell for file in $$(find . -type f -iname "*.atd"); do echo "$$(expr "$$file" : '\./\(.*\)\.atd')_j.ml"; done | paste -sd " " -)
atd_ml_files = $(atd_t_ml_files) $(atd_j_ml_files)

bin/%_t.ml: $(_esy)
	$(eval atd_file := $(shell echo -n "$$(expr "$@" : '\(.*\)\_[tj].ml').atd"))
	esy atdgen -t $(atd_file)

bin/%_j.ml: $(_esy)
	$(eval atd_file := $(shell echo -n "$$(expr "$@" : '\(.*\)\_[tj].ml').atd"))
	esy atdgen -j $(atd_file)

lib/%_t.ml: $(_esy)
	$(eval atd_file := $(shell echo -n "$$(expr "$@" : '\(.*\)\_[tj].ml').atd"))
	esy atdgen -t $(atd_file)

lib/%_j.ml: $(_esy)
	$(eval atd_file := $(shell echo -n "$$(expr "$@" : '\(.*\)\_[tj].ml').atd"))
	esy atdgen -j $(atd_file)


.PHONY: ml-of-atd
ml-of-atd: $(_esy)
	find . -type f -name '*.atd' \
		-exec esy atdgen -t '{}' ';' \
		-exec esy atdgen -j '{}' ';' 

.PHONY: clean-ml-of-atd
clean-ml-of-atd:
	find bin lib -type f -regex '^.*_[jt]\.mli?$$' -exec rm '{}' ';'


ml_files = $(shell for file in $$(find . -type f -iname "*.ml"); do echo "$$file"; done | paste -sd " " -)

_esy/default/build/default/bin/main.exe: $(_esy) $(atd_ml_files) $(ml_files)
	esy build

.PHONY: main.exe
main.exe: _esy/default/build/default/bin/main.exe

.PHONY: rebuild
rebuild: $(_esy)
	esy build

.PHONY: run
run: _esy/default/build/default/bin/main.exe
	./_esy/default/build/default/bin/main.exe _env/config.json

# Build and run main.exe
.PHONY: brun
brun: rebuild run