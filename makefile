
build:
	find . -iname '*.atd' -exec atdgen -t '{}' \; -exec atdgen -j '{}' \;
	dune build bin/main.exe