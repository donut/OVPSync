
build:
	find . -iname '*.atd' -exec atdgen -t '{}' \; -exec atdgen -j '{}' \;
	jbuilder build bin/main.exe