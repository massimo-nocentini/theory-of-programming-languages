interactive:
	sml -Cprint.depth=1000 sources.cm 

compile:
	sml -m sources.cm < /dev/null