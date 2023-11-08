.PHONY: html clean

#
# see the Makefile at the project root.
#
html:
        # --css-include manual.css
	# makeinfo --html --split=chapter cl-readline.texi
	makeinfo --html --no-split cl-readline.texi -o index.html

clean:
	rm -f cl-readline/*
