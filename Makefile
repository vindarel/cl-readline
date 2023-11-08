.PHONY: html

html:
	cd doc && makeinfo --html --no-split cl-readline.texi -o index.html
	# and update the gh-pages branch.
