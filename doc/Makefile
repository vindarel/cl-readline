.PHONY: html clean

#
# Edit cl-readline.texi
# run make html
# or make html-local to have syntax highlighting locally
# then update and commit index.html in the gh-pages branch (the one at the project root, not doc/index.html)
#
html:
        # --css-include manual.css
	# makeinfo --html --split=chapter cl-readline.texi
	makeinfo --html --no-split cl-readline.texi -o index.html

	sed -i "s@<head>@<head> <script type=\"text/javascript\" src=\"https://vindarel.github.io/cl-readline/doc/js/highlight-lisp.js\"></script> <link rel=\"stylesheet\" href=\"https://vindarel.github.io/cl-readline/doc/js/github.css\">@g" index.html
	sed -i "s@</pre>@</code></pre>@g" index.html
	sed -i "s@<pre class=\"lisp\">@<pre><code class=\"lisp\">@g" index.html
	sed -i "s@</body>@</body>  <script> HighlightLisp.highlight_auto(); HighlightLisp.paren_match(); </script>@g" index.html
	# and update the gh-pages branch.

html-local: html
	sed -i "s@https://vindarel.github.io/cl-readline/doc@@g" index.html

clean:
	rm -f cl-readline/*
