clean:
	-rm *.pdf *.tex *.log *.aux *.log *.out *.html *.pyg *.bcf *.fls *.run.xml *.xdv *.fdb_latexmk
	-rm -rf _minted*

dist-clean:
	-rm README.elc README.el
