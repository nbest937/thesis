thesis.pdf: datasets.tex analysis.tex thesis.tex
	bibtex thesis
	R --quiet -e 'tools::texi2dvi( "thesis.tex", pdf = TRUE)'

datasets.tex: datasets.Rnw 
	R CMD Sweave datasets.Rnw

analysis.tex: analysis.Rnw 
	R CMD Sweave analysis.Rnw

rsync: input_files
	rsync --compress --archive --verbose \
--include-from 'input_files' --update \
login.pads:/gpfs/pads/projects/see/data/GBC_best-2011/ data


