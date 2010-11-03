TEXFILES = $(wildcard *.tex)
EPS_DIR = eps
IMAGE_DIR = images
EPS := $(wildcard $(EPS_DIR)/*.eps)
PDF := $(patsubst $(EPS_DIR)/%.eps,$(IMAGE_DIR)/%.pdf,$(EPS))

all: $(PDF) thesis.pdf

thesis.pdf: $(TEXFILES) books.bib
	pdflatex thesis.tex
	bibtex thesis
	pdflatex thesis.tex

$(IMAGE_DIR)/%.pdf: $(EPS_DIR)/%.eps
	epstopdf $< -o $@

clean:
	rm -f *.log *.lot *.lof *.ps *.pdf *.dvi *.aux *.toc *.bbl *.blg *.out *~
