all: semantics.pdf

semantics.pdf: semantics.tex
	TEXINPUTS=".:../latex-pl-syntax/:" pdflatex semantics.tex

clean: phony
	rm -rf *.pdf *.aux *.log

.PHONY: phony
