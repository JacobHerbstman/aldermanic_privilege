all: paper.pdf

paper.pdf: paper.tex sections/*.tex
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make paper.tex

clean:
	latexmk -CA
