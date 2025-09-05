#DEFINITIONS

SLIDES_SECTIONS := $(shell grep '\\input{' slides.tex | grep -v '%.*\\input' | sed 's/^.*\\input{\(.*\.tex\).*/\1/')
TEX_INPUTS := $(shell grep '\\input{.*\.tex}' $(SLIDES_SECTIONS) | grep -v '%.*\\input' | sed 's/^.*\\input{\(.*\.tex\).*/\1/')
IMG_INPUTS := $(shell grep '\\includegraphics' $(SLIDES_SECTIONS) | grep -v '%.*\\includegraphics' | sed 's/^.*\\includegraphics\[.*\]{\([\./a-zA-Z0-9_]*\)}.*/\1/')

all: slides.pdf

clean:
	rm -f *.log *.aux *.out *.toc *.snm *.nav *.bbl *.blg

# --- DEBUGGING LINE (you can keep or remove this) ---
# $(info The following image dependencies were found: [$(IMG_INPUTS)])

slides.pdf: slides.tex $(SLIDES_SECTIONS) $(TEX_INPUTS) $(IMG_INPUTS)
	# First pass to generate .aux files, etc.
	pdflatex -interaction=nonstopmode -draftmode $<
	# Second pass to resolve cross-references and place images correctly
	pdflatex -interaction=nonstopmode $<
	# Clean up temporary files
	rm -f slides.log slides.aux slides.out slides.toc slides.snm *.nav
