# --- DEFINITIONS ---

# Which sections are included by slides.tex
SLIDES_SECTIONS := $(shell grep '\\input{' slides.tex | grep -v '%.*\\input' | sed 's/^.*\\input{\(.*\.tex\).*/\1/')

# Any \input inside those sections (nested inputs)
TEX_INPUTS := $(shell grep '\\input{.*\.tex}' $(SLIDES_SECTIONS) | grep -v '%.*\\input' | sed 's/^.*\\input{\(.*\.tex\).*/\1/')

# Raw image paths as written in the .tex files
IMG_INPUTS_RAW := $(shell grep '\\includegraphics' $(SLIDES_SECTIONS) | grep -v '%.*\\includegraphics' | sed 's/^.*\\includegraphics\[.*\]{\([^}]*\)}.*/\1/')

# Normalize image paths so they're correct relative to slides/ (strip one "../" if author wrote paths relative to sections/)
IMG_INPUTS := $(foreach f,$(IMG_INPUTS_RAW),$(if $(filter ../../%,$(f)),$(patsubst ../../%,../%,$(f)),$(f)))

all: slides.pdf

clean:
	rm -f *.log *.aux *.out *.toc *.snm *.nav *.bbl *.blg *.fls *.fdb_latexmk

# --- Build slides ---

slides.pdf: slides.tex $(SLIDES_SECTIONS) $(TEX_INPUTS) $(IMG_INPUTS)
	pdflatex -interaction=nonstopmode -draftmode $<
	bibtex slides
	pdflatex -interaction=nonstopmode -draftmode $<
	pdflatex -interaction=nonstopmode $<
	rm -f slides.log slides.aux slides.out slides.toc slides.snm *.nav

# --- If an included image lives under ../tasks/*/output/, build it by calling the task's Makefile ---
# Example: dependency ../tasks/create_alderman_strictness_scores/output/Month_FEs_final_strictness_index.png
# becomes:  make -C ../tasks/create_alderman_strictness_scores/code ../output/Month_FEs_final_strictness_index.png
../tasks/%:
	$(MAKE) -C $(subst output/,code/,$(dir $@)) ../output/$(notdir $@)
