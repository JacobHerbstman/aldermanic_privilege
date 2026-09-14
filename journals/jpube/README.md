# Journal of Public Economics submission

`submission/` contains the September 14, 2026 JPubE submission files, rebuilt
from the current manuscript in `paper/`. The August 20 files are preserved in
`archive/2026-08-20/`.

Jacob confirmed submission on September 14, 2026. No submission-portal actions
were performed from this checkout.

## Submission documents

- `submission/working_paper.pdf`: main paper and appendix together, with the AI disclosure.
- `submission/main_paper.pdf`: main paper with the AI disclosure.
- `submission/online_appendix.pdf`: separate appendix.
- `submission/word_count.pdf`: main-text pages for the short-paper word count.
- `submission/cover_letter.pdf`: cover letter with the current numerical results.
- `submission/conflict_of_interest_statement.docx`: the previously prepared declaration of no competing interests.

The cover-letter source is `submission/cover_letter.tex`. The previously agreed
AI declaration is in `ai_disclosure.tex` and appears on its own page immediately
before the references. The public manuscript in `paper/` continues to omit it.

## Rebuild locally

From `paper/`, run:

```sh
make jpube
```

The existing manuscript targets supply the dependencies for results and figures.
The journal variants compile from the same LaTeX source with the disclosure
enabled. Temporary LaTeX files stay in `paper/tmp/jpube/`; the PDFs above are the
submission copies. This command does not interact with any submission website.

The September 14 build has 57 pages including the appendix. The disclosure is
page 22, followed by the references on page 23. Comparison with the current
public manuscript found identical text apart from the disclosure and page
numbers; all 17 embedded figure PDFs match their task outputs. The separate
appendix and word-count PDFs match the current public files byte for byte.
The abstract has 171 words; TeXcount reports 5,491 words across the six main-text
sections. There are five main-text exhibits (four figures and one table).

## Preparation notes

The following suggestions were reviewed before submission. Jacob chose to
submit without adding them; they are retained here as preparation history.

The [JPubE author guide](https://www.sciencedirect.com/journal/journal-of-public-economics/publish/guide-for-authors),
checked September 14, 2026, requires a separate editable highlights file with
3--5 bullets of at most 85 characters each. That file has not yet been prepared.

The guide also requires a research-data repository citation and link, or an
explanation of why data cannot be shared. A submission statement should identify
the code and recorded-source archive and explain that RentHub data require
licensed access through Dewey. Such a statement has not been added to the paper.

No funding information was added. The existing no-competing-interests
declaration was retained.

[Elsevier's AI policy](https://www.elsevier.com/about/policies-and-standards/generative-ai-policies-for-journals)
also asks authors to describe AI assistance with research code in Methods. The
restored declaration mentions coding assistance, but no Methods text has been
added.
