# TMath 390: Mathematical Statistics with R

Course website built with [Quarto](https://quarto.org/).

## Requirements

- [Quarto](https://quarto.org/docs/get-started/) 1.11 or newer
- R 4.3 or newer
- R packages used in the materials: `knitr`, `rmarkdown`, `ggplot2`, `dplyr`, `plotly`, and `shiny`

This machine has Quarto installed at `~/.local/bin/quarto`. Add `~/.local/bin` to your `PATH` if `quarto` is not found.

## Local development

```bash
quarto preview        # live preview at http://localhost:4200
quarto render         # build the site into _site/
```

## Structure

```text
.
├── _quarto.yml          # site configuration and navigation
├── index.qmd            # course landing page
├── syllabus.qmd
├── schedule.qmd
├── objectives.qmd
├── lectures/            # Reveal.js lecture decks and images
├── homework/            # homework sets
├── solutions/           # worked solutions
├── resources/           # practice, reviews, downloads
│   └── downloads/       # original DOCX/PDF handouts
├── data/                # public course datasets
├── apps/                # Shiny application source
├── assets/              # styles and shared assets
└── private/             # NOT published (student data, textbooks, keys)
```

## Adding datasets

Homework and solutions that read data are currently marked `execute: enabled: false` so the site can build without the datasets. Once the files listed in `data/index.qmd` are added to `data/`, remove the `execute: enabled: false` block from the affected `.qmd` files (search for `TODO: re-enable after datasets`). Also replace the **pending dataset** placeholders in `lectures/l29_simple_linear_regression.qmd` with the original inline R expressions.

## Private content

The `private/` directory is excluded from git and from the site build. It contains:

- `private/student_data/` — response data and quiz attendance
- `private/books/` — textbook PDFs
- `private/quiz_database.docx` — quiz bank with answer keys
- `private/tools/grader.r` — internal grading script

Do not move these into a published directory.

## Deployment

Deployment is not configured yet. The rendered site is written to `_site/` and can be published to any static host (GitHub Pages, Netlify, etc.) when you are ready.
