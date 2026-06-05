# Modal Thought

A living paper by Jonathan Phillips, published via GitHub Pages.

## How it works

Article text is written in Markdown files in `content/`, then compiled into `index.html` by Pandoc using the template in `templates/article.html`. Pushing to `main` triggers a GitHub Actions build and deployment automatically.

## Writing

- Add or edit sections in `content/` — files are concatenated in alphabetical order.
- `content/00-meta.md` holds the YAML frontmatter (title, author, abstract).
- Subsequent files (`01-introduction.md`, `02-methods.md`, …) are the article sections.

## Local preview

```bash
bash build.sh   # generates index.html
open index.html
```

Requires [Pandoc](https://pandoc.org) ≥ 3.0.

## Repository structure

- `content/` — Markdown source files for the article
- `templates/` — Pandoc HTML template and CSS
- `data/` — datasets and exports
- `code/` — analysis scripts
- `external/` — imported datasets from related projects
- `build.sh` — local build script
- `.github/workflows/pages.yml` — CI build and deploy
