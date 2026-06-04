# Modal thought

`Modal thought` is an academic research project platform for presenting data, code, and text together in a GitHub Pages site.

## Project structure

- `index.html` — landing page and site entry point
- `data/` — dataset files, exports, and summaries
- `code/` — analysis scripts, notebooks, and tools
- `text/` — documentation, manuscripts, and research notes

## Next steps

1. Add project data and analysis code.
2. Expand the site with interactive visualizations.
3. Configure GitHub Pages once the remote repo is created.

## GitHub Pages

This repository includes an automated GitHub Pages deployment workflow. When you push changes to `main`, the Pages Action will build and publish the site from the repository root.

Published site URL:
- `https://phillipsjs.github.io/modal-thought/`

## Example data import

This project now includes an initial example import from the `modal-spaces` dataset.
See `external/modal-spaces/README.md` for the imported files and `text/modal-spaces-context.md` for context about how the data are structured.

Interactive examples:
- `study1a-explorer.html` — explore Study 1a data with scenario context and participant filtering.
- `figure1.html` — recreate the Figure 1 plot from the modal-spaces write-up with interactive controls.

## Living paper and versioning

This repository is intended to be a living paper rather than a static report. The `living-paper.html` page explains the project philosophy, and `version.json` tracks the current release metadata. Future updates should add version history and interactive exploration pages.
