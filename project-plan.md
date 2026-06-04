# Modal thought project plan

## Goal

Build `Modal thought` as a living paper platform that combines:

- narrative text and methods
- versioned research updates
- interactive data exploration
- transparent code and reproducibility

## Living paper principles

1. **Evolving narrative**
   - The project should clearly show that the current content is not final.
   - Each release should include a version label, date, and summary of changes.
   - Older versions should remain accessible for comparison.

2. **Interactive evidence**
   - Data should be explorable rather than shown only as static charts.
   - Users should be able to filter, inspect, and compare key findings.
   - The site should make it easy to connect data outputs to the analysis code.

3. **Transparent research workflow**
   - Keep data, code, and text separate but linked.
   - Document the provenance of results and how they were produced.
   - Use simple, reusable site components so future updates stay maintainable.

## Initial implementation plan

1. Create a home landing page that introduces the living-paper concept.
2. Add a dedicated living paper page with version metadata and a narrative explanation.
3. Add a version metadata file for the current release (`version.json`).
4. Add a simple interactive prototype page or chart in a future step.
5. Keep the repository documentation in sync with each update.

## Next steps

- [ ] Add a `living-paper.html` page.
- [ ] Add `version.json` and a version history model.
- [ ] Create a small interactive data example.
- [ ] Add a `text/` narrative describing the living paper philosophy.
- [ ] Document the update process in `project-plan.md` and `README.md`.
