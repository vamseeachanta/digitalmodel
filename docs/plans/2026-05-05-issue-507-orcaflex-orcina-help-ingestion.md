# Plan: digitalmodel #507 — Complete Orcina help ingestion: environment, waves, current, vessel pages

**Issue:** https://github.com/vamseeachanta/digitalmodel/issues/507
**Status:** plan-review
**Tier:** T3

## Context

22 reference pages were ingested from Orcina WebHelp (`docs/domains/orcaflex/reference/*.md`), but key sections are missing because the WebHelp site uses JavaScript-rendered content frames that direct HTTP fetches cannot resolve. The downloaded `OrcaFlexHelp.exe` / `OrcaWaveHelp.exe` / `OrcaFxAPIHelp.exe` archives at `/tmp/Orca{Flex,Wave,FxAPI}Help.zip` are CHM-compiled binaries that only `hh.exe -decompile` (Windows) can extract. `scripts/ingest_orcina_help.py` already implements `download` → `convert` (HTML→markdown). The gap is the upstream HTML extraction.

Missing pages: Environment overview, Sea properties, Waves page, Current page, Wind page, VesselType properties, Vessel instance properties, Line data properties, YAML automation/examples. Two viable ingestion paths exist (CHM decompile on Windows; Playwright headless scrape on Linux). Path selection determines deliverables.

## Plan

1. **Decide ingestion path.** Default to **Playwright headless** on `ace-linux-1` (faster, reproducible in CI, no Windows dependency). Add a fallback note for the Windows CHM path in case JS rendering blocks specific pages. Capture decision in `scripts/ingest_orcina_help.py` module docstring.

2. **Add `playwright_scrape` subcommand to `scripts/ingest_orcina_help.py`.** Implement `def scrape_webhelp(base_url, out_dir, page_list)` that:
   - launches headless Chromium via `playwright.sync_api`
   - navigates to each page in the configured list, waits for `networkidle`, extracts `document.body.innerHTML`
   - writes raw HTML per page under `/tmp/orcina_html/<page-slug>.html`
   - reuses the existing `convert` step to produce markdown
   Pin Playwright version in `pyproject.toml` under a `[project.optional-dependencies] orcina-ingest` group so it stays opt-in.

3. **Curate the missing-page list.** New file `scripts/orcina_help_pages.yml` with `target` URL, `slug`, and target markdown filename for each of the 9 missing pages. Match slugs to the existing INDEX.md naming convention (`environment-sea-properties.md`, `environment-waves.md`, etc.).

4. **Run ingestion and review.** Execute `uv run python scripts/ingest_orcina_help.py playwright_scrape --pages scripts/orcina_help_pages.yml --out docs/domains/orcaflex/reference/`. Diff each new markdown file against the live page in a browser; confirm equation rendering, table structure, and inline image refs survived conversion. Strip Orcina nav chrome.

5. **Update INDEX and link.** Update `docs/domains/orcaflex/reference/INDEX.md` with the new pages grouped under "Environment" and "Vessels" sections. Add provenance footer to each new file: `Source: Orcina WebHelp <URL> retrieved <date>`.

## Acceptance Criteria

- [ ] All 9 missing pages exist as markdown under `docs/domains/orcaflex/reference/` with provenance footers
- [ ] `scripts/ingest_orcina_help.py` has a working `playwright_scrape` subcommand documented in `--help`
- [ ] `scripts/orcina_help_pages.yml` lists every scraped page with URL + slug + target filename
- [ ] `docs/domains/orcaflex/reference/INDEX.md` is updated and the new pages render without broken internal links
- [ ] Playwright is added as an optional install group, not a hard dependency

## Open questions

1. Is Playwright + Chromium acceptable on `ace-linux-1` (~300 MB browser binary) or should this run only on a build runner? If runner-only, mark the subcommand `# requires headed CI runner` in docstring.
2. Confirm Orcina ToS permits programmatic scraping of WebHelp; if not, fall back to the Windows CHM decompile path and run on `acma-ansys05`.
