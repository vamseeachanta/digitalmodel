# Plan: standard HTML/PDF reporting engine, CP first consumer (#2212)

> Issue: #2212 | Epic: #2206 | Owner decision: D9 (2026-09-25: "a comprehensive code/html that will handle reporting in a standard way"; D5: end deliverables become HTML/PDF, demos retired)
> Status: plan posted on #2212 for the owner's scope decision; PR1 (engine only) proceeds under the recommended option B because it is needed under either option.

## Why
Three unrelated report engines exist and 86 hand-rolled HTML writers sit beside them (`grep -rliE "<html|<!doctype" src scripts --include=*.py | wc -l`):
1. `src/digitalmodel/report_pack/workflow.py` (974 lines): YAML-driven client pack with document numbering (`DOC_NUMBER_RE`), a `Citation` sidecar, a report-layer manifest and a fail-soft PDF chain Playwright → Edge → Chrome; but HTML by string concatenation, no template, no Plotly, no print CSS, content only from CSV/markdown.
2. `src/digitalmodel/reporting/` (origin/main): typed pydantic block library (`_base.py`, `_backbone.py`, `provenance.py`, `skeleton.py`) and `calc_report.py` with a vendored template `assets/calc_report_format.html` (masthead, footer, provenance colours) and print CSS; no PDF, free-text references instead of `Citation`, no edition label, no Plotly. `ReportRenderer.render_html` already exposes a `plotlyjs_src` hook.
3. `src/digitalmodel/visualization/reporting/cp_html_report.py`: Plotly from CDN, `datetime.now()` in filenames, a fabricated attenuation length; its only importers reference a module that does not exist.

## Scope decision for the owner
- **(A) Extend `report_pack` into the engine.** Pro: document number/revision, citations sidecar, manifest, PDF chain and determinism tests exist. Con: input is YAML + CSV + markdown, not typed result models (the hull_girder demo needed a 263-line data generator to feed it); its tests assert no `<script>` in output, which rules out embedded Plotly; renderer is not a template.
- **(B) Engine in the existing `digitalmodel.reporting` package; `report_pack` delegates to it.** Pro: the typed-model, template, print-CSS and provenance pieces already live there; registered in `docs/registry/module-routing.yaml`. Con: PDF and citations/manifest code moves out of `report_pack` (mechanical).
- **Recommendation: B.** `report_pack` stays the YAML front-end for document-controlled packs and calls the engine for HTML/PDF.

## Architecture (option B)
- **Contract** `reporting/spec.py`: `DocumentMeta` (number, revision, title, project, client, prepared/checked/approved, revision history), `StandardLabel(code_id, edition, provenance)` list, `citations: list[Citation]`, `sections: list[Section]` of `TextBlock` / `TableBlock(columns, units, rows, source)` / `FigureBlock(plotly | svg | image)` / `StatusBlock(PASS|FAIL, governing_case, detail)`, `provenance` (reuse `reporting/provenance.py`), `manifest`, `input_echo`, `appendices`. No timestamps unless supplied (determinism).
- **Template** `reporting/assets/standard_report.html.j2` (Jinja2, already a runtime dependency): header with document number/revision and standard+edition chips, title block, sections, "References cited", "Data sources", "Input echo", footer with PDF status; reuses the calc-report CSS and print CSS so calc and design reports look identical.
- **Plotly, CDN-free**: per-figure `to_html(full_html=False, include_plotlyjs=False)` plus one inline `plotly.offline.get_plotlyjs()` script only when a figure exists; JSON serialised with sorted keys for byte-stable output.
- **PDF** `reporting/pdf.py`: the report_pack chain moved verbatim, plus `--virtual-time-budget` so Plotly draws before print; modes `auto | off | require`; `auto` records "PDF not rendered" in the manifest instead of failing. No new installs.
- **Engine** `reporting/engine.py`: `render_html(spec)`, `write_report(spec, out_dir, stem, pdf)` → HTML, PDF or status, citations JSON, manifest.
- **Domain registration** `reporting/adapters.py`: `@report_adapter("<basename>.<kind>")` registry; one engine hook after router dispatch renders when `cfg["report"]` is present.
- **CP first consumer** (PR2, after #2210): `cathodic_protection/report_adapters.py` for anode design (demand vs time, mass vs count, adequacy with governing case, input echo) and assessment (compliance table, potential vs distance, remaining mass vs years).
- **Second consumer** (PR3): report_pack builds a spec from its YAML and calls the engine; hull_girder demo through the engine.

## Freshness / CI
Deliverables are not committed (`results/`, `reports/` are gitignored). `scripts/check_generated_html.py` stays scoped to `docs/api/**`. Golden HTML snapshots under `tests/reporting/golden/` with the inline plotly script and whitespace normalised. Delete `examples/reporting/cp-anode-design-dnv-rp-b401.{html,yaml}` (CDN KaTeX, no generator).

## Files
Create: `reporting/spec.py`, `engine.py`, `pdf.py`, `figures.py`, `adapters.py`, `assets/standard_report.html.j2`; tests `tests/reporting/test_spec.py`, `test_engine_golden.py`, `test_pdf.py`, `tests/reporting/golden/*.html`; `docs/domains/reporting/standard-report-engine.md`. PR2 adds `cathodic_protection/report_adapters.py`, `tests/cathodic_protection/test_report_adapters.py`, a CP result fixture, the engine hook, and deletes `cp_html_report.py`. PR3 modifies `report_pack/workflow.py` to delegate.

## TDD
Contract: bad document number or revision mismatch rejected; missing provenance raises; `StatusBlock` FAIL requires a governing case. Rendering: golden snapshot byte-equal (no Plotly); golden with Plotly after stripping the inline script; `<script src=` never present; `@media print` present; standard/edition chips in header and footer; input echo present; two runs byte-identical. PDF: `off` → "disabled"; `auto` without a browser → "PDF not rendered" and artifacts still complete; real render skipped when no Edge/Chrome, else asserts `%PDF`.

## Risks
CP result shape changes under #2210 (PR2 targets post-#2210 keys). ~3.6 MB inline plotly.js per HTML. Edge print timing (virtual-time budget). Two citation types (`Reference` vs `Citation`) until calc_report converges. `HTML_REPORTING_STANDARDS.md` forbids static images; hull_girder SVG stays on the legacy path.

## Order of work
1. PR1 engine (this branch): spec, template, figures, pdf (moved), engine, adapters registry, tests; report_pack imports `reporting.pdf` unchanged in behaviour.
2. PR2 CP consumer, after #2210.
3. PR3 report_pack delegation and hull_girder demo through the engine.
