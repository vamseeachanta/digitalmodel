# Standard report engine (`digitalmodel.reporting`)

One template, one CSS, one PDF chain for every domain deliverable (#2212,
epic #2206). A domain fills a typed `ReportSpec`; the engine renders a
self-contained HTML document (Plotly inlined, no CDN, print CSS) and, best
effort, a PDF derivative. Domains never write HTML.

## What it produces

`write_report(spec, out_dir, stem, pdf="auto")` writes, under `out_dir`:

| File | Content |
|---|---|
| `<stem>.html` | Self-contained report: header (document number, revision, standard + edition chips), title block with sign-off and revision history, contents, numbered sections, **References cited**, **Data sources**, **Input echo**, lettered appendices, footer (project, client, engine version, PDF status). |
| `<stem>.pdf` | Only when a renderer succeeded (see PDF modes). |
| `<stem>_citations.json` | The `Citation` sidecar (`{"citations": [...]}`), same shape as `report_pack`. |
| `<stem>_manifest.json` | Artifact names, document control, standards, citation count, PDF status, tool version, optional report-layer manifest. Sorted keys, no timestamp. |

`render_html(spec)` returns the HTML string without touching disk.

## The spec (`digitalmodel.reporting.spec`)

- `DocumentMeta(number, revision, title, project, client, date=None, prepared_by, checked_by, approved_by, revision_history)` — `number` must match `JOB-DOCTYPE-SEQ-REV` (`DOC_NUMBER_RE`, shared with `report_pack`) and end in `-<revision>`; the revision history defaults to one "Issued" row built from the meta.
- `StandardLabel(code_id, edition, provenance)` — rendered as chips in header and footer so the edition a check was made against is never implicit.
- `citations: list[Citation]` — `digitalmodel.citations.schema.Citation` dataclasses (or mappings); validated through the citation schema and stored as dicts.
- `sections: list[Section(key, title, subtitle, blocks)]` where a block is one of:
  - `TextBlock(markdown)` — paragraphs, `#`/`##` headings, bullet and numbered lists, fenced code, `**bold**`, `*em*`, `` `code` `` (escaped; no raw HTML).
  - `TableBlock(title, columns, units, rows, source)` — units and every row must match the column count.
  - `FigureBlock(title, caption, plotly | svg | image_path, figure_id)` — one payload required; `figure_id` is the HTML id.
  - `StatusBlock(label, status="PASS"|"FAIL", governing_case, detail)` — a FAIL must name its governing case.
- `provenance: Provenance` (from `reporting/provenance.py`) — at least one `DataSource` is required at render time (`ProvenanceError` otherwise).
- `manifest: dict` — when non-empty, validated against `MANIFEST_REQUIRED_FIELDS` (the report-layer contract).
- `input_echo: dict` — flattened to dotted keys and rendered as a collapsible table.
- `appendices: list[Section]` — lettered A, B, C.
- `tool_version: str` — defaults to `digitalmodel.__version__`.

Section keys and figure ids must be unique across sections and appendices.

## Registering a domain adapter (`digitalmodel.reporting.adapters`)

```python
from digitalmodel.reporting import ReportSpec, report_adapter

@report_adapter("cathodic_protection.anode_design")   # "<basename>.<kind>"
def anode_design_report(results: dict) -> ReportSpec:
    ...  # build sections/tables/figures from the results dict
```

`build_spec("cathodic_protection.anode_design", results)` runs the adapter.
`maybe_render_report(cfg, basename)` is the engine hook: it is a no-op unless
`cfg["report"]` is a mapping with `kind`; then it builds the spec from
`cfg[basename]`, applies `report.document` / `report.manifest` (YAML owns
document control), and writes the pack to `report.output_dir` (relative to
the config file) using `report.pdf` and `report.stem`. Wiring the hook into
the router is PR2.

Figures: `figure_from_columns("line"|"bar", x, {"series": ys}, title=..., x_label=..., y_label=...)`
returns a plain Plotly figure dict, so adapters need no plotly import.

## PDF modes (`digitalmodel.reporting.pdf`)

`render_pdf(html_path, pdf_path, mode) -> PdfStatus(rendered, engine, message)`.
Chain: Playwright/Chromium, then Microsoft Edge headless
(`msedge --headless --print-to-pdf`, the Windows fallback, also found at its
default install path), then Chrome/Chromium headless. The browser CLI runs with
`--no-pdf-header-footer --virtual-time-budget=10000` so Plotly has drawn before
print. No new installs are required.

- `auto` (default): try; on failure the HTML pack is complete and the
  manifest records "PDF not rendered ..." with what was tried.
- `off`: skip; status "pdf rendering disabled".
- `require`: raise `PdfRenderError` when nothing rendered.

## Determinism rules

- Nothing in the engine reads the clock; dates appear only when supplied.
- Plotly JSON is canonicalised (`PlotlyJSONEncoder`, sorted keys) and each
  figure div uses the caller's `figure_id`; two renders are byte-identical.
- plotly.js (~3.6 MB) is inlined once, in `<head>`, only when a figure carries
  a Plotly payload; there is never a `<script src=`.
- JSON sidecars are written with sorted keys and `\n` line endings.
- Goldens: `tests/reporting/golden/*.html` (plain, and Plotly with the inline
  script stripped). Regenerate with `UPDATE_GOLDENS=1 pytest tests/reporting/test_engine_golden.py`.

## Relationship to the other report paths

- `reporting.calc_report` (house calculation report) supplies the CSS and print
  CSS this engine reuses, so calc and design reports look identical.
- `report_pack` (YAML-driven document packs) already imports the PDF chain and
  document constants from here; delegating its HTML rendering to the engine is PR3.
