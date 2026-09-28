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
- `appendices: list[Section]` — lettered A, B, C by position, unless a
  `Section.label` (e.g. `"Appendix D"`) is set; `report_pack` sets it so an
  explicit YAML letter survives.
- `tool_version: str` — defaults to `digitalmodel.__version__`.

Section keys and figure ids must be unique across sections and appendices.

## Registering a domain adapter (`digitalmodel.reporting.adapters`)

```python
from digitalmodel.reporting import ReportSpec, report_adapter

@report_adapter("cathodic_protection.anode_design")   # "<basename>.<kind>"
def anode_design_report(cfg: dict) -> ReportSpec:
    ...  # build sections/tables/figures from cfg["inputs"] / cfg["results"]
```

An adapter receives the whole routed cfg (`inputs`, the domain's results
block, `report`, `_config_file_path` ...) so it can echo inputs, name the
input file in its provenance and read document control itself.
`build_spec("cathodic_protection.anode_design", cfg)` runs the adapter;
when the key is not registered yet it first imports
`digitalmodel.<basename>.report_adapters` (the convention for where a
domain's adapters live).

`maybe_render_report(cfg, basename)` is the engine hook: it is a no-op unless
`cfg["report"]` is a mapping; then it builds the spec from the cfg, applies
`report.document` / `report.manifest` (YAML owns document control), and
writes the pack to `report.output_dir` (relative to the config file) using
`report.pdf` and `report.stem`. `digitalmodel.engine` calls it after every
domain arm when the routed cfg carries `report: {kind: ...}` (#2212 part 2);
legacy `report:` blocks without `kind` (for example the `artificial_lift`
example's `report: {html: true}`) are left to their domains.

## CP consumer (`digitalmodel.cathodic_protection.report_adapters`)

The first registered domain. Add a `report:` mapping to any
`basename: cathodic_protection` input and the engine writes the HTML/PDF pack,
citations sidecar and manifest beside the results:

```yaml
basename: cathodic_protection
inputs:
  calculation_type: DNV_RP_B401_offshore   # or DNV_RP_F103, ABS_gn_ships_2018, ABS_gn_offshore_2018
  ...                                       # the route's inputs (see tests/fixtures/cathodic_protection/workflow_inputs/)
report:
  kind: anode_design                        # adapter "cathodic_protection.anode_design"
  document:                                 # optional; placeholder X0000-CP-000-00 rev 00 when absent
    number: B0000-RPT-042-01
    revision: "01"
    title: Jacket CP anode design
    project: B0000
    client: Client
  pdf: auto                                 # auto | off | require
  output_dir: results                       # relative to this YAML
  stem: jacket_cp                           # optional; defaults to the basename
```

`cathodic_protection.anode_design` works for every engine-adapter route and lays out:

| Section | B401 offshore | F103 bracelet | ABS ships / offshore |
|---|---|---|---|
| Design basis | design data + zones table (areas, coating category, depth band, climate, a, b) | pipeline geometry, coating, exposure, temperature band, anode data | the route's design data and coating factors |
| Current demand | per-zone initial/mean/final densities and demands + `I(t) = A x i_mean x (a + b t)` line figure (`fig-cp-demand-vs-time`) | coating breakdown (linepipe + field joints) and mean/final demand | densities and demand by surface / phase |
| Anode requirements | mass, N_mass / N_initial / N_final / recommended + bar figure (`fig-cp-anode-counts`) | mass, N_mass / N_final / N, spacing, protected length + bar figure | mass and count |
| Adequacy | use status line, status (governing case + reason + use status), fresh vs depleted R / I table, checks | use status line, route status + `spacing <= 2 x protected length` status, R / I table | use status line, status (+ fresh vs depleted R / I for ships) |
| References | where each cited table was used; citation records go to *References cited* | same | note that the legacy solver carries no cited tables |

Standards chips come from the route's `standard` / `edition` / `provenance`;
citation records are rebuilt from the `code_id revision section` labels the
route emits (DNV-RP-B401 and DNV-RP-F103 wiki pages). The use status comes
from `results["status"]["use_status"]` (owner decision 2026-09-27, see
[cathodic_protection/_index.md](../cathodic_protection/_index.md#use-status)):
B401 offshore and F103 read "approved for client use subject to an
engineer-of-record check", the ABS routes "legacy solver with uncited tables;
not for client use without an independent check". Provenance names the
input YAML (config-dir relative, sha256) and the results mapping digest, so a
report is always a view over its inputs.

`cathodic_protection.assessment` takes a `CPAssessmentReport`
(`cp_reporting`) plus optional CIS survey points / `CISAnalysisResult` and a
`DepletionProfile`: summary + overall compliance status, compliance table
with `standard_reference`, potential-vs-distance figure
(`fig-cp-potential-vs-distance`) when survey points are given,
remaining-life table and remaining-mass-vs-years figure
(`fig-cp-remaining-mass`) when a profile is given, recommendations table.
`build_assessment_spec(report, cis_points=..., cis_result=..., depletion=...)`
is the typed entry; the registered adapter reads the same objects (or their
mappings) from `cfg["assessment"]`.

Goldens: `tests/reporting/golden/cp_anode_design_jacket.html` (plotly.js
stripped) from `tests/fixtures/reporting/cp_anode_design_result.json`, the
routed jacket cfg; `tests/cathodic_protection/test_report_adapters.py` checks
the fixture against a fresh run. The former
`visualization/reporting/cp_html_report.py` (CDN Plotly, fabricated
attenuation length, no importers) was deleted.

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
- `report_pack` (YAML-driven document packs) is a front end of this engine
  (#2212 part 3): `report_pack.workflow.build_report_spec` maps the validated
  YAML onto a `ReportSpec` and the pack's HTML is `render_html(spec)`, its PDF
  `render_pdf(...)`. The pack keeps its own markdown body, citations sidecar
  and manifests. See [standard-report-pack.md](standard-report-pack.md).
- `examples/reporting/cp-anode-design-dnv-rp-b401.{html,yaml}` (a CDN KaTeX
  calc sheet with no generator) was removed; CP reports come from the engine.
