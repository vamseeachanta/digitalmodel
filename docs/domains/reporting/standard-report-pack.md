# Standard report pack (`basename: report_pack`)

A routed workflow that renders a complete engineering report pack following a
**standard marine engineering report structure**, as used across classification
and regulatory deliverables in marine/offshore consulting practice. The
structure is generic and carries no client- or project-specific content; job
numbers and parties are caller-supplied placeholders in the YAML config.

Since #2212 (part 3) `report_pack` is the **YAML front end** of the
[standard report engine](standard-report-engine.md): it validates the config,
builds a `ReportSpec` and lets `digitalmodel.reporting.engine.render_html` and
`digitalmodel.reporting.pdf.render_pdf` produce the HTML and PDF. Only the
markdown body, the citations sidecar and the two manifests are written by the
workflow itself, so a pack looks identical to every other engine deliverable
(header chips, title block, references, data sources, input echo, footer).

## The standard structure

- **Document numbering** — `JOB-DOCTYPE-SEQ-REV` (e.g. `B0000-RPT-001-00`),
  with the trailing two digits as the revision, restated as `Rev N` in the
  title block. The workflow validates the pattern and rejects a document
  number whose revision suffix disagrees with the declared revision.
- **Title / revision block** — document number, revision, title, project,
  client, optional date, prepared/checked/approved rows, and a revision-history
  table (rev, date, description, prepared/checked/approved).
- **Section skeleton** — ordered, numbered sections. The conventional order is
  introduction/scope, references and traceability, design data/basis,
  methodology and assumptions (including a worked sample calculation), results
  (with stated case coverage), conclusions and limitations. Sections are
  caller-defined so domain workflows can adapt the skeleton; results tables and
  figures declared under `results:` are injected into the first section whose
  title contains "result".
- **Lettered appendices** — capital letters (`A`, `B`, ...; explicit or
  auto-assigned), each a titled block of content and/or a listed file set.
  Conventional roles: reference documents, calculation records,
  qualifications/CV.
- **Citations** — every external code/standard is a structured citation
  (`code_id`, `publisher`, `revision`, `section`, `wiki_path`, `note`), reusing
  `digitalmodel.citations.schema.Citation` for structural validation, and
  emitted to a `*_citations.json` sidecar. Each distinct `code_id` also becomes
  a standard/edition chip (`StandardLabel`) in the HTML header and footer, with
  the citation's `revision` as the edition and its `wiki_path` as provenance.
- **Provenance manifest** — `report-layer-manifest.json` declaring: driving
  issue (and optional parent issue), project, artifact class, privacy
  classification, publishability decision, input source IDs, execution tool +
  version, compute environment, source-artifact pointers (licensed material is
  pointed to, never embedded), raw/final output paths, the citation sidecar,
  the generated file manifest, and the full pack file list. Required fields are
  validated fail-closed; a pack without complete provenance is not emitted.

## How the YAML maps onto the engine spec

`build_report_spec(load_pack(cfg))` (`src/digitalmodel/report_pack/workflow.py`)
is the whole mapping; the HTML written by the workflow is byte-for-byte
`render_html(spec)` (asserted in `tests/report_pack/test_report_pack.py`).

| YAML | `ReportSpec` |
|---|---|
| `document` (+ `revision_history`) | `DocumentMeta` / `RevisionRow` |
| `citations[]` | `citations` (sidecar) and one `StandardLabel` per distinct `code_id` |
| `sections[]` | `Section(key="section-<n>")` with a `TextBlock` (markdown) |
| `results.tables[]` | `TableBlock` per CSV (header row = columns; `units_row: true` reads the second CSV row as units, or `units: [...]` gives them inline), appended to every section whose title contains "result" |
| `results.figures[]` | `FigureBlock`: `.svg` is inlined as markup (XML prolog dropped, scripts rejected); `.png/.jpg/.gif` are embedded as data URIs |
| `appendices[]` | `Section(key="appendix-<letter>", label="Appendix <letter>")`; content and/or the `files` list as bullets |
| `manifest.input_source_ids`, `manifest.source_artifacts`, every CSV/figure read | `Provenance` data sources (files carry a `sha256:` content digest, CRLF-normalised); the engine refuses to render with none |
| `manifest` | `ReportSpec.manifest` (same required fields as the report-layer contract) |
| `pdf`, table/figure identifiers, manifest source fields | `input_echo` (portable identifiers only, never host paths) |

## Outputs

For an input config `<stem>.yml` with `output_dir` set:

```text
<stem>_report.md            # primary report body
<stem>_report.html          # engine-rendered, self-contained (inline CSS, no CDN)
<stem>_report.pdf           # optional limited derivative of the HTML source
<stem>_citations.json       # citation sidecar
<stem>_manifest.json        # generated file manifest
report-layer-manifest.json  # provenance manifest
```

Markdown/HTML are the primary artifacts; the PDF is a limited derivative of
the approved HTML source. Packs without Plotly figures contain no `<script>`
at all; the engine inlines plotly.js only when a figure carries a Plotly
payload (report_pack never does today).

## PDF rendering (optional, fail-soft)

The chain is `digitalmodel.reporting.pdf.render_pdf`. `pdf: auto` (default)
tries, in order:

1. Playwright/Chromium (`pip install playwright && playwright install chromium`)
2. Microsoft Edge headless — the documented Windows fallback:
   `msedge --headless --print-to-pdf=<out> <file-url>` (Edge is looked up on
   PATH and in the default `Program Files` locations)
3. Chrome/Chromium headless with the same flags

If none is available the md/html pack is still complete and `pdf_status` in
the file manifest carries the exact reason and remediation. `pdf: require`
turns a missing renderer into a hard error (`PdfRenderError`); `pdf: off`
skips the attempt (`pdf_status` = `pdf rendering disabled (pdf: off)`).

## Determinism

The workflow generates no timestamps — dates appear only if supplied in the
config — so re-running an unchanged config yields byte-identical md, html and
JSON outputs. `compute_environment` in the manifest is caller-supplied
(default `not-recorded`) for the same reason.

## Usage

```bash
python -m digitalmodel examples/workflows/report-pack/input.yml
```

See `examples/workflows/report-pack/` for a complete synthetic example: a
spectral-fatigue screening report whose results table matches the
`fatg_spectral_fatigue` workflow's per-sea-state damage output columns.
`examples/workflows/hull-girder-report-pack/` (inline SVG margin plot) and
`examples/workflows/foam-system-report-pack/` (three CSV tables) run the same
path through the engine end to end.

Config schema and field-level docs: module docstring of
`src/digitalmodel/report_pack/workflow.py`. Tests: `tests/report_pack/`.

## Wide tables and a worked layout example

Every standard HTML table has a separate `.tbl-wrap` with `overflow-x: auto`
and `max-width: 100%`. Result and appendix tables retain their columns, units,
values and source text; document-control tables use the same containment.
Wide tables scroll horizontally inside their block. A narrow table does not
acquire a horizontal scrollbar when it fits the available width.

The method follows [W3C CSS Overflow Level 3, section 3.1](https://www.w3.org/TR/2025/WD-css-overflow-3-20251007/#overflow-properties)
(7 October 2025 working draft): `auto` provides scrolling when content overflows.
The containing-block width supplies the percentage limit; no engineering
coefficient or acceptance limit is introduced.

Worked example inputs are a synthetic CSV with thirty columns named
`result_0` through `result_29` and one row containing `value` in each column.
The column count and desktop/mobile viewports follow
[issue 1625](https://github.com/vamseeachanta/digitalmodel/issues/1625).
To construct this input without introducing engineering data:

```python
import csv
from pathlib import Path

csv_path = Path("wide-results.csv")
with csv_path.open("w", newline="", encoding="utf-8") as stream:
    writer = csv.writer(stream)
    writer.writerow([f"result_{index}" for index in range(30)])
    writer.writerow(["value"] * 30)
```

Declare the file under the pack's results, relative to the input YAML:

```yaml
results:
  tables:
    - title: Synthetic wide results
      csv: wide-results.csv
```

Method: render the pack through the existing workflow, then measure the document
and the result table's wrapper in a browser. The expected answer is
`documentElement.scrollWidth - documentElement.clientWidth == 0`, while the
wide table has `wrapper.scrollWidth > wrapper.clientWidth`. Scrolling the
wrapper to its end exposes `result_29` and its unchanged value. This checks
containment rather than hiding overflow on the page.

The automated worked case also covers a narrow CSV, an unbroken cell, and the
same table in an engine appendix at the issue's desktop and mobile viewport
sizes, with screen and print CSS:

```bash
PYTHONPATH=src uv run python -m pytest tests/report_pack/test_report_pack.py -k 'scroll_container or scroll_locally'
```

Browser checks require Playwright and an installed Chromium, Chrome or Edge;
their absence is reported as a skip. Print emulation checks HTML layout only;
pagination of all wide-table columns in PDF is not established. Standard-chip
masthead wrapping is a separate limitation: a long chip can still widen a mobile
page independently of its tables. The synthetic layout case has no standard
chips so that it isolates table containment.
