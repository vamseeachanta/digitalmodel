# Standard report pack (`basename: report_pack`)

A routed workflow that renders a complete engineering report pack following a
**standard marine engineering report structure**, as used across classification
and regulatory deliverables in marine/offshore consulting practice. The
structure is generic and carries no client- or project-specific content; job
numbers and parties are caller-supplied placeholders in the YAML config.

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
  emitted to a `*_citations.json` sidecar.
- **Provenance manifest** — `report-layer-manifest.json` declaring: driving
  issue (and optional parent issue), project, artifact class, privacy
  classification, publishability decision, input source IDs, execution tool +
  version, compute environment, source-artifact pointers (licensed material is
  pointed to, never embedded), raw/final output paths, the citation sidecar,
  the generated file manifest, and the full pack file list. Required fields are
  validated fail-closed; a pack without complete provenance is not emitted.

## Outputs

For an input config `<stem>.yml` with `output_dir` set:

```text
<stem>_report.md            # primary report body
<stem>_report.html          # self-contained derivative (inline CSS, no CDN)
<stem>_report.pdf           # optional limited derivative of the HTML source
<stem>_citations.json       # citation sidecar
<stem>_manifest.json        # generated file manifest
report-layer-manifest.json  # provenance manifest
```

Markdown/HTML are the primary artifacts; the PDF is a limited derivative of
the approved HTML source.

## PDF rendering (optional, fail-soft)

`pdf: auto` (default) tries, in order:

1. Playwright/Chromium (`pip install playwright && playwright install chromium`)
2. Microsoft Edge headless — the documented Windows fallback:
   `msedge --headless --print-to-pdf=<out> <file-url>` (Edge is looked up on
   PATH and in the default `Program Files` locations)
3. Chrome/Chromium headless with the same flags

If none is available the md/html pack is still complete and `pdf_status` in
the file manifest carries the exact reason and remediation. `pdf: require`
turns a missing renderer into a hard error; `pdf: off` skips the attempt.

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

Config schema and field-level docs: module docstring of
`src/digitalmodel/report_pack/workflow.py`. Tests: `tests/report_pack/`.
