# Multi-output generator destination audit

Report-pack output, the wall-thickness explorer and cross-section reports now
validate their selected destinations before writing. Colliding names, resolved
symlinks, hard links and case-only aliases raise `ValueError`. Report-pack links
are relative to the document that contains them; explorer sidecars can be placed
in a separate directory. No engineering calculations or acceptance limits change.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-22 | Add destination preflight, artifact links and disposable examples to prevent overwritten outputs and broken links. |

## Audit coverage

Issue #2315 describes an archive summary/detail collision caused by replacing
suffix text throughout a filename and by recording only a detail basename.
The linked archive changes are context; this audit does not alter archive data
or either linked pull request.

| Writer | Finding and resulting behavior | Regression coverage |
| --- | --- | --- |
| `report_pack.workflow.router` | Missing bundle preflight; basename-only manifest and Markdown figure links. All selected outputs are checked, including both manifests and enabled PDF. Each link is relative to its containing document's resolved directory. | Output aliases, disabled PDF, extensionless configuration name, suffix text in directory names, relocated manifests, figures with URL-reserved characters. |
| `scripts/capabilities/build_wall_thickness_3d_page.py` | A `.json` page destination collided with its inferred sidecar. Source/output aliases could overwrite the study. Preflight now protects the source and both outputs; `--sidecar` supports a separate destination. | Extensionless output, suffix text in directory names, explicit collisions, source aliases, cross-directory links and relocated page. |
| `subsea.cross_sections.reporting.write_report_files` | Missing bundle preflight; fixed `<name>.tmp` could overwrite and delete unrelated data. Selected outputs are checked before model loading. Temporary files are created exclusively and only that invocation's temporary file is cleaned up. | Identical names, symlink and hard-link aliases, dangling symlinks, unselected formats, preservation of an existing `.tmp` file. |
| `reporting.engine.write_report` | Destination guard already exists, but a relocated manifest retained basename-only links. Links now use the manifest's resolved directory. | Relocated manifest plus existing output collision regressions. |
| `visualization.reporting.exporters.export_all_formats`, `subsea.catenary_riser.report.generate_riser_report` | Destination guards already exist on the audited base. Retained without refactoring. | Existing destination regressions and helper tests remain applicable. |

Canonical identities and relative paths follow Python 3.11
[`Path.resolve` and `Path.samefile`](https://docs.python.org/3.11/library/pathlib.html#pathlib.Path.resolve)
and [`os.path.relpath`](https://docs.python.org/3.11/library/os.path.html#os.path.relpath).
Exclusive temporary creation follows
[`NamedTemporaryFile`](https://docs.python.org/3.11/library/tempfile.html#tempfile.NamedTemporaryFile).
These are filesystem rules; no empirical coefficients or numerical tolerances
are introduced. HTML/Markdown links percent-encode reserved filename characters;
JSON manifests retain ordinary filesystem paths. The explorer accepts relative
local JSON paths and rejects absolute, network and script-bearing URLs.

## Worked example

Input: an extensionless configuration named `analysis`, an output directory
`reports.html.json`, and a synthetic SVG at `inputs/figure.svg`. PDF is disabled.
The empty SVG tests transport only and contains no engineering result.

Method: enumerate the report-pack destinations; resolve each path; reject any
pair that identifies the same file; form each artifact path relative to the
containing document. Independently, moving from `reports.html.json` up to the
example root and down into `inputs` gives `../inputs/figure.svg`. A separate
rejection case makes the Markdown and HTML output names hard links to the input.
SHA-256 before and after rejection must agree.

Run from the repository root, using a new directory under the configured scratch
directory and an environment containing the reporting dependencies:

```bash
PYTHONPATH=src python examples/reporting/output_destinations.py \
  --output-dir "$TMPDIR/output-destination-example"
```

Answer:

```json
{
  "figure_link": "../inputs/figure.svg",
  "collision_rejected": true,
  "retained_bytes_unchanged": true
}
```

`tests/report_pack/test_output_example.py` runs this example. Destination tests
enumerate expected paths independently and compare retained input hashes.

## Limits and further audit

Preflight does not lock the filesystem or provide a transaction across all
outputs. Concurrent path changes and failures after writing begins remain
possible. Distinct existing output files retain each writer's replacement
policy. Hard links are checked only when they exist. Case-only destinations are
rejected conservatively even on case-sensitive filesystems. Relative links
across different filesystem drives are unsupported and raise before writing.
Moving a Markdown report without its referenced figures can still break links.
Symlinked artifacts must be served at their resolved locations for the generated
relative links to work; arbitrary web-server alias mappings are outside scope.

This is a bounded audit of the listed writers, not a claim that every repository
generator has been certified. Further candidates include the galvanic explorer,
the hull-library export families and domain-specific report scripts. No licensed
solver was started or queried.
