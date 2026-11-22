# Demonstration pack retirement regression coverage

The May demonstration packs have been retired. Their digests and dispositions
remain in the [generated retirement register](../../config/generated_retirements.yml).
The demonstration report interface remains available for internal screening.
This guide describes the regression boundary required by issue #2688.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-19 | Document nested-file promotion checks and report compatibility so retirement can be checked without regenerating engineering results. |

## Scope and method

The [retirement suite](../../tests/reporting/test_gtm_pack_retirement.py) reads
every file recursively under `examples/demos/gtm`, including nested output,
media, source, configuration and binary files. It compares lowercase bytes
against retired funding markers, the technical-demo sales action and the
contact-link scheme. Findings identify relative file paths. File extensions,
text decoding and Git tracking do not restrict this demonstration-tree scan.

The repository-wide reference check separately permits retired pack references
only in the register and the three historical archive snapshots named there.
Those snapshots record history and remain unchanged. The repository-wide funding
marker check retains its existing scope; the contact-link check applies to the
demonstration tree.

Report checks exercise both `add_live_mode_teaser()` and
`add_live_mode_teaser("installation")`. Both calls must retain methodology and
results sections, add the internal screening notice and omit the sales action.
The stored reports and existing wave-period qualification checks remain covered.

## Worked example

Inputs: a temporary demo tree with a method note at `output/nested/report.html`.
The fixture contains no retired marker. The scan returns normally.

Method: replace that note with the retired contact-link scheme in uppercase,
surrounded by null and non-UTF-8 bytes. Run the same recursive scan. Bytewise,
case-insensitive matching must still find the scheme.

Answer: the scan raises an assertion identifying `output/nested/report.html`.
The negative-control test catches that assertion; a successful test therefore
proves that contaminated content is rejected. It also checks funding and sales
markers in nested HTML, CSS and binary fixtures. Expected findings come directly
from the injected marker and its known relative path, independently of the scan.

With the test dependencies available, run from the repository root:

```bash
python -m pytest tests/reporting/test_gtm_pack_retirement.py \
  -k demo_tree_scan_rejects_nested_promotional_remnants -q -p no:cacheprovider
```

The example has twelve cases: four marker types crossed with three file paths.
All temporary trees are created through pytest's `tmp_path` fixture; no retired
artifact, external data or engineering solver is required.

## Sources and limits

Issue #2688 defines the retirement and complete-tree regression requirement.
The register records the retirement evidence and historical-reference exception;
`examples/demos/gtm/report_template.py` defines the retained report interface.
The tests establish these software boundaries without introducing equations,
engineering coefficients or acceptance limits. They do not authenticate remote
archive availability or validate the underlying engineering calculations.
