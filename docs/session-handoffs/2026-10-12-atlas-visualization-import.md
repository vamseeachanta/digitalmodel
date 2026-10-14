# Handoff: atlas visualization import

## Goal

Import of the OCIMF visualization module and the FPSO atlas import chain performs
no output filesystem mutation. Explicit chart calls accept caller input and
output paths while preserving numerical methods.

## Current state

The plan is posted on [the issue](https://github.com/vamseeachanta/digitalmodel/issues/2071#issuecomment-6132099969).
The chart module defers dependencies, path resolution and directory creation to
explicit calls. Plot styling is scoped to each chart call. The visualization
package loads its two public submodules on demand.

Baseline verification passed 21 atlas tests. The existing visualization import
suite skipped 29 tests under its unchanged stale-layout marker. Ten new tests
failed on baseline for the directory mutation and missing path parameters.
The corrected atlas import test also failed on baseline for the original mkdir.
Initial verification passed 31 tests; 29 legacy tests retained their existing skip.
Fix-up verification passed 33 tests: 12 chart tests and 21 atlas tests. The two
added routing tests failed before the fix; all ten prior chart tests still pass.
All 159 atlas file digests and the seven scalar values in the FPSO snapshot
are unchanged from baseline.

## Next steps

1. The publishing step created [the draft PR](https://github.com/vamseeachanta/digitalmodel/pull/2420).
   The second-provider review passed with nits; the fix-up addresses output
   defaults, wording and report encoding. Review of the new head remains required.
2. Library output defaults to `ocimf_charts` below the current working directory
   at call time. Command-line output remains `docs/charts/phase2/ocimf`.
3. The requirement below remains to be placed in the appropriate
   contributor coding-standards document; the inspected contributor document
   covers release authorization, so that acceptance criterion remains NOT met.

Import of reusable computational APIs must not perform output filesystem mutations.

## Key files and commands

- `src/digitalmodel/marine_ops/marine_analysis/visualization/ocimf_charts.py`
- `src/digitalmodel/marine_ops/marine_analysis/visualization/__init__.py`
- `tests/marine_ops/marine_analysis/test_ocimf_charts.py`
- Verification: `PYTHONPATH=src uv run --no-sources python -m pytest tests/marine_ops/marine_analysis/test_ocimf_charts.py tests/parametric/test_atlas.py tests/test_module_reorganization.py -q`.

## Gotchas

The original fixed drive default was already replaced at baseline, but import
still attempted mkdir and plotting setup. The new atlas subprocess test imports
the workflow without executing the response: existing response execution creates
a sample coefficient CSV on first use despite its no-file-side-effects docstring.
That execution behavior is tracked separately in
[the response-output issue](https://github.com/vamseeachanta/digitalmodel/issues/2442). Parent profiling, extraction and
validation modules also retain existing plotting configuration; direct module
import isolates the OCIMF configuration check.

The legacy visualization tests retain their skip marker. The report-read test
now specifies UTF-8; the existing import probe remains unchanged. No atlas source,
atlas data, dependency, workflow or test domain map was changed.

## Open claims to release

The GitHub-backed claim key is `issue/2071`; claims are released at closeout.

## How to resume

The PR remains draft. The new head requires second-provider review before owner
merge. The contributor-standard acceptance criterion remains open; the response
output defect is tracked separately. Verification results and publication state
are recorded in the PR body and fix-up comment.
