# Handoff: atlas visualization import

## Goal

Import of the OCIMF visualization module and the FPSO atlas import chain will
perform no output filesystem mutation. Explicit chart calls will accept caller
input and output paths while preserving numerical methods.

## Current state

The plan is posted on [the issue](https://github.com/vamseeachanta/digitalmodel/issues/2071#issuecomment-6132099969).
The chart module defers dependencies, path resolution and directory creation to
explicit calls. Plot styling is scoped to each chart call. The visualization
package loads its two public submodules on demand.

Baseline verification passed 21 atlas tests. The existing visualization import
suite skipped 29 tests under its unchanged stale-layout marker. Ten new tests
failed on baseline for the directory mutation and missing path parameters.
The corrected atlas import test also failed on baseline for the original mkdir.
Final verification passed 31 tests; 29 legacy tests retained their existing skip.
All 159 atlas file digests and the seven scalar values in the FPSO snapshot
are unchanged from baseline.

## Next steps

1. The lane runner will stage only the four files named in its finish manifest,
   commit with repository hooks enabled, and open a draft PR referencing the issue.
2. Claude will review the posted plan and the final change before owner merge.
3. The project manager will place the requirement below in the appropriate
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
That execution behavior is outside this change. Parent profiling, extraction and
validation modules also retain existing plotting configuration; direct module
import isolates the OCIMF configuration check.

The legacy visualization tests retain their skip marker. No existing test,
atlas source, atlas data, dependency, workflow or test domain map was changed.

## Open claims to release

The GitHub-backed claim key is `issue/2071`. The claim was released; helper readback confirmed the key is available.

## How to resume

The lane runner will read the finish manifest and PR body. Any failed check will
require resolution before publication. The worktree edits remain uncommitted
under the job finish rule.
