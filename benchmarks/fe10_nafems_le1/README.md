# FE10 elliptic membrane and constant-stress patch benchmark

[RESULT.md](RESULT.md) records four measured MYSTRAN solves and the unchanged
comparison criteria. The fine LE1 mesh and all patch quantities pass; the coarse
LE1 mesh remains outside the 5.0% band. No licensed software was used.

## Package

- `inputs/`: three LE1 meshes and the five-element distorted patch, unchanged
  from the received package.
- `expected.json`, `expected.md`, `sources/`: reference basis, analytic patch
  solution and citation-only public definition record.
- `compare.py`: supplied offline comparator, with unchanged numerical criteria.
- `results-*.csv`: measured global centroid stresses at the mid-surface.
- `native/`: compact native centroid fibre fields and sanitized ERR diagnostics.
- `console/`: command output, including expected failing red tests and the
  out-of-band coarse comparison.
- `record.json`, `manifest.json`: measured outcomes and SHA-256 provenance.
- `preflight/`, `run-requests/`: historical preparation records; pending statements
  describe the preparation date, superseded by the measured record.

Two excerpts without asserted reuse rights were omitted:
`sources/feenox-nafems-le1-excerpt.txt` and
`sources/featool-ex_planestress5-excerpt.txt`. No file exceeded 1 MB. Public
reference citations and the numerical definition remain available; omitted
excerpts are not required for offline replay.
The upstream implementation excerpt is also omitted; `sources/` retains its
revision, blob and numerical citation without redistributing upstream code.

## Offline replay

From this directory:

```sh
python -I compare.py --le1 results-le1_8x24.csv
python -I compare.py --le1 results-le1_16x48.csv
python -I compare.py --le1 results-le1_32x96.csv
python -I compare.py --patch results-patch.csv
python -I compare.py --selftest
```

The coarse comparison returns 1 by design of the recorded verdict. The other
comparisons and selftest return 0. From the repository root, run:

```sh
python -m pytest tests/solvers/mystran/test_fe10_benchmark.py -q -p no:cacheprovider
```

Solver installation is unnecessary for these tests. The tests replay retained
native fibre fields through the extractor, check all element IDs and digests,
and verify that doubled fine-mesh and patch stresses return numerical FAIL.

## Native execution and recovery

`tools/validate_decks.py` requires pyNastran and cross-references all four decks.
For a fresh native solve, copy a deck into a separate scratch directory and run
`mystran <deck>.bdf` there. Native input checking is included in that solve.
Set numerical-library thread counts explicitly when resources are shared.

```sh
python -I tools/extract_stresses.py --deck inputs/le1_32x96.bdf --f06 native/le1_32x96.F06 --output recovered.csv
```

The native F06 heading specifies local element stresses. ERR identifies the
SPLITD diagonal-bisector basis. The extractor averages the two opposite fibre
stresses and rotates the full symmetric stress tensor to global XY axes.
Using element normal-Y directly would compare different stress components.
Only the supplied planar, positive-normal basic-coordinate decks are supported.

`tools/sanitize_output.py` replaces identifying path, contact and host header
lines in retained copies. Raw files remain in scratch. Native F06 retention
keeps the exact lexical centroid fibre and three in-plane stress fields; echoed
inputs, displacement tables and other stress columns are omitted. Numerical
values in retained fields are unchanged. `manifest.json` hashes the bytes as
committed for numerical, source and console evidence. Markdown documentation
is enumerated separately because formatting hooks may rewrite its layout;
the review packet independently pins its content. `record.json` separately
records original native-output digests.
