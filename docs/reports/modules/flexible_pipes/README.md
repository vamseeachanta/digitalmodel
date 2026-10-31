# Flexible-pipe module analysis

[Read the generated analysis](index.html). Numerical evaluation covers only the
synthetic full-wall geometry and mass example. Structural capacity, standard
design limits, armour homogenization, fatigue, bend-stiffener sizing and
manufactured-pipe behaviour remain `NOT_EVALUATED`.

The existing [example note](../../../../examples/domains/subsea/flexible_pipe_geometry.md)
is the source of every engineering input and the radius-based hand calculation.
The [source register](source_register.json) records exact source digests and
intended-use readiness. The [change record](changes.json) supplies the report's
Revision history; retain earlier rows and append a row on reissue.

From the repository root, set `REPORT_SCRATCH` and `REPORT_OUTPUT` to distinct
directories outside the checkout, within the configured temporary directory.
The cache and temporary-directory environment variables remain those supplied
by the launch environment.

```bash
export REPORT_SCRATCH="$TMPDIR/flexible-report/build-a-scratch"
export REPORT_OUTPUT="$TMPDIR/flexible-report/build-a-output"
PYTHONPATH=src uv run --python 3.11 --no-project python docs/reports/modules/flexible_pipes/build_report.py --scratch "$REPORT_SCRATCH" --output "$REPORT_OUTPUT"
```

Repeat with distinct directories to compare `index.html`, `changes.json` and
`source_register.json`. Builds from identical source bytes with the same Python
runtime are byte-identical: no volatile metadata is removed. The committed
goldens match Python 3.11. A local Python 3.12 rebuild changes the wall-area
relative difference from `0.0e+00` to `1.5e-16`, within the `1e-12` arithmetic
criterion, but fails the byte comparison. Cross-version byte identity has not
been established.

Rebuilding the committed goldens also requires LF source and golden bytes.
The pinned paths currently have no explicit LF checkout attributes. Automatic
CRLF conversion therefore changes raw-byte source digests and the generated
HTML even when calculation values remain unchanged. The rebuild test retains
exact byte comparisons; newline normalization is not a substitute for source
byte preservation. Explicit checkout attributes and a checkout-conversion
regression remain required before cross-checkout reproducibility is claimed.

The calculation JSON intermediate stays in scratch. Delivery copies
only these three generated files into this folder after comparison; the local
builder and README remain authored sources.

```bash
PYTHONPATH=src uv run --python 3.11 --no-project --with pytest python -m pytest tests/subsea/flexible_pipes tests/reporting/test_flexible_pipes_module_report.py -q -rA -p no:cacheprovider --basetemp="$TMPDIR/flexible-report/pytest"
```

The oracle derives areas from inner and outer radii and mass from density times
area, independently of the mean-diameter mass implementation. Every unrounded
worked value is checked at relative tolerance `1e-12`, a software arithmetic
criterion. Display precision follows the example note: dimensions in m have
three decimal places, areas in m² nine, masses in kg/m six, and densities in
kg/m³ one. Display rounding does not establish input accuracy. Relative
differences use one decimal in scientific notation and unrounded decisions.

No browser, solver or fatigue workflow is used by the builder or focused tests.
Deferred qualification remains open under
[issue 3343](https://github.com/vamseeachanta/digitalmodel/issues/3343).
