# Pipe cross-section module analysis

The [self-contained report](index.html) evaluates the existing synthetic bare
and coated example through normal public APIs. [Results](results.json) retain
unrounded values; [history](changes.json) records revisions and the
[manifest](manifest.json) binds the delivered bytes and calculation sources.

## Rebuild

With source imports and the declared Pint dependency available, run from the
repository root. Supply existing or new external directories for scratch and
for a fresh delivery; replace both symbolic directory arguments.

```sh
PYTHONPATH=src python scripts/reports/build_pipe_cross_section_module_report.py --scratch EXTERNAL_SCRATCH --output-dir EXTERNAL_OUTPUT
```

The builder writes calculation evidence only to external scratch and final
files only to the explicit output directory. It uses no remote assets or
calculation service. The reviewed revision and exact source-byte digests are
recorded separately so source drift is visible.

## Evidence and limits

The basis is the [worked example](../../../../src/digitalmodel/structural/pipe_cross_section/docs/worked-layered-pipe-buoyancy.md).
The [report tests](../../../../tests/reporting/test_pipe_cross_section_module_report.py)
separate exact-rational polar integration from production replay, two fresh
byte-identical rebuilds, manifest readback and tampering controls.

The calculation assumes a fully immersed pipe with a sealed dry bore and
omits closure/end masses. Mass equivalents in kg/m differ from force per
length in kN/m. Physical densities/defaults, flooding behavior and installation
acceptance remain NOT_EVALUATED. Optional legacy plotting exports are not
exercised by this builder.
