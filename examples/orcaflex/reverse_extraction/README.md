# Strict YAML to spec example

This synthetic example uses `SingleToSpecConverter` to extract supported fields
from a single OrcaFlex YAML mapping. It displays the conversion report alongside
the produced spec and saves both to explicit output paths. Extraction is
best-effort and requires review; it does not establish lossless round-trip
equivalence or a complete runnable model.

## Run offline

Run from the repository root with an existing Python environment containing
`PyYAML`, `pydantic>=2,<3`, `loguru`, `numpy` and `scipy`. The last four packages
are needed by the current package imports, including shared OpenFOAM utilities;
this example does not run those utilities. Install dependencies before going
offline. For tests, also provide `pytest` and `pytest-asyncio` (the latter is
required by the repository's pytest configuration).

Set `EXAMPLE_OUT` and `EXAMPLE_SCRATCH` to external directories before running.
The input remains read-only, final files use only the explicit output arguments,
and intermediate modular files use `TMPDIR` and are removed by the converter.
The command makes no network request and starts no licensed interface.

```bash
mkdir -p "$EXAMPLE_OUT" "$EXAMPLE_SCRATCH"
PYTHONPATH=src PYTHONDONTWRITEBYTECODE=1 TMPDIR="$EXAMPLE_SCRATCH" \
  python3 examples/orcaflex/reverse_extraction/convert.py \
  --input examples/orcaflex/reverse_extraction/strict.yml \
  --spec-output "$EXAMPLE_OUT/spec.yml" \
  --report-output "$EXAMPLE_OUT/report.json"
```

Both output arguments are required and must be distinct from each other and the
input. Existing output files are replaced. A successful extraction exits
successfully even when sections remain unmapped. An empty input returns a failure
status and writes the failure report without producing a spec.

## Synthetic input basis

The numeric values in [strict.yml](strict.yml) are authored demonstration inputs:
water depth is **42.0 m** and wind speed is **0.0 m/s**, with SI units declared in
`General`. They are illustrative choices, not measured data, design criteria or
solver results. The entrypoint test also uses **84.0 m**, calculated independently
as twice the authored depth, to confirm the input is read rather than hard-coded.
The zero wind speed checks that a supported zero value is retained.

`Groups` contains only empty grouping metadata. It deliberately has no spec
counterpart and does not affect the environmental values. It is an expected gap,
not a conversion failure.

## Recoverable content and limits

- `Environment.WaterDepth` becomes `environment.water.depth`.
- `Environment.WindSpeed` becomes `environment.wind.speed`, including zero.
- `Groups` is omitted from the spec and appears in `unmapped_sections` and
  `expected_gaps`; `actionable_gaps` is empty for this input.
- The report marks extraction as `is_best_effort`. Its confidence is partial for
  this sparse input. Confidence measures populated extraction-map entries; it is
  not an accuracy probability, solver validation or the fraction of all source
  content preserved.

The converter also supports selected `General` simulation fields and other
mapped environmental fields. See
[the extraction map](../../../src/digitalmodel/solvers/orcaflex/format_converter/modular_to_spec.py)
for the current scope. That implementation distinguishes expected out-of-scope
sections from actionable gaps; unknown sections are treated as actionable.
Unmapped reporting operates at section level: an unsupported key inside a known
section need not be listed separately. Even a high confidence score does not
establish full source coverage.

Generated metadata contains placeholders requiring review. Component detail and
engineering intent cannot generally be recovered from this reduced spec. Retain
the original strict YAML, examine the report and the spec's review warning, and
supply missing information before using the spec for further model generation.
The broader reverse-extraction scope remains open in issue #520.
