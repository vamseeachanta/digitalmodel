# Offline reference YAML parsing

The reverse-parser regression resolves the tracked YAML from the test module's
location. It runs from an unrelated temporary working directory, including a case
with a misleading local copy. Missing references raise a file error; they do not
skip the regression. No solver binary or licence is needed.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-21 | Document portable reference discovery and the text-parser example. |

## Inputs and expected answer

The source is the tracked [reference YAML](L01%20Default%20vessel.yml), identified
by its header as OrcaWave 11.5e (2025). The Environment and Bodies sections supply
the following values. These are fixture expectations, not engineering acceptance
limits.

| Input field | Input | Expected canonical result |
| --- | --- | --- |
| `WaterDepth` | 400 m | 400 m |
| `WaterDensity` | 1.025 t/m³ | 1025 kg/m³ |
| `BodyMass` | 9017.95 t | 9017950 kg |
| `PeriodOrFrequency` | Literal period list | 24 frequency entries |
| `WaveHeading` | Literal heading list | 9 heading entries |
| `BodyMeshSymmetry` | `xz plane` | `ValueError` for unsupported mesh symmetry |

Table: Expected properties read from the tracked reference and independent arithmetic.

The mass-unit relation is **1 t = 1000 kg**, from BIPM,
[The International System of Units, ninth edition (2019), version 4.01 (2026),
Table 8, tonne](https://www.bipm.org/documents/20126/41483022/SI-Brochure-9.pdf).
Thus density is `1.025 × 1000 = 1025 kg/m³`, and mass is
`9017.95 × 1000 = 9017950 kg`. The array counts come from counting the literal
entries in the reference, independently of the parser.

## Method and worked example

The executable example is
`TestParseExistingOrcaWaveFile::test_parse_l01_default_vessel` in
[the reverse-parser tests](../../../../../tests/hydrodynamics/diffraction/test_reverse_parsers.py).
It performs these steps:

1. Create an empty working directory or a decoy at the reference's relative path.
2. Change to that temporary directory and resolve the reference relative to the
   test module. Require the tracked reference to exist.
3. Read the original reference with the text parser and check symmetry refusal.
4. Load the YAML as text, change only `BodyMeshSymmetry` to `None`, and write a
   temporary copy. Parse that copy and compare the properties with the table.

With the test dependencies installed, run from the repository root. Set `TMPDIR`
to an external scratch directory first. The import block applies before test
collection, including package initialization.

```bash
PYTHONPATH=src python -c 'import sys; sys.modules["OrcFxAPI"] = None; import pytest; raise SystemExit(pytest.main(sys.argv[1:]))' \
  tests/hydrodynamics/diffraction/test_reverse_parsers.py::TestParseExistingOrcaWaveFile \
  -q -rA -p no:cacheprovider --basetemp="$TMPDIR/reference-parser"
```

Both working-directory cases must pass without missing-reference skips. The
separate missing-reference test requires `FileNotFoundError`. A lookup relative
to the current working directory fails either case: the empty directory has no
reference, and the decoy contains an empty mapping rather than the tracked input.

## Limits

The symmetry change exercises property parsing only. It does not expand the
mesh or establish its physical validity. Mesh loading, solver execution and
hydrodynamic results are outside this example.
