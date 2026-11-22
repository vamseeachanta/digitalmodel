# OrcaWave field-point input example

This example prepares OrcaWave input for the synthetic `airgap_splash` point
request. Readiness is **input-prepared**, not physically validated. Conversion
loads `DiffractionSpec`, checks the mesh and writes YAML plus a packaged mesh;
it establishes no solved airgap, wave elevation, pressure or splash results.

## Revision history

| Revision | Date       | Change and reason                                                 |
| -------- | ---------- | ----------------------------------------------------------------- |
| Initial  | 2026-10-26 | Document offline conversion and input basis; test point portability. |

## Offline conversion

Run from the repository root in a Python environment containing the repository's
runtime dependencies. Set `TMPDIR` to a writable temporary directory outside the
checkout. The command uses committed inputs, needs no downloads and invokes no
solver runner or native interface:

```bash
PYTHONPATH=src python - <<'PY'
import os
from pathlib import Path

from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.spec_converter import SpecConverter

spec_path = Path("examples/workflows/orcawave-field-points/spec.yml")
spec = DiffractionSpec.from_yaml(spec_path)
converter = SpecConverter(spec_path)
assert converter.validate() == []
output = Path(os.environ["TMPDIR"]) / "field-point-prepared"
generated = converter.convert(solver="orcawave", output_dir=output)
assert generated.is_file()
assert (generated.parent / Path(spec.vessel.geometry.mesh_file).name).is_file()
print("Field-point input prepared; no solver results produced.")
PY
```

`input.yml` selects the same `convert_spec` operation with the `orcawave` solver
and `single` format. It is a dispatch configuration, not a solve request. The
command above writes outside the checkout rather than using its `out` setting.
Copying this directory, including `unit_box.gdf`, preserves its relative mesh
reference; conversion resolves the mesh beside `spec.yml`, independently of the
working directory, and packages it beside the generated YAML.

## Input basis

The source mesh is
[`fixtures/acceptance_610/unit_box.gdf`](../../../tests/hydrodynamics/diffraction/fixtures/acceptance_610/unit_box.gdf).
The local copy is byte-identical. Its header defines a length scale of 1.0 m,
gravity of 9.81 m/s², no symmetry and 5 wetted panels (bottom and four sides).
Its vertices span x and y from 0.0 to 1.0 m, and z from -1.0 to 0.0 m; the open
top is the waterline. All coordinates below use that mesh frame in metres.

| Input | Basis |
| --- | --- |
| Version `1.0` | Schema version, not a physical input. |
| Reference point `[0.0, 0.0, 0.0]`, waterline z `0.0` | Mesh origin and open-top elevation. |
| Centre of gravity `[0.5, 0.5, -0.5]` | Midpoint of the mesh bounds, assuming uniform mass distribution. |
| Water density `1025.0 kg/m³` | Synthetic demonstration choice, not measured data. |
| Mass `1025.0 kg` | Density times the box volume: `1.0 × 1.0 × 1.0 = 1.0 m³`; assumes equilibrium displacement. |
| Radii of gyration `[0.4, 0.4, 0.4] m` | Synthetic inertia choices, not derived mass properties. |
| Water depth `100.0 m` | Synthetic finite-depth choice. |
| Frequencies `[0.3, 0.6, 0.9] rad/s` | Synthetic sampling choices, not a convergence study. |
| Headings `[0.0, 90.0, 180.0] degrees` | Synthetic sampling choices, without heading symmetry. |
| Points `[[2.0, 0.0, 0.5], [2.0, 0.0, 1.0], [-2.0, 0.0, 0.5]] m` | Synthetic ordered request for coordinate preservation, not selected clearance locations. |

The output contains those exact rows under `FieldPointX, FieldPointY, FieldPointZ`.
The global `detect_field_points_inside_bodies: true` emits
`DetectAndSkipFieldPointsInsideBodies: Yes`. The group-level
`detect_inside_bodies` flag is informational, as documented by `FieldPointSpec`;
the tests vary it independently to check that the global switch governs output.
No geometric skipping is performed during conversion. These points are above
the waterline; their suitability for any physical output requires separate review.

## Checks and limits

The dedicated test file loads the spec, checks the exact ordered coordinates,
checks both global inside-body settings and converts a relocated copy from an
unrelated working directory. `test_spec_converter.py` covers the shared converter.
These checks establish input preparation only. Mesh convergence, field-point
eligibility, hydrodynamic validity and airgap or splash criteria remain unassessed.
Mean drift and control-surface examples remain separate open work; no workflow
or readiness gate changes are made.
