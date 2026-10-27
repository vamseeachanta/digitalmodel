# Synthetic OrcaWave package example

This example converts a synthetic closed box through `SpecConverter`, with the
existing mesh packaging and quality gates enabled. It checks every emitted
`*MeshFileName` reference in every generated YAML file. It does not run a solver.

## Revision history

| Revision | Date | Change and reason |
| --- | --- | --- |
| Initial | 2026-10-27 | Added a synthetic example to demonstrate the existing packaging API. |

## Run from another working directory

From the repository root, select a writable scratch directory outside the
repository. The command supplies the focused Python dependencies through uv:

```bash
REPO_ROOT="$PWD"
EXAMPLE="$REPO_ROOT/examples/domains/hydrodynamics/synthetic_package"
: "${TMPDIR:?Set TMPDIR to a writable external scratch directory}"
mkdir -p "$TMPDIR/package-example-working"
cd "$TMPDIR/package-example-working"
PYTHONDONTWRITEBYTECODE=1 PYTHONPATH="$REPO_ROOT/src" uv run --no-project \
  --with numpy --with scipy --with pandas --with pydantic --with pyyaml \
  --with click --with loguru --with matplotlib --with plotly \
  --with 'shapely>=2.1' python \
  "$EXAMPLE/package_example.py" \
  --spec "$EXAMPLE/input/spec.yml" \
  --output-dir "$TMPDIR/synthetic-package"
```

The `--spec` argument can be omitted to use the adjacent saved input. Use
`--format modular` and a fresh output directory to exercise modular generation.
The example exits with a failure message when a required mesh is absent; it
prints `Package generated` only after conversion and emitted-reference checks
succeed. It also prints non-blocking quality warnings from the converter.
The Shapely minimum follows the existing geometry API requirement in
[pyproject.toml](../../../../pyproject.toml).

## Paths and package contents

The input spec's `meshes/box.gdf` resolves relative to **the directory containing
the input spec**, regardless of the caller's working directory. Absolute input
mesh paths are accepted by the existing API. Relative output directories resolve
from the caller's working directory, so the command uses an explicit destination.

The converter copies the mesh into that destination and emits `box.gdf` as a
local mesh filename. The single-file input is `synthetic_box.yml`; modular output
has section YAML files and `master.yml`. The example parses all emitted YAML
files and checks that mesh references resolve to existing files inside the
package. The tests also copy the package to another directory and repeat those
checks. Input asset resolution, copying and quality evaluation remain owned by
the existing converter; the example adds no input resolver.

`mesh_quality_report.json` records the existing quality gate outcome. It can
contain diagnostic source locations; this example does not claim that every
diagnostic field is portable. It requires only the emitted solver mesh
references to be local. Generated outputs belong in scratch, outside Git.

## Synthetic input basis

All geometry and analysis settings are constructed for this packaging example;
they describe no real asset. The following is the numerical basis for the input
files and independent test expectations.

| Quantity | Value and basis |
| --- | --- |
| Geometry | A cube of side 2 m: x and y from -1 to 1 m; z from -2 to 0 m. Coordinates are obtained by taking the endpoints on each axis. |
| Topology | Two faces per axis give 6 outward-wound square panels; endpoint combinations give 8 corners. Each edge belongs to 2 faces, giving a closed mesh. |
| Reference and waterline | Origin and waterline at z = 0 m; the cube centre is (0, 0, -1) m. |
| Mesh units and symmetry | GDF unit length 1 m, no reflected half-mesh: symmetry flags 0 and 0. |
| Density and mass | Assumed density 1000 kg/m³; displaced volume = 2 × 2 × 2 = 8 m³; mass = 1000 × 8 = 8000 kg. |
| Gyration radii | Assumed 1 m about each axis, solely to supply positive input values; these are not derived uniform-cube inertias. |
| Water depth | Assumed 20 m, ten cube side lengths. |
| Wave samples | Assumed single angular frequency 1 rad/s and heading 0 degrees, sufficient to exercise input generation. |
| Gravity | 9.80665 m/s², matching the existing `EnvironmentSpec.gravity` default. |
| Schema version | `1.0`, matching the existing `DiffractionSpec.version` default. |

The schema defaults are defined in
[input_schemas.py](../../../../src/digitalmodel/hydrodynamics/diffraction/input_schemas.py).
The GDF header layout is checked by the existing
[quality gate](../../../../src/digitalmodel/hydrodynamics/diffraction/quality_gates.py).
The independent expected quality outcome is `WARNING`: the closed cube has
consistent outward normals, equal square panels and no open edges, but its panel
count is below the existing
[checker](../../../../src/digitalmodel/hydrodynamics/diffraction/geometry_quality.py)
minimum of 100. This is a deliberate coarse demonstration mesh.

## Completeness and limits

Package completeness here means that every emitted mesh reference resolves
inside the package. It does **not** establish solver acceptance, hydrodynamic
accuracy, mesh convergence, frequency coverage or suitability for design.
The top closing face at the waterline and the sparse wave samples are retained
for a closed topology example, not presented as a qualified solver model.

This is partial scope for issue #605. The example covers a single body in both
output formats and a missing body mesh. Auxiliary mesh combinations, filename
collisions and converted mesh provenance already have separate converter tests;
this example does not replace those tests. Transactional output replacement,
package ownership and manifest guarantees described in the historical plan are
not established here. Any remaining issue requirements need assessment against
the current implementation, followed by separate tests or implementation where
required. Licensed solver validation remains separate work.
