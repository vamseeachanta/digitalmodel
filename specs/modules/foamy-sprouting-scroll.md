# Plan: Create Canonical spec.yml for Each OrcaWave Benchmark Geometry

## Context

**WRK-107**: The OrcaWave diffraction benchmarks (barge, ship, spar) each have low-level OrcFxAPI input YAMLs in `source_data/orcawave/`, but no solver-agnostic `spec.yml` that drives analysis programmatically. A `DiffractionSpec` Pydantic schema already exists at `src/digitalmodel/hydrodynamics/diffraction/input_schemas.py` and is consumed by `OrcaWaveRunner._generate_input_files()` to produce OrcaWave input YAMLs.

**Goal**: Create one `spec.yml` per geometry that conforms to `DiffractionSpec` schema. These become the single canonical input for all future OrcaWave analysis — benchmarks, RAOs, hydrodynamic coefficients, new runs.

## Key Finding: Schema Already Exists

The `DiffractionSpec` model (input_schemas.py:543) already has everything needed:
- `vessel` / `bodies` — mesh, inertia, external matrices, fixed DOFs
- `environment` — water_depth, density, gravity
- `frequencies` — period or rad/s, explicit or range
- `wave_headings` — explicit or range, symmetry
- `solver_options` — QTF, load_rao_method, precision
- `outputs` — formats, components, directory
- `metadata` — project, author, tags

The runner at `orcawave_runner.py:442` already consumes this spec via `_generate_input_files(spec, output_dir)`.

**No schema changes needed** — only data files (spec.yml) to create.

## Unit Convention (Codex review: P2 addressed)

The `DiffractionSpec` uses **pure SI** (kg, m, s). The `orcawave_backend.py` handles conversion to OrcaFlex-SI (te, m, s) automatically:
- `mass`: spec=**kg** → backend divides by 1000 → OrcaWave YAML gets **te** (`orcawave_backend.py:266`)
- `water_density`: spec=**kg/m³** (1025.0) → backend divides by 1000 → OrcaWave gets **te/m³** (1.025) (`orcawave_backend.py:180`)
- `inertia_tensor`: spec=**kg·m²** → backend divides by 1000 → OrcaWave gets **te·m²** (`orcawave_backend.py:209`)
- Reverse parser (`reverse_parsers.py:573`) does the inverse: OrcaWave te × 1000 → spec kg

**Inertia representation**: Use `inertia_tensor` (dict with Ixx, Iyy, Izz, Ixy, Ixz, Iyz) for all 3 geometries since the OrcaWave YAMLs provide full tensor values. The backend converts tensor to 3×3 matrix for OrcaWave.

## Mesh Path Resolution (Codex review: P2 addressed)

The `OrcaWaveRunner._copy_mesh_files()` resolves mesh paths relative to the spec.yml directory (`spec_dir`), then copies them to `output_dir`. So `mesh_file: "source_data/orcawave/barge_generated.gdf"` is correct — relative to where the spec.yml lives.

## Files to Create

| File | Source of Truth |
|------|----------------|
| `docs/modules/orcawave/L02_barge_benchmark/spec.yml` | `source_data/orcawave/Barge_Benchmark.yml` |
| `docs/modules/orcawave/L03_ship_benchmark/spec.yml` | `source_data/orcawave/orcawave_001_ship_raos_rev2.yml` |
| `docs/modules/orcawave/L04_spar_benchmark/spec.yml` | `source_data/orcawave/spar_benchmark.yml` |

## Spec.yml Template (DiffractionSpec format)

```yaml
version: "1.0"
analysis_type: diffraction

vessel:
  name: "<geometry_name>"
  type: "<barge|ship|spar>"
  geometry:
    mesh_file: "source_data/orcawave/<mesh_file>"  # relative to spec.yml
    mesh_format: gdf  # or dat
    symmetry: none
    reference_point: [0.0, 0.0, 0.0]
    waterline_z: 0.0
    length_units: m
  inertia:
    mass: <kg>
    centre_of_gravity: [x, y, z]
    radii_of_gyration: [kxx, kyy, kzz]  # OR inertia_tensor
  external_stiffness: null  # 6x6 matrix if needed
  external_damping: null    # 6x6 matrix if needed (ship has roll damping)
  fixed_dofs: null

environment:
  water_depth: <meters>
  water_density: 1025.0

frequencies:
  input_type: period  # or frequency
  values: [...]       # explicit list from existing benchmark

wave_headings:
  values: [0, 45, 90, 135, 180]
  symmetry: false

solver_options:
  remove_irregular_frequencies: true
  qtf_calculation: false  # true for ship
  load_rao_method: both
  precision: double

outputs:
  formats: [csv]
  components: [raos, added_mass, damping]
  directory: benchmark_results

metadata:
  project: "OrcaWave Benchmark Library"
  description: "<geometry> diffraction benchmark"
  tags: [benchmark, <geometry>, validation]
```

## Parameter Extraction Per Geometry

### Barge (L02)
- **Source**: `Barge_Benchmark.yml` — WaterDepth=200, 19 periods (5.236–20.944s), 5 headings
- **Mesh**: `barge_generated.gdf` (Wamit GDF format)
- **Mass**: 16,400 te → 16,400,000 kg; CoG=[0,0,-5.0]; Ixx=1.64e9, Iyy=Izz=3.69e9 kg·m²
- **No external damping/stiffness**

### Ship (L03)
- **Source**: `orcawave_001_ship_raos_rev2.yml` — WaterDepth=30, 20 periods (2–22s), 9 headings (0–180° at 22.5°)
- **Mesh**: `aqwa_001_ship_raos_rev2.dat` (AQWA DAT format), position=[0,0,0.5]
- **Mass**: 9,017.95 te → 9,017,950 kg; CoG=[2.53,0,-1.974]
- **External roll damping**: M44=36,010 N·m·s → 6x6 damping matrix row 4
- **QTF calculation**: enabled (SolveType: Full QTF calculation)
- **Control surface**: auto-generated, panel_size=10, separation=20

### Spar (L04)
- **Source**: `spar_benchmark.yml` — WaterDepth=200, 20 frequencies (0.15–2.32478 rad/s), 5 headings
- **Mesh**: `simple.dat` (AQWA DAT format)
- **Mass**: 55,000 te → 55,000,000 kg; CoG=[0,0,-61.63]; Ixx=Iyy=3.118e8, Izz=2.164e8 te·m²
- **Note**: Frequencies in rad/s (not periods) — set `input_type: frequency`

## Implementation Steps

1. **Create `L02_barge_benchmark/spec.yml`** — extract exact values from `Barge_Benchmark.yml`
2. **Create `L03_ship_benchmark/spec.yml`** — extract from `orcawave_001_ship_raos_rev2.yml`, include QTF and damping
3. **Create `L04_spar_benchmark/spec.yml`** — extract from `spar_benchmark.yml`, use rad/s frequencies
4. **Validate all 3 specs** — `DiffractionSpec.from_yaml("spec.yml")` must parse without errors
5. **Round-trip test** — generate OrcaWave input from spec, compare against existing `source_data/orcawave/*.yml`

## Verification

### 1. Schema validation (all 3 specs parse)
```bash
uv run python -c "
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
for path in [
    'docs/modules/orcawave/L02_barge_benchmark/spec.yml',
    'docs/modules/orcawave/L03_ship_benchmark/spec.yml',
    'docs/modules/orcawave/L04_spar_benchmark/spec.yml',
]:
    spec = DiffractionSpec.from_yaml(path)
    print(f'{path}: OK — {len(spec.frequencies.to_frequencies_rad_s())} freqs, '
          f'{len(spec.wave_headings.to_heading_list())} headings')
"
```

### 2. Semantic equivalence check (Codex review: P1 addressed)
For each geometry, compare spec values against the OrcaWave source YAML:
- **Frequencies**: spec `to_frequencies_rad_s()` must match OrcaWave `PeriodOrFrequency` (converted to rad/s) within 0.01%
- **Headings**: exact match
- **Mass**: spec mass (kg) / 1000 must equal OrcaWave `BodyMass` (te) within 0.01%
- **CoG**: exact match (meters, same reference frame)
- **Inertia**: spec tensor (kg·m²) / 1000 must match OrcaWave tensor (te·m²) within 0.01%
- **Water depth**: exact match
- **Allowed defaults**: solver tolerances, warning levels, output options — these are set by the backend, not the spec

### 3. Round-trip test (generate OrcaWave input from spec, diff key fields)
```bash
uv run python -c "
from digitalmodel.hydrodynamics.diffraction.input_schemas import DiffractionSpec
from digitalmodel.hydrodynamics.diffraction.orcawave_backend import OrcaWaveBackend
# Load spec, generate OrcaWave YAML, compare against source
"
```

## Cross-Review Results

| Reviewer | Verdict | Key Feedback |
|----------|---------|-------------|
| Claude | Template generated | (interactive session — inline review above) |
| Codex | REQUEST_CHANGES | Unit handling, acceptance criteria, mesh paths — all addressed above |
| Gemini | APPROVE | Notes mass units and path verification — addressed above |

## Exclusions

- No schema changes to `input_schemas.py` (already complete)
- No changes to `orcawave_runner.py` (already consumes DiffractionSpec)
- No re-running of benchmarks (specs capture existing parameters)
- Unit box (L01) skipped — AQWA mesh input error, no complete benchmark
