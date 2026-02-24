---
name: diffraction-analysis
description: Master skill for hydrodynamic diffraction analysis - AQWA, OrcaWave, and BEMRosetta integration
version: 1.0.0
updated: 2026-01-27
category: offshore-engineering
triggers:
  - diffraction analysis
  - hydrodynamic analysis
  - RAO analysis
  - added mass
  - radiation damping
  - wave forces
  - BEM solver
  - panel method
  - frequency domain
---

# Diffraction Analysis Master Skill

## Overview

This skill provides guidance on hydrodynamic diffraction/radiation analysis using the available modules in digitalmodel. Three primary modules handle different aspects of the workflow:

| Module | Purpose | Primary Use Case |
|--------|---------|------------------|
| **aqwa** | Native AQWA analysis | Direct AQWA .LIS file processing |
| **orcawave** | OrcaWave diffraction | OrcaFlex-integrated analysis |
| **bemrosetta** | Format conversion | AQWA → OrcaFlex workflow, mesh conversion |
| **diffraction** | Unified schemas | Data structures and comparison framework |

## Version

- **Skill Version**: 1.0.0
- **Updated**: 2026-01-27
- **Category**: Offshore Engineering

## Module Comparison

### When to Use Each Module

| Scenario | Recommended Module | Reason |
|----------|-------------------|--------|
| Parse AQWA .LIS files | `aqwa` or `bemrosetta` | Native parsing, no external dependencies |
| Run OrcaWave analysis | `orcawave` | Direct OrcaFlex API integration |
| Convert AQWA → OrcaFlex | `bemrosetta` | Purpose-built converter with validation |
| Compare AQWA vs OrcaWave | `diffraction` | Unified schema comparison framework |
| Handle QTF data | `bemrosetta` | QTF parser and OrcaFlex export |
| Convert mesh formats | `bemrosetta` | GDF/DAT/STL conversion |
| Store/retrieve coefficients | `hydrodynamics` | Coefficient database |

### Feature Matrix

| Feature | aqwa | orcawave | bemrosetta | diffraction |
|---------|------|----------|------------|-------------|
| Parse AQWA .LIS | ✅ | ❌ | ✅ | ✅ (via converter) |
| Parse OrcaWave | ❌ | ✅ | ❌ | ✅ (via converter) |
| RAO extraction | ✅ | ✅ | ✅ | ✅ |
| Added mass/damping | ✅ | ✅ | ✅ | ✅ |
| QTF (2nd order) | ❌ | ✅ | ✅ | ❌ |
| Export to OrcaFlex | ❌ | Native | ✅ | ✅ |
| Mesh conversion | ❌ | ❌ | ✅ | ❌ |
| Coefficient validation | ❌ | ❌ | ✅ | ✅ |
| Comparison framework | ❌ | ❌ | ❌ | ✅ |

## Unified Data Schema

All modules use the unified schema from `diffraction.output_schemas`:

```python
from digitalmodel.diffraction import (
    DiffractionResults,  # Complete analysis container
    RAOSet, RAOComponent, # RAO data structures
    AddedMassSet, DampingSet,  # Coefficient matrices
    HydrodynamicMatrix,  # 6×6 frequency-dependent matrix
    FrequencyData, HeadingData,  # Discretization
    DOF, Unit,  # Enumerations
)
```

### DiffractionResults Structure
```
DiffractionResults
├── vessel_name: str
├── frequencies: FrequencyData
├── headings: HeadingData
├── raos: RAOSet
│   ├── surge: RAOComponent (magnitude, phase)
│   ├── sway: RAOComponent
│   ├── heave: RAOComponent
│   ├── roll: RAOComponent
│   ├── pitch: RAOComponent
│   └── yaw: RAOComponent
├── added_mass: AddedMassSet
│   └── matrices: List[HydrodynamicMatrix]  # 6×6 per frequency
├── damping: DampingSet
│   └── matrices: List[HydrodynamicMatrix]
└── source_file: str
```

## Typical Workflows

### Workflow 1: AQWA Analysis Only

```python
from digitalmodel.aqwa import AQWAAnalysis

# Direct AQWA analysis
analysis = AQWAAnalysis(folder="aqwa_results/")
analysis.run()
```

### Workflow 2: AQWA → OrcaFlex Conversion

```python
from digitalmodel.bemrosetta import (
    AQWAParser, OrcaFlexConverter, validate_coefficients
)

# Parse AQWA
parser = AQWAParser()
results = parser.parse("analysis.LIS")

# Validate
report = validate_coefficients(results)
if report.is_valid:
    # Convert to OrcaFlex
    converter = OrcaFlexConverter(output_dir="./orcaflex")
    converter.convert(results)
```

### Workflow 3: OrcaWave Analysis

```python
from digitalmodel.orcawave import OrcaWaveAnalysis

# Run OrcaWave (requires OrcFxAPI)
analysis = OrcaWaveAnalysis()
analysis.setup_model(vessel_file="vessel.yml")
analysis.run_diffraction()
results = analysis.get_results()
```

### Workflow 4: AQWA vs OrcaWave Comparison

```python
from digitalmodel.diffraction import (
    DiffractionComparator,
    AQWAConverter,
    OrcaWaveConverter,
)

# Convert both sources to unified schema
aqwa_results = AQWAConverter("aqwa_folder/").convert_to_unified_schema()
orcawave_results = OrcaWaveConverter(model).convert_to_unified_schema()

# Compare
comparator = DiffractionComparator()
report = comparator.compare(aqwa_results, orcawave_results)
print(f"RAO match: {report.rao_match_percentage:.1f}%")
```

### Workflow 5: Complete Pipeline with QTF

```python
from digitalmodel.bemrosetta import (
    AQWAParser, QTFParser, OrcaFlexConverter,
    CoefficientValidator, CausalityChecker,
)

# Parse main results
parser = AQWAParser()
results = parser.parse("analysis.LIS")

# Parse QTF
qtf_parser = QTFParser()
qtf_data = qtf_parser.parse("analysis.QTF")

# Validate
coef_validator = CoefficientValidator(check_symmetry=True)
coef_report = coef_validator.validate(results)

causality_checker = CausalityChecker()
kk_report = causality_checker.validate(results)

# Convert with QTF
converter = OrcaFlexConverter(output_dir="./output")
converter.set_qtf_data(qtf_data)
converter.convert(results)
```

## CLI Commands

### AQWA Module
```bash
# (Uses existing AQWA CLI if available)
```

### BEMRosetta Module
```bash
bemrosetta convert analysis.LIS -o ./output
bemrosetta convert analysis.LIS --qtf analysis.QTF -o ./output
bemrosetta info analysis.LIS
bemrosetta validate analysis.LIS --strict --causality
bemrosetta convert-mesh hull.gdf -o hull.stl
bemrosetta status
```

### Diffraction Module
```bash
# Batch processing
python -m digitalmodel.diffraction.batch_processor config.yml
```

## Output Formats

### OrcaFlex Vessel Type YAML
```yaml
VesselType:
  Name: FPSO
  Category: Vessel
  PrimaryMotion: Calculated (6 DOF)
  RAOOrigin: [0, 0, 0]
  RAOPhaseConvention: AQWA
```

### Coefficient CSV
```csv
Frequency_rad/s,A11,A12,A13,A14,A15,A16,...
0.3,1.0e7,0.0,0.0,0.0,0.0,0.0,...
0.4,1.1e7,0.0,0.0,0.0,0.0,0.0,...
```

### QTF CSV
```csv
Freq1_rad/s,Freq2_rad/s,Heading_deg,Surge_Re,Surge_Im,...
0.3,0.3,0.0,1.2e5,0.0,...
```

## Validation Criteria

### Coefficient Validation
- **Symmetry**: Added mass and damping matrices should be symmetric
- **Positive definiteness**: Diagonal elements non-negative
- **Physical limits**: No NaN/Inf values, reasonable magnitudes

### Kramers-Kronig Causality
- Added mass A(ω) and damping B(ω) must satisfy K-K relations
- Tolerance: typically 10% relative error acceptable

### RAO Validation
- Magnitude non-negative
- Phase in reasonable range (-360° to 360°)
- Physical trends (heave RAO → 1.0 at low frequency)

## Standard Report Format

All diffraction analysis reports MUST follow the **r4 benchmark report format** established in the barge benchmark (AQWA vs OrcaWave). This is the canonical template for all diffraction work going forward.

### Required Report Structure (Single-Page HTML)
1. **Header** — Vessel name, date, overall consensus badge
2. **Input Comparison** — Solver-column table (geometry, mass, environment, damping)
3. **Consensus Summary** — Per-DOF badges (FULL/SPLIT/NO_CONSENSUS)
4. **Per-DOF Analysis** — 2-column grid: text/conclusions left (45%), Plotly plot right (55%)
5. **Full Overlay Plots** — Combined amplitude/phase across all DOFs
6. **Notes** — Auto-generated observations

### Required Plot Conventions
- **Vertical legends** on right side (`x=1.02, y=1.0, orientation="v"`)
- **Heading-first trace ordering** (group solvers under each heading in legend)
- **Significance filtering** — auto-omit headings where response < 1% of DOF peak
- **Monospace fonts** for all numeric values (Cascadia Code / Consolas)

### Required Table Conventions
- Solver names as column headers, headings as rows
- Alternating row colors, hover highlight, dark header theme
- Section separator rows for grouping

### Reference Implementation
- Plotter: `src/digitalmodel/hydrodynamics/diffraction/benchmark_plotter.py`
- Runner: `src/digitalmodel/hydrodynamics/diffraction/benchmark_runner.py`
- Example output: `benchmark_output/barge_benchmark/r4_per_dof_report/`

## Canonical spec.yml Format (DiffractionSpec)

All diffraction analyses should be driven by a `spec.yml` conforming to `DiffractionSpec` (`input_schemas.py`). This is the solver-agnostic input that both OrcaWave and AQWA backends consume.

**Template structure:** `version`, `analysis_type`, `vessel` (name/type/geometry/inertia), `environment`, `frequencies`, `wave_headings`, `solver_options`, `outputs`, `metadata`.

**Live examples:**
- `docs/modules/orcawave/L02_barge_benchmark/spec.yml` -- standard diffraction
- `docs/modules/orcawave/L03_ship_benchmark/spec.yml` -- full QTF + external roll damping
- `docs/modules/orcawave/L04_spar_benchmark/spec.yml` -- rad/s frequency input

**Critical unit rule:** Spec uses pure SI (kg, m, s). Backends convert. But `external_damping`/`external_stiffness` matrices pass through without conversion -- see `orcawave-analysis` skill for details.

## Solver Options Cross-Reference (AQWA vs OrcaWave)

### OrcaWave SolveType Options (6 levels, ascending complexity)

| # | OrcaWave SolveType | What it computes | QTF? | Cost |
|---|---|---|---|---|
| 1 | `Potential formulation only` | First-order potential theory | No | Fastest |
| 2 | `Potentials only` | Legacy alias for #1 | No | Fastest |
| 3 | `Potential and source formulations` | First-order + source formulation | No | Medium |
| 4 | `Potentials and mean drift only` | First-order + mean drift forces | Partial | Medium |
| 5 | `Diagonal QTF only` | First-order + diagonal QTF elements | Partial | Slow |
| 6 | `Full QTF calculation` | Complete bi-frequency QTF | Full | Slowest |

### AQWA RESTART Stages

| RESTART | What it computes | Key OPTIONS |
|---|---|---|
| `1 2` | Geometry + radiation only (added mass, damping — no RAOs) | — |
| `1 5` | Full diffraction with RAOs (standard) | `LHFR` (irregular freq removal) |
| `1 8` | Full QTF (second-order forces) | `LHFR MQTF` |

**AQWA OPTIONS keywords:**
- `LHFR` — Remove irregular frequency effects (lid method)
- `MQTF` — Enable QTF computation (requires RESTART to include stages 6-8)
- `GOON` — Continue past non-fatal mesh errors
- `AHD1` — Generate .AH1 ASCII hydrodynamic database
- `REST END` — Marks end of OPTIONS block; RESTART must follow immediately

### Cross-Solver Equivalence Map

| OrcaWave SolveType | AQWA RESTART | AQWA OPTIONS | spec.yml Mapping |
|---|---|---|---|
| Potential formulation only | `1 2` | — | `analysis_type: radiation` |
| Potential and source formulations | `1 5` | `LHFR` | `analysis_type: diffraction` |
| Potentials and mean drift only | `1 5` | `LHFR MQTF` | `analysis_type: frequency_domain` |
| Full QTF calculation | `1 8` | `LHFR MQTF` | `analysis_type: full_qtf` |

### Current Benchmark Solver Configuration

| Setting | Barge (L02) | Ship (L03) | Spar (L04) |
|---|---|---|---|
| `analysis_type` | diffraction | **full_qtf** | diffraction |
| `qtf_calculation` | false | **true** | false |
| OrcaWave actual | Potential+source | **Full QTF** | Potential+source |
| AQWA RESTART | 1 5 | 1 5 | 1 5 |
| AQWA OPTIONS | LHFR | LHFR **MQTF** | LHFR |
| External damping | none | **M44=36,010** | none |
| Freq count | 19 | 20 | 20 |
| Headings | 5 (0-180, 45° step) | **9** (0-180, 22.5° step) | 5 (0-180, 45° step) |

### Known Backend Bugs (as of 2026-02-12)

**Bug 1 — OrcaWave SolveType not mapped from qtf_calculation:**
`orcawave_backend.py:333` hardcodes `SolveType = "Potential and source formulations"` regardless of `qtf_calculation` flag. When `qtf_calculation: true`, should emit `"Full QTF calculation"`.

**Bug 2 — AQWA RESTART always 1-5:**
`aqwa_backend.py:412` hardcodes `RESTART 1 5` even when `qtf_calculation: true`. Full QTF requires `RESTART 1 8`.

**Bug 3 — SolverOptions schema too coarse:**
`input_schemas.py` SolverOptions uses `qtf_calculation: bool` but cannot represent OrcaWave's 6 SolveType levels (e.g. mean drift only, diagonal QTF only, potential-only mode).

### FIDP External Damping — Impact on RAOs (WRK-132 Finding)

AQWA FIDP (Frequency Independent DamPing) cards are correctly generated and stored (confirmed in LIS output). However, **FIDP has ZERO effect on AQWA stage 1-5 RAOs** — external damping only affects time-domain response (stages 6+). This is by design: AQWA frequency-domain RAOs are computed from potential theory WITHOUT viscous damping.

OrcaWave DOES include external damping in its RAO calculation (via `BodyExternalDampingMatrix*`). This creates a **fundamental asymmetry** between solvers when comparing roll RAOs with external damping present.

### Recommended Methodology for Future Benchmarks

1. **Match solver levels**: Ensure equivalent computation levels between solvers (use the cross-reference table above)
2. **Separate first-order from second-order**: Compare RAOs at the same analysis level before adding QTF
3. **Document external damping**: Note that AQWA freq-domain RAOs exclude external damping; OrcaWave includes it
4. **Fix backend bugs before generating new spec.yml**: Apply Bug 1-3 fixes to ensure spec.yml → solver input fidelity
5. **Run sensitivity studies**: Test with different SolveType levels to quantify impact on results
6. **Use common heading sets**: Both solvers should use the same headings to avoid harmonization interpolation artifacts

## Best Practices

1. **Always validate coefficients** before using in OrcaFlex
2. **Check K-K causality** for added mass/damping consistency
3. **Compare results** when both AQWA and OrcaWave are available
4. **Use unified schema** for interoperability
5. **Document water depth** and frequency range assumptions
6. **Follow the r4 report format** for all diffraction analysis reports

## Related Skills

| Skill | Description |
|-------|-------------|
| **aqwa-analysis** | AQWA .LIS processing and RAO extraction |
| **orcawave-analysis** | OrcaWave diffraction/radiation analysis |
| **bemrosetta** | AQWA → OrcaFlex converter with QTF support |
| **hydrodynamics** | 6×6 matrices, wave spectra, OCIMF loading |
| **orcaflex-rao-import** | Multi-format RAO import to OrcaFlex |
| **orcawave-to-orcaflex** | OrcaWave to OrcaFlex conversion |
| **orcawave-aqwa-benchmark** | Cross-validation comparison |

## Module Locations

```
src/digitalmodel/modules/
├── aqwa/                    # AQWA analysis tools
├── orcawave/                # OrcaWave analysis
├── bemrosetta/              # Format conversion
│   ├── parsers/             # AQWA, QTF parsers
│   ├── converters/          # OrcaFlex export
│   ├── mesh/                # GDF, DAT, STL handlers
│   └── validators/          # Coefficient, causality
├── diffraction/             # Unified schemas
│   ├── output_schemas.py    # DiffractionResults
│   ├── aqwa_converter.py    # AQWA to unified
│   ├── orcawave_converter.py # OrcaWave to unified
│   ├── orcaflex_exporter.py # Export to OrcaFlex
│   └── comparison_framework.py # Compare results
└── hydrodynamics/           # Coefficient database
```

## References

- OrcaFlex Documentation: https://www.orcina.com/webhelp/OrcaFlex/
- WAMIT Manual: https://www.wamit.com/manual.htm
- BEMRosetta: https://github.com/BEMRosetta/BEMRosetta
