# Diffraction Analysis Suite

### Unified Hydrodynamic Diffraction -- From Raw Solver Output to Validated Engineering Data

---

## The Challenge

Hydrodynamic diffraction analysis is the foundation of every floating structure design -- FPSOs, semi-submersibles, spars, and TLPs all depend on accurate wave-body interaction data to size moorings, predict motions, and qualify structural fatigue life. Yet the current state of practice imposes severe friction:

- **Multi-tool fragmentation** -- engineers juggle AQWA, OrcaWave, and downstream tools with incompatible formats, manually transferring results between systems
- **No cross-solver verification** -- running the same vessel through two solvers requires weeks of manual reformatting and side-by-side comparison
- **Silent data errors** -- non-physical added mass matrices, broken phase conventions, and asymmetric coefficients propagate undetected into mooring and structural models
- **Late design rework** -- validation gaps discovered during detailed engineering force costly re-runs of analyses performed months earlier
- **Batch bottlenecks** -- parametric studies across draft, trim, heading, and frequency require manual setup of hundreds of individual runs

Design teams need a unified framework that eliminates format translation, enforces physics-based validation, and scales from single-vessel checks to fleet-wide parametric campaigns.

---

## The Solution

**Diffraction Analysis Suite** is a comprehensive hydrodynamic analysis ecosystem (~24,000 lines of code across 100+ files) that unifies AQWA and OrcaWave solvers behind a single data schema, validates every result against physics-based criteria, and exports to any downstream tool -- all driven from the command line or Python API.

```
  Canonical Specification (spec.yml)
        |
        v
  +---------------------------+
  |   Input Generators         |  Two solver backends:
  |  (AQWA .DAT | OrcaWave    |  AQWA (ANSYS boundary element)
  |   .yml)                    |  OrcaWave (Orcina diffraction)
  +---------------------------+
        |
        v
  +---------------------------+
  |   Solver Execution         |  Parallel batch execution
  |  (Local | Queue | Batch)   |  Job scheduling, progress
  |                            |  tracking, error recovery
  +---------------------------+
        |
        v
  +---------------------------+
  |   Unified Parser           |  AQWA .LIS/.DAT parser
  |  (DiffractionResults)      |  OrcaWave native parser
  |                            |  6-DOF RAOs, 6x6 matrices
  +---------------------------+
        |
        v
  +---------------------------+
  |   Validation Engine        |  8+ physics checks:
  |  (Symmetry | PSD | K-K |  |  Causality, semi-definiteness,
  |   RAO | Geometry)          |  resonance, mesh quality
  +---------------------------+
        |
        v
  Validated Export: OrcaFlex YAML | CSV | Excel | HTML Reports
```

---

## Feature Highlights

### Unified Data Schema

Every solver's output maps to a single **DiffractionResults** object -- no more format-specific scripts:

| Field | Content |
|-------|---------|
| **vessel_name** | Vessel identifier |
| **frequencies** | Frequency vector (Hz or rad/s with unit tracking) |
| **headings** | Wave heading set with convention metadata |
| **raos** | RAOSet -- all 6 DOF (surge, sway, heave, roll, pitch, yaw) |
| **added_mass** | 6x6 frequency-dependent matrices per frequency |
| **damping** | 6x6 frequency-dependent radiation damping matrices |
| **source_file** | Provenance tracking back to raw solver output |

### Dual-Solver Support

| Solver | Integration Depth | Key Capabilities |
|--------|-------------------|------------------|
| **AQWA** | Native .LIS/.DAT parsing, input generation, batch execution | 4,500+ LOC, 37+ tests (WRK-025), batch campaigns (WRK-027) |
| **OrcaWave** | Native solver orchestration, file preparation, postprocessing | 1,900+ LOC, 63 tests at 90.35% coverage (WRK-030) |

Both solvers produce identical **DiffractionResults** objects. Switch solvers by changing one line in your spec file.

### Multi-Solver Comparison

Run the same vessel through both AQWA and OrcaWave, then:

- **Statistical comparison** -- frequency-by-frequency delta analysis across all 6 DOF
- **Consensus metrics** -- quantified agreement scores between solvers
- **Interactive benchmark plots** -- Plotly HTML reports with overlay RAO curves, added mass comparisons, and damping cross-plots

### Canonical Specification Format

Define your analysis once in **spec.yml** and generate inputs for any solver:

```
spec.yml  -->  AQWA .DAT input files
          -->  OrcaWave .yml configuration
          -->  Mesh files (GDF, DAT, STL, OBJ)
```

Reverse parsers convert existing AQWA or OrcaWave files back into spec.yml, enabling migration of legacy analyses into the unified framework.

### Mesh Pipeline

The **PanelMesh** common format handles all mesh operations:

| Capability | Detail |
|------------|--------|
| **Format conversion** | GDF, DAT, STL, OBJ -- bidirectional |
| **Parametric generation** | GMSH integration for automated mesh creation |
| **Quality validation** | Panel area, thickness, aspect ratio, normal orientation checks |
| **Solver targeting** | Export mesh in the exact format each solver expects |

### BEMRosetta Integration

**4,700+ lines** of format conversion, QTF (Quadratic Transfer Function) parsing, and causality validation. Bridge results between any boundary element method solver through the BEMRosetta ecosystem.

---

## Validation Suite

The framework enforces **8+ physics-based validation checks** that catch errors before they reach downstream models:

| Validation | What It Catches |
|------------|-----------------|
| **Coefficient Symmetry** | Off-diagonal asymmetry in added mass or damping matrices |
| **Positive Semi-Definiteness** | Non-physical negative eigenvalues in hydrodynamic matrices |
| **Kramers-Kronig Causality** | Inconsistency between added mass and damping (violates linear potential theory) |
| **RAO Quality** | Incorrect phase conventions, anomalous peaks, missing DOFs |
| **Vessel Type Classification** | RAO patterns inconsistent with declared vessel type |
| **Peak Detection** | Unreported or spurious resonance peaks in motion response |
| **Geometry Quality** | Degenerate panels, excessive aspect ratios, inverted normals |
| **Resonance Detection** | Undocumented natural frequencies in output data |

Every validation produces a structured report with severity levels, file locations, and remediation guidance.

---

## Sample Output

A single parse-and-validate call returns the complete hydrodynamic dataset:

```json
{
  "vessel_name": "FPSO Alpha",
  "frequencies": {
    "values": [0.02, 0.04, 0.06, 0.08, 0.10, 0.12, 0.15, 0.20, 0.30],
    "unit": "Hz",
    "count": 9
  },
  "headings": {
    "values": [0, 45, 90, 135, 180],
    "convention": "from",
    "unit": "deg"
  },
  "raos": {
    "heave": {
      "amplitude": [[1.00, 0.98, 0.95, 0.97, 1.00], "...per_frequency"],
      "phase": [[-2.1, -3.4, -5.6, -3.2, -2.1], "...per_frequency"]
    },
    "roll": {
      "amplitude": [[0.01, 2.34, 4.12, 2.34, 0.01], "...per_frequency"],
      "phase": [[0.0, -45.2, -89.7, -134.8, -180.0], "...per_frequency"]
    }
  },
  "added_mass": {
    "frequency_index": 0,
    "matrix_6x6": [
      [1.23e7, 0.0, 0.0, 0.0, -4.56e7, 0.0],
      [0.0, 1.23e7, 0.0, 4.56e7, 0.0, 0.0],
      [0.0, 0.0, 8.91e7, 0.0, 0.0, 0.0],
      [0.0, 4.56e7, 0.0, 3.21e9, 0.0, 0.0],
      [-4.56e7, 0.0, 0.0, 0.0, 5.67e9, 0.0],
      [0.0, 0.0, 0.0, 0.0, 0.0, 1.11e9]
    ]
  },
  "damping": {
    "frequency_index": 0,
    "matrix_6x6": "...(symmetric 6x6, same structure)"
  },
  "validation": {
    "symmetry": "PASS",
    "positive_semi_definite": "PASS",
    "kramers_kronig_causality": "PASS",
    "rao_quality": "PASS",
    "geometry_quality": "PASS"
  },
  "source_file": "fpso_alpha_aqwa_results.LIS"
}
```

**Reading this result**: The vessel "FPSO Alpha" was analyzed across 9 frequencies and 5 headings. All 6 DOF RAOs are captured with amplitude and phase. The 6x6 added mass and damping matrices are frequency-dependent and fully validated -- symmetry, positive semi-definiteness, and Kramers-Kronig causality all pass. The data is ready for direct export to OrcaFlex, Excel, or any downstream analysis tool.

---

## Before and After

| Capability | Manual Multi-Tool Workflow | Diffraction Analysis Suite |
|------------|---------------------------|----------------------------|
| Solver support | One solver per project | **2 solvers, unified schema** |
| Format conversion | Manual scripting per project | **Automated: AQWA, OrcaWave, BEMRosetta** |
| Cross-solver comparison | Weeks of manual alignment | **Single CLI command** |
| Data validation | Visual spot-checks | **8+ automated physics checks** |
| Kramers-Kronig causality | Rarely performed | **Automatic on every parse** |
| Mesh handling | Solver-specific formats only | **PanelMesh: GDF, DAT, STL, OBJ + GMSH** |
| Parametric studies | Manual setup per run | **Batch execution with job queuing** |
| Canonical spec format | None (recreate inputs each time) | **spec.yml: define once, run anywhere** |
| Reverse parsing | Not available | **AQWA .DAT and OrcaWave .yml to spec.yml** |
| Export targets | Manual reformatting | **OrcaFlex YAML, CSV, Excel, JSON, HTML** |
| Reporting | Screenshots and spreadsheets | **Interactive Plotly HTML reports** |
| Test coverage | None | **129+ automated tests** |

---

## Business Benefits

**Eliminate Format Translation Overhead**
A single canonical specification drives both AQWA and OrcaWave. No more maintaining parallel input decks, reformatting results between tools, or writing one-off conversion scripts.

**Catch Errors Before They Propagate**
Eight physics-based validation checks run automatically on every parse. Non-physical matrices, broken phase conventions, and causality violations are flagged before they contaminate mooring analyses, structural models, or fatigue assessments.

**Enable Cross-Solver Verification**
Run the same vessel through both solvers with a single spec file. Statistical comparison and consensus metrics give quantified confidence in your hydrodynamic data -- essential for class submissions and design reviews.

**Scale to Fleet-Wide Campaigns**
Batch execution with parallel processing, job queuing, and automated reporting transforms parametric studies from week-long manual efforts into overnight automated campaigns.

**Reduce Late-Stage Design Rework**
Validation at the point of analysis -- not months later during detailed engineering -- catches issues when they cost hours to fix, not weeks.

**Preserve Institutional Knowledge**
The canonical spec.yml format captures analysis intent, solver configuration, and mesh parameters in a version-controlled, human-readable file. Analyses are reproducible regardless of which engineer runs them.

**No Vendor Lock-In**
Export to OrcaFlex YAML, CSV, Excel, JSON, or HTML. Your hydrodynamic data is never trapped in a proprietary format.

---

## Technical Specifications

| Specification | Detail |
|---------------|--------|
| **Language** | Python 3.11+ |
| **Codebase** | ~24,000 lines of code across 100+ files |
| **Solver Backends** | AQWA (ANSYS), OrcaWave (Orcina) |
| **Data Schema** | DiffractionResults -- 6-DOF RAOs, 6x6 added mass and damping |
| **Mesh Formats** | GDF, DAT, STL, OBJ (PanelMesh common format) |
| **Mesh Generation** | GMSH integration for parametric hull generation |
| **Export Formats** | OrcaFlex YAML, CSV, Excel, JSON, HTML (Plotly) |
| **Validation Checks** | 8+ (symmetry, PSD, Kramers-Kronig, RAO, geometry, resonance) |
| **Format Converter** | BEMRosetta integration (4,700+ LOC) |
| **CLI Commands** | 15+ (convert, compare, batch, validate, mesh, plot, parse) |
| **Automated Tests** | 129+ across 7 work items (WRK-025 through WRK-063) |
| **Test Coverage** | 90.35% (OrcaWave module) |
| **Data Models** | Pydantic v2 validated schemas throughout |
| **Reporting** | Interactive Plotly HTML with overlay comparisons |
| **Batch Execution** | Parallel processing with job queuing and error recovery |

---

## How It Works -- In Three Steps

**Step 1: Define**
Create a canonical **spec.yml** describing the vessel geometry, analysis frequencies, wave headings, and solver preferences. Or reverse-parse an existing AQWA .DAT or OrcaWave .yml file to generate the spec automatically. The framework validates the specification against the schema before proceeding.

**Step 2: Execute**
The framework generates solver-specific input files, submits runs (single or batch with parallel execution), monitors progress, and parses raw output into the unified **DiffractionResults** schema. All 8+ validation checks run automatically. For multi-solver campaigns, both AQWA and OrcaWave results are parsed into identical data structures for direct comparison.

**Step 3: Deliver**
Export validated results to OrcaFlex YAML for time-domain simulation, CSV or Excel for engineering review, JSON for programmatic integration, or interactive Plotly HTML reports for design presentations. Cross-solver comparison reports overlay RAO curves, added mass trends, and damping coefficients with statistical agreement metrics.

---

## Who Benefits

| Role | Value |
|------|-------|
| **Naval Architects** | Unified diffraction workflow across AQWA and OrcaWave -- define once, run on any solver |
| **Hydrodynamic Engineers** | Automated validation catches non-physical results before they enter downstream models |
| **Mooring Engineers** | Validated, OrcaFlex-ready hydrodynamic data with full provenance tracking |
| **Structural Engineers** | Consistent motion RAOs and load transfer functions across all analysis variants |
| **Project Engineers** | Batch parametric studies that previously took weeks reduced to overnight campaigns |
| **Design Review Leads** | Cross-solver comparison reports with quantified consensus for class submissions |
| **Technical Managers** | Reproducible, version-controlled analyses independent of individual engineer workflows |

---

## Get Started

The **Diffraction Analysis Suite** is available as a Python package with a full CLI interface.

**Quick start with the CLI:**

```bash
# Parse AQWA results into unified format
dm convert-aqwa --input results.LIS --output vessel_data.yml

# Parse OrcaWave results
dm convert-orcawave --input orcawave_results/ --output vessel_data.yml

# Compare results from two solvers
dm compare --aqwa aqwa_data.yml --orcawave orcawave_data.yml --report comparison.html

# Validate hydrodynamic data
dm validate-spec --input vessel_data.yml

# Run batch parametric study
dm batch --spec parametric_spec.yml --parallel 8 --output results/

# Convert and validate mesh
dm mesh-convert --input hull.gdf --output hull.dat --validate

# Plot RAOs interactively
dm plot-raos --input vessel_data.yml --output raos.html
```

**To learn more or request a technical evaluation:**

| | |
|---|---|
| **Email** | [contact@yourcompany.com] |
| **Web** | [www.yourcompany.com/diffraction] |
| **Technical Documentation** | Available upon request |

---

<sub>Diffraction Analysis Suite -- Unified solvers. Physics-validated data. From specification to export in a single framework.</sub>
