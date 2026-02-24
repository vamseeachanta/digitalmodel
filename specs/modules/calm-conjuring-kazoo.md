# Pipeline Installation - Modular Conversion & Postprocessing Standardization

## Plan Location
**Spec File:** `specs/modules/calm-conjuring-kazoo.md`
**Module:** OrcaFlex → Pipeline → Installation

---

## Overview
Convert 24in pipeline installation models to modular format and standardize postprocessing for reuse across offshore structures.

---

## Part 1: Modular Format Conversion

### 1.1 Target Directory Structure
```
docs/modules/orcaflex/pipeline/installation/floating/24in_pipeline/
├── modular/                          # NEW: Modular format
│   ├── master.yml                    # Main composition file
│   ├── includes/
│   │   ├── 01_general.yml           # Simulation settings, mesh size
│   │   ├── 02_var_data.yml          # Variable data tables
│   │   ├── 03_environment.yml       # Wave, current, wind
│   │   ├── 05_line_types.yml        # Pipeline properties
│   │   ├── 07_lines.yml             # Pipeline instance
│   │   ├── 08_buoys.yml             # Buoyancy modules
│   │   ├── 09_shapes.yml            # Seabed, vessel visualization
│   │   ├── 10_groups.yml            # Pipeline groups (optional)
│   │   └── 12_winches.yml           # Tensioner/winch (if used)
│   ├── inputs/
│   │   └── parameters.yml           # Extracted parameters for variation
│   ├── components/                   # NEW: Reusable components
│   │   ├── pipe/
│   │   │   ├── 24in_X65_WT25mm.yml
│   │   │   ├── 24in_X65_WT31mm.yml
│   │   │   └── ...
│   │   ├── coating/
│   │   │   ├── cwc_60mm.yml
│   │   │   ├── cwc_80mm.yml
│   │   │   └── ...
│   │   ├── buoyancy/
│   │   │   ├── buoy_50kN_spacing_3800mm.yml
│   │   │   ├── buoy_7kN_spacing_1500mm.yml
│   │   │   └── ...
│   │   └── environment/
│   │       ├── env_001yr.yml
│   │       ├── env_95NE.yml
│   │       └── ...
│   ├── tension/                      # Tension configurations
│   │   ├── 1500kN.yml
│   │   ├── 2000kN.yml
│   │   └── 2500kN.yml
│   └── runs/                         # Analysis run files
│       ├── 1500kN_env_001yr_090deg.yml
│       └── ...
├── monolithic/                       # EXISTING: Keep for reference
└── postproc/                         # Postprocessing (shared)
    ├── dm_pipeline_postproc.yml
    ├── generate_html_report.py
    └── results/
```

### 1.2 File Naming Conventions

**Include Files (Numbered Prefix):**
| # | Filename | Content |
|---|----------|---------|
| 01 | `01_general.yml` | Simulation settings, mesh, time step |
| 02 | `02_var_data.yml` | Variable data tables |
| 03 | `03_environment.yml` | Wave, current, wind |
| 05 | `05_line_types.yml` | Pipeline type definitions |
| 07 | `07_lines.yml` | Pipeline instances |
| 08 | `08_buoys.yml` | Buoyancy module definitions |
| 09 | `09_shapes.yml` | 3D shapes (seabed, vessel) |
| 10 | `10_groups.yml` | Object groups |
| 12 | `12_winches.yml` | Tensioner/winch |

**Component Files:**
```
{component_type}_{key_param}_{value}.yml

Examples:
  pipe/24in_X65_WT25mm.yml        # 24" OD, X65 steel, 25mm wall
  coating/cwc_60mm_1800kgm3.yml   # 60mm CWC, 1800 kg/m³
  buoyancy/buoy_50kN_sp3800mm.yml # 50kN uplift, 3800mm spacing
```

**Run Files:**
```
{tension}kN_env_{environment}_{heading}deg.yml

Examples:
  1500kN_env_001yr_090deg.yml
  2000kN_env_95NE_135deg.yml
```

**Parameters File (`inputs/parameters.yml`):**
```yaml
# Pipeline Installation Parameters
# 24in Floating Pipeline

# Pipeline Properties
pipe_od_inch: 24
pipe_wt_mm: 25.4
pipe_grade: X65
pipe_smys_mpa: 448.2

# Coating
cwc_thickness_mm: 60
cwc_density_kgm3: 1800

# Buoyancy
buoy_uplift_kn: 50
buoy_spacing_mm: 3800
float_ratio: 1.2  # Upthrust / Submerged Weight

# Mesh
mesh_size_m: 1.5
mesh_fine_m: 0.5  # At TDP and vessel

# Simulation
simulation_duration_s: 200
time_step_s: 0.1
```

### 1.3 Mesh Refinement Strategy

**Target Mesh Sizes:**
| Zone | Arclength Range | Mesh Size |
|------|-----------------|-----------|
| Vessel End (End A) | 0 - 50m | 0.5m |
| Upper Catenary | 50m - 500m | 1.5m |
| Overbend | 500m - 1500m | 1.5m |
| Sagbend/TDP | 1500m - 2500m | 0.5m |
| Seabed | 2500m - End | 1.5m |
| End B | Last 50m | 0.5m |

**Implementation in `01_general.yml`:**
```yaml
General:
  TargetSegmentLength: 1.5  # Default mesh

# In 07_lines.yml - variable segmentation
Line:
  pipeline:
    TargetSegmentLength:
      - [0, 50, 0.5]       # Vessel end
      - [50, 500, 1.5]     # Upper catenary
      - [1500, 2500, 0.5]  # TDP zone
      - [4850, 4900, 0.5]  # End B
```

---

## Part 2: Postprocessing Standardization

### 2.1 Standard Configuration File
**Location:** `postproc/dm_pipeline_postproc_standard.yml`

### 2.2 Required Extraction Sections

| Section | OrcaFlex Reference | Description |
|---------|-------------------|-------------|
| End A | `objectExtra: End A` | Vessel connection |
| End B | `objectExtra: End B` | Seabed termination |
| TDP | `objectExtra: Touchdown` | Touchdown point |
| Overbend | `ArcLength: [max_curvature_loc]` | Maximum curvature in overbend |
| Sagbend | `ArcLength: [sag_loc]` | Maximum sag point |
| Along Line | `RangeGraph` | Full arclength distribution |

### 2.3 Required Parameters

**Summary Statistics (per section):**
| Parameter | Variable Name | Units | Static | Dynamic |
|-----------|--------------|-------|--------|---------|
| Effective Tension | `Effective Tension` | kN | ✓ | Min/Max/Mean |
| Bending Moment | `Bend Moment` | kN.m | ✓ | Min/Max |
| Max Bending Stress | `Max Bending Stress` | kPa→MPa | ✓ | Min/Max |
| Direct Tensile Stress | `Direct Tensile Stress` | kPa→MPa | ✓ | Min/Max |
| Direct Tensile Strain | `Direct Tensile Strain` | - →% | ✓ | Min/Max |
| Curvature | `Curvature` | 1/m | ✓ | Min/Max |
| Global X | `x` | m | ✓ | Min/Max |
| Global Y | `y` | m | ✓ | Min/Max |
| Global Z | `z` | m | ✓ | Min/Max |
| Seabed Clearance | `Seabed Clearance` | m | ✓ | Min |

**Utilization Calculations (post-extraction):**
| Check | Formula | Limit |
|-------|---------|-------|
| Bending Utilization | `MaxBendStress / SMYS` | ≤ 1.0 |
| Combined Utilization | `(MaxBendStress + MaxTensStress) / SMYS` | ≤ 1.0 |
| DNV-OS-F101 | Per code | Per code |
| API RP 1111 | Per code | Per code |

**Range Graphs (along full arclength):**
| Parameter | Statistic | Period |
|-----------|-----------|--------|
| y (Global Y) | Min/Max/Mean | LatestWave |
| Effective Tension | Min/Max/Mean | LatestWave |
| Curvature | Min/Max/Mean | LatestWave |
| Max Bending Stress | Max | LatestWave |
| Direct Tensile Strain | Max | LatestWave |

### 2.4 Output Structure
```
postproc/
├── results/
│   ├── summary_static.csv           # Static results all sections
│   ├── summary_dynamic.csv          # Dynamic min/max/mean
│   ├── summary_utilization.csv      # Code check utilizations
│   ├── rangegraph_y.csv             # Global Y along arclength
│   ├── rangegraph_tension.csv       # Tension along arclength
│   ├── rangegraph_curvature.csv     # Curvature along arclength
│   └── timeseries_critical.csv      # Time series at TDP, End A
├── results_interactive.html         # Plotly dashboard
└── report_summary.html              # Summary report
```

---

## Part 3: Tasks Breakdown

### Task 1: Mesh Refinement (Priority: High)
- [ ] Define variable segmentation in `01_general.yml`
- [ ] Update `07_lines.yml` with zone-based mesh sizes
- [ ] Run mesh convergence study (coarse vs fine)
- [ ] Document mesh recommendations

### Task 2: Create Component Library (Priority: High)
- [ ] Create `components/pipe/` directory with pipe property files
- [ ] Create `components/coating/` directory with coating files
- [ ] Create `components/buoyancy/` directory with buoy configurations
- [ ] Create `components/environment/` directory with env files
- [ ] Update master.yml to reference components

### Task 3: Convert to Modular Format (Priority: High)
- [ ] Create `modular/` directory structure
- [ ] Extract includes from monolithic basefile
- [ ] Create `inputs/parameters.yml` with key parameters
- [ ] Create `master.yml` that composes includes
- [ ] Validate against monolithic results

### Task 4: Standardize Postprocessing (Priority: High)
- [ ] Create `dm_pipeline_postproc_standard.yml` template
- [ ] Add all required sections (End A, End B, TDP, etc.)
- [ ] Add all required parameters per section
- [ ] Add range graph configurations
- [ ] Add utilization calculation logic
- [ ] Update `generate_html_report.py` for new parameters

### Task 5: Documentation (Priority: Medium)
- [ ] Document modular structure in README.md
- [ ] Document postprocessing configuration options
- [ ] Create example parameter variation workflow

---

## Commonalities with Other Structures

This standardization applies to:
- **Risers:** Catenary, lazy wave, steep wave, pliant wave
- **Umbilicals & Flowlines:** Similar line analysis
- **Mooring Lines:** Tension, curvature extraction
- **J-tube Pull-in:** Installation analysis
- **S-lay/J-lay:** Pipelay installation

**Reusable Components:**
- Environment definitions
- Postprocessing configuration templates
- HTML report generator
- Utilization calculation functions

---

## Verification Checklist

- [ ] Modular model produces same static results as monolithic
- [ ] Mesh refinement shows <5% difference in key parameters
- [ ] All postprocessing parameters extract correctly
- [ ] Utilization calculations match hand calculations
- [ ] HTML report displays all required charts
- [ ] CSV outputs match OrcaFlex GUI values
