# OrcaFlex Agent - CALM Buoy Project Learnings and Patterns

## Executive Summary

**Status**: ⚠️ **LEARNINGS NOT FULLY CAPTURED IN AGENT**

The CALM buoy project work was completed **manually** without systematic use of the OrcaFlex agent. While successful patterns were established, they have NOT been encoded into agent workflows or templates.

**Critical Gap**: Manually discovered best practices need to be formalized into agent capabilities.

---

## Current Agent Status

### What Exists
✅ **Performance optimization** - File size-based thread optimization (docs/ORCAFLEX_AUTO_OPTIMIZATION_GUIDE.md)
✅ **Post-processing framework** - Parallel processing with auto-optimization
✅ **Example files** - C06 CALM Buoy reference (docs/domains/orcaflex/mooring/buoy/C06 CALM Buoy/)
✅ **Basic documentation** - Installation, dynamic analysis guides

### What's Missing
❌ **CALM buoy project patterns** - File organization, modular structure
❌ **Base file templates** - Reusable vessel types, line types, environmental conditions
❌ **Naming conventions** - Flat file structure patterns
❌ **Workflow automation** - Agent-driven file generation
❌ **Post-processing templates** - Standard dm_vessel.yml patterns
❌ **Validation rules** - RAO amplitude requirements, AMD structure validation

---

## Proven Patterns from CALM Buoy Work

### 1. Modular File Structure

**Reference Projects:**
- `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/`
- `projects/modules/calm/nse_100m/TEST_OPERABILITY/orcaflex/base_files/`
- `D:\1522\ctr7\rev_a09\base_files`

**Pattern: Modular Base Files**
```
base_files/
├── 01_general.yml                    # Analysis settings, simulation time
├── 02_var_data.yml                   # Variables, data tables
├── 03_environment.yml                # Base environment (references env/)
├── 04_vessel_*.yml                   # Vessel type definitions (includes _04_vessel_type_*.yml)
├── 05_lines.yml                      # Line definitions (includes _05_line_types.yml, _07_lines.yml)
├── 06_buoys.yml                      # Buoy definitions
├── 08_groups.yml                     # Object groups
├── _90_calculated_positions_*.yml    # Automated position calculations
└── env/
    ├── current_000deg_1yr.yml        # Standalone, reusable current profile
    ├── wind_000deg_1yr.yml           # Standalone, reusable wind profile
    ├── waves_000deg_1yr.yml          # Standalone, reusable wave definition
    └── _env_000deg_1yr.yml           # Composite env file (includes above)
```

**Key Principles:**
1. **Reference, don't duplicate** - Base files referenced via `includefile`, not copied
2. **Standalone environmental files** - Each env component (wave, wind, current) is independent
3. **Composite patterns** - `_env_*.yml` files combine standalone components
4. **Type separation** - Vessel/line TYPES in separate files from INSTANCES

### 2. Environmental File Patterns

**Reference:**
- `D:\1522\ctr7\rev_a09\base_files\env\`

**Proven Structure:**
```yaml
# env_001yr_000deg.yml - Composite environmental condition
- includefile: _env_wave_001yr_000deg.yml
- includefile: _env_wind_001yr_000deg.yml
- includefile: _env_curr_1ms_000deg.yml
```

**Naming Convention:**
- `env_{return_period}_{heading}.yml` - Main env file
- `_env_wave_{return_period}_{heading}.yml` - Wave component
- `_env_wind_{return_period}_{heading}.yml` - Wind component
- `_env_curr_{speed}_{heading}.yml` - Current component

**Return Periods**: 001yr, 010yr, 100yr
**Headings**: 000deg, 015deg, 030deg, ..., 345deg (24 directions)

### 3. Analysis Model Structure

**Reference:**
- `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/analysis_models/`
- `D:\1522\ctr7\rev_a09\03c_100yr/`

**Flat File Pattern:**
```
analysis_models/
├── CALM_atb_000deg_1yr_simple.yml
├── CALM_atb_000deg_1yr_simple.sim
├── CALM_atb_030deg_1yr_simple.yml
├── CALM_atb_030deg_1yr_simple.sim
├── NSE_CALM_001_000deg_1yr_simple.yml
└── NSE_CALM_001_000deg_1yr_simple.sim
```

**YAML Content (minimal):**
```yaml
BaseFile: ../base_files/calm_buoy_simple_base_crowley.yml
includefile: ../base_files/env/_env_000deg_1yr.yml
```

**Naming Convention:**
- `{project}_{heading}_{return_period}_{condition}.yml`
- Examples:
  - `CALM_atb_000deg_1yr_simple`
  - `fsts_l015_lwl_ncl_090deg`
  - `NSE_CALM_001_000deg_no_load`

### 4. Vessel Configuration Patterns

**Critical Learnings:**

#### RAO Amplitude Requirements
```yaml
# ❌ WRONG - Negative amplitudes cause OrcaFlex errors
RAOPeriodOrFrequency, RAOSurgeAmp, RAOSurgePhase:
  - [5, -0.015, 85]  # ERROR: Cannot be negative

# ✅ CORRECT - All amplitudes positive, directionality via phase
RAOPeriodOrFrequency, RAOSurgeAmp, RAOSurgePhase:
  - [5, 0.015, 265]  # Positive amplitude, 180° phase shift
```

**Rule**: ALL RAO amplitudes must be ≥ 0. Use phase angles (0-360°) for directionality.

#### MomentOfInertiaTensor Format
```yaml
# ❌ WRONG - Inline with missing values
MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ:
  - [0.191e6, 0, 0]
  - [13.1e9, 0]      # Missing column
  - [13.1e9, 0, 0]

# ✅ CORRECT - Full 3x3 matrix
MomentOfInertiaTensorX, MomentOfInertiaTensorY, MomentOfInertiaTensorZ:
  - [0.191e6, 0, 0]
  - [0, 13.1e9, 0]   # Full row
  - [0, 0, 13.1e9]
```

#### AMD (Added Mass and Damping) Structure
```yaml
# ✅ CORRECT - Frequency dependent method
AMDMethod: Frequency dependent
FrequencyDependentAddedMassAndDamping:
  - AMDPeriodOrFrequency: 4
    AddedMassMatrixX, AddedMassMatrixY, ...:
      - [120.4, 0, 118.8, 0, 9297.5, 0]
      # ... 6x6 matrix
    DampingX, DampingY, DampingZ, ...:
      - [360.9, 0, 121.65, 0, 16.34e3, 0]
      # ... 6x6 matrix
```

**Rule**: Working files use `FrequencyDependentAddedMassAndDamping`, not simple `AddedMassAndDamping`.

### 5. Post-Processing Configuration

**Reference:**
- `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/scripts/dm_vessel.yml`
- `D:\1522\ctr7\rev_a09\postproc\dm_fsts_lngc.yml`

**Standard Structure:**
```yaml
meta:
  library: digitalmodel
  basename: orcaflex_post_process
  label: vessel

default:
  log_level: INFO

Analysis:
  result_folder: ../results
  file_name: vessel_postprocess

file_management:
  input_directory: ../analysis_models
  output_directory: NULL
  filename:
    pattern: CALM_atb  # Match file naming pattern

orcaflex:
  postprocess:
    summary:
      flag: True
    time_series:
      flag: True

summary_settings:
  groups:
    - Label: vessel1_6dof_dyn
      Columns:
        - ObjectName: Vessel1
          Variable: Dynamic x
          Statistic_Type: Min
          Label: vessel1_dynamic_x_min

    - Label: calm_buoy_6dof_abs
      Columns:
        - ObjectName: CALMTop
          Variable: X
          Statistic_Type: Min
          Label: CALMTop_X_abs_min

    - Label: line_tension
      Columns:
        - ObjectName: Mooring1
          Variable: Effective Tension
          Statistic_Type: Min
          Label: Mooring1_eff_tension_min
```

**Key Patterns:**
1. **Object name matching** - Must match actual OrcaFlex model object names (Vessel1, CALMTop, Mooring1-6)
2. **Analysis section required** - Must specify `result_folder` and `file_name`
3. **Pattern matching** - Use project prefix to match multiple files
4. **Standard outputs**:
   - Vessel 6DOF (dynamic and absolute)
   - CALM buoy positions
   - All mooring line tensions
   - Time series for critical lines

### 6. Output Organization

**Reference Structure:**
```
scripts/
├── dm_vessel.yml                      # Post-processing config
└── results/
    ├── dm_vessel_vessel_vessel1_6dof_dyn.csv
    ├── dm_vessel_vessel_vessel1_6dof_abs.csv
    ├── dm_vessel_vessel_calm_buoy_6dof_abs.csv
    ├── dm_vessel_vessel_line_tension.csv
    ├── CALM_atb_000deg_1yr_simple_Mooring1.csv
    └── ... (time series for each simulation)
```

**Naming Patterns:**
- Summary: `{config_name}_{label}_{group_label}.csv`
- Time series: `{simulation_name}_{object_name}.csv`

---

## Critical Findings - Post-Processing Work

### Issues Resolved
1. **YAML syntax error** - `pattern: *` → `pattern: "CALM_atb"`
2. **Object name mismatch** - `FST1` → `Vessel1`, `Strut1_Body` → `Mooring1`
3. **Missing configuration** - Added `Analysis` section with result paths

### Performance Results
- **6 simulations** processed in **6.7 seconds**
- **1.12 seconds per file** average
- **3 parallel workers** used
- **10 CSV output files** generated (1.3 MB total)

### Critical Mooring Tensions Found
- ⚠️ **Mooring5 (CALM ATB 030deg): 2267.69 kN** - Requires engineering review
- Mooring4 (CALM ATB 000deg): 864.59 kN
- Mooring5 (NSE 000deg): 1058.71 kN

---

## Gaps in Agent Capabilities

### 1. File Generation Not Automated
**Current**: Manual creation of base files, env files, analysis models
**Needed**: Agent commands to generate complete file sets
```bash
# Should exist:
orcaflex-agent generate-base-files --project calm_buoy --vessel crowley650
orcaflex-agent generate-env-files --return-periods 1,10,100 --headings all
orcaflex-agent generate-analysis-models --base calm_buoy_base --env-dir env/
```

### 2. No Validation Templates
**Current**: Manual troubleshooting of OrcaFlex errors
**Needed**: Pre-validation before file generation
- RAO amplitude checker
- Matrix structure validator
- Object name consistency checker
- AMD method validator

### 3. No Pattern Templates
**Current**: Copy-paste from working examples
**Needed**: Reusable templates with parameter substitution
```yaml
# Template: vessel_type_template.yml
Name: {{vessel_name}}
VesselType: {{vessel_type}}
Length: {{length}}
Draught: {{draught}}
# ... with parameter injection
```

### 4. Post-Processing Not Integrated
**Current**: Separate manual configuration of dm_vessel.yml
**Needed**: Auto-generation from model inspection
```bash
# Should exist:
orcaflex-agent generate-postproc --model-dir analysis_models/ --output scripts/
# Should auto-detect: Vessel1, CALMTop, Mooring1-6, Hawser
```

### 5. No Workflow Orchestration
**Current**: Manual execution of each step
**Needed**: End-to-end workflow automation
```bash
# Should exist:
orcaflex-agent run-workflow calm_buoy_operability
# Should execute:
#   1. Generate base files
#   2. Generate env files
#   3. Generate analysis models
#   4. Run simulations
#   5. Post-process results
#   6. Generate summary reports
```

---

## Recommended Actions

### Immediate (High Priority)
1. ✅ **Document patterns** (THIS FILE)
2. 📝 **Create agent specification** for CALM buoy workflows
3. 🔧 **Build validation utilities** (RAO, matrix, object name checkers)
4. 📋 **Create file templates** for base files, env files, analysis models

### Short-Term (Next Sprint)
1. 🤖 **Implement agent commands** for file generation
2. 🔄 **Add pattern-based automation** for post-processing config
3. ✅ **Create working examples** repository with all patterns
4. 📊 **Build validation reports** for model consistency

### Long-Term (Strategic)
1. 🧠 **Machine learning** from successful patterns
2. 🔍 **Auto-detection** of model structure and object names
3. 🚀 **Full workflow orchestration** from project definition to results
4. 📈 **Performance profiling** and optimization recommendations

---

## Files to Reference for Agent Development

### Pattern Examples
1. **Base Files**: `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/base_files/`
2. **Env Files**: `D:\1522\ctr7\rev_a09\base_files\env\`
3. **Analysis Models**: `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/analysis_models/`
4. **Post-Processing**: `projects/modules/calm/baltic_039m/TEST_OPERABILITY/orcaflex/scripts/dm_vessel.yml`

### Documentation
1. **Optimization Guide**: `docs/ORCAFLEX_AUTO_OPTIMIZATION_GUIDE.md`
2. **CALM Buoy Example**: `docs/domains/orcaflex/mooring/buoy/C06 CALM Buoy/`
3. **User Prompt**: `docs/domains/orcaflex/mooring/buoy/C06 CALM Buoy/user_prompt.md`

### Working Reference Project
1. **Complete Example**: `D:\1522\ctr7\rev_a09\`
   - Base files: `base_files/`
   - Env files: `base_files/env/`
   - Analysis: `03c_100yr/`
   - Post-proc: `postproc/`

---

## Next Steps for Agent Enhancement

1. **Review this document** with engineering team
2. **Prioritize patterns** to encode first
3. **Create agent specification** using SPARC methodology
4. **Implement validation utilities** as standalone tools
5. **Build file generation** commands with templates
6. **Test automation** on new CALM buoy project
7. **Iterate and refine** based on real-world usage

---

**Document Status**: Draft for Review
**Created**: 2025-11-15
**Author**: Claude Code (based on manual CALM buoy work)
**Next Review**: After agent specification complete
