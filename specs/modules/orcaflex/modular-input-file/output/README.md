# OrcaFlex Modular Input File Structure

## Overview

This directory contains modular YAML files for OrcaFlex CALM (Catenary Anchor Leg Mooring) buoy analysis. The files have been split from two monolithic input files into reusable, modular components following OrcaFlex's `includefile` methodology.

## Source Files

1. **C06_CALM_Buoy.yml** (2,566 lines) - Simple buoy representation with discrete wave components
2. **C06_Discretised_CALM_Buoy.yml** (3,075 lines) - Discretised buoy representation with JONSWAP spectrum

## Modular Architecture

### Reusable Core Modules (11 files)

These modules are shared between both configurations:

| File | Section | Description | Lines |
|------|---------|-------------|-------|
| `_01a_units_analysis.yml` | General | Units, analysis data, Jacobian settings | 7 |
| `_01b_statics.yml` | General | Static analysis parameters | 5 |
| `_01c_dynamics.yml` | General | Dynamic analysis parameters | 7 |
| `_01d_stages.yml` | General | Stage duration and restart settings | 11 |
| `_01e_view.yml` | General | Default view parameters | 7 |
| `_02_variable_data.yml` | VariableData | Generic drag coefficient table | 17 |
| `_03a_sea_density.yml` | Environment | Sea properties and density | 9 |
| `_03b_seabed.yml` | Environment | Seabed model and properties | 9 |
| `_03d_current.yml` | Environment | Current profile | 13 |
| `_03e_wind.yml` | Environment | Wind parameters | 17 |
| `_04_vessel_types.yml` | VesselTypes | Vessel Type1 with RAOs | 1,450 |
| `_05_line_types.yml` | LineTypes | Line type definitions (Chain, Hose, Hawser) | 188 |
| `_06_vessels_buoys.yml` | Vessels | Main vessel object | 27 |

### Variant Modules - Configuration Specific (8 files)

#### Simple CALM Buoy Configuration

| File | Section | Description | Key Features |
|------|---------|-------------|--------------|
| `_03c_waves.yml` | Environment | User-specified wave components | 100 discrete components with periods/amplitudes/phases |
| `_07_lines.yml` | Lines | Mooring lines (simple) | 10 lines connecting to "CALM Base" |
| `_08_groups.yml` | Groups | Object groups (simple) | 2-buoy system grouping |

#### Discretised CALM Buoy Configuration

| File | Section | Description | Key Features |
|------|---------|-------------|--------------|
| `_03c_waves_jonswap.yml` | Environment | JONSWAP spectrum waves | Hs=2m, Tz=6s, gamma=3.3 |
| `_06_buoys_discretised.yml` | 6DBuoys | Discretised buoy system | 8 buoys: CALM Top + Master + 6 Radials |
| `_07_lines_discretised.yml` | Lines | Mooring lines (discretised) | 10 lines connecting to "Master" |
| `_08_groups_discretised.yml` | Groups | Object groups (discretised) | 8-buoy system grouping |

## Main Configuration Files

### 1. Simple CALM Buoy (`calm_buoy_base.yml`)

Uses monolithic buoy representation with discrete wave components:

```yaml
General: [_01a through _01e]
VariableData: [_02]
Environment: [_03a, _03b, _03c_waves, _03d, _03e]
VesselTypes: [_04]
LineTypes: [_05]
Vessels: [_06_vessels_buoys]
Lines: [_07_lines]
Groups: [_08_groups]
```

**Use Case:** Traditional CALM buoy analysis with time-history wave input

### 2. Discretised CALM Buoy (`discretised_calm_buoy_base.yml`)

Uses distributed buoy representation with spectrum-based waves:

```yaml
General: [_01a through _01e]
VariableData: [_02]
Environment: [_03a, _03b, _03c_waves_jonswap, _03d, _03e]
VesselTypes: [_04]
LineTypes: [_05]
Vessels: [_06_vessels_buoys]
6DBuoys: [_06_buoys_discretised]
Lines: [_07_lines_discretised]
Groups: [_08_groups_discretised]
```

**Use Case:** Advanced CALM buoy analysis with discretised structural representation

## Key Differences Between Configurations

| Aspect | Simple CALM Buoy | Discretised CALM Buoy |
|--------|------------------|----------------------|
| **Buoy Representation** | Monolithic (CALM Base + Top) | Distributed (Master + 6 Radials + Top) |
| **Number of 6D Buoys** | 2 | 8 |
| **Total Buoy Mass** | 250.001 kg | ~250 kg (distributed) |
| **Wave Input** | User-specified 100 components | JONSWAP spectrum |
| **Wave Definition** | Deterministic time-history | Stochastic spectrum |
| **Line Connections** | CALM Base | Master buoy |
| **Structural Fidelity** | Simplified | High-fidelity discretised |

## Module Organization

### Naming Convention

```
_XX[a-z]_<descriptive_name>[_variant].yml
```

- `XX` = Section number (01-08)
- `[a-z]` = Sub-section letter (for subdivided sections)
- `<descriptive_name>` = Clear description of content
- `[_variant]` = Optional variant identifier (e.g., _jonswap, _discretised)

### Section Numbering

| Number | Section | Sub-divisions |
|--------|---------|---------------|
| 01 | General | a=units/analysis, b=statics, c=dynamics, d=stages, e=view |
| 02 | VariableData | Single file |
| 03 | Environment | a=sea/density, b=seabed, c=waves, d=current, e=wind |
| 04 | VesselTypes | Single file |
| 05 | LineTypes | Single file |
| 06 | Vessels/Buoys | vessels_buoys + buoys_discretised (variant) |
| 07 | Lines | Single file + discretised (variant) |
| 08 | Groups | Single file + discretised (variant) |

## Usage in OrcaFlex

### Loading a Configuration

1. Open OrcaFlex
2. File → Open
3. Navigate to `output/` directory
4. Select either:
   - `calm_buoy_base.yml` (simple configuration)
   - `discretised_calm_buoy_base.yml` (discretised configuration)
5. OrcaFlex will automatically load all referenced `includefile` modules

### Modifying Parameters

**To change a specific parameter:**

1. Identify which module contains the parameter (see tables above)
2. Edit only that module file
3. Save the module file
4. Reload the main configuration file in OrcaFlex
5. Changes will be reflected automatically via `includefile` references

**Example:** To change wave height in discretised model:
- Edit: `_03c_waves_jonswap.yml`
- Modify: `WaveHs: 2` → `WaveHs: 3`
- Save and reload `discretised_calm_buoy_base.yml`

### Creating Custom Configurations

Mix and match modules to create custom configurations:

```yaml
# Example: Simple buoy with JONSWAP waves
General: [_01a through _01e]
VariableData: [_02]
Environment: [_03a, _03b, _03c_waves_jonswap, _03d, _03e]  # Use JONSWAP
VesselTypes: [_04]
LineTypes: [_05]
Vessels: [_06_vessels_buoys]
Lines: [_07_lines]  # Use simple lines
Groups: [_08_groups]  # Use simple groups
```

## File Statistics

### Total Modular Files

- **Core modules:** 13 files (reusable)
- **Variant modules:** 4 pairs = 8 files
- **Main configuration files:** 2 files
- **Documentation:** 1 README
- **Total:** 24 files

### Reusability Analysis

- **100% Reusable:** 13 modules (56.5%)
- **Variant-specific:** 8 modules (34.8%)
- **Configuration files:** 2 (8.7%)

### Size Breakdown

| Category | Total Lines | Percentage |
|----------|-------------|------------|
| Reusable modules | ~1,760 lines | 68% |
| Variant modules | ~830 lines | 32% |

## Benefits of Modular Approach

1. **Reusability:** Core modules used in both configurations, reducing duplication
2. **Maintainability:** Changes to common parameters only need updates in one place
3. **Clarity:** Each file has a clear, single responsibility
4. **Flexibility:** Easy to create new configurations by mixing modules
5. **Version Control:** Git-friendly with smaller, focused files
6. **Collaboration:** Team members can work on different modules simultaneously
7. **Documentation:** Self-documenting structure with clear naming

## Best Practices

### When to Create Variants

Create variant modules when:
- Physical representation differs (simple vs. discretised buoys)
- Analysis methods differ (deterministic vs. stochastic)
- Environmental conditions vary significantly
- Different modeling philosophies are used

### When to Reuse Modules

Reuse modules when:
- Physical properties are identical
- Analysis settings are the same
- Environmental parameters match
- Object definitions are unchanged

### Module Size Guidelines

- **Small modules** (5-20 lines): Analysis settings, view parameters
- **Medium modules** (20-100 lines): Environment sections, simple objects
- **Large modules** (100-500 lines): Vessel types with RAOs, line definitions
- **Very large modules** (500+ lines): Complex discretised structures

### Naming Conventions

- Use lowercase with underscores
- Lead with section number
- Include variant descriptor for alternatives
- Keep names descriptive but concise

## Validation

To validate the modular files:

```bash
# From the module root directory
./scripts/run_validation.sh

# Or directly with Python
python scripts/validate_modules.py --report
```

Both configurations have been extracted from validated OrcaFlex models:
- ✅ Original files load successfully in OrcaFlex 11.5e
- ✅ All YAML syntax preserved exactly
- ✅ Indentation and formatting maintained
- ✅ No data loss during modularization
- ✅ Automated validation suite available

For detailed validation guide, see: `docs/VALIDATION_GUIDE.md`

## Future Enhancements

Potential improvements to this modular structure:

1. **Parameter Templates:** Create templates for common parameter variations
2. **Automated Assembly:** Scripts to build configurations from module selections
3. **Validation Suite:** Tests to verify module compatibility
4. **Version Control:** Semantic versioning for module updates
5. **Module Dependencies:** Document which modules require others
6. **Cross-Reference Map:** Visual diagram of module relationships

## Directory Structure

```
modular-input-file/
├── input/                          # Original source files
│   ├── C06_CALM_Buoy.yml
│   └── C06_Discretised_CALM_Buoy.yml
├── output/                         # Modular output files
│   ├── calm_buoy_base.yml         # Main config (simple)
│   ├── discretised_calm_buoy_base.yml  # Main config (discretised)
│   ├── _01a_units_analysis.yml    # General modules
│   ├── ... (20 module files)
│   ├── README.md                  # This file
│   └── VALIDATION_STATUS.md       # Current validation report
├── scripts/                        # Validation and utility scripts
│   ├── validate_modules.py        # Python validation script
│   └── run_validation.sh          # Bash wrapper script
└── docs/                          # Documentation
    └── VALIDATION_GUIDE.md        # Validation usage guide
```

## Support and Documentation

For more information on OrcaFlex YAML usage:
- OrcaFlex Documentation: https://www.orcina.com/webhelp/OrcaFlex/
- YAML Specification: https://yaml.org/spec/1.1/
- Project Documentation: `docs/modules/orcaflex/notes/pre_yml_file_management/`
- Validation Guide: `docs/VALIDATION_GUIDE.md`

---

**Generated:** Based on C06_CALM_Buoy.yml and C06_Discretised_CALM_Buoy.yml
**Format:** OrcaFlex 11.5e YAML 1.1
**Status:** Production-ready modular input files
