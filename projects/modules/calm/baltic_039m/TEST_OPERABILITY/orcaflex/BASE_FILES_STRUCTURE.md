# Base Files Structure - CALM Buoy Models

## Overview

The `base_files/` directory contains **two complete CALM buoy model configurations**:

1. **Simple Model** - Single vessel representation
2. **Discretised Model** - 8-buoy discretised representation with JONSWAP waves

Both models share the same base module files to minimize duplication.

## Directory Structure

```
base_files/
├── README.md                              # Documentation
│
├── calm_buoy_simple_base.yml             # MAIN MODEL - Simple (1.1 KB)
├── calm_buoy_discretised_base.yml        # MAIN MODEL - Discretised (1.2 KB)
│
├── _01a_units_analysis.yml               # Analysis settings (144 bytes)
├── _01b_statics.yml                      # Static analysis (166 bytes)
├── _01c_dynamics.yml                     # Dynamic analysis (191 bytes)
├── _01d_stages.yml                       # Load stages (243 bytes)
├── _01e_view.yml                         # View settings (283 bytes)
│
├── _02_variable_data.yml                 # Variable data (438 bytes)
│
├── _03a_sea_density.yml                  # Sea properties (217 bytes)
├── _03b_seabed.yml                       # Seabed (183 bytes)
│
├── _03c_waves_base.yml                   # WAVES - Simple (5.0 KB)
├── _03c_waves_jonswap_base.yml          # WAVES - Discretised (957 bytes)
├── _03d_current_base.yml                 # CURRENT (545 bytes)
├── _03e_wind_base.yml                    # WIND (595 bytes)
│
├── _04_vessel_types.yml                  # Vessel types (117 KB)
├── _05_line_types.yml                    # Line types (4.5 KB)
│
├── _06_vessels_buoys.yml                 # Buoy vessel (808 bytes) - Simple
├── _06_buoys_discretised.yml            # 8 buoys (26 KB) - Discretised
│
├── _07_lines.yml                         # Mooring lines (17 KB) - Simple
├── _07_lines_discretised.yml            # Lines discretised (17 KB) - Discretised
│
├── _08_groups.yml                        # Groups (350 bytes) - Simple
└── _08_groups_discretised.yml           # Groups discretised (558 bytes) - Discretised
```

**Total: 24 files** (244 KB including documentation)

## Two Model Variants

### 1. Simple Model (`calm_buoy_simple_base.yml`)

**Purpose**: Preliminary analysis, fast computation, single vessel representation

**References**:
- Standard wave spectrum (_03c_waves_base.yml) - 100 user-specified components
- Single buoy vessel (_06_vessels_buoys.yml)
- Simple mooring lines (_07_lines.yml)
- Basic groups (_08_groups.yml)

**Use Cases**:
- Preliminary design studies
- Quick operability assessments
- Sensitivity analyses
- Model development and testing

### 2. Discretised Model (`calm_buoy_discretised_base.yml`)

**Purpose**: Detailed analysis, 8-buoy representation, JONSWAP spectrum

**References**:
- JONSWAP wave spectrum (_03c_waves_jonswap_base.yml) - Frequency domain
- 8 discretised buoys (_06_buoys_discretised.yml)
- Discretised mooring lines (_07_lines_discretised.yml)
- Discretised groups (_08_groups_discretised.yml)

**Use Cases**:
- Detailed design verification
- High-fidelity motion analysis
- Mooring load distribution studies
- Final design documentation

## Environment Module Variants

### Wave Modules

| File | Model Type | Description | Size |
|------|-----------|-------------|------|
| `_03c_waves_base.yml` | Simple | 100 user-specified components | 5.0 KB |
| `_03c_waves_jonswap_base.yml` | Discretised | JONSWAP spectrum | 957 bytes |

**Both configured for 1-year return period**:
- Hs = 2.5m
- Tp = 7.5s (Simple) / Tz = 5.3s (Discretised)
- Direction = 0° (placeholder for load case override)

### Current Module

| File | Description | Size |
|------|-------------|------|
| `_03d_current_base.yml` | Depth-dependent profile | 545 bytes |

**Configuration**:
- Surface speed: 1.4 m/s (1-year)
- Profile: 0m (40%), 45m (40%), 100m (20%)
- Direction: 0° (placeholder)

### Wind Module

| File | Description | Size |
|------|-------------|------|
| `_03e_wind_base.yml` | NPD spectrum | 595 bytes |

**Configuration**:
- Speed: 20 m/s (1-year)
- Spectrum: NPD with 1000 components
- Direction: 0° (placeholder)

## Shared vs Model-Specific Files

### Shared Across Both Models (16 files)
- Analysis settings (_01a through _01e)
- Variable data (_02)
- Environment base (_03a, _03b, _03d, _03e)
- Vessel types (_04)
- Line types (_05)
- Main buoy vessel (_06_vessels_buoys)

### Simple Model Only (4 files)
- _03c_waves_base.yml
- _07_lines.yml
- _08_groups.yml
- calm_buoy_simple_base.yml

### Discretised Model Only (4 files)
- _03c_waves_jonswap_base.yml
- _06_buoys_discretised.yml
- _07_lines_discretised.yml
- _08_groups_discretised.yml
- calm_buoy_discretised_base.yml

## Usage

### Open Simple Model in OrcaFlex

```bash
# Navigate to base_files directory
cd projects/TEST_OPERABILITY/orcaflex/base_files

# Open in OrcaFlex
File → Open → calm_buoy_simple_base.yml
```

### Open Discretised Model in OrcaFlex

```bash
# Navigate to base_files directory
cd projects/TEST_OPERABILITY/orcaflex/base_files

# Open in OrcaFlex
File → Open → calm_buoy_discretised_base.yml
```

### Create Load Case Variants

Both models use the same base environment files with direction=0°. To run operability analysis:

1. Base files remain unchanged (single copy)
2. Create load case override files that:
   - Reference base_files modules
   - Override only wave/current/wind directions
3. Batch script applies overrides for each heading

## Model Comparison

| Feature | Simple | Discretised |
|---------|--------|-------------|
| **Buoy Representation** | Single vessel | 8 x 6D buoys |
| **Wave Spectrum** | User components | JONSWAP |
| **Computation Speed** | Fast | Slower |
| **Accuracy** | Good | Excellent |
| **File Size** | Smaller | Larger |
| **Use Case** | Preliminary | Detailed |

## Maintenance

### Update Wave Spectrum

**For Simple Model**:
1. Edit `_03c_waves_base.yml`
2. Modify wave components or return period
3. Applies to all load cases using simple model

**For Discretised Model**:
1. Edit `_03c_waves_jonswap_base.yml`
2. Modify JONSWAP parameters (Hs, Tz, gamma)
3. Applies to all load cases using discretised model

### Update Current or Wind

Edit `_03d_current_base.yml` or `_03e_wind_base.yml`:
- Changes apply to **BOTH** simple and discretised models
- Single source of truth for current/wind conditions

### Switch Return Period

To change from 1-year to 10-year:

**Simple Model Waves**:
```yaml
# _03c_waves_base.yml
# Update wave heights in component amplitudes
# Hs: 2.5m → 5.0m
# Tp: 7.5s → 11.0s
```

**Discretised Model Waves**:
```yaml
# _03c_waves_jonswap_base.yml
WaveHs: 5.0  # Changed from 2.5
WaveTz: 7.8  # Changed from 5.3
```

**Current**:
```yaml
# _03d_current_base.yml
RefCurrentSpeed: 1.9  # Changed from 1.4
```

**Wind**:
```yaml
# _03e_wind_base.yml
WindSpeed: 30  # Changed from 20
```

## Benefits of Dual Model Structure

✅ **Flexibility** - Choose appropriate fidelity for task
✅ **Single source** - One copy of shared modules
✅ **Easy comparison** - Run same load cases in both models
✅ **Efficient workflow** - Start with simple, refine with discretised
✅ **Industry standard** - Follows McDermott Woodfibre convention

## References

- **Project**: North Sea CALM Buoy - Example Project (NSE_CALM_001)
- **Simple Model**: Fast preliminary analysis
- **Discretised Model**: Detailed design verification
- **Return Period**: 1-year (API RP 2SK operability)
- **Based On**: McDermott Woodfibre FST project structure

---

*Last updated: 2025-11-11*
*Project: TEST_OPERABILITY*
