# Load Scaling Program Input Specification

## Overview

This document specifies the complete input data structure and requirements for the Reference Seastate Scaling Fatigue Analysis module. The load scaling program performs fatigue analysis using reference seastate scaling methodology with metocean condition mapping.

## 1. Master Configuration File

### 1.1 Master Input Configuration YAML
**Location**: `input/master_input_config_[sample|production].yml`

The master configuration file consolidates all analysis parameters and file references.

#### Required Structure:
```yaml
default:
  # Analysis identification
  analysis_name: string          # Human-readable analysis name
  analysis_type: string          # Must be "reference-seastate-scaling"
  project_id: string             # Unique project identifier
  structure: string              # Target structure (e.g., "Strut1")
  
  # Analysis parameters
  parameters:
    sample_duration_seconds: int   # Duration of each time series sample
    time_step_seconds: float       # Time step in tension time series
    sample_points: int             # Number of points in each sample
    seconds_per_year: int          # Seconds per year (31557600)
    design_life_years: int         # Target design life
    
  # File paths - Reference conditions
  reference_files:
    base_directory: string         # Base directory for reference files
    file_pattern: string           # Glob pattern for reference files
    files: list[string]            # List of reference file names
    column_name: string            # Column name containing tension data
    
  # File paths - Sea state conditions  
  seastate_files:
    base_directory: string         # Base directory for seastate files
    file_pattern: string           # Glob pattern for seastate files
    files: dict                    # Mapping FC001-FC010 to file names
    column_name: string            # Column name containing tension data
    
  # Fatigue conditions (metocean data)
  fatigue_conditions:
    file: string                   # Path to fatigue conditions CSV
    data: dict                     # Inline fatigue condition definitions
        
  # Scaling factors
  scaling_factors:
    wind:
      reference_speed_mps: float   # Reference wind speed
      exponent: float              # Wind scaling exponent
      formula: string              # Formula description
    wave:
      reference_hs_m: float        # Reference wave height
      exponent: float              # Wave scaling exponent  
      formula: string              # Formula description
      
  # Tension to stress conversion
  tension_to_stress:
    file: string                   # Path to conversion function CSV
    method: string                 # Conversion method
    conversion_table: list         # Table of tension-stress pairs
    conversion_factor: float       # Linear conversion factor
    includes_scf: bool             # Whether SCF is included
    
  # S-N curve parameters
  sn_curve:
    curve_type: string             # S-N curve standard
    description: string            # Curve description
    formula: string                # Mathematical formula
    parameters:
      a: float                     # Fatigue strength coefficient
      m: float                     # Fatigue strength exponent
    stress_units: string           # Stress units
    thickness_correction: bool     # Whether thickness correction applied
    
  # Rainflow counting settings
  rainflow:
    method: string                 # Rainflow counting standard
    normalize_yearly: bool         # Whether to normalize to yearly cycles
    min_range_kN: float           # Minimum cycle range threshold
    
  # Output settings
  output:
    base_directory: string         # Base output directory
    save_intermediate: bool        # Save intermediate results
    intermediate_files: dict       # Intermediate file naming
    final_results: string          # Final results file name
    generate_plots: bool           # Generate visualization plots
    plot_formats: list[string]     # Plot output formats
    
  # Validation thresholds
  validation:
    min_fatigue_life_years: float  # Minimum acceptable fatigue life
    max_damage_per_year: float     # Maximum annual damage rate
    occurrence_sum_tolerance: float # Tolerance for occurrence sum
    
  # Processing options
  processing:
    parallel_processing: bool      # Enable parallel processing
    num_workers: int               # Number of worker processes
    verbose: bool                  # Verbose output
    debug_mode: bool               # Debug mode
```

## 2. Reference Seastate Definitions

### 2.1 Reference Seastate Definitions CSV
**Location**: `input/reference_seastate_definitions_[sample|production].csv`

Defines the mapping between load cases and environmental configurations.

#### Schema:
| Field | Type | Description | Required | Validation |
|-------|------|-------------|----------|------------|
| Load Case | string | Load case identifier (F001, F002, etc.) | Yes | Must match pattern F\d{3} |
| environment_config_basename | string | Environment configuration basename | Yes | Must match reference data files |
| FST1 | string | FST1 vessel loading state | Yes | "Light" or "Full" |
| FST2 | string | FST2 vessel loading state | Yes | "Light" or "Full" |
| LNGC-Partner (Small) | string | Small LNGC state | No | "Full", "Ballast", or "-" |
| LNCG-Excalibur (Large) | string | Large LNGC state | No | "Full", "Ballast", or "-" |
| Mooring Status | string | Mooring integrity status | Yes | "Intact" or "Damaged" |
| Environment Condition | string | Environmental condition type | Yes | "Scatter Diagrams" |
| Water Level | string | Water level condition | Yes | "MWL" (Mean Water Level) |
| Headings | string | Wave heading reference | Yes | "See Columns V-Z" |
| Wind Dir [deg] | float | Wind direction in degrees | No | 0-360 |
| Vw [m/s] | float | Wind speed in m/s | No | ≥ 0 |
| Wave Dir [deg] | float | Wave direction in degrees | No | 0-360 |
| Hs [m] | float | Significant wave height in meters | No | ≥ 0 |
| Tp [s] | float | Peak wave period in seconds | No | > 0 |
| env reference | string | Environment reference identifier | Yes | Must match fatigue seastates |
| config reference | string | Configuration reference prefix | Yes | Must match reference data naming |

#### Data Validation Requirements:
- All load cases must have unique identifiers
- Environment config basename must match available reference data files
- Wind/wave parameters must be physically reasonable
- Reference identifiers must link to fatigue seastate definitions

## 3. Fatigue Seastates

### 3.1 Fatigue Seastates CSV
**Location**: `input/fatigue_seastates_[sample|production].csv`

Defines the metocean conditions for fatigue analysis.

#### Schema:
| Field | Type | Description | Required | Validation |
|-------|------|-------------|----------|------------|
| Row | int | Row identifier | Yes | Sequential positive integers |
| Wind Speed (m/s) | float | Wind speed in m/s | Yes | 0 ≤ value ≤ 50 |
| Wind Dir (°) | float | Wind direction in degrees | Yes | 0 ≤ value < 360 |
| Hs (m) | float | Significant wave height in meters | Yes | 0 ≤ value ≤ 20 |
| Tp (s) | float | Peak wave period in seconds | Yes | 1 ≤ value ≤ 25 |
| Wave Dir (°) | float | Wave direction in degrees | Yes | 0 ≤ value < 360 |
| Occurrence (%) | float | Probability of occurrence as percentage | Yes | 0 < value ≤ 100 |

#### Data Validation Requirements:
- Sum of all occurrence percentages must equal 100% (±tolerance)
- Wave parameters must satisfy physical relationships (Tp vs Hs)
- Wind and wave directions must be consistent with coordinate system
- No duplicate combinations of environmental parameters

## 4. Reference Data Files

### 4.1 Time Series Data Structure
**Location**: `reference_data/`

Time series tension data files containing effective tension at vessel end.

#### File Naming Convention:
```
{vessel_config}_{lngc_config}_{water_level}_{env_condition}_{component}.csv
```

**Components:**
- `vessel_config`: FST loading state (fsts_l015, fsts_l095)
- `lngc_config`: LNGC configuration (125km3_l100_pb, 125km3_l000_pb, or omitted)
- `water_level`: Water level condition (mwl)
- `env_condition`: Environmental condition (wave01, wind01, etc.)
- `component`: Structural component (Strut1, Strut2, etc.)

#### Schema:
| Field | Type | Description | Required | Validation |
|-------|------|-------------|----------|------------|
| Time | float | Time in seconds | Yes | Monotonically increasing, ≥ 0 |
| Effective Tension at Vessel End | float | Tension in kN | Yes | Must be physically reasonable |

#### Data Validation Requirements:
- Time series must be uniformly spaced
- Time step must match configuration file specifications
- Tension values must be positive
- No missing or NaN values allowed
- Duration must match sample_duration_seconds parameter

### 4.2 Required Reference Configurations:

#### Vessel Configurations:
1. **FSTs Light (l015)** - FSTs at 15% loading, no LNGC
2. **FSTs Full (l095)** - FSTs at 95% loading, no LNGC  
3. **FSTs Light + LNGC Full** - FSTs at 15% with 125k m³ LNGC at 100%
4. **FSTs Full + LNGC Light** - FSTs at 95% with 125k m³ LNGC at 0%

#### Environmental Conditions:
- **Wave conditions**: wave01 through wave18 (18 conditions)
- **Wind conditions**: wind01 through wind16 (16 conditions)

#### Structural Components:
- **Mooring struts**: Strut1 through Strut8 (8 struts)

## 5. Directory Structure Requirements

```
input/
├── master_input_config_sample.yml          # Sample configuration
├── master_input_config_production.yml      # Production configuration
├── reference_seastate_definitions_sample.csv    # Sample reference definitions
├── reference_seastate_definitions_production.csv # Production reference definitions
├── fatigue_seastates_sample.csv            # Sample fatigue conditions
├── fatigue_seastates_production.csv        # Production fatigue conditions
└── reference_seastates.xlsx                # Reference seastate specifications

reference_data/
├── fsts_l015_mwl_wave01_Strut1.csv        # Light FSTs, wave condition 1, Strut 1
├── fsts_l015_mwl_wave01_Strut2.csv        # Light FSTs, wave condition 1, Strut 2
├── ...                                     # Additional wave conditions
├── fsts_l015_mwl_wind01_Strut1.csv        # Light FSTs, wind condition 1, Strut 1
├── ...                                     # Additional wind conditions
├── fsts_l095_mwl_wave01_Strut1.csv        # Full FSTs, wave condition 1, Strut 1
├── ...                                     # Additional configurations
├── fsts_l015_125km3_l100_pb_mwl_wave01_Strut1.csv # FSTs + LNGC combinations
└── ...                                     # All configuration combinations
```

## 6. Data Relationships and Dependencies

### 6.1 File Linking Logic:
1. **Reference Definitions → Reference Data**: `config reference` field links to reference data file prefixes
2. **Reference Definitions → Fatigue Seastates**: `env reference` field links to environmental conditions
3. **Master Config → All Files**: Provides paths and parameters for all input files

### 6.2 Scaling Methodology:
- Reference conditions defined in reference data files
- Target conditions defined in fatigue seastates
- Scaling factors calculated based on wind/wave parameter ratios
- Applied to reference tension time series to generate target responses

### 6.3 Cross-Validation Requirements:
- All referenced files must exist and be readable
- Environmental parameter ranges must be consistent across files
- Structural component naming must be consistent
- Time series duration and sampling must match configuration

## 7. Error Handling and Validation

### 7.1 Required Validation Checks:
1. **File Existence**: All referenced files must exist
2. **Schema Compliance**: All CSV files must match required schemas
3. **Data Integrity**: No missing, NaN, or invalid values
4. **Physical Constraints**: All parameters within realistic ranges
5. **Mathematical Consistency**: Occurrence percentages sum to 100%
6. **Temporal Consistency**: Time series properly formatted and complete
7. **Cross-Reference Integrity**: All file links and references valid

### 7.2 Error Reporting:
- Clear error messages identifying specific validation failures
- Line numbers and field names for CSV parsing errors
- Suggested corrections for common validation failures
- Summary of all validation issues before processing termination

## 8. Performance Considerations

### 8.1 File Size Optimization:
- CSV files should use appropriate precision (avoid unnecessary decimal places)
- Large time series files should be compressed if storage is constrained
- Consider chunked processing for very large datasets

### 8.2 Memory Management:
- Time series data loading should be optimized for available memory
- Parallel processing configuration should match system capabilities
- Intermediate file cleanup should be configurable

## 9. Version Control and Metadata

### 9.1 Required Metadata:
- File creation dates and modification timestamps
- Data source and provenance information
- Processing version and configuration used
- Validation status and last validation date

### 9.2 Change Management:
- Input file changes should be versioned
- Configuration changes should be documented
- Backward compatibility considerations for configuration updates

This specification ensures consistent, validated input data for reliable fatigue analysis using the reference seastate scaling methodology.