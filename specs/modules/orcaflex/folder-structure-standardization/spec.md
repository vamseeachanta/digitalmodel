# OrcaFlex Folder Structure Standardization Specification

## Overview
Standardize folder structure across all OrcaFlex-related modules and submodules to ensure consistency, maintainability, and ease of review across the digitalmodel repository.

## Problem Statement
Currently, OrcaFlex modules have inconsistent folder structures:
- Various output locations (`.csv`, `plots`, `visual`, `Data`, `Plot`)
- Mixed path conventions (absolute vs relative)
- Inconsistent naming patterns
- Different organizational approaches across modules

## Solution
Implement a unified folder structure standard for all OrcaFlex modules that:
- Uses relative paths exclusively
- Maintains consistent output organization
- Follows clear naming conventions
- Supports all analysis types (static, dynamic, mooring, visualization)

## Technical Specification

### 1. Standard Folder Structure

```
specs/modules/orcaflex/<module-name>/
├── README.md                       # Module documentation
├── USAGE.md                        # Usage instructions
├── spec.md                         # Module specification
├── tasks.md                        # Implementation tasks
├── prompt.md                       # Original prompt/requirements
├── task_summary.md                 # Task execution summary
│
├── config/                         # Configuration files
│   ├── analysis/                   # Analysis configurations
│   │   ├── *.yml                   # YAML config files
│   │   └── templates/              # Config templates
│   ├── visualization/              # Visualization configs
│   └── batch/                      # Batch processing configs
│
├── input/                          # Input files
│   ├── models/                     # OrcaFlex model files
│   │   ├── *.sim                   # Simulation files
│   │   ├── *.dat                   # Data files
│   │   └── *.yml                   # Model configurations
│   ├── data/                       # Input data files
│   │   ├── *.csv                   # CSV data
│   │   └── *.xlsx                  # Excel data
│   └── parameters/                 # Parameter files
│       └── *.csv                   # Target parameters
│
├── output/                         # All output files
│   ├── analysis/                   # Analysis results
│   │   ├── *.csv                   # CSV results
│   │   ├── *.json                  # JSON results
│   │   └── *.log                   # Analysis logs
│   ├── visual/                     # Visualizations
│   │   ├── *.png                   # PNG images
│   │   ├── *.jpg                   # JPG images
│   │   └── *.pdf                   # PDF reports
│   ├── reports/                    # Generated reports
│   │   ├── *.pdf                   # PDF reports
│   │   ├── *.html                  # HTML reports
│   │   └── *.xlsx                  # Excel reports
│   └── logs/                       # Execution logs
│       └── *.log                   # Log files
│
├── scripts/                        # Processing scripts
│   ├── *.py                        # Python scripts
│   ├── *.sh                        # Shell scripts
│   └── *.bat                       # Batch scripts
│
├── tests/                          # Module tests
│   ├── unit/                       # Unit tests
│   ├── integration/                # Integration tests
│   └── fixtures/                   # Test fixtures
│
└── docs/                           # Additional documentation
    ├── diagrams/                   # Architecture diagrams
    │   └── *.mermaid              # Mermaid diagrams
    └── examples/                   # Usage examples
```

### 2. Path Conventions

#### 2.1 Always Use Relative Paths
```yaml
# CORRECT - Relative paths from repository root
file_management:
  input_directory: specs/modules/orcaflex/mooring-tension/input/models
  output_directory: specs/modules/orcaflex/mooring-tension/output/analysis
  
# INCORRECT - Absolute paths
file_management:
  input_directory: D:/github/digitalmodel/specs/modules/orcaflex/...
```

#### 2.2 Standard Path Variables
```yaml
# Standard configuration structure
file_management:
  input_directory: specs/modules/orcaflex/<module>/input/models
  output_directory: specs/modules/orcaflex/<module>/output/analysis
  plot_directory: specs/modules/orcaflex/<module>/output/visual
  report_directory: specs/modules/orcaflex/<module>/output/reports
  log_directory: specs/modules/orcaflex/<module>/output/logs
```

### 3. Naming Conventions

#### 3.1 File Naming Patterns
```
# Analysis outputs
<model_name>_<analysis_type>_<timestamp>.csv
Example: fsts_l015_pretension_analysis_20250901.csv

# Visualizations
<model_name>_<plot_type>_<view>.png
Example: fsts_l015_mooring_forces_elevation.png

# Reports
<analysis_type>_report_<date>.pdf
Example: mooring_analysis_report_20250901.pdf

# Logs
<process_name>_<timestamp>.log
Example: batch_analysis_20250901_143000.log
```

#### 3.2 Directory Names
- Use lowercase with hyphens for module names
- Use lowercase with underscores for subdirectories
- No spaces or special characters

### 4. Configuration Standards

#### 4.1 YAML Configuration Template
```yaml
# Standard OrcaFlex module configuration
meta:
  library: digitalmodel
  module: orcaflex
  submodule: <specific-module>
  version: 1.0.0

file_management:
  input_directory: specs/modules/orcaflex/<module>/input/models
  output_directory: specs/modules/orcaflex/<module>/output/analysis
  plot_directory: specs/modules/orcaflex/<module>/output/visual
  report_directory: specs/modules/orcaflex/<module>/output/reports
  log_directory: specs/modules/orcaflex/<module>/output/logs
  
  filename:
    pattern: <file_pattern>
    extension: [sim, yml, dat]
    filters:
      contains: []
      not_contains: [includefile, backup]

visualization:
  enabled: true
  output_directory: specs/modules/orcaflex/<module>/output/visual
  formats: [png, pdf]
  dpi: 150

analysis:
  type: <analysis_type>
  parameters:
    # Analysis-specific parameters

output:
  save_csv: true
  save_json: false
  save_plots: true
  generate_report: true
```

### 5. Module Types and Their Specifics

#### 5.1 Mooring Analysis Modules
```
mooring-tension-iteration/
├── input/
│   ├── models/         # .sim files
│   └── parameters/     # target_pretension.csv files
├── output/
│   ├── analysis/       # *_pretension_analysis.csv
│   └── visual/         # mooring force plots
```

#### 5.2 Post-Processing Modules
```
postprocess-optimization/
├── input/
│   ├── models/         # .sim files
│   └── data/          # time series data
├── output/
│   ├── analysis/      # processed results
│   ├── visual/        # time traces, histograms
│   └── reports/       # summary reports
```

#### 5.3 Visualization Modules
```
visualization-suite/
├── input/
│   └── models/        # .sim files for visualization
├── output/
│   └── visual/        # elevation, plan, 3D views
```

### 6. Migration Strategy

#### 6.1 Phase 1: New Modules
- All new modules MUST follow this structure
- Use provided templates

#### 6.2 Phase 2: Critical Modules
Priority modules to migrate:
1. mooring-tension-iteration
2. postprocess-optimization
3. browser-interface
4. force-analysis

#### 6.3 Phase 3: All Modules
- Systematic migration of remaining modules
- Update all references and dependencies

### 7. Validation Rules

#### 7.1 Pre-commit Checks
- Verify relative paths only
- Check folder structure compliance
- Validate naming conventions

#### 7.2 CI/CD Integration
- Automated structure validation
- Path consistency checks
- Output organization verification

## Implementation Requirements

### Must Have
- [ ] Relative paths only
- [ ] Consistent output organization
- [ ] Standard naming conventions
- [ ] Clear separation of input/output/config

### Should Have
- [ ] Automated migration scripts
- [ ] Validation tools
- [ ] Template generators

### Nice to Have
- [ ] VS Code snippets for configs
- [ ] Automated folder creation
- [ ] Path intellisense support

## Success Criteria
1. All OrcaFlex modules follow identical structure
2. Zero absolute paths in configurations
3. Consistent output locations across all analyses
4. Simplified review process for users
5. Reduced configuration errors

## Risk Mitigation
- Maintain backward compatibility during migration
- Provide clear migration guides
- Automated validation before commits
- Comprehensive documentation

## Timeline
- Week 1: Implement for new modules
- Week 2-3: Migrate priority modules
- Week 4-5: Complete migration
- Week 6: Validation and documentation

## Dependencies
- Update mooring.py visualization integration
- Modify base configurations
- Update all analysis scripts
- Revise documentation