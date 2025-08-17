# OrcaFlex File Ecosystem Folder Structure Specification

## Overview
This specification defines a comprehensive, standardized folder structure for the OrcaFlex file ecosystem within DigitalModel. The structure eliminates legacy .dat files in favor of YAML-based workflows, provides clear separation between inputs, processing, and outputs, and supports iterative analysis workflows with full traceability.

## Objectives
1. **Standardize on YAML format** - Deprecate .dat files completely
2. **Clear separation of concerns** - Inputs, processing, outputs in distinct locations
3. **Support iterative workflows** - Built-in versioning and iteration tracking
4. **Enable parallel processing** - Structure optimized for batch operations
5. **Maintain traceability** - Full audit trail of all analyses

## Proposed Folder Structure

```
digitalmodel/
├── data/                                    # All data files
│   └── orcaflex/                           # OrcaFlex-specific data
│       ├── models/                         # Source model files (YAML only)
│       │   ├── templates/                  # Template models for reuse
│       │   │   ├── mooring_systems/
│       │   │   ├── vessels/
│       │   │   └── environments/
│       │   ├── projects/                   # Project-specific models
│       │   │   └── {project_name}/
│       │   │       ├── base/               # Base models
│       │   │       ├── variants/           # Model variants
│       │   │       └── includefiles/       # Shared includefiles
│       │   └── library/                    # Reusable components
│       │       ├── line_types/
│       │       ├── vessels/
│       │       └── constraints/
│       │
│       ├── inputs/                         # Analysis input files
│       │   ├── configurations/             # Analysis configurations
│       │   │   ├── batch_configs/          # Batch processing configs
│       │   │   ├── simulation_settings/    # Simulation parameters
│       │   │   └── analysis_cases/         # Load case definitions
│       │   ├── target_values/              # Target values for iterations
│       │   │   ├── tensions/               # Target tension CSVs
│       │   │   ├── positions/              # Target position data
│       │   │   └── clearances/             # Clearance requirements
│       │   └── environmental/              # Environmental data
│       │       ├── waves/                  # Wave data
│       │       ├── currents/               # Current profiles
│       │       └── wind/                   # Wind data
│       │
│       ├── workspace/                      # Active processing area
│       │   ├── active/                     # Currently processing
│       │   │   └── {timestamp}_{analysis}/ # Timestamped workspace
│       │   │       ├── models/             # Working copies
│       │   │       ├── iterations/         # Iteration tracking
│       │   │       └── logs/               # Processing logs
│       │   └── staging/                    # Pre-processing staging
│       │
│       ├── outputs/                        # Analysis outputs
│       │   ├── simulations/                # Simulation files
│       │   │   └── {project_name}/
│       │   │       └── {analysis_date}/
│       │   │           ├── *.sim           # Simulation results
│       │   │           └── metadata.json   # Simulation metadata
│       │   ├── results/                    # Processed results
│       │   │   └── {project_name}/
│       │   │       └── {analysis_date}/
│       │   │           ├── csv/            # CSV outputs
│       │   │           ├── reports/        # Analysis reports
│       │   │           └── plots/          # Visualization outputs
│       │   └── iterations/                 # Iteration histories
│       │       └── {project_name}/
│       │           └── {iteration_id}/
│       │               ├── history.json    # Iteration history
│       │               ├── convergence/    # Convergence data
│       │               └── adjustments/    # Applied adjustments
│       │
│       └── archive/                        # Archived analyses
│           ├── completed/                  # Completed projects
│           │   └── {year}/
│           │       └── {project_name}/
│           └── deprecated/                 # Legacy .dat files (temporary)
│               └── migration_tracking.json
│
├── src/modules/orcaflex/                   # Source code
│   ├── core/                              # Core functionality
│   │   ├── model_manager.py               # Model file management
│   │   ├── workspace_manager.py           # Workspace lifecycle
│   │   └── archive_manager.py             # Archive operations
│   ├── workflows/                         # Analysis workflows
│   │   ├── mooring_tension_iteration/
│   │   ├── batch_processing/
│   │   └── fatigue_analysis/
│   └── utils/                             # Utilities
│       ├── file_organizer.py              # File organization
│       ├── path_resolver.py               # Path resolution
│       └── migration/                     # Migration tools
│           ├── dat_to_yml_converter.py
│           └── structure_migrator.py
│
└── config/                                # Configuration
    └── orcaflex/
        ├── folder_structure.yaml          # Structure configuration
        ├── naming_conventions.yaml        # Naming rules
        └── retention_policy.yaml          # Archive policies
```

## Key Design Decisions

### 1. YAML-Only Policy
**Decision:** Complete deprecation of .dat files
- All models must be in YAML format
- Automatic conversion for legacy .dat files on import
- No new .dat files will be created or accepted

### 2. Workspace Concept
**Purpose:** Isolated processing environments
- Each analysis gets a timestamped workspace
- Prevents conflicts during parallel processing
- Maintains clean separation between runs

### 3. Three-Stage Lifecycle
```
inputs → workspace (active processing) → outputs → archive
```
- Clear data flow from source to results
- Workspace for temporary/working files
- Archive for long-term storage

### 4. Project-Based Organization
- All files organized by project
- Consistent structure across projects
- Easy to locate related analyses

## File Naming Conventions

### Model Files
```
{project}_{component}_{version}.yml
Example: windstar_mooring_system_v2.yml
```

### Simulation Results
```
{project}_{analysis}_{timestamp}.sim
Example: windstar_tension_check_20241117_143022.sim
```

### Iteration Files
```
iteration_{number}_{type}_{timestamp}.{ext}
Example: iteration_05_tensions_20241117_143022.csv
```

### Archive Names
```
{year}_{month}_{project}_{analysis}_archive.tar.gz
Example: 2024_11_windstar_mooring_complete_archive.tar.gz
```

## Workflow Integration

### 1. Model Preparation
```python
# Load from templates or projects
model_path = "data/orcaflex/models/projects/windstar/base/model.yml"
includefile_path = "data/orcaflex/models/projects/windstar/includefiles/adjustments.yml"
```

### 2. Workspace Creation
```python
# Create timestamped workspace
workspace = "data/orcaflex/workspace/active/20241117_143022_mooring_analysis/"
```

### 3. Processing
```python
# Run analysis in workspace
# All temporary files contained within workspace
```

### 4. Output Storage
```python
# Store results in structured outputs
sim_output = "data/orcaflex/outputs/simulations/windstar/2024-11-17/*.sim"
csv_output = "data/orcaflex/outputs/results/windstar/2024-11-17/csv/*.csv"
```

### 5. Archival
```python
# Archive completed analyses
archive_path = "data/orcaflex/archive/completed/2024/windstar/"
```

## Migration Strategy

### Phase 1: Structure Creation
1. Create new folder structure
2. Implement path management utilities
3. Update configuration files

### Phase 2: .dat File Migration
1. Scan for all .dat files
2. Convert to YAML format
3. Validate converted files
4. Move originals to deprecated folder

### Phase 3: Code Updates
1. Update all file references
2. Modify workflows for new structure
3. Update batch processing configurations

### Phase 4: Documentation
1. Update all documentation
2. Create migration guide
3. Train users on new structure

## Benefits

### 1. Version Control
- YAML files are diff-friendly
- Clear history of changes
- Easy rollback capabilities

### 2. Automation
- Consistent structure enables automation
- Batch processing simplified
- Easy to script workflows

### 3. Collaboration
- Human-readable formats
- Clear organization
- Reduced learning curve

### 4. Maintenance
- Single format to maintain
- Clear retention policies
- Automated archival

## Implementation Requirements

### Required Utilities
1. **Path Resolver** - Resolve paths based on project/analysis
2. **Workspace Manager** - Create/manage workspace lifecycle
3. **Archive Manager** - Handle archival and retrieval
4. **Migration Tool** - Convert .dat to YAML
5. **File Organizer** - Organize existing files into new structure

### Configuration Files
1. **folder_structure.yaml** - Define structure rules
2. **naming_conventions.yaml** - Naming patterns and validation
3. **retention_policy.yaml** - Archive and cleanup rules

### Monitoring
1. **Workspace usage tracking**
2. **Archive size monitoring**
3. **Migration progress tracking**
4. **Structure compliance validation**

## Success Criteria
1. ✅ All .dat files successfully migrated to YAML
2. ✅ Zero .dat files in active use
3. ✅ All workflows using new structure
4. ✅ Automated archival working
5. ✅ Documentation fully updated
6. ✅ User training completed

## Risks and Mitigations

### Risk 1: Legacy Dependencies
**Mitigation:** Maintain deprecated folder temporarily with clear EOL date

### Risk 2: User Resistance
**Mitigation:** Provide migration tools and comprehensive training

### Risk 3: Path Breaking Changes
**Mitigation:** Implement compatibility layer during transition

## Next Steps
1. Review and approve specification
2. Implement folder structure utilities
3. Create migration tools
4. Begin phased migration
5. Update documentation
6. Train users