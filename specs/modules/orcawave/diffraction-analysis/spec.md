# OrcaWave Diffraction Analysis Workflow Specification

## Overview
Automated workflow for hydrodynamic diffraction analysis using OrcaWave, converting GMsh mesh files to OrcaWave input, executing analysis with parallel validation, and post-processing results for OrcaFlex integration.

## Workflow Steps

### Step 1: Geometry Preparation (User/Manual)
**Responsibility:** User  
**Tool:** GMsh Program  
**Reference:** `agents/gmsh` - GMsh agent documentation and capabilities

- User creates or imports geometry in GMsh
- Generates high-quality panel mesh suitable for diffraction analysis
- Exports mesh in GMsh .msh format
- **Input:** CAD geometry or existing mesh
- **Output:** `Sea Cypress_0.25 Mesh_Ascii.msh` file

### Step 2: OrcaWave Input File Generation (AI/Automated)
**Responsibility:** AI  
**Script:** `generate_orcawave_input.py`

- Convert GMsh .msh file to OrcaWave-compatible format
- Generate Wamit GDF reference files
- Create YAML configuration for OrcaWave analysis
- **Input:** `specs/modules/orcawave/diffraction-analysis/inputs/geometry/Sea Cypress_0.25 Mesh_Ascii.msh`
- **Reference:** `specs/modules/orcawave/diffraction-analysis/inputs/orcawave/go-by/` (example files)
- **Output:** `specs/modules/orcawave/diffraction-analysis/inputs/orcawave/` (generated configs)

### Step 3: Analysis Execution (AI/User)
**Responsibility:** AI provides script, User executes  
**Script:** `execute_orcawave_parallel.py`

#### AI Tasks:
- Provide Python script with parallel validation capabilities
- Test configuration in parallel threads before proposing
- Generate validation reports
- Create batch execution files

#### User Tasks:
- Review validation results
- Execute analysis using batch file or GUI
- Monitor execution progress

#### Parallel Validation Tests:
1. Configuration validation
2. Mesh file validation
3. Numerical stability check
4. Memory usage estimation

### Step 4: Post-Processing (AI/User)
**Responsibility:** AI provides script, User executes  
**Script:** `postprocess_orcawave_parallel.py`

#### AI Tasks:
- Create post-processing script with parallel data extraction
- Test extraction in parallel threads before proposing
- Generate visualization scripts
- Create OrcaFlex integration formats

#### User Tasks:
- Execute post-processing batch or GUI
- Review extracted data and visualizations
- Import results into OrcaFlex

#### Key Outputs:
- RAOs (Response Amplitude Operators)
- Added mass coefficients
- Damping coefficients
- Wave excitation forces
- Mean drift forces
- QTF (Quadratic Transfer Functions)

## Technical Architecture

### File Structure
```
diffraction-analysis/
├── inputs/
│   ├── geometry/              # GMsh mesh files (.msh)
│   └── orcawave/
│       ├── go-by/             # Reference examples
│       └── *.yml              # Generated configs
├── scripts/
│   ├── generate_orcawave_input.py      # Step 2
│   ├── execute_orcawave_parallel.py    # Step 3
│   ├── postprocess_orcawave_parallel.py # Step 4
│   └── run_*.bat                        # Batch runners
├── outputs/
│   └── processed/             # OrcaFlex-ready data
└── validation/                # Test reports
```

### Agent Delegation

| Task | Primary Agent | Secondary Agent | Role |
|------|--------------|-----------------|------|
| Mesh Generation | GMsh Agent | - | Create panel mesh from geometry |
| Input Generation | OrcaWave Agent | GMsh Agent | Convert mesh formats |
| Validation | Testing Agent | OrcaWave Agent | Parallel validation tests |
| Execution | OrcaWave Agent | - | Run diffraction analysis |
| Post-Processing | OrcaWave Agent | Testing Agent | Extract and validate results |

### Parallel Processing Strategy

All scripts implement parallel processing for:
- **Validation**: 4 concurrent validation tests
- **Data Extraction**: Multiple file processing
- **Visualization**: Concurrent plot generation
- **Testing**: Background validation before user execution

## Success Criteria

### Functional Requirements
- ✅ GMsh .msh file successfully converted to OrcaWave input
- ✅ OrcaWave analysis executes without errors
- ✅ All hydrodynamic coefficients extracted
- ✅ OrcaFlex-compatible output generated

### Performance Requirements
- Parallel validation completed in <30 seconds
- Post-processing handles multiple result files concurrently
- Memory usage estimation accurate within 20%
- All scripts use UV environment

### Quality Requirements
- Validation catches configuration errors before execution
- Comprehensive error handling and logging
- Clear user feedback at each step
- Reproducible results

## Implementation Status

### Completed Components
- ✅ `generate_orcawave_input.py` - GMsh to OrcaWave converter
- ✅ `execute_orcawave_parallel.py` - Parallel validation and execution
- ✅ `postprocess_orcawave_parallel.py` - Result extraction and processing
- ✅ `run_complete_workflow.bat` - End-to-end execution
- ✅ `run_with_parallel_test.bat` - Testing with UV environment

### Integration Points
- GMsh Agent: Mesh quality validation and optimization
- OrcaWave Agent: Analysis configuration and execution
- OrcaFlex: Direct import of processed YAML/JSON files
- UV Environment: All scripts use repository Python environment

## Risk Management

### Technical Risks
| Risk | Mitigation |
|------|------------|
| Invalid mesh geometry | Pre-validation in parallel before execution |
| OrcaWave license unavailable | Dry-run mode with validation only |
| Large memory requirements | Memory estimation before execution |
| Incompatible file formats | Multiple format support with fallbacks |

### Process Risks
| Risk | Mitigation |
|------|------------|
| User unfamiliar with GMsh | Reference GMsh agent documentation |
| Validation warnings ignored | Clear severity indicators in reports |
| Post-processing incomplete | Progress tracking with resumption capability |

## References

- GMsh Agent: `agents/gmsh/README.md`
- OrcaWave Agent: `agents/orcawave/README.md`
- Go-by Examples: `inputs/orcawave/go-by/`
- OrcaWave Documentation: Vendor documentation
- OrcaFlex Integration: `specs/modules/orcaflex/orcawave-results-integration/`