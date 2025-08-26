# Prepare Diffraction Analysis Inputs Specification

## Overview
This specification details the automated workflow for preparing OrcaWave diffraction analysis inputs from various data sources, including Excel files, geometry files, and configuration templates.

## Goals
1. Automate extraction of vessel data from Excel spreadsheets
2. Convert GMsh geometry files to OrcaWave-compatible formats
3. Generate OrcaWave input configuration files from templates
4. Provide execution scripts for running OrcaWave analysis
5. Create post-processing tools for extracting key output data
6. Enable parallel processing for improved performance

## Current State Analysis

### Existing Capabilities
- **OrcaWave Agent**: Full diffraction analysis automation with COM API integration
- **GMsh Agent**: Mesh generation and optimization for offshore structures  
- **Existing Scripts**: Process_orcawave_results.py with Excel/CSV output capabilities
- **Templates**: Go-by configuration files in YAML format
- **Geometry Files**: Sea Cypress .msh file ready for conversion

### Gaps to Address
1. Direct GMsh .msh to OrcaWave GDF converter
2. Excel data extraction for vessel parameters
3. Template-based YAML generation with variable substitution
4. Batch/GUI execution scripts with parallel validation
5. Enhanced post-processing for specific output requirements

## Technical Requirements

### Input Requirements
- **Geometry**: GMsh .msh file (Sea Cypress_0.25 Mesh_Ascii.msh)
- **Vessel Data**: Excel files with mass, inertia, CoG properties
- **Configuration**: Go-by YAML templates for OrcaWave settings
- **Environment**: Wave periods, headings, water depth parameters

### Output Requirements
- **OrcaWave Input**: Complete YAML configuration files
- **Geometry**: Converted GDF files with validation
- **Scripts**: Batch/Python execution scripts  
- **Results**: Extracted RAOs, QTFs, hydrodynamic coefficients
- **Formats**: Excel workbooks, CSV matrices, OrcaFlex YAML

### Processing Requirements
- **Parallel Execution**: 3x performance improvement through parallelization
- **Validation**: Geometry checks, configuration validation
- **Error Handling**: Comprehensive error reporting and recovery
- **Logging**: Detailed progress tracking and diagnostics

## Architecture

### Component Structure
```
prepare-diffraction-analysis-inputs/
├── data_extraction/
│   ├── excel_reader.py       # Extract vessel data from Excel
│   └── parameter_mapper.py   # Map Excel data to YAML fields
├── geometry_conversion/
│   ├── msh_to_gdf.py         # Convert GMsh to OrcaWave GDF
│   └── geometry_validator.py # Validate converted geometry
├── input_generation/
│   ├── yaml_generator.py     # Generate OrcaWave input files
│   └── template_processor.py # Process go-by templates
├── execution/
│   ├── run_orcawave.bat      # Windows batch script
│   ├── run_orcawave.py       # Python execution wrapper
│   └── parallel_executor.py  # Parallel validation/execution
└── post_processing/
    ├── extract_results.py     # Extract key output data
    └── format_outputs.py      # Format for downstream use
```

### Data Flow
1. **Excel → Parameters**: Extract vessel properties
2. **GMsh → GDF**: Convert geometry with validation
3. **Template + Parameters → YAML**: Generate input files
4. **YAML → OrcaWave**: Execute analysis (user/automated)
5. **Results → Excel/CSV**: Extract and format outputs

## Implementation Details

### Phase 1: Data Extraction Module
- Create Excel reader using openpyxl/pandas
- Define parameter mapping schema
- Implement validation for required fields
- Support multiple vessel configurations

### Phase 2: Geometry Conversion
- Implement GMsh .msh parser
- Convert to WAMIT GDF format
- Validate mesh properties (watertight, normals)
- Generate control surfaces if needed

### Phase 3: Input File Generation
- Load go-by YAML templates
- Implement variable substitution engine
- Merge extracted data with templates
- Validate complete configuration

### Phase 4: Execution Scripts
- Create Windows batch scripts for GUI execution
- Develop Python wrapper with COM API support
- Implement parallel validation checks
- Add progress monitoring and logging

### Phase 5: Post-Processing Tools
- Extract RAOs by DOF and heading
- Process hydrodynamic coefficients
- Generate Excel workbooks with multiple sheets
- Create OrcaFlex-compatible outputs

## Agent Delegation

### Primary Agents
- **OrcaWave Agent**: Main orchestration and OrcaWave API operations
- **GMsh Agent**: Geometry processing and mesh operations
- **Testing Agent**: Parallel test execution and validation

### Task Delegation Matrix
| Task Component | Assigned Agent | Capabilities Used |
|---------------|---------------|-------------------|
| Excel extraction | OrcaWave Agent | Data processing, validation |
| Geometry conversion | GMsh Agent | Mesh parsing, format conversion |
| YAML generation | OrcaWave Agent | Template processing, configuration |
| Parallel validation | Testing Agent | Concurrent execution, reporting |
| Post-processing | OrcaWave Agent | Results extraction, formatting |

### Inter-Agent Communication
- Agents share geometry validation results
- GMsh Agent provides mesh metrics to OrcaWave Agent
- Testing Agent reports validation status to all agents
- Coordinated error handling and recovery

## Testing Strategy

### Unit Testing
- Excel reader with sample data files
- Geometry converter with test meshes
- YAML generator with template validation
- Result extractor with mock outputs

### Integration Testing
- End-to-end workflow validation
- Parallel processing performance
- Error recovery scenarios
- Output format compliance

### Validation Testing
- Geometry validation (normals, watertight)
- Configuration completeness checks
- Hydrodynamic coefficient validation
- RAO symmetry and magnitude checks

## Success Criteria
1. ✅ Automated Excel data extraction (< 5 seconds)
2. ✅ GMsh to GDF conversion with validation (< 30 seconds)
3. ✅ Template-based YAML generation (< 2 seconds)
4. ✅ Parallel validation 3x faster than serial
5. ✅ Complete post-processing to Excel/CSV (< 10 seconds)
6. ✅ All outputs compatible with OrcaFlex import

## Risk Mitigation
- **Geometry Issues**: Implement fallback to simplified meshes
- **Excel Format Changes**: Use flexible column mapping
- **OrcaWave Licensing**: Provide mock mode for testing
- **Large Files**: Implement chunked processing
- **Parallel Failures**: Graceful degradation to serial

## Dependencies
- Python 3.10+ with uv environment
- openpyxl/pandas for Excel operations
- PyYAML for configuration handling
- numpy/scipy for numerical operations
- trimesh for geometry validation
- OrcaWave COM API (when available)

## Timeline Estimate
- Phase 1 (Data Extraction): 2 hours
- Phase 2 (Geometry Conversion): 3 hours
- Phase 3 (Input Generation): 2 hours
- Phase 4 (Execution Scripts): 2 hours
- Phase 5 (Post-Processing): 3 hours
- Testing & Validation: 2 hours
- **Total Estimate**: 14 hours

## References
- OrcaWave Agent: `agents/orcawave/`
- GMsh Agent: `agents/gmsh/`
- Existing Scripts: `specs/modules/orcawave/diffraction-analysis/scripts/`
- Go-by Templates: `specs/modules/orcawave/diffraction-analysis/inputs/orcawave/go-by/`
- Geometry Files: `specs/modules/orcawave/diffraction-analysis/inputs/geometry/`