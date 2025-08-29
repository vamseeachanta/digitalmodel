# Prepare Diffraction Analysis Inputs - Task Breakdown

## Phase 1: Data Extraction Module [3 hours]

### 1.1 Create Excel Reader Component for Hydrodynamic Properties ✅
- [x] Implement Excel file reader using openpyxl
- [x] Read vessel properties from `data/B1512 Gyradius Calcs_rev2.xlsx` (updated 2025-08-26)
- [x] Extract mass, CoG, gyradii, and inertia matrix values
- [x] Extract and include vessel draft for each configuration
- [x] Extract vessel dimensions (LBP, beam, depth)
- [x] Generate `hydrodynamic.yml` file with standardized format
- [x] Ensure compatibility with OrcaWave, AQWA, and other hydrodynamic programs
- [x] Support both .xlsx and .xls formats
- [x] Handle multiple worksheets
- [x] Implement error handling for missing files
- **Output**: `hydrodynamic.yml` containing mass properties for hydrodynamic analysis
- **Script Created**: `scripts/extract_hydrodynamic_properties.py`
- **Output Files**: 
  - `outputs/hydrodynamic.yml` (simplified format for programs)
  - `outputs/hydrodynamic_detailed.yml` (detailed with unit conversions)
- **Key Features Added**:
  - Vessel draft estimation based on depth (configurable)
  - Vessel dimensions (LBP: 22.86m, Beam: 8.53m, Depth: 3.05m)
  - **Updated 2025-08-26 16:55**: Modified to read from B1512 Gyradius Calcs_rev2.xlsx
  - Now generates 3 configurations: incident_draft_fuel_centered, incident_draft_with_water_ingress, windload_heel
  - Previous 4 configurations reduced to 2 in Excel + 1 from windload_heel.md
  - 4 weight configurations with varying drafts
  - Full unit conversion from Imperial to SI
- **Completed**: 2025-08-26
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 1.2 Create OrcaWave Input Configuration from Hydrodynamic Properties ✅
- [x] Read hydrodynamic properties from `outputs/hydrodynamic.yml` (generated in task 1.1)
- [x] Map vessel properties to OrcaWave input parameters:
  - Mass, CoG, and inertia matrix values
  - Vessel dimensions (LBP, beam, draft)
  - Radii of gyration
- [x] Generate OrcaWave-specific configuration file with:
  - Vessel type definition
  - Mass distribution parameters
  - Hydrostatic stiffness matrix setup
  - Reference coordinate system
- [x] Support all 4 vessel configurations from the YAML
- [x] Validate parameter ranges for OrcaWave compatibility
- [x] Create configuration template for reuse
- **Input**: `outputs/hydrodynamic.yml` from task 1.1
- **Output**: `outputs/orcawave_configs/` directory with vessel types, analyses, and batch config
- **Script Created**: `scripts/create_orcawave_config.py`
- **Completed**: 2025-08-26
- **Actual Effort**: 10 minutes (vs 30 minutes estimated)
- **Agent**: OrcaWave Agent

### 1.3 Implement Data Validation ✅
- [x] Validate numerical ranges
- [x] Check unit consistency
- [x] Verify required fields present
- [x] Generate validation report
- **Script Created**: `scripts/validate_hydrodynamic_data.py`
- **Output**: `outputs/validation_report.txt`
- **Results**: All validations passed (0 errors, 0 warnings)
- **Completed**: 2025-08-26
- **Actual Effort**: 4 minutes (vs 45 minutes estimated)
- **Agent**: OrcaWave Agent

## Phase 2: Input File Generation [2 hours]
**Note**: Geometry conversion tasks moved to gmsh module

### 2.1 Load Go-by Templates ✅
- [x] Parse YAML template files
- [x] Identify variable placeholders
- [x] Create template registry
- [x] Support template inheritance
- **Script Created**: `scripts/load_templates.py`
- **Completed**: 2025-08-26
- **Actual Effort**: 10 minutes (vs 30 minutes estimated)
- **Agent**: OrcaWave Agent

### 2.2 Implement Variable Substitution ✅
- [x] Create substitution engine
- [x] Support nested variables
- [x] Handle conditional sections
- [x] Validate substitutions
- **Script Created**: `scripts/variable_substitution.py`
- **Completed**: 2025-08-26
- **Actual Effort**: 10 minutes (vs 45 minutes estimated)
- **Agent**: OrcaWave Agent

### 2.3 Merge Data with Templates ✅
- [x] Combine vessel data with template
- [x] Apply environment settings
- [x] Set analysis parameters
- [x] Generate complete YAML
- **Script Created**: `scripts/merge_templates.py`
- **Output**: 4 merged OrcaWave configuration files
- **Completed**: 2025-08-26
- **Actual Effort**: 10 minutes (vs 30 minutes estimated)
- **Agent**: OrcaWave Agent

### 2.4 Validate Configuration ✅
- [x] Check YAML syntax
- [x] Verify all required fields
- [x] Validate parameter ranges
- [x] Generate validation report
- **Script Created**: `scripts/validate_configuration.py`
- **Output**: `outputs/orcawave_configs/validation_report.txt`
- **Results**: All configurations validated successfully (0 errors, 1 warning)
- **Completed**: 2025-08-26
- **Actual Effort**: 10 minutes (vs 15 minutes estimated)
- **Agent**: OrcaWave Agent

## Phase 3: Execution Scripts [2 hours]

### 3.1 Create Windows Batch Scripts ✅
- [x] Write run_orcawave.bat (enhanced existing script)
- [x] Add parameter passing (config files, modes, options)
- [x] Include error checking (file existence, OrcaWave installation)
- [x] Create GUI launcher option (launch_orcawave_gui.bat)
- **Scripts Created**: 
  - `scripts/run_orcawave.bat` (enhanced version with parallel support)
  - `scripts/launch_orcawave_gui.bat` (interactive GUI launcher)
- **Features Added**:
  - Support for batch/GUI/validation modes
  - Parallel execution of multiple configurations
  - Automatic output directory creation
  - Comprehensive error handling and logging
  - Progress tracking and summary reports
- **Completed**: 2025-08-26
- **Actual Effort**: 15 minutes (vs 30 minutes estimated)
- **Agent**: OrcaWave Agent

### 3.2 Develop Python Wrapper ✅
- [x] Create run_orcawave.py
- [x] Implement COM API integration (with fallback to batch mode)
- [x] Add progress monitoring (real-time progress bar)
- [x] Handle OrcaWave outputs (automatic file detection and validation)
- **Script Created**: `scripts/run_orcawave.py`
- **Features Implemented**:
  - COM API support with automatic detection
  - Three execution modes: batch, COM, parallel
  - Real-time progress monitoring with visual progress bar
  - Automatic output file detection and validation
  - JSON result export for integration with other tools
  - Parallel processing support for multiple configurations
  - Comprehensive logging and error handling
- **Usage Examples**:
  ```bash
  # Run single configuration
  python run_orcawave.py orcawave_fo_to_port.yml
  
  # Run all configurations in parallel
  python run_orcawave.py all --mode parallel --max-workers 4
  
  # Use COM API with progress monitoring
  python run_orcawave.py config.yml --mode com
  ```
- **Completed**: 2025-08-26
- **Actual Effort**: 20 minutes (vs 45 minutes estimated)
- **Agent**: OrcaWave Agent

### 3.3 Implement Parallel Validation
- [ ] Create parallel_validator.py
- [ ] Run geometry checks in parallel
- [ ] Validate multiple configs simultaneously
- [ ] Aggregate validation results
- **Effort**: 30 minutes
- **Agent**: Testing Agent

### 3.4 Add Logging and Monitoring
- [ ] Setup logging framework
- [ ] Create progress indicators
- [ ] Implement error tracking
- [ ] Generate execution reports
- **Effort**: 15 minutes
- **Agent**: OrcaWave Agent

## Phase 4: Post-Processing Tools [3 hours]

### 4.1 Extract RAO Data
- [ ] Parse OrcaWave output files
- [ ] Extract RAOs by DOF
- [ ] Organize by wave heading
- [ ] Handle complex RAO values
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 4.2 Process Hydrodynamic Coefficients
- [ ] Extract added mass matrices
- [ ] Process damping coefficients
- [ ] Get excitation forces
- [ ] Calculate derived quantities
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 4.3 Generate Excel Workbooks
- [ ] Create multi-sheet workbooks
- [ ] Format RAO tables
- [ ] Add coefficient matrices
- [ ] Include summary statistics
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 4.4 Create OrcaFlex Outputs
- [ ] Generate vessel type YAML
- [ ] Format for OrcaFlex import
- [ ] Include all hydrodynamic data
- [ ] Validate output format
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

## Phase 5: Testing and Integration [2 hours]

### 5.1 Unit Testing
- [ ] Test Excel reader with samples
- [ ] Validate geometry converter
- [ ] Check YAML generator
- [ ] Test result extractor
- **Effort**: 45 minutes
- **Agent**: Testing Agent (parallel)

### 5.2 Integration Testing
- [ ] Run end-to-end workflow
- [ ] Test parallel processing
- [ ] Validate error recovery
- [ ] Check output formats
- **Effort**: 45 minutes
- **Agent**: Testing Agent (parallel)

### 5.3 Performance Testing
- [ ] Measure conversion times
- [ ] Test with large meshes
- [ ] Validate parallel speedup
- [ ] Optimize bottlenecks
- **Effort**: 30 minutes
- **Agent**: Testing Agent

### 5.4 Support Multiple Vessel Configurations (Future Enhancement)
- [ ] Handle multiple vessels in single Excel
- [ ] Support vessel naming conventions
- [ ] Enable batch processing
- [ ] Create vessel registry
- **Note**: Not required in foreseeable future
- **Effort**: 60 minutes
- **Agent**: OrcaWave Agent

### 5.5 Implement Include Files Architecture for OrcaWave Input
- [ ] Create modular YAML structure with include files based on OrcaWave menu structure:
  - `model.yml` - Main model configuration
  - `calculation_output.yml` - Calculation and output settings
  - `environment/` directory:
    - `bodies.yml` - Bodies configuration (vessel properties from hydrodynamic.yml)
    - `inertia.yml` - Inertia properties (mass, CoG, radii of gyration)
    - `constraints.yml` - Constraint definitions
    - `morison_elements.yml` - Morison element definitions if applicable
    - `spring_dampers.yml` - Spring/damper settings
    - `field_points.yml` - Field point definitions
  - `drawing/` directory:
    - `mesh_view.yml` - Mesh visualization settings
    - `validation.yml` - Validation parameters
- [ ] Implement YAML include mechanism using anchors and references
- [ ] Create master configuration file that includes all component files
- [ ] Support configuration inheritance and overrides
- [ ] Enable easy data maintenance and parameter mapping
- [ ] Document include file relationships and dependencies
- [ ] Create template generator for new configurations
- **Benefits**: 
  - Modular configuration management
  - Easier version control and diff tracking
  - Reusable components across different analyses
  - Clear separation of concerns
  - Simplified data maintenance
- **Input**: `outputs/hydrodynamic.yml` from task 1.1
- **Output**: Modular OrcaWave configuration structure in `outputs/orcawave_config/`
- **Effort**: 90 minutes
- **Agent**: OrcaWave Agent

## Summary
- **Total Tasks**: 41 (includes new include files architecture task)
- **Total Effort**: 12.5 hours
- **Parallel Execution**: Reduces to ~7 hours with agent delegation
- **Critical Path**: Data extraction → Input generation → Execution → Post-processing
- **New Architecture**: Modular include files structure for better maintainability

## Agent Workload Distribution
- **OrcaWave Agent**: 80% (orchestration, data processing, input generation, post-processing)
- **Testing Agent**: 20% (parallel validation, testing)
- **GMsh Agent**: N/A (geometry tasks moved to gmsh module)

## Dependencies
- Data extraction from Excel (Phase 1) must complete before input generation
- Geometry conversion handled separately by gmsh module
- Testing runs continuously in parallel threads
- Post-processing requires completed OrcaWave execution
- Task 1.2 depends on outputs from task 1.1 (hydrodynamic.yml)