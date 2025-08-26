# Prepare Diffraction Analysis Inputs - Task Breakdown

## Phase 1: Data Extraction Module [3 hours]

### 1.1 Create Excel Reader Component
- [ ] Implement Excel file reader using openpyxl
- [ ] Support both .xlsx and .xls formats
- [ ] Handle multiple worksheets
- [ ] Implement error handling for missing files
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 1.2 Define Parameter Mapping Schema
- [ ] Create vessel parameter schema (mass, inertia, CoG)
- [ ] Map Excel columns to YAML fields
- [ ] Support flexible column naming
- [ ] Document required vs optional fields
- **Effort**: 30 minutes
- **Agent**: OrcaWave Agent

### 1.3 Implement Data Validation
- [ ] Validate numerical ranges
- [ ] Check unit consistency
- [ ] Verify required fields present
- [ ] Generate validation report
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 1.4 Support Multiple Vessel Configurations
- [ ] Handle multiple vessels in single Excel
- [ ] Support vessel naming conventions
- [ ] Enable batch processing
- [ ] Create vessel registry
- **Effort**: 60 minutes
- **Agent**: OrcaWave Agent

## Phase 2: Geometry Conversion [3 hours]

### 2.1 Implement GMsh .msh Parser
- [ ] Parse GMsh ASCII format
- [ ] Extract node coordinates
- [ ] Process element connectivity
- [ ] Handle different element types
- **Effort**: 60 minutes
- **Agent**: GMsh Agent

### 2.2 Convert to WAMIT GDF Format
- [ ] Generate GDF header
- [ ] Write vertex coordinates
- [ ] Create panel definitions
- [ ] Add symmetry planes if applicable
- **Effort**: 45 minutes
- **Agent**: GMsh Agent

### 2.3 Validate Mesh Properties
- [ ] Check mesh is watertight
- [ ] Verify normal orientations
- [ ] Calculate panel aspect ratios
- [ ] Report mesh statistics
- **Effort**: 45 minutes
- **Agent**: GMsh Agent (parallel with Testing Agent)

### 2.4 Generate Control Surfaces
- [ ] Create automatic control surface
- [ ] Set separation distance
- [ ] Include free surface panels
- [ ] Optimize panel sizing
- **Effort**: 30 minutes
- **Agent**: GMsh Agent

## Phase 3: Input File Generation [2 hours]

### 3.1 Load Go-by Templates
- [ ] Parse YAML template files
- [ ] Identify variable placeholders
- [ ] Create template registry
- [ ] Support template inheritance
- **Effort**: 30 minutes
- **Agent**: OrcaWave Agent

### 3.2 Implement Variable Substitution
- [ ] Create substitution engine
- [ ] Support nested variables
- [ ] Handle conditional sections
- [ ] Validate substitutions
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 3.3 Merge Data with Templates
- [ ] Combine vessel data with template
- [ ] Apply environment settings
- [ ] Set analysis parameters
- [ ] Generate complete YAML
- **Effort**: 30 minutes
- **Agent**: OrcaWave Agent

### 3.4 Validate Configuration
- [ ] Check YAML syntax
- [ ] Verify all required fields
- [ ] Validate parameter ranges
- [ ] Generate validation report
- **Effort**: 15 minutes
- **Agent**: Testing Agent (parallel)

## Phase 4: Execution Scripts [2 hours]

### 4.1 Create Windows Batch Scripts
- [ ] Write run_orcawave.bat
- [ ] Add parameter passing
- [ ] Include error checking
- [ ] Create GUI launcher option
- **Effort**: 30 minutes
- **Agent**: OrcaWave Agent

### 4.2 Develop Python Wrapper
- [ ] Create run_orcawave.py
- [ ] Implement COM API integration
- [ ] Add progress monitoring
- [ ] Handle OrcaWave outputs
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 4.3 Implement Parallel Validation
- [ ] Create parallel_validator.py
- [ ] Run geometry checks in parallel
- [ ] Validate multiple configs simultaneously
- [ ] Aggregate validation results
- **Effort**: 30 minutes
- **Agent**: Testing Agent

### 4.4 Add Logging and Monitoring
- [ ] Setup logging framework
- [ ] Create progress indicators
- [ ] Implement error tracking
- [ ] Generate execution reports
- **Effort**: 15 minutes
- **Agent**: OrcaWave Agent

## Phase 5: Post-Processing Tools [3 hours]

### 5.1 Extract RAO Data
- [ ] Parse OrcaWave output files
- [ ] Extract RAOs by DOF
- [ ] Organize by wave heading
- [ ] Handle complex RAO values
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 5.2 Process Hydrodynamic Coefficients
- [ ] Extract added mass matrices
- [ ] Process damping coefficients
- [ ] Get excitation forces
- [ ] Calculate derived quantities
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 5.3 Generate Excel Workbooks
- [ ] Create multi-sheet workbooks
- [ ] Format RAO tables
- [ ] Add coefficient matrices
- [ ] Include summary statistics
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

### 5.4 Create OrcaFlex Outputs
- [ ] Generate vessel type YAML
- [ ] Format for OrcaFlex import
- [ ] Include all hydrodynamic data
- [ ] Validate output format
- **Effort**: 45 minutes
- **Agent**: OrcaWave Agent

## Phase 6: Testing and Integration [2 hours]

### 6.1 Unit Testing
- [ ] Test Excel reader with samples
- [ ] Validate geometry converter
- [ ] Check YAML generator
- [ ] Test result extractor
- **Effort**: 45 minutes
- **Agent**: Testing Agent (parallel)

### 6.2 Integration Testing
- [ ] Run end-to-end workflow
- [ ] Test parallel processing
- [ ] Validate error recovery
- [ ] Check output formats
- **Effort**: 45 minutes
- **Agent**: Testing Agent (parallel)

### 6.3 Performance Testing
- [ ] Measure conversion times
- [ ] Test with large meshes
- [ ] Validate parallel speedup
- [ ] Optimize bottlenecks
- **Effort**: 30 minutes
- **Agent**: Testing Agent

## Summary
- **Total Tasks**: 44
- **Total Effort**: 14 hours
- **Parallel Execution**: Reduces to ~8 hours with agent delegation
- **Critical Path**: Geometry conversion → Input generation → Execution

## Agent Workload Distribution
- **OrcaWave Agent**: 60% (orchestration, data processing, post-processing)
- **GMsh Agent**: 20% (geometry conversion, validation)
- **Testing Agent**: 20% (parallel validation, testing)

## Dependencies
- GMsh agent must complete geometry conversion before input generation
- Excel extraction can run in parallel with geometry conversion
- Testing runs continuously in parallel threads
- Post-processing requires completed OrcaWave execution