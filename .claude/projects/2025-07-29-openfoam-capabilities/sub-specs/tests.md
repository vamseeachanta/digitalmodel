# Tests Specification

This is the tests coverage details for the spec detailed in @.agent-os/projects/2025-07-29-openfoam-capabilities/spec.md

> Created: 2025-07-29
> Version: 1.0.0

## Test Coverage

### Unit Tests

**OpenFOAMUtilities**
- Test OpenFOAM dictionary file reading and writing
- Test boundary condition formatting and validation
- Test mesh parameter calculation functions
- Test file path resolution and validation
- Test PyFoam wrapper functions error handling

**MeshGenerator**
- Test blockMesh dictionary generation from parameters
- Test snappyHexMesh configuration creation
- Test domain sizing calculations based on geometry
- Test refinement region specifications
- Test mesh quality metric calculations

**SolverConfiguration**
- Test solver selection based on physics type
- Test turbulence model configuration
- Test numerical scheme selection
- Test convergence criteria setup
- Test parallel decomposition settings

**PostProcessor**
- Test force coefficient extraction from OpenFOAM logs
- Test flow field data conversion to DataFrame
- Test VTK file reading and data extraction
- Test convergence history parsing
- Test result aggregation for multiple cases

### Integration Tests

**OpenFOAM Installation Verification**
- Verify OpenFOAM executable detection
- Test environment variable configuration
- Validate PyFoam library integration
- Check ParaView post-processing connection
- Test MPI functionality for parallel runs

**Hull Resistance Analysis Workflow**
- Test complete workflow from STL geometry to force results
- Verify mesh generation for complex hull geometry
- Validate boundary condition application
- Test solver execution and convergence
- Verify force integration and coefficient calculation

**Configuration-Driven Execution**
- Test YAML configuration parsing and validation
- Verify configuration to OpenFOAM case translation
- Test parameter sweep functionality
- Validate output directory structure creation
- Test error handling for invalid configurations

**Parallel Processing**
- Test case decomposition for multiple processors
- Verify parallel execution with ProcessPoolExecutor
- Test result reconstruction from parallel runs
- Validate load balancing effectiveness
- Test error recovery in parallel execution

### Feature Tests

**End-to-End Scenarios**

1. **Basic Hull Analysis**
   - Load hull geometry from STL file
   - Generate mesh with specified refinement
   - Run steady-state CFD simulation
   - Extract drag and lift coefficients
   - Generate visualization plots

2. **Parameter Study**
   - Define velocity range in configuration
   - Execute multiple CFD cases in parallel
   - Aggregate results across all cases
   - Generate performance curves
   - Export data to Excel format

3. **Test Case Validation**
   - Run standard validation cases (flat plate, sphere)
   - Compare results with analytical solutions
   - Generate validation report
   - Check mesh convergence behavior
   - Verify solver accuracy metrics

### Mocking Requirements

**OpenFOAM Executable Mocking**
- Mock Strategy: Create fake OpenFOAM commands that return pre-recorded outputs
- Purpose: Enable testing without OpenFOAM installation
- Implementation: Mock subprocess calls with fixture data

**PyFoam Library Mocking**
- Mock Strategy: Patch PyFoam imports with mock objects returning test data
- Purpose: Test integration logic without dependencies
- Implementation: Use unittest.mock with realistic response patterns

**File System Mocking**
- Mock Strategy: Use temporary directories and fixture files
- Purpose: Test file operations without affecting system
- Implementation: pytest tmp_path fixtures with sample OpenFOAM cases

**MPI Execution Mocking**
- Mock Strategy: Simulate parallel execution with sequential calls
- Purpose: Test parallel logic on single-core systems
- Implementation: Mock mpirun commands with process pool simulation

## Test Data Requirements

### Fixture Files
- Sample STL geometries (simple shapes for unit tests)
- Pre-generated OpenFOAM case directories
- Example convergence logs from real simulations
- Reference force calculation results
- Mock PyFoam response data

### Validation Benchmarks
- Flat plate boundary layer (compare with Blasius solution)
- Flow around cylinder (compare drag coefficient)
- NACA airfoil profiles (compare with experimental data)
- Simple hull forms with published resistance data

## Performance Testing

### Benchmarks
- Mesh generation time vs. cell count
- Solver iteration time scaling
- Parallel speedup measurements
- Memory usage profiling
- File I/O performance metrics

### Stress Tests
- Large mesh handling (>10M cells)
- Long-running simulations (>10k iterations)
- Multiple concurrent case execution
- Memory-constrained environments
- Network file system performance

## Test Execution Strategy

1. **Local Development**: Run unit tests and mocked integration tests
2. **CI Pipeline**: Execute full test suite with mock OpenFOAM
3. **Linux Test Servers**: Run real OpenFOAM integration tests
4. **Validation Suite**: Periodic execution of physics validation cases
5. **Performance Regression**: Track performance metrics over releases