# Technical Specification

This is the technical specification for the spec detailed in @.agent-os/projects/2025-07-29-openfoam-capabilities/spec.md

> Created: 2025-07-29
> Version: 1.0.0

## Technical Requirements

### OpenFOAM Installation Requirements
- OpenFOAM v2312 or compatible version for optimal stability
- Linux operating system (Ubuntu 22.04 LTS or RHEL 8+ recommended)
- Minimum 8GB RAM, 16GB recommended for complex simulations
- GCC compiler suite (version 9.0 or higher)
- MPI implementation (OpenMPI or MPICH) for parallel processing
- ParaView for post-processing visualization

### Integration Architecture
- Module location: `src/digitalmodel/modules/openfoam/`
- Configuration schema following DigitalModel YAML patterns
- ProcessPoolExecutor support for parallel case execution
- Mock API patterns for testing without OpenFOAM installation
- Results output compatible with existing visualization framework

### CFD Analysis Requirements
- Support for incompressible flow solvers (simpleFoam, pimpleFoam)
- Automated mesh generation using blockMesh and snappyHexMesh
- Boundary condition configuration via YAML
- Convergence monitoring and automatic stopping criteria
- Post-processing data extraction to CSV/Excel formats

### Hull Analysis Specifications
- STL geometry import capability
- Automated domain sizing based on hull dimensions
- Free surface modeling capability for resistance prediction
- Force and moment calculation on hull surfaces
- Wake field extraction and visualization

## Approach Options

**Option A:** Direct OpenFOAM API Integration
- Pros: Maximum control, real-time monitoring possible, efficient data transfer
- Cons: Complex implementation, version dependency, harder to maintain

**Option B:** File-Based Integration with PyFoam (Selected)
- Pros: Version flexibility, easier debugging, follows DigitalModel patterns, robust error handling
- Cons: File I/O overhead, less real-time capability

**Rationale:** Option B selected for consistency with existing DigitalModel architecture and proven reliability in production environments. PyFoam provides stable interface across OpenFOAM versions.

## External Dependencies

- **PyFoam** (>=2023.6) - Python library for OpenFOAM automation
  - **Justification:** Industry-standard tool for OpenFOAM scripting, extensive functionality, active maintenance
  
- **meshio** (>=5.3.0) - Mesh file format conversion
  - **Justification:** Required for converting between CAD formats and OpenFOAM mesh format
  
- **pandas** (already in use) - Data processing and analysis
  - **Justification:** Consistent with DigitalModel data handling patterns

- **vtk** (>=9.0.0) - Optional for advanced post-processing
  - **Justification:** Enhanced visualization capabilities beyond basic ParaView integration

## Module Structure

```
src/digitalmodel/modules/openfoam/
├── __init__.py              # Module interface
├── analysis.py              # Main CFD analysis orchestrator
├── mesh.py                  # Mesh generation utilities
├── solvers.py               # Solver configuration and execution
├── post_process.py          # Results extraction and formatting
├── utilities.py             # OpenFOAM file manipulation helpers
├── config_schemas.py        # YAML validation schemas
└── templates/               # OpenFOAM case templates
    ├── hull_resistance/
    ├── flow_around_structure/
    └── test_cases/
```

## Configuration Schema

```yaml
openfoam_analysis:
  case_name: "hull_resistance_analysis"
  geometry:
    file: "path/to/hull.stl"
    scale: 1.0
    units: "meters"
  
  mesh:
    type: "snappyHexMesh"
    base_cell_size: 0.5
    refinement_levels:
      surface: 2
      wake: 1
    layers:
      n_layers: 5
      expansion_ratio: 1.2
  
  physics:
    solver: "simpleFoam"
    turbulence_model: "kOmegaSST"
    fluid_properties:
      density: 1025.0  # kg/m3
      viscosity: 1.0e-6  # m2/s
  
  boundary_conditions:
    inlet:
      type: "velocity"
      value: [10.0, 0.0, 0.0]  # m/s
    outlet:
      type: "pressure"
      value: 0.0  # Pa
    hull:
      type: "wall"
      wall_function: true
  
  simulation:
    end_time: 1000  # iterations for steady-state
    write_interval: 100
    convergence_criteria:
      residual_target: 1.0e-5
      force_tolerance: 0.01
  
  post_processing:
    forces:
      patches: ["hull"]
      reference_area: 50.0  # m2
      reference_length: 10.0  # m
    flow_field:
      slice_locations: [0, 5, 10]  # x-coordinates
      variables: ["U", "p", "k"]
  
  output:
    directory: "results/cfd/"
    formats: ["csv", "vtk", "images"]
```

## Performance Considerations

- Parallel mesh generation using domain decomposition
- Automatic core allocation based on mesh size
- Memory estimation before simulation start
- Checkpoint/restart capability for long-running simulations
- Results streaming for large datasets

## Error Handling

- Geometry validation before mesh generation
- Mesh quality checks with automatic refinement
- Solver stability monitoring with fallback options
- Comprehensive logging of OpenFOAM output
- Graceful handling of convergence failures