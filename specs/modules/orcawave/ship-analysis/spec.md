# OrcaWave Comprehensive Ship Analysis Module Specification

## Executive Summary
This specification defines a comprehensive OrcaWave ship wave-structure interaction analysis module that automates diffraction/radiation calculations, second-order wave force computations, and hydrodynamic database generation for ship design and offshore operations. The module provides seamless integration with OrcaFlex for time-domain simulations and generates publication-ready reports for regulatory compliance.

## Problem Statement
Naval architects and offshore engineers need accurate wave load predictions for:
- Ship motion analysis in waves
- Wave-induced structural loads
- Second-order drift forces for mooring design
- Hydrodynamic database generation for OrcaFlex
- Green water and air gap assessments
- Regulatory compliance documentation

Current workflows require manual setup of OrcaWave models, extensive post-processing, and manual transfer to OrcaFlex. This module automates the entire workflow with validation and standardized reporting.

## Proposed Solution

### Core Architecture
```
orcawave-ship/
├── input/
│   ├── geometry/         # Hull geometry and mesh definition
│   ├── environment/      # Wave conditions and spectra
│   ├── configuration/    # Analysis settings and parameters
│   └── validation/       # Input checking and verification
├── processing/
│   ├── diffraction/      # Wave diffraction analysis
│   ├── radiation/        # Radiation damping and added mass
│   ├── qtf/             # Second-order force calculations
│   └── database/         # Hydrodynamic database generation
└── output/
    ├── hydrodynamics/    # Coefficients and matrices
    ├── loads/            # Wave loads and pressures
    ├── visualization/    # Plots and animations
    └── reports/          # Comprehensive documentation
```

## Technical Specification

### 1. Input Requirements

#### 1.1 Geometry Definition
```yaml
geometry:
  hull_definition:
    type: "mesh"  # mesh, parametric, or nurbs
    source:
      gdf_file: "ship_hull.gdf"  # OrcaWave GDF format
      units: "meters"
    
    dimensions:
      length_overall: 320.0      # LOA in meters
      length_waterline: 310.0    # LWL
      beam: 58.0                 # Maximum beam
      draft: 20.5                # Design draft
      displacement: 297000       # tonnes
      block_coefficient: 0.82    # Cb
    
    mesh_properties:
      panel_size: 
        target: 5.0              # Target panel size (m)
        min: 2.0                 # Minimum allowed
        max: 10.0                # Maximum allowed
      refinement_zones:
        - location: "waterline"
          factor: 0.5            # Refinement factor
        - location: "bilge"
          factor: 0.7
      quality_criteria:
        aspect_ratio_max: 4.0
        skewness_max: 0.7
        waterline_panels: 100    # Min panels at waterline
  
  appendages:
    include_appendages: false    # Simplified for wave loads
    
  symmetry:
    port_starboard: true         # Use symmetry plane
    forward_aft: false           # No fore-aft symmetry
```

#### 1.2 Mass and Hydrostatics
```yaml
mass_properties:
  center_of_gravity:
    x: 140.5    # from AP (m)
    y: 0.0      # from CL (m)
    z: 16.8     # above BL (m)
  
  mass_matrix:
    mass: 297000000  # kg
    inertia:
      ixx: 1.61e11   # kg.m²
      iyy: 4.75e12   # kg.m²
      izz: 4.97e12   # kg.m²
      ixy: 0.0       # Products of inertia
      ixz: 0.0
      iyz: 0.0
  
  hydrostatics:
    waterplane_area: 14500      # m²
    gm_transverse: 3.2          # m
    gm_longitudinal: 320.0      # m
    lcb: 142.0                  # from AP (m)
    lcf: 138.0                  # from AP (m)
    tpc: 148.5                  # tonnes/cm
```

#### 1.3 Wave Environment
```yaml
wave_environment:
  water_properties:
    depth: "infinite"           # or specify depth in meters
    density: 1025.0            # kg/m³
    kinematic_viscosity: 1.19e-6  # m²/s
  
  wave_frequencies:
    type: "automatic"          # automatic, manual, or standard
    range:
      min: 0.02                # Hz (50s period)
      max: 2.0                 # Hz (0.5s period)
    distribution: "logarithmic"
    count: 50
    
  wave_directions:
    range:
      start: 0.0               # Head seas
      end: 180.0               # Following seas
    increment: 15.0            # degrees
    reference: "ship_heading"  # or "north"
    
  wave_spectra:
    - type: "JONSWAP"
      hs: 6.0                  # Significant height (m)
      tp: 12.0                 # Peak period (s)
      gamma: 3.3               # Peak enhancement
      direction: 45.0          # Mean direction
      spreading: 30.0          # Directional spreading
    
    - type: "PM"
      hs: 8.0
      tp: 14.0
      direction: 90.0
```

#### 1.4 Analysis Configuration
```yaml
analysis:
  diffraction:
    enabled: true
    method: "panel_method"     # panel_method or source_distribution
    formulation: "pulsating_source"
    forward_speed: 0.0         # knots (stationary)
    include_irregular_frequencies: true
    removal_method: "lid_integration"
    
  radiation:
    enabled: true
    degrees_of_freedom: [surge, sway, heave, roll, pitch, yaw]
    damping:
      include_viscous: false   # Pure potential flow
      include_forward_speed: false
    
  second_order:
    enabled: true
    qtf_method: "full"         # full, diagonal, or newman
    components:
      mean_drift: true
      slow_drift: true
      sum_frequency: false     # Usually not needed for ships
    frequency_pairs:
      selection: "automatic"
      max_difference: 0.1      # Hz
      
  loads:
    pressure_integration: true
    panel_pressures: true
    sectional_loads: true
    stations: 20               # Number of sections
    
  database_export:
    orcaflex_format: true
    wamit_format: false
    aqwa_format: false
```

### 2. Processing Workflow

#### 2.1 Pre-Processing
```python
class OrcaWavePreProcessor:
    def validate_geometry(self):
        # Check mesh quality
        # Verify waterline definition
        # Ensure panel closure
        # Validate symmetry conditions
        
    def optimize_mesh(self):
        # Refine at waterline
        # Coarsen far from body
        # Check convergence criteria
        
    def prepare_analysis(self):
        # Set up frequency array
        # Configure wave directions
        # Initialize matrices
```

#### 2.2 Diffraction Analysis
```python
class DiffractionSolver:
    def solve_diffraction_problem(self):
        # Solve boundary value problem
        # Calculate wave excitation forces
        # Compute pressure distributions
        # Handle irregular frequencies
        
    def calculate_wave_loads(self):
        # Integrate pressures
        # Calculate sectional loads
        # Compute global forces/moments
```

#### 2.3 Radiation Analysis
```python
class RadiationSolver:
    def solve_radiation_problem(self):
        # Calculate added mass matrices
        # Compute radiation damping
        # Generate impulse response functions
        
    def validate_results(self):
        # Check symmetry properties
        # Verify high-frequency limits
        # Validate energy conservation
```

#### 2.4 Second-Order Forces
```python
class QTFProcessor:
    def calculate_mean_drift(self):
        # Near-field pressure integration
        # Far-field momentum method
        # Validate with energy conservation
        
    def calculate_slow_drift(self):
        # Full QTF matrix calculation
        # Newman approximation option
        # Difference frequency forces
```

### 3. Output Requirements

#### 3.1 Hydrodynamic Coefficients
```yaml
outputs:
  hydrodynamic_data:
    added_mass:
      format: "6x6_matrix"
      frequency_dependent: true
      file: "added_mass.h5"
      units: 
        translational: "kg"
        rotational: "kg.m²"
        coupling: "kg.m"
    
    damping:
      format: "6x6_matrix"
      components:
        radiation: "damping_radiation.h5"
        wave_drift: "damping_drift.h5"
      units: 
        translational: "kg/s"
        rotational: "kg.m²/s"
    
    excitation_forces:
      format: "complex_amplitude"
      components: [magnitude, phase]
      reference: "cog"  # or waterline
      file: "excitation_forces.h5"
```

#### 3.2 Response Functions
```yaml
  response_functions:
    raos:
      format: "complex"
      degrees_of_freedom: 6
      reference_point: "cog"
      units:
        surge: "m/m"
        sway: "m/m"
        heave: "m/m"
        roll: "deg/m"
        pitch: "deg/m"
        yaw: "deg/m"
      file: "raos.csv"
    
    wave_loads:
      sectional:
        stations: 20
        components: [shear, moment, torsion]
        format: "rao"
        file: "sectional_loads.csv"
      
      global:
        components: [fx, fy, fz, mx, my, mz]
        format: "transfer_function"
        file: "global_loads.csv"
```

#### 3.3 Second-Order Results
```yaml
  second_order:
    mean_drift:
      format: "force_per_amplitude_squared"
      components: [surge, sway, yaw]
      file: "mean_drift_forces.csv"
    
    qtf_matrices:
      format: "full_matrix"
      frequency_pairs: "all_computed"
      components:
        difference: "qtf_difference.h5"
        sum: "qtf_sum.h5"
      units: "kN/(m²)"
```

#### 3.4 Pressure and Visualization
```yaml
  pressure_results:
    panel_pressures:
      format: "time_series"
      sampling: 0.1  # seconds
      duration: 600  # seconds
      file: "panel_pressures.h5"
    
    pressure_animation:
      format: "mp4"
      fps: 30
      duration: 60
      file: "pressure_animation.mp4"
    
    contour_plots:
      wave_periods: [6, 8, 10, 12, 14]
      wave_headings: [0, 45, 90, 135, 180]
      format: "png"
      directory: "pressure_contours/"
```

#### 3.5 OrcaFlex Database
```yaml
  orcaflex_export:
    vessel_type:
      name: "Ship_320m"
      database_file: "ship_hydrodynamics.yml"
      include:
        displacement_raos: true
        load_raos: true
        added_mass: true
        damping: true
        qtf: true
    
    validation:
      check_symmetry: true
      verify_limits: true
      test_interpolation: true
```

### 4. Reporting System

#### 4.1 Technical Report
```yaml
reports:
  technical_report:
    format: "pdf"
    sections:
      - executive_summary
      - methodology
      - mesh_convergence
      - hydrodynamic_coefficients
      - response_amplitude_operators
      - wave_load_analysis
      - second_order_forces
      - validation_results
      - conclusions
    
    appendices:
      - input_files
      - numerical_settings
      - convergence_studies
      - benchmark_comparisons
```

#### 4.2 Visualization Package
```yaml
  visualization:
    rao_plots:
      - polar_plots_by_period
      - magnitude_phase_plots
      - 3d_surface_plots
    
    coefficient_plots:
      - added_mass_vs_frequency
      - damping_vs_frequency
      - excitation_force_plots
    
    animation:
      - wave_interaction
      - pressure_distribution
      - motion_response
```

#### 4.3 Compliance Documentation
```yaml
  compliance:
    standards:
      - API_RP_2SK
      - DNV_OS_E301
      - ISO_19901_7
    
    checks:
      - mesh_independence
      - frequency_resolution
      - convergence_criteria
      - physical_bounds
    
    format: "structured_report"
    file: "compliance_report.xlsx"
```

## Implementation Architecture

### Module Structure
```python
# src/digitalmodel/modules/orcawave/ship/
ship/
├── __init__.py
├── input/
│   ├── geometry_processor.py    # Mesh handling and validation
│   ├── environment_setup.py     # Wave condition configuration
│   └── config_validator.py      # Input validation
├── solvers/
│   ├── diffraction_solver.py    # Diffraction problem solver
│   ├── radiation_solver.py      # Radiation problem solver
│   └── qtf_calculator.py        # Second-order force calculation
├── output/
│   ├── database_exporter.py     # OrcaFlex database generation
│   ├── report_generator.py      # Automated reporting
│   └── visualizer.py            # Plotting and animation
├── integration/
│   ├── orcaflex_interface.py    # OrcaFlex integration
│   ├── validation.py            # Result validation
│   └── benchmarks.py            # Standard test cases
└── cli.py                       # Command-line interface
```

### Key Classes
```python
class OrcaWaveShipAnalysis:
    """Main orchestrator for OrcaWave ship analysis"""
    
    def __init__(self, config_path: str):
        self.config = self.load_configuration(config_path)
        self.geometry = GeometryProcessor()
        self.environment = EnvironmentSetup()
        self.diffraction = DiffractionSolver()
        self.radiation = RadiationSolver()
        self.qtf = QTFCalculator()
        self.exporter = DatabaseExporter()
        
    def run_analysis(self):
        """Execute complete OrcaWave analysis workflow"""
        # Validate and prepare inputs
        self.geometry.process_mesh(self.config)
        self.environment.setup_waves(self.config)
        
        # Run first-order analysis
        excitation = self.diffraction.solve(self.geometry, self.environment)
        coefficients = self.radiation.solve(self.geometry)
        
        # Calculate second-order forces
        qtf = self.qtf.calculate(excitation, coefficients)
        
        # Export results
        self.exporter.create_orcaflex_database(coefficients, excitation, qtf)
        
    def validate_results(self):
        """Comprehensive validation of results"""
        # Check physical bounds
        # Verify symmetry properties
        # Compare with benchmarks
        # Assess convergence
```

## Integration Requirements

### 1. OrcaFlex Integration
- Direct vessel type creation from OrcaWave results
- Automatic database format conversion
- Motion validation between programs
- Load case transfer

### 2. CAD Integration
- Import from Rhino/CATIA/SolidWorks
- Mesh generation from NURBS
- Surface repair and healing
- Automatic waterline cutting

### 3. Classification Software
- Export to class society tools
- Compliance report generation
- Rule check automation
- Load case mapping

## Performance Specifications

### Computational Requirements
- Handle meshes up to 50,000 panels
- Process 50 frequencies × 13 headings in < 2 hours
- Full QTF calculation in < 6 hours
- Support distributed computing

### Accuracy Targets
- Mesh convergence within 2%
- Match analytical solutions within 1%
- Benchmark validation within 3%
- Force balance within 0.1%

## Validation and Testing

### Benchmark Cases
1. **Rectangular Barge**: Analytical solution comparison
2. **Wigley Hull**: Published experimental data
3. **FPSO Standard**: Industry benchmark model
4. **Container Ship**: Full-scale validation data

### Convergence Studies
- Mesh density convergence
- Frequency resolution study
- Panel aspect ratio sensitivity
- Truncation distance effects

## Risk Mitigation

### Technical Risks
1. **Large Meshes**: Implement adaptive refinement
2. **Numerical Issues**: Irregular frequency removal
3. **QTF Computation**: Parallel processing for speed
4. **Memory Management**: Stream processing for large datasets

### Process Risks
1. **License Availability**: Queue management system
2. **Data Transfer**: Automated validation checks
3. **User Errors**: Comprehensive input validation
4. **System Integration**: Standardized interfaces

## Future Enhancements

### Phase 2 Features
- Forward speed effects
- Shallow water modifications
- Multi-body interactions
- Hydroelastic coupling

### Advanced Capabilities
- Machine learning for mesh optimization
- Real-time analysis updates
- Cloud computing integration
- Digital twin connectivity

## Success Criteria

1. **Accuracy**: Results within 3% of validation cases
2. **Performance**: 5x speed improvement over manual workflow
3. **Reliability**: 99% successful run completion
4. **Integration**: Seamless OrcaFlex database transfer
5. **Compliance**: Automated regulatory checks passing

## Dependencies

### Software Requirements
- OrcaWave 11.4 or later
- Python 3.10+
- NumPy, SciPy, Pandas
- HDF5 for data storage
- VTK for visualization

### Hardware Requirements
- 32GB RAM minimum (64GB recommended)
- 8+ CPU cores for parallel processing
- 500GB storage for results
- GPU for visualization (optional)

## Regulatory Compliance

### Standards Coverage
- API RP 2SK - Station Keeping
- DNV-OS-E301 - Position Mooring
- ISO 19901-7 - Offshore Structures
- IACS UR W11 - Wave Loads
- Class Society Rules (ABS, DNV, LR)