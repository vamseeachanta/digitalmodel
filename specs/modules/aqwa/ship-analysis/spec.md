# AQWA Comprehensive Ship Analysis Module Specification

## Executive Summary
This specification defines a comprehensive AQWA ship hydrodynamic analysis module that provides end-to-end workflow automation for ship motion analysis, wave loads, stability assessments, and regulatory compliance reporting. The module will handle all AQWA analysis types including frequency domain (AQWA-LINE), time domain (AQWA-DRIFT/NAUT), and stability analysis (AQWA-LIBRIUM).

## Problem Statement
Ship designers and naval architects need to perform complex hydrodynamic analyses for:
- Seakeeping performance evaluation
- Wave load predictions
- Stability assessments
- Mooring and positioning analysis
- Regulatory compliance documentation

Current workflows require manual setup of multiple AQWA runs, data extraction, and report generation. This module automates the entire process with comprehensive input validation and standardized reporting.

## Proposed Solution

### Core Architecture
```
aqwa-ship/
├── input/
│   ├── geometry/      # Hull geometry and mass properties
│   ├── environment/   # Wave, wind, current definitions
│   ├── operations/    # Loading conditions and operations
│   └── analysis/      # Analysis settings and parameters
├── processing/
│   ├── pre-processor/ # Input validation and mesh generation
│   ├── solver/        # AQWA solver management
│   └── post-processor/# Results extraction and analysis
└── output/
    ├── results/       # Raw and processed results
    ├── reports/       # Comprehensive reports
    └── visualization/ # Plots and animations
```

## Technical Specification

### 1. Input Requirements

#### 1.1 Geometry Inputs
```yaml
geometry:
  hull:
    type: "mesh"  # mesh, parametric, or gdf
    source:
      mesh_file: "ship_hull.dat"  # AQWA .DAT file
      units: "meters"
    properties:
      length_overall: 320.0  # LOA in meters
      beam: 58.0            # Beam in meters
      draft: 20.5           # Design draft
      displacement: 297000  # Displacement in tonnes
  
  compartments:
    - name: "Cargo Hold 1"
      boundaries: [x_min, x_max, y_min, y_max, z_min, z_max]
      permeability: 0.98
    - name: "Engine Room"
      boundaries: [...]
      permeability: 0.85
  
  appendages:
    - type: "rudder"
      area: 135.0  # m²
      location: [x, y, z]
    - type: "bilge_keel"
      length: 120.0
      width: 0.8
```

#### 1.2 Mass Properties
```yaml
mass_properties:
  center_of_gravity:
    x: 140.5  # from AP
    y: 0.0    # from CL
    z: 16.8   # above BL
  
  mass_distribution:
    total_mass: 297000  # tonnes
    radii_of_gyration:
      kxx: 23.2  # Roll radius
      kyy: 80.0  # Pitch radius
      kzz: 81.5  # Yaw radius
  
  loading_conditions:
    - name: "Full Load"
      draft: 20.5
      displacement: 297000
      vcg: 16.8
    - name: "Ballast"
      draft: 12.3
      displacement: 165000
      vcg: 14.2
```

#### 1.3 Environmental Conditions
```yaml
environment:
  water_depth: 1000.0  # meters (deep water)
  water_density: 1025.0  # kg/m³
  
  waves:
    spectrum_type: "JONSWAP"  # or PM, ISSC, User-defined
    parameters:
      hs: 6.0     # Significant wave height
      tp: 12.0    # Peak period
      gamma: 3.3  # Peak enhancement factor
    
    directions:
      start: 0.0
      end: 180.0
      increment: 15.0
    
    frequencies:
      type: "automatic"  # or user-defined
      range: [0.01, 2.0]  # rad/s
      count: 50
  
  current:
    profile: "uniform"  # or shear
    speed: 1.5  # m/s
    direction: 45.0  # degrees
  
  wind:
    speed: 25.0  # m/s
    direction: 30.0  # degrees
    spectrum: "NPD"  # or API, constant
```

#### 1.4 Analysis Settings
```yaml
analysis:
  types:
    - frequency_domain:
        enabled: true
        radiation_damping: true
        diffraction: true
        forward_speed: [0, 5, 10, 15]  # knots
    
    - time_domain:
        enabled: true
        duration: 10800  # 3 hours
        time_step: 0.1  # seconds
        ramp_time: 100  # seconds
    
    - stability:
        enabled: true
        intact_stability: true
        damage_stability: true
        criteria: "IMO A.749"
  
  mooring:
    enabled: false  # Enable for station-keeping analysis
    
  drift_forces:
    enabled: true
    newman_approximation: true
```

### 2. Processing Workflow

#### 2.1 Pre-Processing
- Validate all input parameters
- Generate/validate mesh quality
- Check hydrostatic equilibrium
- Create AQWA input files (.DAT, .AQD)

#### 2.2 Solver Execution
- Run AQWA-LINE for frequency domain
- Run AQWA-DRIFT for time domain
- Run AQWA-LIBRIUM for stability
- Monitor convergence and errors

#### 2.3 Post-Processing
- Extract hydrodynamic coefficients
- Calculate RAOs (Response Amplitude Operators)
- Process time series results
- Generate statistical summaries

### 3. Output Requirements

#### 3.1 Hydrodynamic Results
```yaml
outputs:
  hydrodynamics:
    added_mass:
      format: "matrix"  # 6x6 matrix for each frequency
      file: "added_mass.csv"
    
    damping:
      radiation_damping: "damping_radiation.csv"
      viscous_damping: "damping_viscous.csv"
    
    excitation_forces:
      wave_forces: "forces_wave.csv"
      drift_forces: "forces_drift.csv"
```

#### 3.2 Motion Results
```yaml
  motions:
    raos:
      format: "complex"  # magnitude and phase
      degrees_of_freedom: [surge, sway, heave, roll, pitch, yaw]
      file: "raos_all.csv"
    
    time_series:
      format: "time_history"
      sampling_rate: 10  # Hz
      statistics: [mean, std, max, min, significant]
      file: "motions_time.csv"
    
    spectra:
      format: "frequency_spectrum"
      file: "motion_spectra.csv"
```

#### 3.3 Load Results
```yaml
  loads:
    sectional_loads:
      stations: 20  # Number of sections
      components: [shear, moment, torsion]
      file: "sectional_loads.csv"
    
    pressure_distribution:
      format: "contour_data"
      file: "pressure_distribution.csv"
    
    global_loads:
      components: [fx, fy, fz, mx, my, mz]
      statistics: [mean, std, max, design_value]
      file: "global_loads.csv"
```

#### 3.4 Reporting
```yaml
  reports:
    executive_summary:
      format: "markdown"
      content:
        - key_findings
        - compliance_status
        - recommendations
      file: "executive_summary.md"
    
    technical_report:
      format: "pdf"
      sections:
        - input_summary
        - methodology
        - results_analysis
        - validation
        - conclusions
      file: "technical_report.pdf"
    
    compliance_report:
      standards: ["IMO", "Class_Rules", "Flag_State"]
      format: "structured"
      file: "compliance_report.xlsx"
```

### 4. Visualization Outputs
```yaml
visualization:
  plots:
    - rao_polar_plots
    - motion_time_series
    - wave_contours
    - pressure_animations
    - stability_curves
  
  formats:
    static: ["png", "svg", "pdf"]
    interactive: ["html", "plotly"]
    animations: ["mp4", "gif"]
```

## Implementation Details

### Module Structure
```python
# src/digitalmodel/modules/aqwa/ship/
ship/
├── __init__.py
├── input_processor.py      # Input validation and preparation
├── geometry_handler.py     # Hull geometry processing
├── environment_setup.py    # Environmental condition setup
├── solver_manager.py       # AQWA solver interface
├── results_extractor.py    # Results extraction from AQWA files
├── post_processor.py       # Statistical and spectral analysis
├── report_generator.py     # Automated report generation
├── visualization.py        # Plotting and animation
└── cli.py                 # Command-line interface
```

### Key Classes
```python
class AqwaShipAnalysis:
    """Main orchestrator for AQWA ship analysis"""
    
    def __init__(self, config_file: str):
        self.config = self.load_config(config_file)
        self.validator = InputValidator()
        self.pre_processor = PreProcessor()
        self.solver = AqwaSolver()
        self.post_processor = PostProcessor()
        self.reporter = ReportGenerator()
    
    def run_analysis(self):
        """Execute complete analysis workflow"""
        # Validate inputs
        self.validator.validate(self.config)
        
        # Pre-process
        aqwa_input = self.pre_processor.prepare(self.config)
        
        # Run solver
        results = self.solver.execute(aqwa_input)
        
        # Post-process
        processed = self.post_processor.process(results)
        
        # Generate reports
        self.reporter.generate(processed, self.config)
```

## Integration Points

### 1. With Existing AQWA Module
- Leverage existing `aqwa_reader.py` for file parsing
- Use `aqwa_utilities.py` for common functions
- Extend `aqwa_post_process.py` for ship-specific analysis

### 2. With Other Modules
- **OrcaFlex**: For coupled analysis
- **Structural**: For strength assessment
- **Visualization**: For advanced plotting

### 3. External Interfaces
- **ANSYS Workbench**: Direct integration
- **Class Society Tools**: Data exchange
- **Regulatory Databases**: Compliance checking

## Success Criteria
1. **Automation**: 90% reduction in manual setup time
2. **Accuracy**: Results match manual AQWA runs within 0.1%
3. **Compliance**: Automated checks against all major standards
4. **Performance**: Process large models (>100k panels) in <1 hour
5. **Reporting**: Generate publication-ready reports automatically

## Risk Mitigation
1. **License Management**: Implement queuing for AQWA licenses
2. **Large Models**: Implement mesh optimization and parallel processing
3. **Convergence Issues**: Automated parameter adjustment and re-run
4. **Data Validation**: Comprehensive input checking with clear error messages

## Future Enhancements
1. Machine learning for optimal mesh generation
2. Real-time analysis dashboard
3. Cloud-based solver distribution
4. Integration with digital twin platforms
5. Automated optimization workflows

## Dependencies
- ANSYS AQWA (2024 R2 or later)
- Python 3.10+
- NumPy, Pandas, Matplotlib
- PyANSYS libraries
- Report generation: ReportLab, Jinja2

## Compliance Standards
- IMO MSC.267(85) - Intact Stability Code
- IMO A.749(18) - Stability Criteria
- IACS UR S11 - Longitudinal Strength
- ISO 12215 - Small Craft Hull Construction
- Class Society Rules (DNV-GL, ABS, LR)

## Performance Requirements
- Handle models up to 500k panels
- Process 100 load cases in parallel
- Generate reports within 5 minutes
- Support distributed computing