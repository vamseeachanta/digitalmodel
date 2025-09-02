# OrcaFlex Riser Comprehensive Analysis Module Specification

## Overview

This specification defines a comprehensive OrcaFlex riser analysis module that provides standardized workflows for all riser types (drilling risers, SCRs, lazy-wave risers, TTRs, hybrid risers) with complete input definition, analysis execution, and advanced reporting capabilities.

## Purpose

Provide a unified, production-ready module for comprehensive riser analysis in OrcaFlex that:
- Standardizes riser modeling workflows across all riser types
- Automates complex analysis sequences (static, dynamic, fatigue, VIV)
- Generates engineering-grade reports with complete documentation
- Ensures compliance with industry standards (API, DNV, ISO)
- Integrates with existing OrcaFlex ecosystem and universal runner

## System Architecture

### Module Structure
```
src/digitalmodel/modules/orcaflex/riser/
├── __init__.py
├── __main__.py                    # CLI entry point
├── core/
│   ├── riser_model.py             # Base riser model class
│   ├── riser_types.py             # Specific riser type implementations
│   ├── configuration.py           # Configuration management
│   └── validation.py              # Input validation and checks
├── analysis/
│   ├── static_analysis.py         # Static equilibrium analysis
│   ├── dynamic_analysis.py        # Time-domain dynamic analysis
│   ├── fatigue_analysis.py        # Fatigue damage calculations
│   ├── viv_analysis.py            # VIV assessment integration
│   └── extreme_analysis.py        # Extreme response analysis
├── components/
│   ├── stack_up.py                # Riser stack-up builder
│   ├── joints.py                  # Joint modeling
│   ├── tensioners.py              # Tensioner systems
│   ├── buoyancy.py                # Buoyancy modules
│   └── connectors.py              # Connectors and interfaces
├── environment/
│   ├── metocean.py                # Metocean data processing
│   ├── soil.py                    # Soil interaction models
│   └── vessel.py                  # Vessel motion RAOs
├── reporting/
│   ├── report_generator.py        # Main report orchestrator
│   ├── templates/                 # Report templates
│   ├── visualizations.py          # Plot generation
│   └── exporters.py               # Data export utilities
├── utilities/
│   ├── unit_conversion.py         # Unit system management
│   ├── material_library.py        # Material properties database
│   └── standards.py               # Industry standards library
└── configs/
    ├── templates/                  # Configuration templates
    └── defaults.yml                # Default settings

```

## Detailed Input Specifications

### 1. Riser Configuration Inputs

#### 1.1 Riser Type Definition
```yaml
riser:
  type: "drilling" | "scr" | "lazy_wave" | "ttr" | "hybrid" | "flexible"
  subtype: "top_tensioned" | "free_hanging" | "compliant" 
  configuration:
    total_length: 2000.0  # meters
    water_depth: 1500.0   # meters
    hang_off_angle: 0.0   # degrees from vertical
    azimuth: 0.0          # degrees from North
```

#### 1.2 Riser Stack-Up Definition
```yaml
stack_up:
  source: "excel" | "yaml" | "database"
  excel_config:
    file_path: "riser_stackup.xlsx"
    sheets:
      components: "Stack-Up"
      joints: "Joints"
      materials: "Materials"
  components:
    - section_id: "BOP_CONNECTOR"
      length: 5.0
      outer_diameter: 0.5334  # 21 inch
      wall_thickness: 0.0254  # 1 inch
      material: "API_5L_X65"
      dry_weight: 500.0       # kg/m
      buoyancy_coverage: 0.0
    - section_id: "RISER_JOINT"
      count: 150
      unit_length: 12.192     # 40 ft joints
      outer_diameter: 0.5334
      wall_thickness: 0.0254
      material: "API_5L_X65"
      dry_weight: 350.0
      buoyancy_coverage: 0.85
      buoyancy_type: "syntactic_foam"
```

#### 1.3 Material Properties
```yaml
materials:
  API_5L_X65:
    youngs_modulus: 207.0e9    # Pa
    poisson_ratio: 0.3
    yield_strength: 448.0e6     # Pa
    tensile_strength: 530.0e6   # Pa
    density: 7850.0             # kg/m³
    fatigue_curve: "DNV_C1"
  syntactic_foam:
    density: 640.0              # kg/m³
    crush_pressure: 10.0e6      # Pa
    water_absorption: 0.02      # fraction
```

#### 1.4 Connection Systems
```yaml
connections:
  top_connection:
    type: "tensioner_ring" | "flex_joint" | "stress_joint"
    tensioner_system:
      type: "direct" | "hydro_pneumatic" | "ram"
      tensioners_count: 6
      tension_per_tensioner: 150000.0  # N
      stroke: 15.0                     # meters
      stiffness: 50000.0               # N/m
  bottom_connection:
    type: "ball_joint" | "flex_joint" | "fixed"
    ball_joint:
      max_angle: 10.0                  # degrees
      stiffness: 1.0e8                 # N.m/rad
```

### 2. Environmental Inputs

#### 2.1 Metocean Conditions
```yaml
environment:
  water_depth: 1500.0
  analysis_cases:
    - case_id: "100yr_storm"
      description: "100-year extreme storm"
      wave:
        spectrum_type: "JONSWAP"
        Hs: 12.5              # significant wave height (m)
        Tp: 14.0              # peak period (s)
        gamma: 2.5            # peak enhancement factor
        direction: 0.0        # degrees
      current:
        profile_type: "power_law"
        surface_velocity: 1.5   # m/s
        exponent: 0.143
        direction: 45.0          # degrees
      wind:
        velocity_1hr: 35.0       # m/s at 10m
        spectrum: "API"
        direction: 0.0
    - case_id: "operating"
      wave:
        spectrum_type: "PM"
        Hs: 4.5
        Tp: 9.0
      current:
        profile_type: "linear"
        surface_velocity: 0.8
        bottom_velocity: 0.2
```

#### 2.2 Vessel Motion
```yaml
vessel:
  type: "semi_submersible" | "drillship" | "FPSO" | "TLP"
  displacement: 50000.0        # tonnes
  draft: 20.0                  # meters
  RAO_source: "file" | "calculated"
  RAO_file: "vessel_RAOs.yml"
  offset_conditions:
    - case: "near"
      surge: 10.0              # meters
      sway: 5.0
      heave: 0.0
    - case: "far"
      surge: 50.0              # 3% water depth
      sway: 25.0
      heave: 0.0
```

#### 2.3 Soil Properties
```yaml
soil:
  model: "linear" | "non_linear" | "API_clay" | "API_sand"
  properties:
    shear_strength_gradient: 3.0    # kPa/m
    mudline_shear_strength: 5.0     # kPa
    friction_coefficient: 0.3
    lateral_resistance: "P-y curves"
    axial_resistance: "T-z curves"
```

### 3. Analysis Control Inputs

#### 3.1 Analysis Types
```yaml
analysis:
  static:
    enabled: true
    target_top_tension: 1500000.0    # N
    iterations: 100
    tolerance: 0.001
  dynamic:
    enabled: true
    simulation_time: 10800.0          # 3 hours
    time_step: 0.1                   # seconds
    ramp_time: 600.0                 # 10 minutes
    output_interval: 1.0
  fatigue:
    enabled: true
    method: "rainflow" | "spectral"
    design_life: 20.0                 # years
    safety_factor: 10.0
    sn_curve: "DNV_C1"
    wave_scatter: "wave_scatter.xlsx"
  viv:
    enabled: true
    method: "SHEAR7" | "VIVA" | "empirical"
    current_profiles: "current_profiles.yml"
    strouhal_number: 0.2
    damping_ratio: 0.01
  extreme:
    enabled: true
    return_periods: [1, 10, 100, 1000]
    method: "Gumbel" | "Weibull" | "GEV"
```

#### 3.2 Output Control
```yaml
outputs:
  time_series:
    enabled: true
    variables:
      - "Effective Tension"
      - "Bending Moment"
      - "Von Mises Stress"
      - "Curvature"
      - "Contact Force"
    locations:
      - "Top connector"
      - "Stress joint"
      - "Buoyancy top"
      - "Buoyancy bottom"
      - "Touch down point"
      - "Bottom connector"
  statistics:
    enabled: true
    percentiles: [0, 5, 50, 95, 100]
    include_fft: true
  visualization:
    enabled: true
    views: ["elevation", "plan", "3D"]
    animation: true
    snapshot_times: [0, 1800, 3600, 5400, 7200]
```

### 4. Reporting Specifications

#### 4.1 Report Configuration
```yaml
reporting:
  format: "excel" | "word" | "pdf" | "html"
  template: "standard" | "detailed" | "executive"
  sections:
    - executive_summary
    - input_summary
    - static_analysis_results
    - dynamic_analysis_results
    - fatigue_assessment
    - viv_assessment
    - extreme_value_analysis
    - code_compliance_check
    - conclusions_recommendations
  
  visualizations:
    - configuration_schematic
    - tension_profiles
    - stress_distributions
    - fatigue_damage_maps
    - viv_response_plots
    - extreme_value_plots
    
  data_tables:
    - maximum_responses
    - fatigue_life_summary
    - code_check_results
    - sensitivity_analysis
```

## Required Outputs

### 1. Analysis Results

#### 1.1 Static Analysis Outputs
- Riser configuration profile (X, Y, Z coordinates)
- Effective tension distribution
- True wall tension distribution
- Bending moment distribution
- Stress distribution (von Mises, axial, hoop)
- Contact forces at touchdown
- Hang-off loads

#### 1.2 Dynamic Analysis Outputs
- Time series data for all specified variables
- Statistical summaries (min, max, mean, std, percentiles)
- Response spectra (FFT analysis)
- Stress range histograms
- Motion envelopes

#### 1.3 Fatigue Analysis Outputs
- Damage distribution along riser
- Fatigue life at critical locations
- S-N curve utilization
- Damage contribution by sea state
- Safety factor assessment

#### 1.4 VIV Analysis Outputs
- VIV amplitudes vs depth
- Fatigue damage from VIV
- Lock-in regions identification
- Suppression device effectiveness

#### 1.5 Extreme Analysis Outputs
- Extreme value distributions
- Return period estimates
- Confidence intervals
- Design values recommendation

### 2. Engineering Reports

#### 2.1 Executive Summary Report
```
RISER ANALYSIS EXECUTIVE SUMMARY
================================
Project: [Project Name]
Riser Type: [Type]
Water Depth: [Depth] m
Analysis Date: [Date]

KEY FINDINGS:
- Maximum Tension: [Value] kN (Location: [Location])
- Maximum Stress: [Value] MPa ([Utilization]% of yield)
- Minimum Fatigue Life: [Years] years (Location: [Location])
- VIV Risk: [Low/Medium/High]
- Code Compliance: [PASS/FAIL]

RECOMMENDATIONS:
1. [Recommendation 1]
2. [Recommendation 2]
```

#### 2.2 Detailed Technical Report Structure
1. **Cover Page and Document Control**
2. **Executive Summary** (2-3 pages)
3. **Introduction and Scope**
4. **Design Basis and Standards**
5. **Riser System Description**
6. **Environmental Conditions**
7. **Analysis Methodology**
8. **Results and Discussion**
   - Static Analysis Results
   - Dynamic Response Analysis
   - Fatigue Life Assessment
   - VIV Assessment
   - Extreme Response Analysis
9. **Code Compliance Verification**
10. **Conclusions and Recommendations**
11. **References**
12. **Appendices**
    - A: Input Data Tables
    - B: Detailed Results
    - C: Calculation Notes

### 3. Data Exports

#### 3.1 Standard CSV Exports
- `riser_configuration.csv` - Static configuration results
- `time_series_[variable]_[location].csv` - Dynamic time series
- `statistics_summary.csv` - Statistical summaries
- `fatigue_results.csv` - Fatigue damage and life
- `extreme_values.csv` - Extreme response estimates

#### 3.2 Visualization Outputs
- `riser_configuration.png/jpg` - Configuration plots
- `tension_profile.png` - Tension distribution
- `stress_contour.png` - Stress distribution
- `fatigue_damage_map.png` - Damage distribution
- `viv_response.png` - VIV amplitude plots
- `animation.mp4` - Dynamic response animation (optional)

## Integration Requirements

### 1. OrcaFlex API Integration
- Full compatibility with OrcFxAPI 11.x
- Support for batch mode execution
- License management and queuing
- Error handling and recovery

### 2. Universal Runner Integration
- Compatible with pattern-based discovery
- Support for parallel execution
- Progress reporting via JSON
- Integration with existing batch configs

### 3. Standards Compliance
- API RP 2RD - Design of Risers for FPSs and TLPs
- API STD 2RD - Dynamic Risers for Floating Production Systems
- DNV-ST-F201 - Dynamic Risers
- ISO 13624-1 - Marine drilling riser equipment
- ISO 13628-7 - Completion/workover riser systems

### 4. Data Format Standards
- Input: YAML, Excel, CSV, JSON
- Output: Excel, CSV, JSON, HDF5
- Reports: Word, PDF, HTML
- Plots: PNG, JPG, SVG, PDF

## Performance Requirements

### 1. Computational Performance
- Static analysis: < 30 seconds per case
- Dynamic analysis: < 2 hours for 3-hour simulation
- Fatigue analysis: < 5 minutes per load case
- Batch processing: 8 parallel workers optimal
- Memory usage: < 8 GB per analysis

### 2. Scalability
- Support for 100+ load cases
- Batch processing of multiple risers
- Parametric study capabilities
- Cloud deployment ready

## Quality Assurance

### 1. Validation Requirements
- Input validation against schema
- Physical constraint checking
- Range validation for all parameters
- Cross-validation of related inputs

### 2. Testing Requirements
- Unit tests for all components
- Integration tests with OrcaFlex
- Regression tests against validated cases
- Performance benchmarking

### 3. Documentation Requirements
- API documentation (Sphinx)
- User manual with examples
- Validation report against known solutions
- Best practices guide

## CLI Interface Specification

```bash
# Basic execution
python -m digitalmodel.modules.orcaflex.riser --config riser_config.yml

# Pattern-based batch processing
python -m digitalmodel.modules.orcaflex.riser \
  --pattern "*.yml" \
  --input-directory ./configs \
  --output-directory ./results \
  --parallel 8

# Specific analysis types
python -m digitalmodel.modules.orcaflex.riser \
  --config riser.yml \
  --analysis-type fatigue \
  --report-format excel

# With visualization
python -m digitalmodel.modules.orcaflex.riser \
  --config riser.yml \
  --visualize \
  --animation \
  --output-directory ./reports
```

## Agent Delegation Strategy

### Primary Agent: OrcaFlex Agent
- Owns overall riser analysis workflow
- Coordinates with sub-agents
- Ensures standards compliance

### Supporting Agents:
- **Signal Analysis Agent**: Fatigue and rainflow counting
- **Testing Agent**: Parallel test execution
- **Documentation Agent**: Report generation
- **Visualization Agent**: Plot and animation creation

## Success Criteria

1. **Functional Success**
   - All riser types supported
   - Complete analysis chain functional
   - Reports generated automatically
   - Standards compliance verified

2. **Performance Success**
   - Meets performance targets
   - Efficient parallel processing
   - Optimized memory usage

3. **Quality Success**
   - Comprehensive test coverage (>80%)
   - Validated against known solutions
   - Clear documentation
   - User-friendly CLI

## Risk Mitigation

1. **Technical Risks**
   - OrcaFlex license availability → Mock mode for testing
   - Large file sizes → Streaming and chunked processing
   - Complex configurations → Schema validation

2. **Integration Risks**
   - API version compatibility → Version detection
   - Data format variations → Adapters and converters
   - Legacy system support → Backward compatibility layer

## Next Steps

1. Review and approve specification
2. Set up development environment
3. Implement core riser model
4. Develop analysis engines
5. Create reporting system
6. Integration testing
7. Documentation and examples
8. User acceptance testing
9. Production deployment

---

*Specification Version: 1.0*
*Date: 2025-09-02*
*Status: Draft for Review*