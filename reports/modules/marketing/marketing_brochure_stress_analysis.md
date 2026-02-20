![Digital Model Logo](../../../assets/logo/digitalmodel_logo.svg)

# Stress Analysis Module
## Advanced Structural Stress Assessment for Engineering Assets

---

### Overview

The Digital Model Stress Analysis Module provides comprehensive stress assessment capabilities for pipes, plates, and structural components under combined loading conditions. Featuring Von Mises stress calculations, nonlinear plasticity models, and stress-strain analysis, this module delivers accurate structural integrity assessments for critical offshore and marine applications.

**Key Value Proposition**: Perform multi-axial stress analysis with industry-standard methods in 70% less time while ensuring ASME, API, and DNV code compliance.

---

### Core Capabilities

- **Von Mises Stress Analysis** - Combined loading (pressure, tension, bending)
- **Pipe Stress Calculations** - ASME B31 compliant pipe analysis
- **Nonlinear Plasticity** - Yield criteria and hardening models
- **Stress-Strain Analysis** - Multiple material models (Ramberg-Osgood, Bilinear)
- **Tension-Moment Interaction** - Envelope generation for design limits
- **Material Modeling** - Advanced constitutive models with curve fitting
- **Safety Factor Calculations** - Code-based allowable stress verification

---

### Industry Standards Compliance

#### Engineering Codes Supported
- **ASME B31** - Pipe stress analysis and design
- **ASME BPVC Section VIII** - Pressure vessel design
- **API RP 2A** - Fixed offshore platform design
- **DNV-OS-F101** - Submarine pipeline systems
- **ASTM Standards** - Material properties and testing

#### Analysis Methods
- **Von Mises Yield Criterion** - Ductile material failure
- **Tresca Yield Criterion** - Maximum shear stress theory
- **Classical Plasticity** - Return mapping algorithms
- **Ramberg-Osgood Model** - Nonlinear stress-strain behavior
- **Bilinear Hardening** - Elastic-plastic material response

---

### Technical Features

#### Von Mises Stress Analysis
- **Multi-axial Stress States**: Combined loading calculations
- **Pipe Geometry**: Automatic section properties calculation
- **Pressure Stresses**: Hoop, longitudinal, radial components
- **Bending Effects**: Moment-induced stress distribution
- **Combined Loading**: Interactive stress superposition
- **Safety Verification**: Allowable stress factor application

#### Nonlinear Plasticity Analysis
- **Yield Criteria**: Von Mises, Tresca implementations
- **Hardening Models**: Linear, Isotropic, Kinematic
- **Incremental Solution**: Stress-strain path integration
- **Return Mapping**: Consistent tangent algorithm
- **Plastic Work**: Energy dissipation calculation
- **Failure Prediction**: Strain-based failure criteria

#### Stress-Strain Analysis
- **Material Models**: Linear elastic, Ramberg-Osgood, Bilinear
- **Curve Generation**: Stress-strain relationship modeling
- **Parameter Fitting**: Automated curve fitting from test data
- **Engineering Properties**: Yield strength, ultimate strength, modulus
- **Model Comparison**: Best-fit model selection
- **Validation Tools**: Experimental data comparison

#### Pipe Stress Calculator
- **Geometric Analysis**: OD, WT, section properties
- **Internal Pressure**: Hoop and longitudinal stresses
- **External Pressure**: Collapse and buckling checks
- **Axial Force**: Tension and compression effects
- **Bending Moment**: Combined stress calculation
- **Interaction Envelopes**: Design limit curves

---

## Page 2: Benefits & Integration

### Key Benefits

#### 1. **Efficiency & Accuracy**
   - **70% time savings** compared to manual calculations
   - **Automated code compliance** checking
   - **Vectorized calculations** for batch processing
   - **Built-in safety factors** per industry codes
   - **Instant verification** of design adequacy

#### 2. **Comprehensive Analysis**
   - **Multi-axial stress states** - full 3D stress tensor
   - **Nonlinear behavior** - beyond elastic limit
   - **Material variability** - multiple steel grades supported
   - **Loading combinations** - all relevant load cases
   - **Interaction effects** - tension-moment-pressure

#### 3. **Design Optimization**
   - **Parametric studies** - wall thickness optimization
   - **Material selection** - compare steel grades
   - **Safety margin** quantification
   - **Limit state** identification
   - **Cost optimization** through accurate sizing

#### 4. **Quality Assurance**
   - **Legacy validation** - matches proven calculations
   - **Code compliance** - ASME, API, DNV standards
   - **Engineering accuracy** - rigorous formulations
   - **Comprehensive testing** - 80+ dedicated test cases
   - **Audit trail** - full calculation documentation

---

### Output Examples

#### 1. Von Mises Stress Analysis Report
```
================================================================================
PIPE STRESS ANALYSIS REPORT
================================================================================
GEOMETRY:
  Outer Diameter: 247.65 mm
  Wall Thickness: 34.93 mm
  Inner Diameter: 177.79 mm

MATERIAL PROPERTIES:
  Yield Strength: 552 MPa
  Ultimate Strength: 620 MPa
  Elastic Modulus: 210 GPa
  Poisson's Ratio: 0.30

LOADING CONDITIONS:
  Internal Pressure: 10.0 MPa
  Axial Force: 1.0 MN (tension)
  Bending Moment: 0.1 MN⋅m

STRESS RESULTS:
  Hoop Stress: 35.5 MPa
  Longitudinal Stress: 28.3 MPa
  Radial Stress: -5.0 MPa
  Von Mises Stress: 42.7 MPa

ASSESSMENT:
  Safety Factor: 12.9
  Utilization: 7.7%
  Status: ✓ ACCEPTABLE
================================================================================
```

#### 2. Tension-Moment Interaction Envelope
- **Design curve** - allowable combinations
- **Applied load point** - current state visualization
- **Safety margin** - distance to envelope
- **Optimization path** - recommended design changes

#### 3. Stress-Strain Curve Comparison
- **Experimental data** overlay
- **Ramberg-Osgood fit** with parameters
- **Bilinear approximation**
- **Engineering properties** extraction
- **Goodness of fit** metrics

#### 4. Nonlinear Plasticity Evolution
- **Stress-strain path** - loading history
- **Yield surface** - current state
- **Plastic strain** accumulation
- **Hardening evolution** - material response
- **Energy dissipation** - plastic work

---

### Quick Start Example

```python
from digitalmodel.stress import PipeStressAnalyzer, PipeGeometry, MaterialProperties, LoadingCondition

# Define pipe geometry
geometry = PipeGeometry(
    outer_diameter=0.24765,  # m
    wall_thickness=0.034925  # m
)

# Define material (API 5L X65)
material = MaterialProperties(
    yield_strength=5.52e8,   # Pa
    ultimate_strength=6.20e8,
    elastic_modulus=2.1e11,
    poisson_ratio=0.3
)

# Create analyzer
analyzer = PipeStressAnalyzer(geometry, material)

# Define loading
loading = LoadingCondition(
    internal_pressure=10e6,  # Pa
    axial_force=1e6,         # N
    bending_moment=1e5       # N⋅m
)

# Analyze stress
results = analyzer.calculate_combined_stress(loading)
print(f"Von Mises: {results['von_mises']/1e6:.1f} MPa")
print(f"Safety Factor: {results['safety_factor']:.2f}")
```

---

### Integration Capabilities

#### Compatible With
- **OrcaFlex** - Dynamic analysis results import
- **ANSYS** - FEA verification
- **Excel** - Design spreadsheet integration
- **Python/NumPy** - Scientific computing
- **Pandas** - Data analysis workflows

#### Input Formats
- **Geometry**: Dictionaries, JSON, YAML, CSV
- **Materials**: Built-in steel grades + custom properties
- **Loading**: Time-series or static conditions
- **Configuration**: Python, YAML, JSON

#### Output Formats
- **Reports**: Text, HTML, PDF
- **Data**: CSV, JSON, Excel
- **Plots**: PNG, SVG, PDF, interactive HTML
- **Engineering**: LaTeX documentation

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **Analysis Types** | 3 (VM, Nonlinear, Stress-Strain) |
| **Material Models** | 5+ (Linear, R-O, Bilinear, etc.) |
| **Yield Criteria** | 2 (Von Mises, Tresca) |
| **Test Coverage** | 80+ dedicated tests |
| **Code Standards** | 5 (ASME, API, DNV, ASTM) |
| **Performance** | <10ms per calculation |

---

### Real-World Applications

- **Pipelines** - On-bottom and suspended span analysis
- **Risers** - SCR, TTR stress verification
- **Pressure Vessels** - Subsea equipment design
- **Structural Members** - Platform jacket analysis
- **Flexibles/Umbilicals** - Tensile armor stress
- **Mooring Components** - Chain and wire stress
- **Wellhead Equipment** - Casing and tubing stress

---

### Key Differentiators

#### 1. **Legacy Compatibility**
- Matches proven VMStressCalculations code
- Same geometric formulations
- Compatible parameter sets
- Engineering accuracy maintained

#### 2. **Modern Implementation**
- Vectorized NumPy operations
- Object-oriented architecture
- Comprehensive error handling
- Type hints and documentation

#### 3. **Extensibility**
- Custom material models
- User-defined yield criteria
- Flexible hardening laws
- Integration-ready API

---

### About Digital Model

**Digital Model** is a comprehensive engineering asset lifecycle management platform featuring:

- **20+ years** offshore/subsea engineering experience
- **200+ SURF engineers'** collective insights validated
- **Production-ready** - active use in major offshore projects
- **704+ Python modules** - comprehensive capability coverage
- **1,971+ test cases** - rigorous quality assurance
- **Open architecture** - MIT license, GitHub-hosted

**Dedicated to Mark Cerkovnik** - Chief Engineer, mentor, and inspiration.

---

### Contact & Resources

**Technical Support**
- Email: vamsee.achanta@aceengineer.com
- GitHub: https://github.com/vamseeachanta/digitalmodel

**Documentation**
- Module Guide: `/docs/domains/stress/README.md`
- API Reference: `/docs/api/stress.md`
- Examples: `/examples/stress/`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

---

*Digital Model Stress Analysis Module - Version 2.0.0*
*Professional Engineering Software for Offshore & Marine Applications*
*© 2025 Digital Model Project - MIT License*
