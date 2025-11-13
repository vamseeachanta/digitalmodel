# Marine Analysis Module
## Advanced Hydrodynamic Analysis for Offshore & Marine Vessels

---

### Overview

The Digital Model Marine Analysis Module provides comprehensive hydrodynamic analysis capabilities for floating vessels, offshore platforms, and marine structures. With unified RAO (Response Amplitude Operator) processing, wave load calculations, and hydrodynamic coefficient management, this module streamlines marine engineering workflows from concept to detailed design.

**Key Value Proposition**: Process multi-source hydrodynamic data 80% faster with unified RAO handling, supporting AQWA, OrcaFlex, WAMIT, and custom formats for complete marine engineering analysis.

---

### Core Capabilities

- **Unified RAO Processing** - AQWA, OrcaFlex, WAMIT, custom format support
- **Hydrodynamic Coefficients** - Added mass, damping, wave exciting forces
- **Wave Load Calculations** - DNV-RP-H103 compliant wave loading
- **Vessel Motion Analysis** - 6-DOF motion prediction and verification
- **Environmental Loading** - Current, wind, wave combined effects
- **Data Extraction** - OCIMF database, mooring properties, RAO data
- **Interactive Visualizations** - Professional HTML reports with Plotly
- **Performance Profiling** - Optimization and bottleneck analysis

---

### Industry Standards Compliance

#### Marine Engineering Standards
- **DNV-RP-H103** - Wave loads and hydrodynamics
- **OCIMF** - Oil Companies International Marine Forum standards
- **ABS Rules** - Floating production installations
- **API RP 2SK** - Floating structures design and analysis

#### Hydrodynamic Analysis Software
- **AQWA** - ANSYS hydrodynamic analysis (.LIS format)
- **OrcaFlex** - Dynamic analysis (YAML format)
- **WAMIT** - Wave analysis (custom formats)
- **Custom Tools** - Flexible parser architecture

---

### Technical Features

#### Unified RAO Reader (v2.0)
- **Multi-Format Support**: AQWA .LIS, OrcaFlex YAML, WAMIT, custom
- **Automatic Detection**: Smart format identification
- **Type Conversion**: Displacement, velocity, acceleration RAOs
- **Validation**: Comprehensive data quality checks
- **Interpolation**: Frequency and heading interpolation
- **Metadata Management**: Complete provenance tracking

#### RAO Data Models
```python
# Unified data structure for all RAO types
class UnifiedRAOData:
    displacement: DisplacementRAO
    velocity: VelocityRAO
    acceleration: AccelerationRAO
    metadata: RAOMetadata
    source_format: SourceFormat
```

- **6-DOF Coverage**: Surge, Sway, Heave, Roll, Pitch, Yaw
- **Complex Amplitude**: Magnitude and phase for all motions
- **Multi-Frequency**: Broad frequency range support
- **Multi-Heading**: 0-360° heading coverage
- **Unit Handling**: Automatic unit conversion

#### Hydrodynamic Coefficients
- **Added Mass**: Frequency-dependent matrices (6x6)
- **Radiation Damping**: Dissipation coefficients
- **Hydrostatic Restoring**: Stiffness matrices
- **Wave Exciting Forces**: First and second-order
- **Drift Forces**: Mean and slowly-varying
- **QTF Matrices**: Quadratic Transfer Functions

#### Wave Load Calculations (DNV-RP-H103)
- **Circular Sections**: Morison equation implementation
- **Rectangular Sections**: Customized drag/inertia coefficients
- **Current Profiles**: Power law, logarithmic, custom
- **Wave Kinematics**: Regular and irregular wave theories
- **Combined Loading**: Wave + current interaction

#### Environmental Data Processing
- **Wave Spectra**: JONSWAP, Pierson-Moskowitz, custom
- **Current Profiles**: Surface, sub-surface, bottom
- **Wind Loading**: API RP 2A wind coefficients
- **Multi-Directional**: Environmental direction spreading

---

## Page 2: Benefits & Integration

### Key Benefits

#### 1. **Workflow Efficiency**
   - **80% time reduction** in RAO data processing
   - **Unified interface** for all hydrodynamic tools
   - **Automated validation** - catch errors early
   - **Batch processing** - multiple vessels/conditions
   - **Reusable workflows** - YAML configuration templates

#### 2. **Data Quality & Accuracy**
   - **Comprehensive validation** - 15+ quality checks
   - **Unit consistency** - automatic conversion and verification
   - **Frequency range** validation
   - **Heading coverage** completeness checks
   - **Physical plausibility** - magnitude and phase verification

#### 3. **Multi-Tool Integration**
   - **AQWA interoperability** - seamless .LIS file import
   - **OrcaFlex compatibility** - YAML-based workflows
   - **WAMIT support** - standard format parsing
   - **Custom formats** - extensible parser framework
   - **API integration** - Python, REST, command-line

#### 4. **Professional Outputs**
   - **Interactive HTML reports** - Plotly-based visualizations
   - **Publication-quality plots** - RAO magnitude and phase
   - **Comparison tools** - multi-source verification
   - **Export capabilities** - CSV, JSON, HDF5, Excel
   - **Documentation** - automated report generation

---

### Output Examples

#### 1. RAO Polar Plots
```
Interactive Plotly visualization showing:
- RAO magnitude vs. frequency (0-2 Hz)
- All 6 DOFs (Surge, Sway, Heave, Roll, Pitch, Yaw)
- Multiple headings (0, 45, 90, 135, 180°)
- Phase information overlay
- Hover tooltips with exact values
```

#### 2. Vessel Motion Comparison Report
```html
<!DOCTYPE html>
<html>
<head>
    <title>RAO Comparison Report - FPSO Vessel</title>
</head>
<body>
    <h1>Multi-Source RAO Validation</h1>
    <section>
        <h2>Heave RAO Comparison</h2>
        <div id="heave-plot"><!-- Interactive Plotly chart --></div>
        <table>
            <tr><th>Source</th><th>Peak Freq (rad/s)</th><th>Peak RAO (m/m)</th></tr>
            <tr><td>AQWA</td><td>0.65</td><td>1.23</td></tr>
            <tr><td>OrcaFlex</td><td>0.66</td><td>1.21</td></tr>
            <tr><td>Difference</td><td>1.5%</td><td>1.6%</td></tr>
        </table>
    </section>
</body>
</html>
```

#### 3. Hydrodynamic Coefficient Matrices
```
Added Mass Matrix at ω = 0.65 rad/s (tonnes, tonne⋅m)
┌                                                    ┐
│  12,345    125     -45      23    -8,456    -234  │  Surge
│    125  12,890     312    8,234     -127      45  │  Sway
│    -45     312  13,456      89    -234     -123  │  Heave
│     23   8,234      89  45,678      234      12  │  Roll
│ -8,456    -127    -234     234  123,456   1,234  │  Pitch
│   -234      45    -123      12    1,234  89,012  │  Yaw
└                                                    ┘
     ↓        ↓        ↓        ↓        ↓        ↓
   Surge    Sway   Heave    Roll    Pitch     Yaw
```

#### 4. Wave Load Distribution
- **Force vs. Depth** plots for cylindrical members
- **Drag vs. Inertia** component breakdown
- **Total loading** envelope across wave periods
- **Critical members** identification

---

### Quick Start Example

```python
from digitalmodel.modules.marine_analysis import UnifiedRAOReader, read_rao_file

# Automatic format detection and parsing
rao_data = read_rao_file('vessel_motion.LIS')  # AQWA format

# Access RAO data
heave_rao = rao_data.displacement.heave
print(f"Frequencies: {rao_data.frequencies}")
print(f"Headings: {rao_data.headings}")

# Plot RAO
from digitalmodel.modules.marine_analysis import RAOPlotter
plotter = RAOPlotter(rao_data)
plotter.plot_all_dofs(
    save_path='rao_summary.html',
    interactive=True
)

# Export to other formats
rao_data.to_orcaflex_yaml('vessel_rao.yml')
rao_data.to_csv('vessel_rao.csv')
```

---

### Integration Capabilities

#### Compatible With
- **AQWA** - Direct .LIS file import and export
- **OrcaFlex** - YAML-based data exchange
- **WAMIT** - Standard format parsing
- **ANSYS** - Hydrodynamic analysis results
- **Excel** - CSV/spreadsheet workflows
- **Python** - NumPy, Pandas, SciPy integration

#### Input Formats
- **AQWA**: .LIS (list files)
- **OrcaFlex**: .YML (YAML configuration)
- **WAMIT**: Custom text formats
- **CSV**: Tabular data
- **JSON/HDF5**: Structured data

#### Output Formats
- **Reports**: HTML (interactive), PDF, Markdown
- **Data**: CSV, JSON, Excel, HDF5, YAML
- **Plots**: PNG, SVG, PDF, interactive HTML
- **Hydrodynamic**: AQWA, OrcaFlex, WAMIT formats

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **Supported Formats** | 4+ (AQWA, OrcaFlex, WAMIT, custom) |
| **RAO Types** | 3 (Displacement, Velocity, Acceleration) |
| **DOFs Analyzed** | 6 (Surge, Sway, Heave, Roll, Pitch, Yaw) |
| **Validation Checks** | 15+ quality assurance tests |
| **Test Coverage** | 100+ dedicated tests |
| **Performance** | Process 10k+ RAO points in <2 sec |

---

### Real-World Applications

- **FPSO Design** - Motion analysis and mooring design
- **Semi-Submersibles** - Platform motion verification
- **Spar Platforms** - Heave and pitch response
- **TLPs** - Tension leg platform dynamics
- **Production Vessels** - Operational limits analysis
- **Offshore Wind** - Floating foundation analysis
- **Ship Design** - Seakeeping analysis

---

### Key Differentiators

#### 1. **Unified Interface**
- Single API for all hydrodynamic tools
- Consistent data models across formats
- Automatic unit handling
- Smart format detection

#### 2. **Production Quality**
- Battle-tested on 200+ projects
- Comprehensive validation suite
- Professional reporting
- Audit trail generation

#### 3. **Extensibility**
- Plugin architecture for new formats
- Custom parsers easily added
- Flexible data models
- API-first design

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
- Module Guide: `/src/digitalmodel/modules/marine_analysis/`
- Examples: `/examples/marine_analysis/`
- Reports: `/docs/reports/rao_qa/`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

---

*Digital Model Marine Analysis Module - Version 2.2.0*
*Professional Engineering Software for Offshore & Marine Applications*
*© 2025 Digital Model Project - MIT License*
