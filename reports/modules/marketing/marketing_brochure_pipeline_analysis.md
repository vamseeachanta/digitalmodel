# Pipeline Analysis Module
## Advanced Pipeline Design & Integrity Assessment

---

### Overview

The Digital Model Pipeline Analysis Module provides comprehensive pipeline engineering capabilities for subsea and onshore applications. From hydraulic sizing to structural integrity assessment, this module delivers accurate, code-compliant analysis for pipeline design, installation, and operations.

**Key Value Proposition**: Complete pipeline analysis from concept to operations - sizing, capacity, on-bottom stability, spanning, and integrity assessment in a single integrated platform.

---

### Core Capabilities

- **Hydraulic Pipeline Sizing** - Flow capacity, pressure drop, erosional velocity
- **Pipe Capacity Analysis** - Burst, collapse, combined loading per API/DNV
- **On-Bottom Stability** - DNV-RP-F109 concrete weight coating design
- **Spanning Analysis** - Free span assessment and vortex-induced vibration
- **Installation Analysis** - S-lay, J-lay, reel-lay support
- **Integrity Management** - Corrosion, wall thickness monitoring
- **Material Properties** - Comprehensive pipe grade database (API 5L, DNV)

---

### Industry Standards Compliance

#### Pipeline Design Codes
- **DNV-OS-F101** - Submarine pipeline systems (2023 edition)
- **DNV-RP-F109** - On-bottom stability design
- **API RP 1111** - Design, construction, operation of pipeline systems
- **API STD 2RD** - Dynamic risers
- **ASME B31.4/B31.8** - Liquid/gas transmission pipelines

#### Material Standards
- **API 5L** - Line pipe specifications (X42, X52, X65, X70, X80)
- **ASTM** - Material testing and properties
- **ISO 3183** - Petroleum and natural gas industries - steel pipe

---

### Technical Features

#### Hydraulic Pipeline Sizing
- **Multi-Phase Flow**: Oil, gas, water mixtures
- **Pressure Drop**: Darcy-Weisbach, Hazen-Williams equations
- **Flow Regimes**: Laminar, turbulent, transitional
- **Erosional Velocity**: API RP 14E compliance
- **Heat Transfer**: Temperature profile calculation
- **Thermal Hydraulics**: Wax deposition, hydrate formation
- **Pumping Requirements**: Pressure boost, pump spacing

#### Pipe Capacity Analysis (API STD 2RD)
- **Burst Pressure**: Internal pressure capacity
- **Collapse Pressure**: External pressure capacity
- **Combined Loading**: Tension, bending, pressure interaction
- **Safety Factors**: Code-prescribed design margins
- **Corrosion Allowance**: Wall thickness degradation
- **Temperature Effects**: Material de-rating
- **Strain-Based Design**: Plastic strain limits

#### On-Bottom Stability (DNV-RP-F109)
- **Hydrodynamic Forces**: Drag, lift, inertia
- **Soil Resistance**: Friction, passive soil
- **Concrete Weight Coating**: Thickness, density design
- **Current Profiles**: Steady, oscillatory, combined
- **Wave Loading**: Regular and irregular waves
- **Stability Modes**: Lateral, vertical, longitudinal
- **Installation Limits**: Allowable current/wave

#### Free Span Analysis
- **Span Length**: Critical span determination
- **Vortex Shedding**: Frequency and amplitude
- **Stress Assessment**: Combined static and dynamic
- **Fatigue Life**: VIV-induced fatigue damage
- **Remediation**: Grout bag, rock dump design
- **Monitoring**: In-service span detection

---

## Page 2: Benefits & Integration

### Key Benefits

#### 1. **Design Efficiency**
   - **50% faster** than traditional spreadsheet methods
   - **Integrated workflow** - sizing to integrity in one tool
   - **Parametric studies** - rapid sensitivity analysis
   - **Optimization** - minimum weight coating, wall thickness
   - **Reusable templates** - project-specific configurations

#### 2. **Code Compliance**
   - **Multi-code support** - API, DNV, ASME selection
   - **Latest revisions** - DNV-OS-F101 (2023), API STD 2RD
   - **Automated checks** - design criteria verification
   - **Safety margins** - explicit factor tracking
   - **Audit trail** - full calculation documentation

#### 3. **Material Database**
   - **50+ pipe grades** - API 5L, DNV material database
   - **Temperature de-rating** - SMYS reduction curves
   - **Corrosion rates** - Marine, CO2, H2S environments
   - **Coating properties** - FBE, 3LPE, concrete
   - **Custom materials** - user-defined properties

#### 4. **Installation Support**
   - **S-lay analysis** - sagbend, stinger geometry
   - **J-lay analysis** - touchdown point, angle
   - **Reel-lay** - strain limits, pipe qualification
   - **Abandonment/Recovery** - critical load cases
   - **Dynamic positioning** - vessel offset effects

---

### Output Examples

#### 1. Pipeline Sizing Report
```
================================================================================
PIPELINE HYDRAULIC ANALYSIS
================================================================================
DESIGN CONDITIONS:
  Fluid: Crude Oil (API 35)
  Flow Rate: 100,000 bpd
  Temperature: 60°C
  Pressure: 50 bar (inlet)
  Length: 50 km

RECOMMENDED PIPE SIZE:
  Nominal Diameter: 20 inch (508 mm)
  Wall Thickness: 18.3 mm (API 5L X65)
  Schedule: API Std 5L

FLOW CHARACTERISTICS:
  Velocity: 2.1 m/s (ACCEPTABLE - below 5 m/s erosional limit)
  Reynolds Number: 1.2e6 (Turbulent)
  Friction Factor: 0.0145
  Pressure Drop: 12.3 bar over 50 km
  Outlet Pressure: 37.7 bar

ASSESSMENT:
  Erosional Velocity Limit: 5.2 m/s (API RP 14E)
  Safety Margin: 147%
  Status: ✓ DESIGN ACCEPTABLE
================================================================================
```

#### 2. On-Bottom Stability Analysis
```
DNV-RP-F109 Stability Analysis Results:
- Pipeline: 20" OD, 18.3mm WT, API 5L X65
- Water Depth: 1,500 m
- Current: 1.2 m/s (100-year storm)
- Waves: Hs=12m, Tp=14s

Required Concrete Weight Coating:
- Thickness: 75 mm
- Density: 3,040 kg/m³
- Submerged Weight: 1.2 kN/m

Stability Check:
- Lateral Stability: FoS = 1.15 (Required: 1.1) ✓
- Vertical Stability: FoS = 1.22 (Required: 1.1) ✓
- Status: STABLE
```

#### 3. Free Span Assessment
```
Free Span Analysis (DNV-RP-F105):
- Span Length: 35 m
- Soil Stiffness: 50 kN/m/m
- Natural Frequency: 0.82 Hz
- VIV Lock-in Range: 0.65-0.90 Hz

Vortex-Induced Vibration:
- Peak Current: 0.8 m/s
- Vortex Shedding Frequency: 0.75 Hz
- Lock-in Condition: YES
- Fatigue Life: 12 years

Recommendation:
- Install grout bags to reduce span to <25m
- Alternative: Rock dump to raise elevation
```

---

### Quick Start Example

```python
from digitalmodel.modules.pipeline import PipelineSizer, PipeGeometry

# Hydraulic sizing
sizer = PipelineSizer(
    flow_rate=100000,  # bpd
    fluid_type='crude_oil',
    api_gravity=35,
    temperature=60,  # °C
    inlet_pressure=50e5,  # Pa
    length=50000  # m
)

# Get recommended size
result = sizer.recommend_diameter(
    max_velocity=5.0,  # m/s (erosional limit)
    material='API 5L X65'
)

print(f"Recommended: {result['diameter_inch']} inch")
print(f"Wall Thickness: {result['wall_thickness_mm']:.1f} mm")
print(f"Velocity: {result['velocity_ms']:.2f} m/s")
print(f"Pressure Drop: {result['pressure_drop_bar']:.1f} bar")

# On-bottom stability
from digitalmodel.modules.pipeline import StabilityAnalyzer

analyzer = StabilityAnalyzer(
    pipe_od=0.508,  # m
    pipe_wt=0.0183,  # m
    water_depth=1500,  # m
    current=1.2,  # m/s
    wave_hs=12,  # m
    wave_tp=14  # s
)

coating = analyzer.design_concrete_coating(
    target_fos=1.1
)

print(f"Concrete Thickness: {coating['thickness_mm']:.0f} mm")
print(f"Density: {coating['density_kgm3']:.0f} kg/m³")
```

---

### Integration Capabilities

#### Compatible With
- **OrcaFlex** - Dynamic pipeline analysis
- **AutoPIPE** - Stress analysis integration
- **Caesar II** - Piping stress verification
- **Excel** - Design spreadsheet workflows
- **GIS Systems** - Route optimization

#### Input Formats
- **Geometry**: CSV, JSON, YAML
- **Materials**: Built-in database + custom
- **Environmental**: Metocean data import
- **Configuration**: YAML, JSON, Python

#### Output Formats
- **Reports**: HTML, PDF, Text, Markdown
- **Data**: CSV, JSON, Excel, HDF5
- **Plots**: PNG, SVG, PDF, interactive HTML
- **Engineering**: LaTeX, Word documents

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **Analysis Types** | 6 (Sizing, Capacity, Stability, etc.) |
| **Design Codes** | 7 (DNV, API, ASME) |
| **Pipe Grades** | 50+ (API 5L, DNV database) |
| **Flow Models** | 8+ (Single/multi-phase) |
| **Test Coverage** | 120+ dedicated tests |
| **Performance** | <50ms per analysis |

---

### Real-World Applications

- **Subsea Export Pipelines** - Oil and gas transmission
- **Subsea Flowlines** - Inter-field connections
- **Risers** - SCR, flexible, rigid connections
- **Onshore Pipelines** - Transportation systems
- **Water Injection** - Secondary recovery
- **Gas Lift** - Production optimization
- **Umbilicals** - Hydraulic and chemical injection

---

### About Digital Model

**Digital Model** is a comprehensive engineering asset lifecycle management platform featuring:

- **20+ years** offshore/subsea engineering experience
- **200+ SURF engineers'** collective insights validated
- **Production-ready** - active use in major offshore projects
- **704+ Python modules** - comprehensive capability coverage
- **1,971+ test cases** - rigorous quality assurance

**Dedicated to Mark Cerkovnik** - Chief Engineer, mentor, and inspiration.

---

### Contact & Resources

**Technical Support**
- Email: vamsee.achanta@aceengineer.com
- GitHub: https://github.com/vamseeachanta/digitalmodel

**Documentation**
- Module: `/src/digitalmodel/modules/pipeline/`
- Examples: `/examples/pipeline/`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

---

*Digital Model Pipeline Analysis Module - Version 2.0.0*
*Professional Engineering Software for Offshore & Marine Applications*
*© 2025 Digital Model Project - MIT License*
