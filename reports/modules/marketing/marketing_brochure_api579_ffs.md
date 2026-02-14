![Digital Model Logo](../../../assets/logo/digitalmodel_logo.svg)

# API 579 Fitness for Service Module
## Industrial Asset Integrity & Corrosion Management Assessment

---

### Overview

The Digital Model API 579 Fitness for Service Module provides fitness-for-service (FFS) assessments for industrial structures, pressure vessels, piping systems, and storage tanks. Implementing API 579-1/ASME FFS-1 methodology for general and local metal loss, plus BS 7910 fracture mechanics for crack-like flaws, this module delivers remaining life predictions, fitness evaluations, and integrity management support for corroded and damaged equipment.

**Key Value Proposition**: Extend asset life through data-driven integrity assessments while maintaining safety and regulatory compliance -- perform API 579 Part 4/5 FFS evaluations and BS 7910 fracture assessments with validated engineering calculations.

---

### Core Capabilities

| Capability | Status |
|------------|--------|
| **API 579 Part 4 General Metal Loss (GML)** | [Implemented] |
| **API 579 Part 5 Local Metal Loss (LML)** -- Level 1 & 2 | [Implemented] |
| **BS 7910 Fracture Mechanics** -- FAD, critical flaw limits, fatigue crack growth | [Implemented] |
| **ASME B31G Modified** -- Data visualization and plotting | [Partial] |
| **Remaining Strength Factor (RSF)** -- Quantitative integrity assessment | [Implemented] |
| **Corrosion Rate Analysis** -- Historical trending, remaining life prediction | [Implemented] |
| **PipeCapacity** -- Burst pressure per ASME B31.4/B31.8, API RP 1111, API STD 2RD, API TR 5C3, 30 CFR 250, DNV-OS-F101 | [Implemented] |
| **PipeCapacity** -- Collapse and propagation pressure (DNV-OS-F101, API STD 2RD, API TR 5C3) | [Implemented] |
| **PipeCapacity** -- Unrestrained pipe longitudinal stress | [Planned] |
| **PipeCapacity** -- Equivalent stress (Von Mises inversion for B31.4/B31.8) | [Planned] |
| **API 579 Part 6 Pitting Corrosion** | [Planned] |
| **API 579 Part 3 Brittle Fracture** | [Planned] |
| **High-Temperature Creep** | [Planned] |

---

### Industry Standards Compliance

#### FFS Assessment Codes
- **API 579-1/ASME FFS-1** - Fitness-For-Service (Parts 4 & 5 implemented)
- **BS 7910** - Guide to methods for assessing the acceptability of flaws in metallic structures (fracture mechanics implemented)
- **ASME B31.G** - Manual for Determining Remaining Strength of Corroded Pipelines (data visualization only)

#### Pipeline Design Codes [Implemented]
- **ASME B31.4** - Pipeline Transportation Systems for Liquids
- **ASME B31.8** - Gas Transmission and Distribution Piping Systems
- **API RP 1111** - Design, Construction, Operation, and Maintenance of Offshore Hydrocarbon Pipelines
- **API STD 2RD** - Dynamic Risers for Floating Production Systems
- **API TR 5C3** - Technical Report on Equations and Calculations for Casing, Tubing, and Line Pipe
- **30 CFR Part 250** - Oil and Gas and Sulphur Operations in the Outer Continental Shelf
- **DNV-OS-F101 / DNV-ST-F101** - Submarine Pipeline Systems

#### Material & Design Codes
- **API 5L** - Line Pipe Specifications (referenced in pipe capacity calculations)
- **ASTM Standards** - Material properties via YAML configuration input

---

### Technical Features

#### Part 4: Assessment of General Metal Loss [Implemented]
- **Level 1 & 2 Assessment**: Circumferential and longitudinal thickness averaging
- **Assessment Length Determination**: Iterative marching algorithm with ceiling factor control
- **Minimum Thickness**: Code-compliant calculations (ASME B31.4/B31.8)
- **Future Corrosion Allowance**: Remaining life prediction with configurable FCA rates
- **Wall Thickness Acceptability Ratio**: Pass/fail determination per API 579
- **MAWP vs FCA Curves**: Interpolated acceptable future corrosion allowance
- **Contour Visualization**: UT grid heatmaps (circumferential and transposed views)
- **Double-Pass Averaging**: Circumferential wrap-around marching with ceiling limit

#### Part 5: Local Metal Loss Assessment [Implemented]
- **Local Thin Areas (LTA)**: Longitudinal flaw length parameter calculation
- **Remaining Strength Factor**: RSF calculation with Folias factor (Mt) interpolation
- **Level 1 Evaluation**: Rt floor and MAWPr determination
- **Level 2 Evaluation**: Sorted longitudinal CTP with area-averaged assessment
- **Flaw Characterization**: Length, depth, location from UT grid data
- **MAWP Reduction**: Reduced MAWP calculation for flawed components
- **Multi-FCA Analysis**: Parametric FCA sweep with acceptance curves

#### BS 7910 Fracture Mechanics / Crack-Like Flaw Assessment [Implemented]
- **Failure Assessment Diagram (FAD)**: BS 7910:2013 Option 1 FAD curves
- **Critical Flaw Limits**: Unstable fracture limit determination
- **Fatigue Crack Growth**: Paris law with histogram loading (time-marching and approximate methods)
- **Initial Allowable Flaw**: Back-calculated initial flaw size for target service life
- **Multi-Component Analysis**: Flaw growth by component, location, orientation, service life
- **Visualization Suite**: FAD plots, fracture limits, minimum allowable flaws, growth rates

#### ASME B31G Modified [Partial -- data visualization only]
- **Data Import**: Excel-based B31G defect data reading
- **Visualization**: Safe operating length vs. wall thickness plots
- **Full B31G Modified calculation engine planned for future release**

#### Corrosion Rate Analysis [Implemented]
- **Historical Data**: Thickness measurement trending from UT grids
- **Remaining Life Calculation**: Based on measured vs. minimum thickness and corrosion rate
- **FCA Rate Floor**: Configurable minimum future corrosion rate
- **Historical vs. Specified Rate**: Option to use measured historical rate or user-specified rate

---

## Page 2: Benefits & Integration

### Key Benefits

#### 1. **Asset Life Extension**
   - **Data-driven remaining life predictions** through API 579 Part 4/5 methodology
   - **Deferred capital costs** - quantify margin to support continued operation
   - **Operating continuity** - engineering basis for run/repair/replace decisions
   - **Regulatory confidence** - code-compliant RSF and MAWP calculations

#### 2. **Safety & Compliance**
   - **API 579 methodology** for GML and LML assessments (Parts 4 & 5)
   - **BS 7910 fracture mechanics** for crack-like flaw assessment
   - **Multi-level assessment** - Level 1 and Level 2 per API 579
   - **Audit-ready output** - CSV, Excel, and image-based calculation records

#### 3. **Corrosion Management**
   - **Remaining life forecasting** from UT grid data
   - **FCA parametric studies** - MAWP vs. future corrosion allowance curves
   - **Thickness monitoring** - contour plots and trending from inspection data
   - **Corrosion allowance** - future degradation planning with configurable rates

#### 4. **Engineering Efficiency**
   - **Automated grid processing** - circumferential marching with wrap-around
   - **Batch FCA analysis** - sweep multiple future corrosion scenarios
   - **Template-based** - YAML configuration for reusable assessment setups
   - **Multi-format output** - CSV tables, PNG plots, Excel summaries

---

### Output Examples

#### 1. Part 4 General Metal Loss Assessment Report
```
================================================================================
API 579 PART 4 - GENERAL METAL LOSS ASSESSMENT
================================================================================
COMPONENT IDENTIFICATION:
  Equipment ID: V-101 (Crude Desalter)
  Component: Cylindrical Shell
  Design Code: ASME Section VIII Div 1
  Year Installed: 1998
  Material: SA-516 Grade 70

DIMENSIONAL DATA:
  Diameter (D): 3,000 mm
  Original Thickness (tnom): 25.4 mm
  Current Minimum Thickness (tmm): 18.3 mm
  Length: 6,000 mm

OPERATING CONDITIONS:
  Design Pressure: 10.0 bar
  Design Temperature: 150 deg C
  Corrosion Allowance: 3.0 mm
  Inspection Date: 2025-01-15

ASSESSMENT RESULTS:
  Required Thickness (treq): 15.2 mm (ASME VIII-1)
  Remaining Thickness: 18.3 mm
  Remaining Strength Factor (RSF): 1.20
  Future Corrosion Allowance: 3.0 mm
  Predicted Retirement Date: 2032 (7 years remaining)

LEVEL 2 ASSESSMENT:
  Allowable General Metal Loss: 7.1 mm (from tnom)
  Actual Metal Loss: 7.1 mm
  Utilization: 100% (at limit)
  Safety Margin: 3.1 mm (above treq)

RECOMMENDATION:
  Status: ACCEPTABLE FOR CONTINUED SERVICE
  Next Inspection: 2028-01 (3-year interval)
  Action: Monitor - implement corrosion mitigation program

NOTES:
  - Corrosion rate: 0.3 mm/year (based on 27-year service)
  - Remaining life calculated assuming constant corrosion rate
  - Recommend increased inspection frequency after 2030
================================================================================
```

#### 2. Part 5 Local Thin Area (LTA) Assessment
```
API 579 PART 5 - LOCAL METAL LOSS ASSESSMENT

Component: 12" Crude Oil Pipeline
Material: API 5L X52
Design Pressure: 50 bar

LTA CHARACTERIZATION:
  Flaw Type: Longitudinal groove
  Length (s): 450 mm
  Width (c): 180 mm
  Maximum Depth: 6.5 mm (from tnom=12.7mm)
  Remaining Thickness at Flaw: 6.2 mm
  Location: 6 o'clock position

LEVEL 2 ASSESSMENT:
  Remaining Strength Factor (RSF): 0.82
  Allowable RSF: 0.90 (per API 579 Table 5.2)
  Assessment: ACCEPTABLE

CRITICAL FLAW DIMENSIONS:
  Maximum Allowable Depth: 7.8 mm
  Current Depth: 6.5 mm
  Safety Margin: 1.3 mm (20%)

INTERACTION CHECK:
  Nearest LTA Distance: 850 mm
  Minimum Spacing (2*sqrt(Dt)): 618 mm
  Interaction: NO (flaws independent)

RECOMMENDATION:
  - Monitor at next turnaround (2 years)
  - If depth >7.5mm, perform repair
  - Consider cathodic protection upgrade
```

#### 3. Remaining Life Prediction
```
CORROSION RATE ANALYSIS & LIFE PREDICTION

Historical Thickness Measurements:
  1998: 25.4 mm (original)
  2005: 23.8 mm
  2012: 21.5 mm
  2018: 19.7 mm
  2025: 18.3 mm

Statistical Analysis:
  Linear Regression Corrosion Rate: 0.263 mm/year
  Confidence Interval (95%): 0.21-0.32 mm/year
  R-squared Value: 0.987 (excellent fit)

Remaining Life Calculation:
  Current Thickness: 18.3 mm
  Retirement Thickness: 15.2 mm (treq)
  Remaining Margin: 3.1 mm
  Predicted Life: 11.8 years (to 2037)
  Conservative Life (95% CI): 9.7 years (to 2035)

Next Inspection Interval:
  Recommended: 3 years (2028-01)
  Basis: 1/3 of remaining life
  Projected Thickness: 17.5 mm
  Margin at Next Inspection: 2.3 mm
```

---

### Quick Start Example

```python
from digitalmodel.asset_integrity.engine import engine

# Run a complete API 579 GML + LML assessment from YAML config
cfg = engine("path/to/assessment_config.yml")

# Results are stored in cfg['Result']
# - cfg['Result']['Circumference'] contains GML summary per grid
# - LML results include RSF, MAWP, MAWPr at Level 1 and Level 2

# For fracture mechanics (BS 7910):
from digitalmodel.asset_integrity.fracture_mechanics import fracture_mechanics
cfg = fracture_mechanics(cfg)
# Produces FAD curves, critical flaw limits, and fatigue crack growth results

# For pipe capacity (burst/collapse):
from digitalmodel.structural.pipe_capacity.pipe_capacity import PipeCapacity
pc = PipeCapacity()
cfg = pc.router(cfg)
# Evaluates burst pressure per ASME B31.4/B31.8, API RP 1111, etc.
```

---

### Integration Capabilities

#### Input Formats
- **Thickness Data**: Excel (UT grid data with configurable sheet/row/column mapping)
- **Assessment Configuration**: YAML input files defining geometry, materials, operating conditions
- **Histogram Loading**: Excel-based fatigue histogram data for BS 7910 crack growth
- **Simulated Grids**: Programmatic grid generation for parametric studies

#### Output Formats
- **Tabular Results**: CSV files (GML summary, LML results, fracture limits, flaw growth)
- **Visualization**: PNG plots (contour maps, MAWP curves, FAD diagrams, flaw growth charts)
- **Excel Export**: Multi-sheet workbooks with formatted LML results
- **YAML**: Full configuration and result state for reproducibility

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **API 579 Parts Implemented** | 2 (Part 4 GML + Part 5 LML) |
| **Additional Standards** | BS 7910 fracture mechanics (FAD, crack growth, critical flaw limits) |
| **Assessment Levels** | Level 1 and Level 2 (per API 579) |
| **Material Properties** | Configurable via YAML input (material grades, SMYS, SMTS, derating factors) |
| **Pipeline Design Codes** | 7 (ASME B31.4, B31.8, API RP 1111, API STD 2RD, API TR 5C3, 30 CFR 250, DNV-OS-F101) |
| **Pipe Capacity Calculations** | Burst pressure [Implemented], Collapse [Implemented], Propagation [Implemented], Longitudinal stress [Planned], Equivalent stress [Planned] |
| **Test Suite** | Unit and integration tests included (API 579 standalone calculations + YAML-driven end-to-end tests) |
| **Code Compliance** | API 579-1/ASME FFS-1, BS 7910:2013 |

---

### Implementation Status Summary

| Module / Feature | Status | Notes |
|------------------|--------|-------|
| API 579 Part 4 (GML) | [Implemented] | Circumferential marching, assessment length, remaining life |
| API 579 Part 5 (LML) | [Implemented] | Level 1 & 2, RSF, Folias factor, MAWP reduction |
| BS 7910 Fracture Mechanics | [Implemented] | FAD, critical flaw limits, fatigue crack growth, initial flaw sizing |
| ASME B31G Modified | [Partial] | Data visualization only -- full calculation engine planned |
| PipeCapacity -- Burst Pressure | [Implemented] | ASME B31.4/B31.8, API RP 1111, API STD 2RD, 30 CFR 250, DNV F101 |
| PipeCapacity -- Collapse Pressure | [Implemented] | API STD 2RD, API TR 5C3, DNV F101 |
| PipeCapacity -- Propagation Pressure | [Implemented] | API RP 1111, DNV F101 |
| PipeCapacity -- Longitudinal Stress (Unrestrained) | [Planned] | Code present in OtherMethodsTobeIncorporated but not integrated |
| PipeCapacity -- Equivalent Stress (Von Mises) | [Planned] | Code present in OtherMethodsTobeIncorporated but not integrated |
| Material Database | [Planned] | `get_material_properties()` is a stub; materials supplied via YAML config |
| API 579 Part 3 (Brittle Fracture) | [Planned] | Not yet implemented |
| API 579 Part 6 (Pitting Corrosion) | [Planned] | Not yet implemented |
| API 579 Parts 7-14 | [Planned] | Not yet implemented |
| Corrosion Rate Analysis | [Implemented] | Historical/specified rate, FCA floor, remaining life |

---

### Real-World Applications

- **Refineries** - Process vessels, heat exchangers, piping corrosion assessment
- **Petrochemical Plants** - Reactor vessels, distillation columns
- **Offshore Platforms** - Pressure vessels, process equipment, piping
- **Pipelines** - Transmission pipelines, gathering systems (GML/LML and B31G)
- **Storage Tanks** - Crude, product, chemical storage
- **Subsea Systems** - Risers and pipelines (pipe capacity per DNV, API standards)

---

### Key Differentiators

#### 1. **Focused API 579 Implementation**
- Parts 4 (GML) and 5 (LML) with full Level 1 and Level 2 assessment
- BS 7910 fracture mechanics for crack-like flaw assessment
- Iterative circumferential marching algorithm with ceiling factor logic
- Code-compliant RSF, MAWP, and remaining life calculations

#### 2. **Multi-Code Pipe Capacity**
- 7 pipeline design codes implemented for burst pressure
- Collapse and propagation per DNV-OS-F101, API STD 2RD, API TR 5C3
- Unified configuration-driven evaluation across all codes

#### 3. **Engineering Rigor**
- Validated against API 579 example problems
- BS 7910 FAD with fatigue crack growth integration
- YAML-driven reproducible assessments

#### 4. **Practical Efficiency**
- Template-based YAML assessments
- Batch FCA parametric sweeps
- Multi-format output (CSV, Excel, PNG, YAML)

---

### About Digital Model

**Digital Model** is a comprehensive engineering asset lifecycle management platform featuring:

- **20+ years** offshore/subsea engineering experience
- **Production-ready** - active use in offshore engineering projects
- **700+ Python modules** - comprehensive capability coverage
- **Open architecture** - MIT license, GitHub-hosted

**Dedicated to Mark Cerkovnik** - Chief Engineer, mentor, and inspiration.

---

### Contact & Resources

**Technical Support**
- Email: vamsee.achanta@aceengineer.com
- GitHub: https://github.com/vamseeachanta/digitalmodel

**Documentation**
- Module Guide: `/docs/modules/ffs/README.md`
- API Reference: `/docs/api/ffs.md`
- Examples: `/examples/ffs/`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

**Professional Services**
- Custom FFS assessment development
- API 579 and BS 7910 consulting
- Integrity management system integration

---

### Regulatory & Insurance Benefits

#### Regulatory Compliance
- **API 579-1/ASME FFS-1** - Industry-accepted FFS methodology (Parts 4 & 5)
- **BS 7910** - Fracture mechanics assessment per British Standard
- **ASME B31.4/B31.8** - Pipeline design code compliance
- **API RP 1111, API STD 2RD** - Offshore pipeline and riser design codes

#### Quality Assurance
- **Audit trail** - YAML configuration + CSV/Excel result output for reproducibility
- **Version control** - Assessment history tracking via Git
- **Code-compliant calculations** - Validated against API 579 examples

---

*Digital Model API 579 Fitness for Service Module - Version 1.0.0*
*Engineering Software for Asset Integrity Management*
*Copyright 2025 Digital Model Project - MIT License*

**Standards Implemented:**
- API 579-1/ASME FFS-1 (Parts 4 & 5)
- BS 7910:2013 (Fracture Mechanics)
- ASME B31.4, B31.8
- API RP 1111, API STD 2RD, API TR 5C3
- 30 CFR Part 250
- DNV-OS-F101 / DNV-ST-F101
