![Digital Model Logo](../../../assets/logo/digitalmodel_logo.svg)

# OrcaFlex Integration Module
## Automated Dynamic Analysis Workflows for OrcaFlex

---

### Overview

The Digital Model OrcaFlex Integration Module provides comprehensive automation for OrcaFlex dynamic analysis workflows. From automated model generation from YAML configurations to advanced post-processing and results extraction, this module transforms OrcaFlex from a GUI-driven tool into a fully automated analysis platform.

**Key Value Proposition**: Reduce OrcaFlex modeling time by 90% with automated model generation from single-source YAML files, enabling parametric studies, batch processing, and comprehensive post-processing workflows.

---

### Core Capabilities

- **Automated Model Generation** - Create OrcaFlex models from YAML/JSON configurations
- **Post-Processing Framework** - Extract and analyze results systematically
- **Results Dashboard** - Real-time browser-based results visualization
- **Batch Analysis** - Run multiple load cases automatically
- **Time-Series Extraction** - Comprehensive data extraction and filtering
- **Statistical Analysis** - Min/max/mean/std/extreme value statistics
- **RAO Integration** - Seamless hydrodynamic data import
- **Configuration Management** - Version-controlled model definitions

---

### Industry Standards Compliance

#### OrcaFlex Capabilities
- **Version Support**: OrcaFlex 11.x+ compatible
- **Model Types**: Risers, moorings, vessels, pipelines, cables
- **Analysis Types**: Static, dynamic, modal, fatigue
- **Hydrodynamics**: RAO-based vessel motion, wave/current loading

#### Engineering Standards
- **API RP 2SK** - Mooring design and analysis
- **DNV-OS-E301** - Position mooring offshore
- **ISO 19901-7** - Station-keeping systems
- **API RP 17B** - Flexible pipe design

---

### Technical Features

#### Automated Model Generation
- **YAML-Based Configuration**: Human-readable model definitions
- **Parametric Models**: Variable substitution and templates
- **Asset Libraries**: Reusable component definitions
- **Batch Generation**: Create multiple models from configurations
- **Validation**: Pre-flight checks before OrcaFlex execution
- **Version Control**: Git-friendly ASCII configurations

**Example YAML Structure:**
```yaml
vessel:
  name: "FPSO_Turret_Moored"
  length: 320  # m
  beam: 58
  draft: 21
  mass: 180000  # tonnes
  rao_file: "vessel_heave_rao.csv"

mooring_lines:
  - name: "Mooring_Line_1"
    type: "catenary"
    length: 1500  # m
    diameter: 0.152  # m (6 inch chain)
    material: "R4_Studless_Chain"
    fairlead: [45, 0, -15]
    anchor: [1200, 0, -1500]
```

#### Post-Processing Engine
- **Time-Series Extraction**: All object types and variables
- **Statistical Analysis**: Comprehensive statistics per load case
- **Extreme Value Analysis**: Return period calculations
- **Fatigue Analysis**: Rainflow counting integration
- **Spectral Analysis**: FFT and PSD calculations
- **Comparison Tools**: Multi-case comparison and trending

#### Results Dashboard (Real-Time Browser Interface)
- **WebSocket-Based**: Live results streaming during analysis
- **Interactive Plots**: Plotly-based time-series visualization
- **Multi-Variable**: Compare multiple sensors simultaneously
- **Export Capabilities**: CSV, JSON, images
- **Responsive Design**: Desktop and tablet compatible

#### Browser Interface for Results
- **File Search Engine**: Find OrcaFlex files by content
- **Parameter Service**: Extract model parameters
- **Pattern Engine**: Regex-based data extraction
- **Max Force Finder**: Identify critical load cases
- **Configuration Manager**: Central settings management

---

## Page 2: Benefits & Workflow Automation

### Key Benefits

#### 1. **Modeling Efficiency**
   - **90% time reduction** in model creation
   - **Single-source truth** - YAML drives all models
   - **Parametric workflows** - change one file, generate 100 models
   - **Error reduction** - validation before execution
   - **Reproducibility** - version-controlled configurations

#### 2. **Analysis Automation**
   - **Batch processing** - run overnight, results in morning
   - **Load case management** - systematic environmental coverage
   - **Parallel execution** - multi-core utilization
   - **Queue management** - schedule and prioritize runs
   - **Automatic archiving** - organized results storage

#### 3. **Post-Processing Power**
   - **Comprehensive extraction** - all variables, all time steps
   - **Statistical summaries** - instant min/max/mean/std
   - **Extreme value analysis** - 10-year, 100-year return periods
   - **Fatigue integration** - pipe to fatigue module
   - **Custom calculations** - Python-based derived variables

#### 4. **Real-Time Monitoring**
   - **Live results** - watch analysis progress
   - **Early termination** - stop if diverging
   - **Resource monitoring** - CPU, memory usage
   - **Email notifications** - completion alerts
   - **Error handling** - automatic retry logic

---

### Workflow Example

#### Traditional Manual Workflow
```
1. Open OrcaFlex GUI
2. Manually build model (4-6 hours)
3. Set up load cases manually
4. Run analysis, wait
5. Manually extract results
6. Copy to Excel for post-processing
7. Repeat for each parametric variation

Total Time: 2-3 days for 10 load cases
```

#### Automated Digital Model Workflow
```
1. Edit YAML configuration (30 min)
2. Run: python generate_models.py (1 min)
3. Run: python batch_analysis.py (4 hours unattended)
4. View results dashboard (instant)
5. Export comprehensive results (2 min)

Total Active Time: 35 minutes
Total Elapsed: 4.5 hours (mostly unattended)
```

**Time Savings: 90% reduction, 24x faster**

---

### Output Examples

#### 1. Automated Model Generation Log
```
================================================================================
ORCAFLEX MODEL GENERATION
================================================================================
Configuration: turret_moored_fpso.yml
Generating models for 36 load cases...

[✓] Vessel: FPSO_Turret_Moored
    - Mass: 180,000 tonnes
    - RAO file: vessel_heave_rao.csv
    - Validation: PASSED

[✓] Mooring System: 12 lines
    - Line 1-4: R4 Studless Chain (6", 1500m)
    - Line 5-8: R4 Studless Chain (6", 1800m)
    - Line 9-12: R4 Studless Chain (6", 2000m)
    - Validation: PASSED

[✓] Environmental Conditions: 36 cases
    - Wave: Hs=2-16m, Tp=8-18s, Dir=0-330°
    - Current: 0.5-1.5 m/s
    - Validation: PASSED

Generated: 36 OrcaFlex model files (.sim)
Location: models/turret_fpso_2025/
Status: READY FOR ANALYSIS
================================================================================
```

#### 2. Results Dashboard Screenshot
```html
<!DOCTYPE html>
<html>
<head><title>OrcaFlex Results Dashboard</title></head>
<body>
    <h1>Mooring Analysis Results - Live</h1>

    <div class="metrics">
        <div>Analysis Progress: 28/36 cases complete</div>
        <div>Runtime: 3.2 hours</div>
        <div>Est. Completion: 15 minutes</div>
    </div>

    <div id="tension-plot">
        <!-- Interactive Plotly time-series -->
        <!-- Mooring Line 1 Tension vs Time -->
    </div>

    <table class="results">
        <tr><th>Case</th><th>Max Tension</th><th>Mean</th><th>Std</th><th>Status</th></tr>
        <tr><td>Hs12_Tp14_Dir0</td><td>2,345 kN</td><td>1,890</td><td>234</td><td>✓</td></tr>
        <tr><td>Hs14_Tp16_Dir45</td><td>2,678 kN</td><td>2,123</td><td>289</td><td>✓</td></tr>
        <tr><td>Hs16_Tp18_Dir90</td><td>3,012 kN</td><td>2,456</td><td>345</td><td>⏳</td></tr>
    </table>
</body>
</html>
```

#### 3. Statistical Summary Report
```
OrcaFlex Post-Processing Summary
Case: Hs16_Tp18_Dir90

Mooring Line 1 - Top Tension:
  Maximum: 3,012 kN (at t=2,456s)
  Minimum: 1,234 kN (at t=892s)
  Mean: 2,456 kN
  Std Dev: 345 kN
  95th Percentile: 2,987 kN
  Extreme (3-hour): 3,045 kN

Fatigue Assessment:
  Rainflow Cycles: 1,234
  Damage Index: 0.023
  Life: 43 years
```

---

### Quick Start Example

```python
from digitalmodel.modules.orcaflex import ModelGenerator, PostProcessor

# 1. Generate OrcaFlex model from YAML
generator = ModelGenerator('turret_fpso.yml')
model = generator.create_model()
model.SaveData('turret_fpso.sim')

# 2. Run batch analysis
from digitalmodel.modules.orcaflex import BatchAnalyzer

analyzer = BatchAnalyzer('load_cases.yml')
results = analyzer.run_all(parallel=True, cores=8)

# 3. Post-process results
processor = PostProcessor('results/')
stats = processor.extract_statistics(
    object_name='Mooring Line 1',
    variable='Effective Tension',
    statistics=['max', 'mean', 'std', 'extreme']
)

# 4. Generate HTML report
processor.create_dashboard(
    save_path='dashboard.html',
    include_time_series=True,
    interactive=True
)
```

---

### Integration Capabilities

#### Compatible With
- **OrcaFlex 11.x+** - Full API integration
- **Python OrcFxAPI** - Direct object manipulation
- **AQWA/WAMIT** - RAO data import
- **Fatigue Module** - Stress range to damage
- **Excel** - Results export and reporting

#### Input Formats
- **Configuration**: YAML, JSON
- **RAO Data**: CSV, AQWA .LIS, OrcaFlex YAML
- **Environmental**: JSON, CSV (wave, current, wind)
- **Templates**: Reusable YAML snippets

#### Output Formats
- **OrcaFlex Models**: .sim, .dat
- **Results**: .sim (with results), CSV, JSON, HDF5
- **Reports**: HTML (interactive), PDF, Markdown
- **Plots**: PNG, SVG, PDF, interactive HTML

---

### Module Statistics

| Metric | Value |
|--------|-------|
| **Model Types Supported** | 10+ (Vessel, Line, Riser, etc.) |
| **Post-Processing Variables** | 200+ OrcaFlex variables |
| **Batch Capacity** | 1000+ models/day |
| **Dashboard Features** | 15+ interactive widgets |
| **Test Coverage** | 200+ integration tests |
| **Performance** | 50+ models/hour generated |

---

### Real-World Applications

- **FPSO Mooring Design** - Turret and spread mooring
- **Semi-Submersible** - Station-keeping analysis
- **TLP Tendon Analysis** - Fatigue and extreme loads
- **SCR Design** - Touchdown dynamics
- **Flexible Riser** - Lazy wave configuration
- **Umbilical Analysis** - Installation and in-service
- **Tow Analysis** - Offshore structure transport

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
- Module: `/src/digitalmodel/modules/orcaflex/`
- Examples: `/examples/orcaflex/`
- Dashboard: `/specs/modules/orcaflex/results-dashboard/`

**Installation**
```bash
pip install git+https://github.com/vamseeachanta/digitalmodel.git
```

---

*Digital Model OrcaFlex Integration Module - Version 2.0.0*
*Professional Engineering Software for Offshore & Marine Applications*
*© 2025 Digital Model Project - MIT License*
