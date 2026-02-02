# Digital Model Examples

## Overview

This directory contains module-based examples for offshore engineering analysis and design using the Digital Model framework.

## 📁 Module Organization

Examples are organized by engineering discipline and asset type:

```
examples/
├── calm_buoy/              # CALM buoy mooring systems
├── fpso/                   # FPSO mooring and analysis
├── mooring/                # General mooring design
├── fatigue/                # Fatigue life assessment
├── hydrodynamics/          # Hydrodynamic analysis
├── stress/                 # Structural stress analysis
├── api_standards/          # API code implementations
├── ocimf/                  # OCIMF guidelines
├── reservoir/              # Reservoir engineering
├── input_files/            # Sample input configurations
├── tutorials/              # Step-by-step tutorials
├── QUICKSTART.md           # Quick start guide
├── QUICK_START_FPSO.md     # FPSO-specific quick start
├── README_fpso_analysis.md # FPSO analysis details
└── WORKFLOW_SUMMARY.md     # General workflow summary
```

---

## 🚀 Quick Start by Module

### CALM Buoy Systems

```bash
# Generate OrcaFlex model from YAML configuration
python scripts/generate_calm_buoy_project.py \
  --config examples/calm_buoy/north_sea_calm_project.yml \
  --validate

# Documentation
cat examples/calm_buoy/README.md
```

**Key Files:**
- `./calm_buoy/north_sea_calm_project.yml` - Complete CALM buoy configuration

### FPSO Mooring Analysis

```bash
# Run FPSO mooring analysis
python examples/fpso/fpso_mooring_analysis.py

# Interactive notebook
jupyter notebook examples/fpso/fpso_mooring_analysis.ipynb
```

**Key Files:**
- `./fpso/fpso_mooring_analysis.py` - FPSO analysis script
- `./fpso/fpso_mooring_analysis.ipynb` - Interactive notebook

### Mooring Line Design

```bash
# Lazy wave configuration example
python examples/mooring/lazy_wave_example.py
```

**Key Files:**
- `./mooring/lazy_wave_example.py` - Lazy wave riser design

### Fatigue Analysis

```bash
# Basic fatigue analysis
python examples/fatigue/fatigue_analysis_examples.py

# Complete fatigue workflow
python examples/fatigue/advanced_examples/complete_fatigue_analysis.py
```

**Key Files:**
- `./fatigue/fatigue_analysis_examples.py` - Basic examples
- `./fatigue/advanced_examples/` - Advanced workflows

### Hydrodynamic Analysis

```bash
# Generate hydrodynamic charts
python examples/hydrodynamics/generate_hydro_charts.py

# Interactive analysis
jupyter notebook examples/hydrodynamics/hydro_coefficients_example.ipynb
```

**Key Files:**
- `./hydrodynamics/generate_hydro_charts.py` - Chart generation
- `./hydrodynamics/hydro_coefficients_example.ipynb` - Interactive analysis

### Stress Analysis

```bash
# Plate capacity examples
python examples/stress/plate_capacity_examples.py

# Von Mises stress
python examples/stress/stress_examples/vm_stress_example.py
```

**Key Files:**
- `./stress/plate_capacity_examples.py` - Plate analysis
- `./stress/stress_examples/` - Detailed stress workflows

### API Standards Implementation

```bash
# API Std 2RD pipeline design
python examples/api_standards/apistd2rd_demo.py
```

**Key Files:**
- `./api_standards/apistd2rd_demo.py` - API 2RD demonstration

### OCIMF Guidelines

```bash
# OCIMF MEG4/SMOG examples
python examples/ocimf/ocimf_demo.py

# Interactive visualization
jupyter notebook examples/ocimf/ocimf_visualization_example.ipynb
```

**Key Files:**
- `./ocimf/ocimf_demo.py` - OCIMF standards
- `./ocimf/ocimf_visualization_example.ipynb` - Visualizations

### Reservoir Engineering

```bash
# Reservoir analysis
python examples/reservoir/reservoir_analysis_examples.py
```

**Key Files:**
- `./reservoir/reservoir_analysis_examples.py` - Production forecasting

---

## 📚 Learning Path

### Beginners

1. **Start with tutorials:**
   ```bash
   jupyter notebook examples/tutorials/01_getting_started.ipynb
   ```

2. **Run simple examples:**
   - `./stress/stress_examples/simple_demo.py`
   - `./mooring/lazy_wave_example.py`

3. **Read documentation:**
   - `QUICKSTART.md` - General quick start
   - `QUICK_START_FPSO.md` - FPSO-specific guide

### Intermediate

1. **Module-specific workflows:**
   - CALM buoy project generation
   - FPSO mooring analysis
   - Fatigue life assessment

2. **Jupyter notebooks:**
   - `./fpso/fpso_mooring_analysis.ipynb`
   - `./hydrodynamics/hydro_coefficients_example.ipynb`
   - `./ocimf/ocimf_visualization_example.ipynb`

3. **Input file customization:**
   - `./input_files/` - Sample YAML configurations

### Advanced

1. **Complete workflows:**
   - `./fatigue/advanced_examples/complete_fatigue_analysis.py`
   - `./stress/stress_examples/nonlinear_example.py`

2. **Multi-fidelity analysis:**
   - CALM buoy preliminary → detailed
   - FPSO sensitivity studies

3. **Custom development:**
   - Extend existing modules
   - Create new analysis workflows

---

## 🔧 Module Dependencies

### Required Packages

```bash
# Install from repository root
pip install -e .

# Or install requirements
pip install -r examples/requirements.txt
```

### Optional Packages

- **Jupyter:** For interactive notebooks
  ```bash
  pip install jupyter
  ```

- **OrcaFlex:** For OrcaFlex model generation
  - Requires OrcaFlex license
  - See `docs/CALM_BUOY_PROJECT_WORKFLOW.md`

---

## 📊 Data Files

### Input Files (`./input_files/`)

Sample YAML configurations for various analyses:
- `api_std_2rd/pipeline_example_basic.yml` - Pipeline design
- `fatigue_analysis/offshore_structure_fatigue.yml` - Fatigue assessment
- `reservoir_analysis/field_example_basic.yml` - Reservoir analysis

### Output Directories

Examples generate outputs in:
- `examples/output/` - General outputs
- `examples/outputs/` - Legacy outputs
- `examples/data/` - Generated data files

---

## 🎯 Common Workflows

### 1. CALM Buoy Project Setup

```bash
# Copy template
cp examples/calm_buoy/north_sea_calm_project.yml projects/my_calm.yml

# Edit configuration
vim projects/my_calm.yml

# Generate OrcaFlex model
python scripts/generate_calm_buoy_project.py \
  --config projects/my_calm.yml \
  --fidelity preliminary \
  --validate

# Review validation reports
firefox projects/CALM_001/reports/validation/*.html
```

### 2. FPSO Mooring Analysis

```bash
# Run analysis
python examples/fpso/fpso_mooring_analysis.py

# Review results in outputs/
ls -lh examples/outputs/fpso_*
```

### 3. Fatigue Life Assessment

```bash
# Complete fatigue workflow
python examples/fatigue/advanced_examples/complete_fatigue_analysis.py \
  --input examples/input_files/fatigue_analysis/offshore_structure_fatigue.yml \
  --output examples/output/fatigue_results
```

### 4. API Code Compliance

```bash
# Pipeline design per API 2RD
python examples/api_standards/apistd2rd_demo.py \
  --input examples/input_files/api_std_2rd/pipeline_example_basic.yml
```

---

## 📖 Documentation Links

### Module-Specific

- **CALM Buoy:** `docs/CALM_BUOY_PROJECT_WORKFLOW.md`
- **FPSO:** `README_fpso_analysis.md`
- **Tutorials:** `./tutorials/README.md`

### General Guides

- **Quick Start:** `QUICKSTART.md`
- **Workflow Summary:** `WORKFLOW_SUMMARY.md`
- **Repository Root:** `../README.md`

### API Reference

See `src/digitalmodel/` for module source code and docstrings.

---

## 🔍 Finding Examples

### By Asset Type

| Asset Type | Module | Key Files |
|------------|--------|-----------|
| CALM Buoy | `./calm_buoy/` | `north_sea_calm_project.yml` |
| FPSO | `./fpso/` | `fpso_mooring_analysis.py` |
| Mooring Lines | `./mooring/` | `lazy_wave_example.py` |

### By Analysis Type

| Analysis | Module | Key Files |
|----------|--------|-----------|
| Fatigue | `./fatigue/` | `fatigue_analysis_examples.py` |
| Stress | `./stress/` | `plate_capacity_examples.py` |
| Hydrodynamics | `./hydrodynamics/` | `generate_hydro_charts.py` |
| Reservoir | `./reservoir/` | `reservoir_analysis_examples.py` |

### By Standard

| Standard | Module | Key Files |
|----------|--------|-----------|
| API | `./api_standards/` | `apistd2rd_demo.py` |
| OCIMF | `./ocimf/` | `ocimf_demo.py` |
| DNV | `./fatigue/`, `./stress/` | Multiple files |

---

## 🤝 Contributing Examples

When adding new examples:

1. **Place in appropriate module directory:**
   ```
   examples/<module_name>/your_example.py
   ```

2. **Update module README:**
   ```
   examples/<module_name>/README.md
   ```

3. **Add input files if needed:**
   ```
   examples/input_files/<module_name>/your_input.yml
   ```

4. **Include docstrings and comments:**
   ```python
   """
   ABOUTME: Brief description of example
   ABOUTME: What it demonstrates
   """
   ```

5. **Test the example:**
   ```bash
   python examples/<module_name>/your_example.py
   ```

---

## ⚠️ Notes

- **OrcaFlex Examples:** Require OrcaFlex installation
- **Large Files:** Some examples generate large output files
- **Runtime:** Complex analyses may take several minutes
- **Python Version:** Requires Python 3.9+

---

## 📞 Support

- **Issues:** https://github.com/your-org/digitalmodel/issues
- **Documentation:** `docs/` directory
- **Module READMEs:** Each `./*/README.md`

---

**Last Updated:** 2025-01-15
**Version:** 2.0 (Module-based organization)
