# Digital Model - Engineering Asset Lifecycle Management

[![Python](https://img.shields.io/badge/Python-3.8%2B-blue.svg)](https://www.python.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![Commits](https://img.shields.io/badge/Commits-305%2B-orange.svg)](https://github.com/vamseeachanta/digitalmodel)

Sustainable digital models for engineering assets built with sustainable engineering processes and solutions. Single source of truth for offshore, subsea, and marine engineering analysis.

**Dedicated to Mark Cerkovnik** - Chief Engineer, mentor, and inspiration.

---

## 🎯 Vision

Utilize a single source of ASCII inputs (promoting single source of truth) to generate equivalent analytical models encompassing the complete lifecycle operations of engineering assets:

- **Analytical Calculations** - Fatigue, stress, hydrodynamics, structural capacity
- **Computational Analysis** - FEA models, time-domain simulations, frequency analysis
- **3D CAD Models** - Automated geometry generation
- **3D Animations** - Visual representations of designs
- **Drawing Automation** - Engineering documentation

<img src="docs/digital_model_architecure.svg" width="800" alt="Digital Model Architecture"/>

---

## 📊 Repository Overview

| Category | Count | Description |
|----------|-------|-------------|
| **Python Modules** | 704+ | Core engineering libraries |
| **Test Files** | 1,971+ | Comprehensive test coverage |
| **Examples** | 12+ | Working demonstrations |
| **Documentation** | 352+ | Technical guides and references |
| **Commits** | 305+ | Active development history |

---

## 🚀 Key Features

### ✅ Single Source of Truth
- ASCII-based input files (.yml, .json, .csv)
- Generate FE models, analytical calculations, 3D CAD, animations, and drawings from one source
- Version-controlled engineering data

### ✅ Modular Architecture
- Import engineering assets using configuration files
- Reusable components across projects
- Standardized naming conventions for cross-team collaboration

### ✅ Analytical QA
- Automated verification of computational results
- Cross-validation between analytical and numerical methods
- Weight checks, load verification, structural capacity validation

### ✅ Industry-Proven Workflows
- 20+ years of hands-on engineering experience
- Validated by 200+ SURF engineers
- Production-ready for offshore and marine projects

---

## 🔬 Core Capabilities

### Fatigue Analysis Module ⭐ NEW
**Comprehensive S-N Curve Database & Analysis Tools**

- **221 S-N curves** from **17 international standards**:
  - DNV (81 curves), BS 7608 (79), ABS (24), BP (25), Norsok (15), Bureau Veritas (14), API (2), Titanium (4)
- **Joint types**: Plated welded, Tubular, Tubular nodal
- **Environments**: In Air, Seawater with CP, Free Corrosion
- **Advanced plotting**: Log-log, linear-log, SCF application, fatigue limit handling
- **Data formats**: CSV, JSON for easy integration

```python
from digitalmodel.fatigue.sn_curve_plotter import SNCurvePlotter

# Initialize plotter
plotter = SNCurvePlotter()

# Plot S-N curves with stress concentration factor
plotter.plot_curves(
    lookup_indices=[64, 175, 183],
    scf=2.0,
    include_fatigue_limit=True,
    plot_type='log-log',
    save_path='sn_curves.png'
)
```

**Quick Access:**
- Data: `/data/fatigue/`
- Examples: `/examples/fatigue/`
- Documentation: `/data/fatigue/README.md`

---

### Structural Analysis

#### Stress Analysis
- Von Mises stress calculations
- Nonlinear stress-strain analysis
- Multi-axial stress states
- Stress concentration factors

#### Plate Capacity
- Buckling analysis (DNV, API standards)
- Ultimate strength calculations
- Multi-plate structural systems
- Stiffened panel analysis

---

### Time-Series Analysis

#### Signal Processing
- Fast Fourier Transform (FFT)
- Inverse FFT (iFFT)
- Peak energy frequency identification
- Signal integration and differentiation
- Statistical analysis

#### Hydrodynamics
- Wave load calculations (DNV-RP-H103)
- Drag and inertia coefficients
- Circular and rectangular section hydrodynamics
- Current profile generation

---

### Engineering Asset Modeling

#### Risers
- **Catenary Risers (SCR)** - Simple Catenary Riser analysis
- **Lazy Wave Risers (SLWR)** - Buoyancy-supported configurations
- **Stack-up calculations** - Material properties, weights, tensions

#### Mooring Systems
- **SALM** - Single Anchor Line Mooring
- **Buoy systems** - Hydrodynamic analysis
- **Anchor design** - Load capacity verification

#### Subsea Infrastructure
- **Pipelines** - On-bottom stability, span analysis
- **Flexibles/Umbilicals** - Configuration design
- **Rigid Jumpers** - Structural analysis

#### Vessels
- Light Service Vessels
- Intervention Vessels
- Ship design calculations

---

### Software Integration

#### OrcaFlex
- Automated model generation from YAML
- Post-processing and results extraction
- Time-series analysis integration

#### WAMIT
- Hydrodynamic coefficient processing
- RAO (Response Amplitude Operator) analysis

#### AQWA
- Model setup automation
- Results post-processing

#### BEMRosetta
- Boundary element method integration

---

## 📦 Installation

### Quick Start (pip)
```bash
# Install from GitHub
pip install git+https://github.com/vamseeachanta/digitalmodel.git

# Or clone for development
git clone https://github.com/vamseeachanta/digitalmodel.git
cd digitalmodel
pip install -e .
```

### Using Conda Environment
```bash
# Create environment from YAML
conda env create -f dev_tools/environment.yml
conda activate digitalmodel

# Or install package in existing environment
pip install -e .
```

---

## 🎯 Quick Examples

### 1. Fatigue S-N Curve Analysis
```python
from digitalmodel.fatigue.sn_curve_plotter import SNCurvePlotter

plotter = SNCurvePlotter()

# List available curves
dnv_curves = plotter.list_curves(
    curve_type_filter='DNV',
    environment_filter='Air'
)
print(dnv_curves)

# Compare multiple standards
plotter.create_comparison_plot(
    reference_index=64,  # API curve
    comparison_indices=[175, 183, 190],  # BS, BV, ABS
    scf=1.5,
    save_path='standard_comparison.png'
)
```

### 2. Catenary Riser Analysis
```python
from digitalmodel.tests import test_catenary_riser

# Run analysis with YAML configuration
test_catenary_riser.main()
```

### 3. Stress Analysis
```python
from digitalmodel.stress import StressAnalyzer

analyzer = StressAnalyzer()
vm_stress = analyzer.von_mises_stress(
    sigma_x=100,  # MPa
    sigma_y=80,
    sigma_z=60,
    tau_xy=20,
    tau_yz=15,
    tau_xz=10
)
print(f"Von Mises Stress: {vm_stress:.2f} MPa")
```

### 4. Plate Buckling Analysis
```python
from digitalmodel.analysis.plate_capacity import PlateCapacity

plate = PlateCapacity(
    length=2.0,      # meters
    width=1.0,
    thickness=0.02,
    material='API 5L X65'
)

capacity = plate.calculate_buckling_capacity()
print(f"Buckling Capacity: {capacity:.2f} kN")
```

---

## 📚 Documentation

### Getting Started
- [Installation Guide](docs/installation.md)
- [Quick Start Tutorial](docs/quickstart.md)
- [Configuration Files](docs/configuration.md)

### Module Documentation
- [Fatigue Analysis](data/fatigue/README.md) ⭐
- [Stress Analysis](docs/modules/stress/README.md)
- [OrcaFlex Integration](docs/modules/orcaflex/README.md)
- [Marine Engineering](docs/modules/marine-engineering/README.md)

### API Reference
- [Core API](docs/api/core.md)
- [Analysis Modules](docs/api/analysis.md)
- [Utilities](docs/api/utilities.md)

---

## 🗂️ Repository Structure

```
digitalmodel/
├── data/                    # Engineering data and databases
│   └── fatigue/            # S-N curve database ⭐
│       ├── fatigue_curves_structured.csv
│       ├── fatigue_curves_references.csv
│       └── README.md
├── src/digitalmodel/        # Core library
│   ├── fatigue/            # Fatigue analysis
│   ├── stress/             # Stress analysis
│   ├── analysis/           # Structural analysis
│   ├── reservoir/          # Reservoir engineering
│   ├── common/             # Shared utilities
│   └── modules/            # Specialized modules
├── examples/                # Working examples
│   ├── fatigue/            # Fatigue examples ⭐
│   ├── stress/             # Stress examples
│   └── *.py                # Various demos
├── docs/                    # Documentation
│   └── modules/            # Module-specific docs
├── tests/                   # Test suite
└── tools/                   # Development tools
```

---

## 🔧 Development Tools

### Repository Hygiene
```bash
# Check for root directory violations
python tools/cleanup-root.py --dry-run

# Automatic cleanup
python tools/cleanup-root.py

# Enable pre-commit hook
git config core.hooksPath .githooks
```

### Testing
```bash
# Run all tests
pytest

# Run specific module tests
pytest tests/test_fatigue_analysis.py

# Run with coverage
pytest --cov=digitalmodel --cov-report=html
```

### Code Quality
```bash
# Format code
black src/digitalmodel

# Lint
flake8 src/digitalmodel

# Type checking
mypy src/digitalmodel
```

---

## 🌟 Recent Additions

### September 2025
- ✨ **Fatigue S-N Curve Database** - 221 curves from 17 international standards
- ✨ **Advanced Plotting Module** - Log-log, linear-log, SCF, fatigue limits
- ✨ **CLI Tools** - Command-line interface for curve analysis
- ✨ **Comprehensive Examples** - 8 example scripts demonstrating capabilities

### Key Features
- Direct GitHub data access for remote projects
- Multiple integration methods (URL, submodule, clone, sparse checkout)
- Production-ready plotting matching industry Excel tools
- Full documentation and API reference

---

## 🤝 Contributing

Contributions welcome! This library benefits from:
- 20+ years offshore/subsea engineering experience
- 200+ SURF engineers' collective insights
- Active production use in major projects

### How to Contribute
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

---

## 📖 Citation

When using this library in academic or commercial work, please cite:

```bibtex
@software{digitalmodel2025,
  author = {Achanta, Vamsee},
  title = {Digital Model: Engineering Asset Lifecycle Management},
  year = {2025},
  publisher = {GitHub},
  url = {https://github.com/vamseeachanta/digitalmodel}
}
```

For the S-N Curve Database specifically:
```bibtex
@dataset{sn_curve_database2025,
  author = {Achanta, Vamsee},
  title = {Fatigue S-N Curve Database for Offshore Structures},
  year = {2025},
  publisher = {GitHub},
  url = {https://github.com/vamseeachanta/digitalmodel/tree/main/data/fatigue},
  note = {221 curves from 17 international standards}
}
```

---

## 📧 Contact & Support

- **GitHub Issues**: [Report bugs or request features](https://github.com/vamseeachanta/digitalmodel/issues)
- **Email**: vamsee.achanta@aceengineer.com
- **LinkedIn**: [Vamsee Achanta](https://www.linkedin.com/in/vamseeachanta)

---

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

## 🙏 Acknowledgments

**Dedicated to Mark Cerkovnik** - Chief Engineer whose insights shaped major portions of this work and repository.

Special thanks to:
- 200+ SURF engineers who contributed insights over 20 years
- Open-source community for foundational tools
- Industry standards organizations (DNV, API, ABS, BS, Norsok, Bureau Veritas)

---

## 🔗 Related Projects

- [OrcaFlex](https://www.orcina.com/orcaflex/) - Dynamic analysis software
- [WAMIT](https://www.wamit.com/) - Wave analysis program
- [AQWA](https://www.ansys.com/products/structures/ansys-aqwa) - Hydrodynamic analysis
- [BEMRosetta](https://github.com/BEMRosetta/BEMRosetta) - BEM analysis tool

---

## 📊 Project Status

- ✅ **Active Development** - Regular updates and improvements
- ✅ **Production Ready** - Used in active engineering projects
- ✅ **Well Documented** - 352+ documentation files
- ✅ **Tested** - 1,971+ test cases
- ✅ **Industry Validated** - 20+ years of engineering experience

---

**Last Updated**: September 30, 2025
**Version**: 2.0.0
**Total Commits**: 305+

