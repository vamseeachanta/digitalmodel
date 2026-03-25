# Marine Engineering Jupyter Tutorials

Comprehensive interactive tutorials for the marine engineering analysis module. Learn offshore engineering through hands-on examples with real-world applications.

## 📚 Tutorial Series

### 🎯 Beginner Level

#### Tutorial 1: Getting Started (15 minutes)
**File:** `01_getting_started.ipynb`

Learn the fundamentals:
- Install and import marine engineering modules
- Run your first catenary calculation
- Generate wave spectra (JONSWAP, P-M)
- Create professional visualizations
- Understand module architecture

**Prerequisites:** Basic Python knowledge
**Outputs:** Catenary plots, wave spectrum charts

---

### 🔧 Intermediate Level

#### Tutorial 2: Catenary Analysis Deep Dive (30 minutes)
**File:** `02_catenary_deep_dive.ipynb`

Master catenary calculations:
- Compare simplified vs BVP solvers
- Perform parameter sensitivity analysis
- Analyze multi-segment lazy-wave risers
- Real FPSO case study (200k DWT)
- Optimize mooring line design

**Prerequisites:** Tutorial 1
**Outputs:** Solver comparison charts, sensitivity plots, lazy-wave configurations

---

#### Tutorial 3: Wave Spectra Mastery (25 minutes)
**File:** `03_wave_spectra.ipynb`

Deep dive into wave analysis:
- JONSWAP parameter selection guide
- Pierson-Moskowitz for fully developed seas
- Spectral moments interpretation (m₀, m₁, m₂, m₄)
- Irregular wave synthesis
- Integration with vessel motion analysis

**Prerequisites:** Tutorial 1
**Outputs:** Spectral analysis charts, parameter studies

---

#### Tutorial 4: Mooring System Design (35 minutes)
**File:** `04_mooring_design.ipynb`

Complete mooring system workflow:
- Component database exploration (336 components)
- Chain, wire rope, synthetic line selection
- 8-point spread mooring layout
- Tension analysis per API RP 2SK
- Safety factor calculations
- Design iteration examples

**Prerequisites:** Tutorials 1, 2
**Outputs:** Mooring layouts, component tables, tension diagrams

---

#### Tutorial 5: Environmental Loading (30 minutes)
**File:** `05_environmental_loading.ipynb`

OCIMF environmental forces:
- OCIMF database queries (186+ vessels)
- Wind force calculations
- Current profile effects
- Multi-directional analysis
- Combined loading scenarios
- Visualization and validation

**Prerequisites:** Tutorial 1
**Outputs:** Force diagrams, polar plots, coefficient surfaces

---

### 🚀 Advanced Level

#### Tutorial 6: Complete FPSO Analysis (45 minutes)
**File:** `06_fpso_complete_analysis.ipynb`

End-to-end FPSO design:
- Define vessel properties (200k DWT VLCC)
- Environmental conditions (100-year storm)
- Calculate wind/current/wave forces
- Design 12-line mooring system
- Analyze line tensions and offsets
- Generate professional engineering report

**Prerequisites:** Tutorials 1-5
**Outputs:** Complete FPSO analysis report, design drawings

---

#### Tutorial 7: Advanced Optimization (40 minutes)
**File:** `07_advanced_optimization.ipynb`

Optimization techniques:
- Multi-objective optimization (cost vs performance)
- Mooring pattern optimization (radial, spread, taut)
- Cost-performance tradeoff analysis
- Monte Carlo sensitivity analysis
- Probabilistic design
- Risk assessment

**Prerequisites:** Tutorials 1-6
**Outputs:** Optimization results, Pareto fronts, sensitivity charts

---

## 🗂️ Tutorial Structure

Each tutorial follows this format:

```
1. Learning Objectives
2. Setup & Imports
3. Theory & Background
4. Hands-On Examples
5. Interactive Exercises
6. Visualization
7. Summary & Key Takeaways
8. Next Steps
```

## 📊 Expected Outputs

All tutorials generate:
- **Professional plots** (matplotlib/plotly)
- **Data tables** (pandas DataFrames)
- **Export files** (PNG, HTML, CSV, PDF)
- **Session data** (pickle for continuity)

## 🛠️ Installation

```bash
# Core dependencies
pip install numpy scipy matplotlib pandas plotly ipywidgets

# Optional for PDF export
pip install nbconvert kaleido

# Optional for interactive widgets
jupyter nbextension enable --py widgetsnbextension
```

## 🚦 Learning Path

### Path 1: Mooring Design Engineer
1. Tutorial 1 (Getting Started)
2. Tutorial 2 (Catenary Deep Dive)
3. Tutorial 4 (Mooring System Design)
4. Tutorial 6 (Complete FPSO Analysis)

### Path 2: Hydrodynamic Analyst
1. Tutorial 1 (Getting Started)
2. Tutorial 3 (Wave Spectra Mastery)
3. Tutorial 5 (Environmental Loading)
4. Tutorial 6 (Complete FPSO Analysis)

### Path 3: Optimization Specialist
1. Tutorial 1 (Getting Started)
2. Tutorial 2 (Catenary Deep Dive)
3. Tutorial 4 (Mooring System Design)
4. Tutorial 7 (Advanced Optimization)

### Path 4: Complete Mastery (Recommended)
Follow tutorials 1 → 2 → 3 → 4 → 5 → 6 → 7 sequentially

## 📁 Supporting Files

### Data Files (`../data/`)
- `tutorial1_session.pkl` - Session data from Tutorial 1
- `ocimf_coefficients.csv` - OCIMF vessel database
- `mooring_components.csv` - Component specifications
- `fpso_geometry.json` - Vessel geometry data
- `wave_spectra_reference.xlsx` - Reference wave data

### Utility Functions (`../utils/`)
- `plotting_utils.py` - Professional plot templates
- `report_generator.py` - Automated report generation
- `validation_tools.py` - Excel comparison tools

## 📖 Industry Standards Referenced

- **API RP 2SK** - Stationkeeping Systems for Floating Offshore Structures
- **DNV-OS-E301** - Position Mooring
- **DNV-RP-C205** - Environmental Conditions and Environmental Loads
- **ISO 19901-7** - Stationkeeping Systems
- **OCIMF MEG4** - Mooring Equipment Guidelines

## 🎨 Visualization Gallery

Each tutorial produces publication-quality figures:

- **Catenary shapes** - Line geometry and tension distribution
- **Wave spectra** - JONSWAP/P-M spectral analysis
- **Mooring layouts** - Top-view system configuration
- **Force diagrams** - Vector plots and polar diagrams
- **3D surfaces** - Coefficient interpolation surfaces
- **Sensitivity charts** - Parameter variation analysis
- **Optimization plots** - Pareto fronts and convergence

## 💡 Interactive Features

All tutorials include:
- ✅ Progress indicators
- ✅ Real-time calculations
- ✅ Interactive sliders (ipywidgets)
- ✅ Downloadable results
- ✅ Copy-paste code blocks
- ✅ Inline documentation

## 🔗 Quick Links

- [Tutorial 1 - Getting Started](01_getting_started.ipynb)
- [Tutorial 2 - Catenary Deep Dive](02_catenary_deep_dive.ipynb)
- [Tutorial 3 - Wave Spectra](03_wave_spectra.ipynb)
- [Tutorial 4 - Mooring Design](04_mooring_design.ipynb)
- [Tutorial 5 - Environmental Loading](05_environmental_loading.ipynb)
- [Tutorial 6 - FPSO Analysis](06_fpso_complete_analysis.ipynb)
- [Tutorial 7 - Advanced Optimization](07_advanced_optimization.ipynb)

## 📝 Practice Exercises

Each tutorial includes exercises:

**Beginner:**
- Modify parameters and observe changes
- Recreate plots with different data
- Export results to different formats

**Intermediate:**
- Combine multiple analyses
- Create custom visualizations
- Validate against reference solutions

**Advanced:**
- Develop custom solvers
- Implement new optimization methods
- Create automated workflows

## 🏆 Completion Certificate

Complete all 7 tutorials to gain proficiency in:
- ✓ Mooring line catenary analysis
- ✓ Wave spectral analysis
- ✓ Environmental loading calculations
- ✓ Component database utilization
- ✓ FPSO mooring system design
- ✓ Multi-objective optimization
- ✓ Professional report generation

## 📞 Support

- **Documentation:** See module README files
- **Examples:** Check `../examples/` directory
- **Issues:** Report on GitHub
- **Discussions:** Engineering forums

## 🔄 Updates

**Version 1.0** (Current)
- 7 complete tutorials
- 336 component database
- OCIMF integration
- Professional visualizations

**Planned for v1.1:**
- Video walkthroughs
- Cloud notebook support (Colab/Binder)
- Additional case studies
- Machine learning integration

---

**Start your journey:** Open [Tutorial 1 - Getting Started](01_getting_started.ipynb)

**Estimated total time:** 3-4 hours for complete series
