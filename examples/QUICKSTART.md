# Quick Start Guide - Marine Engineering Tutorials

Get started with the marine engineering tutorials in 5 minutes!

## 🚀 Quick Installation

### Option 1: UV Environment (Recommended)
```bash
# Navigate to project root
cd /path/to/digitalmodel

# Install with UV
uv pip install -r examples/requirements.txt

# Launch Jupyter
jupyter notebook examples/tutorials/
```

### Option 2: Standard pip
```bash
# Install requirements
pip install -r examples/requirements.txt

# Launch Jupyter
jupyter notebook examples/tutorials/
```

### Option 3: Google Colab (No Installation)
Upload notebooks to Google Colab and add this at the top:
```python
!pip install numpy scipy matplotlib pandas plotly ipywidgets
```

## 📚 First Tutorial (5 minutes)

1. **Open Tutorial 1**
   ```bash
   jupyter notebook examples/tutorials/01_getting_started.ipynb
   ```

2. **Run All Cells**
   - Press `Shift + Enter` to run each cell
   - Or: `Cell > Run All` from menu

3. **View Results**
   - Catenary shape plot
   - Wave spectrum analysis
   - Professional visualizations

## 📊 What You'll Learn

### Tutorial 1 (15 min) - Getting Started
- Import modules and setup
- First catenary calculation
- Generate wave spectrum
- Create visualizations

**Output:** `tutorial1_catenary.png`, `tutorial1_wave_spectrum.png`

### Tutorial 2 (30 min) - Catenary Deep Dive
- Solver comparison (simplified vs BVP)
- Sensitivity analysis
- Lazy-wave risers
- FPSO case study

**Output:** `tutorial2_sensitivity.png`, `tutorial2_lazy_wave.png`

### Tutorials 3-7 (Advanced)
Continue with remaining tutorials for complete mastery.

## 🎯 Quick Examples

### Example 1: Catenary Calculation
```python
from marine_engineering.catenary.solver import CatenarySolver, CatenaryInput

params = CatenaryInput(
    length=500,
    horizontal_span=400,
    vertical_span=50,
    weight_per_length=1200,
    ea_stiffness=800_000_000
)

solver = CatenarySolver()
results = solver.solve(params)

print(f"Fairlead Tension: {results.total_tension_fairlead/1000:.1f} kN")
```

### Example 2: Wave Spectrum
```python
from marine_engineering.wave_spectra.spectra import JONSWAPSpectrum, WaveSpectrumParameters

params = WaveSpectrumParameters(Hs=4.0, Tp=10.0, gamma=3.3)
jonswap = JONSWAPSpectrum(params)
spectrum = jonswap.compute_spectrum()
stats = jonswap.get_spectral_statistics()

print(f"Hs: {stats['Hs']:.2f} m, Tz: {stats['Tz']:.2f} s")
```

### Example 3: Environmental Forces
```python
from marine_engineering.environmental_loading.ocimf import OCIMFDatabase, EnvironmentalForces

db = OCIMFDatabase('data/ocimf_coefficients.csv')
env_forces = EnvironmentalForces(db)

# Calculate forces...
```

## 🗂️ File Structure

```
examples/
├── tutorials/
│   ├── 01_getting_started.ipynb
│   ├── 02_catenary_deep_dive.ipynb
│   ├── 03_wave_spectra.ipynb
│   ├── 04_mooring_design.ipynb
│   ├── 05_environmental_loading.ipynb
│   ├── 06_fpso_complete_analysis.ipynb
│   ├── 07_advanced_optimization.ipynb
│   └── README.md
├── data/
│   ├── ocimf_coefficients.csv
│   ├── mooring_components.csv
│   └── tutorial1_session.pkl
├── utils/
│   ├── plotting_utils.py
│   └── __init__.py
├── output/
│   └── (generated plots)
├── requirements.txt
└── QUICKSTART.md (this file)
```

## ⚙️ Configuration

### Matplotlib Backend
For interactive plots in Jupyter:
```python
%matplotlib widget  # Interactive
# or
%matplotlib inline  # Static (default)
```

### Plot Quality
Adjust in notebooks:
```python
import matplotlib.pyplot as plt
plt.rcParams['figure.dpi'] = 150  # Higher quality
plt.rcParams['figure.figsize'] = (14, 8)  # Larger size
```

## 🔧 Troubleshooting

### Issue: Module not found
```bash
# Ensure path is correct
import sys
from pathlib import Path
sys.path.append(str(Path.cwd().parent.parent / 'src'))
```

### Issue: Plots not showing
```python
# Use explicit show
import matplotlib.pyplot as plt
plt.show()

# Or enable inline backend
%matplotlib inline
```

### Issue: Widget not working
```bash
# Enable widget extension
jupyter nbextension enable --py widgetsnbextension
```

## 📖 Learning Paths

### Path 1: Quick Overview (1 hour)
1. Tutorial 1 - Getting Started (15 min)
2. Tutorial 2 - Sections 1-2 only (15 min)
3. Tutorial 6 - Overview section (30 min)

### Path 2: Mooring Engineer (2 hours)
1. Tutorial 1 (15 min)
2. Tutorial 2 (30 min)
3. Tutorial 4 (35 min)
4. Tutorial 6 (45 min)

### Path 3: Complete Course (4 hours)
All tutorials 1 → 7 sequentially

## 🎓 Next Steps

After completing tutorials:

1. **Practice:** Modify examples with your own data
2. **Explore:** Check `../examples/` for more use cases
3. **Extend:** Create custom analysis workflows
4. **Share:** Export results to PDF/HTML

## 📞 Support

- **Documentation:** See tutorial README files
- **Examples:** Browse completed notebooks
- **Issues:** Check module documentation
- **Community:** Offshore engineering forums

## 🏁 Ready to Start?

**Open your first tutorial now:**
```bash
jupyter notebook examples/tutorials/01_getting_started.ipynb
```

**Time to complete:** 15 minutes
**What you'll build:** Professional catenary and wave analysis

---

*Happy Learning! 🌊⚓*
