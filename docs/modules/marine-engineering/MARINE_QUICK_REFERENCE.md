# Marine Engineering Module - Quick Reference Guide

**Version:** 1.0 | **Last Updated:** October 3, 2025

---

## 🚀 Quick Start (5 Minutes)

### Installation
```bash
# Clone repository
git clone https://github.com/vamseeachanta/digitalmodel.git
cd digitalmodel

# Install with UV (recommended)
uv sync

# Or with pip
pip install -e .

# Verify installation
python -c "from marine_engineering import *; print('✅ Marine Engineering Ready')"
```

### First Analysis (Complete Example)
```python
from marine_engineering.wave_spectra import JONSWAPSpectrum, WaveSpectrumParameters
from marine_engineering.catenary import CatenarySolver, CatenaryInput

# 1. Generate wave spectrum
params = WaveSpectrumParameters(Hs=3.5, Tp=10.0, gamma=3.3)
spectrum = JONSWAPSpectrum(params)
S = spectrum.compute_spectrum()

# 2. Solve mooring catenary
solver = CatenarySolver()
result = solver.solve(CatenaryInput(
    horizontal_distance=1000.0,
    vertical_distance=150.0,
    line_length=1200.0,
    weight_per_length=200.0,
    horizontal_tension=500000.0
))

print(f"Max tension: {result.max_tension/1e6:.2f} MN")
print(f"Touchdown point: {result.touchdown_point:.1f} m")
```

---

## 📦 Module Overview

### 11 Core Modules

| Module | Purpose | Key Classes | Excel Formulas |
|--------|---------|-------------|----------------|
| `mooring_analysis` | Component database | `ComponentDatabase`, `ChainProperties` | 2,434 |
| `catenary` | Mooring line solver | `CatenarySolver`, `LazyWaveSolver` | 448 |
| `wave_spectra` | Sea state generation | `JONSWAPSpectrum`, `PiersonMoskowitzSpectrum` | 127 |
| `environmental_loading` | Wind/current forces | `OCIMFDatabase`, `EnvironmentalForces` | 712 |
| `hydrodynamic_coefficients` | Added mass/damping | `CoefficientDatabase`, `FrequencyDependentMatrix` | 312 |

**Total:** 3,869 Excel formulas implemented

---

## 🔧 Common Use Cases

### 1. Mooring Line Analysis

#### Basic Catenary
```python
from marine_engineering.catenary import CatenarySolver, CatenaryInput

solver = CatenarySolver()
result = solver.solve(CatenaryInput(
    horizontal_distance=1000.0,  # m
    vertical_distance=150.0,      # m
    line_length=1200.0,           # m
    weight_per_length=200.0,      # N/m
    horizontal_tension=500000.0   # N
))

# Access results
print(f"Max tension: {result.max_tension:.0f} N")
print(f"Touchdown: {result.touchdown_point:.1f} m from anchor")
print(f"Suspended length: {result.suspended_length:.1f} m")
```

#### Lazy Wave Riser
```python
from marine_engineering.catenary import LazyWaveSolver, LazyWaveInput, BuoyancySection

solver = LazyWaveSolver()
result = solver.solve(LazyWaveInput(
    horizontal_distance=2000.0,
    vertical_distance=1500.0,
    total_length=3500.0,
    base_weight=300.0,  # N/m
    buoyancy_sections=[
        BuoyancySection(start=500, length=800, buoyancy_factor=0.8),
        BuoyancySection(start=1800, length=600, buoyancy_factor=0.6)
    ]
))
```

#### Component Database Lookup
```python
from marine_engineering.mooring_analysis import ComponentDatabase, ChainGrade

db = ComponentDatabase()

# Get chain properties
chain = db.get_chain_properties(
    diameter=120,  # mm
    grade=ChainGrade.R4
)

print(f"MBL: {chain.minimum_breaking_load/1e6:.1f} MN")
print(f"Weight: {chain.weight_in_air:.1f} kg/m")
print(f"K2 factor: {chain.design_factor_k2:.2f}")
```

---

### 2. Wave Analysis

#### JONSWAP Spectrum
```python
from marine_engineering.wave_spectra import JONSWAPSpectrum, WaveSpectrumParameters

# Define sea state
params = WaveSpectrumParameters(
    Hs=3.5,              # Significant wave height (m)
    Tp=10.0,             # Peak period (s)
    gamma=3.3,           # Peak enhancement factor
    n_frequencies=100    # Number of frequencies
)

# Generate spectrum
spectrum = JONSWAPSpectrum(params)
S = spectrum.compute_spectrum()

# Get spectral properties
moments = spectrum.spectral_moments()
print(f"Hs (recovered): {moments['Hs']:.2f} m")
print(f"Tz (zero-crossing): {moments['Tz']:.2f} s")
print(f"Peak frequency: {moments['peak_frequency']:.3f} rad/s")
```

#### Pierson-Moskowitz Spectrum
```python
from marine_engineering.wave_spectra import PiersonMoskowitzSpectrum

# Fully developed sea
params = WaveSpectrumParameters(Hs=4.0, Tp=12.0)
spectrum = PiersonMoskowitzSpectrum(params)
S = spectrum.compute_spectrum()

# Generate time series
time, elevation = spectrum.generate_time_series(
    duration=3600,    # 1 hour
    dt=0.1           # 0.1 second time step
)
```

---

### 3. Environmental Forces (OCIMF)

#### Wind & Current Loading
```python
from marine_engineering.environmental_loading import (
    OCIMFDatabase, EnvironmentalConditions, VesselGeometry, EnvironmentalForces
)

# Load database
db = OCIMFDatabase("data/marine_engineering/ocimf_database.csv")

# Define conditions
conditions = EnvironmentalConditions(
    wind_speed=30.0,        # m/s (1-min mean)
    current_speed=1.5,      # m/s
    heading=45.0,           # degrees (0=head seas)
    displacement=150000.0   # tonnes
)

# Calculate forces
forces = db.calculate_forces(
    vessel_id="FPSO_STANDARD",
    conditions=conditions
)

print(f"Wind Fx: {forces.wind_surge/1e6:.2f} MN")
print(f"Wind Fy: {forces.wind_sway/1e6:.2f} MN")
print(f"Wind Mz: {forces.wind_yaw/1e6:.2f} MN·m")
print(f"Total Fx: {forces.total_surge/1e6:.2f} MN")
```

#### Multi-Heading Analysis
```python
import numpy as np

headings = np.linspace(0, 180, 13)  # 0° to 180° in 15° steps
results = []

for heading in headings:
    conditions.heading = heading
    forces = db.calculate_forces("FPSO_STANDARD", conditions)
    results.append({
        'heading': heading,
        'fx': forces.total_surge,
        'fy': forces.total_sway,
        'mz': forces.total_yaw
    })

# Find worst case
worst_fy = max(results, key=lambda x: abs(x['fy']))
print(f"Max lateral force at {worst_fy['heading']:.0f}°: {worst_fy['fy']/1e6:.2f} MN")
```

---

### 4. Hydrodynamic Coefficients

#### Load and Interpolate
```python
from marine_engineering.hydrodynamic_coefficients import CoefficientDatabase

# Load database
db = CoefficientDatabase()
db.load_from_directory("data/marine_engineering/hydrodynamic")

# Interpolate at specific frequency
freq = 0.5  # rad/s
added_mass = db.get_added_mass(freq)
damping = db.get_damping(freq)

print(f"Added mass (surge): {added_mass[0,0]/1e6:.2f} tonnes")
print(f"Damping (surge): {damping[0,0]/1e6:.2f} N·s/m")
```

#### Natural Frequency Calculation
```python
import numpy as np

# System properties
mass = 50000.0  # tonnes
stiffness = 5e6  # N/m

# Get added mass at multiple frequencies
frequencies = np.linspace(0.1, 2.0, 50)
natural_freqs = []

for omega in frequencies:
    A = db.get_added_mass(omega)
    total_mass = mass + A[0,0]/1e6  # tonnes
    omega_n = np.sqrt(stiffness * 1000 / total_mass)
    natural_freqs.append(omega_n)

# Find natural frequency where ω = ωn
idx = np.argmin(np.abs(frequencies - natural_freqs))
print(f"Natural frequency: {frequencies[idx]:.3f} rad/s")
```

---

### 5. Integration Workflows

#### Complete Mooring Analysis
```python
from marine_engineering.wave_spectra import JONSWAPSpectrum, WaveSpectrumParameters
from marine_engineering.environmental_loading import OCIMFDatabase, EnvironmentalConditions
from marine_engineering.catenary import CatenarySolver, CatenaryInput

# 1. Define environment
wave_params = WaveSpectrumParameters(Hs=4.0, Tp=11.0)
env_conditions = EnvironmentalConditions(
    wind_speed=35.0,
    current_speed=2.0,
    heading=90.0,  # Beam seas
    displacement=180000.0
)

# 2. Calculate forces
ocimf_db = OCIMFDatabase("ocimf_database.csv")
forces = ocimf_db.calculate_forces("FPSO_1", env_conditions)

# 3. Distribute to mooring lines (8-point spread)
num_lines = 8
force_per_line = forces.total_surge / num_lines

# 4. Analyze each line
catenary_solver = CatenarySolver()
for i in range(num_lines):
    result = catenary_solver.solve(CatenaryInput(
        horizontal_distance=1200.0,
        vertical_distance=180.0,
        line_length=1500.0,
        weight_per_length=250.0,
        horizontal_tension=force_per_line
    ))

    print(f"Line {i+1}: Max tension = {result.max_tension/1e6:.2f} MN")
```

#### FPSO Design Envelope
```python
import numpy as np
import pandas as pd

# Define design space
Hs_range = np.linspace(1.0, 6.0, 11)
Tp_range = np.linspace(6.0, 14.0, 9)
heading_range = np.linspace(0, 180, 13)

results = []

for Hs in Hs_range:
    for Tp in Tp_range:
        for heading in heading_range:
            # Wave spectrum
            spectrum = JONSWAPSpectrum(WaveSpectrumParameters(Hs=Hs, Tp=Tp))

            # Environmental forces
            conditions = EnvironmentalConditions(
                wind_speed=Hs * 8.5,  # Correlation
                current_speed=0.8,
                heading=heading,
                displacement=170000.0
            )
            forces = ocimf_db.calculate_forces("FPSO_1", conditions)

            # Mooring
            mooring_result = catenary_solver.solve(CatenaryInput(
                horizontal_distance=1100.0,
                vertical_distance=165.0,
                line_length=1400.0,
                weight_per_length=230.0,
                horizontal_tension=forces.total_surge / 8
            ))

            results.append({
                'Hs': Hs,
                'Tp': Tp,
                'heading': heading,
                'max_tension': mooring_result.max_tension,
                'total_force': forces.total_surge
            })

# Create design envelope
df = pd.DataFrame(results)
critical_cases = df.nlargest(10, 'max_tension')
print(critical_cases)
```

---

## 📊 Performance Guidelines

### Execution Times (Typical)

| Operation | Time | Notes |
|-----------|------|-------|
| Wave spectrum (100 freq) | 0.6 ms | Target: <10 ms ✅ |
| OCIMF lookup | 0.1 ms | Target: <1 ms ✅ |
| Catenary solution | 0.2 ms | Target: <10 ms ✅ |
| Hydro interpolation | 0.4 ms | Target: <5 ms ✅ |
| Complete workflow | <1 s | Target: <5 s ✅ |

### Optimization Tips

#### 1. Caching for Repeated Calculations
```python
from functools import lru_cache

@lru_cache(maxsize=1000)
def cached_ocimf_lookup(vessel_id, heading, displacement):
    return db.calculate_forces(vessel_id, conditions)
```

#### 2. Vectorization for Batch Processing
```python
# Instead of loops
headings = np.linspace(0, 180, 13)
forces_batch = [db.calculate_forces("FPSO", conditions) for h in headings]

# Use vectorized operations where possible
results = db.calculate_forces_batch(vessel_id, conditions_array)
```

#### 3. Lazy Loading for Large Databases
```python
# Only load what you need
db = CoefficientDatabase()
db.load_frequency_range(omega_min=0.3, omega_max=1.5)  # Subset
```

---

## 🧪 Testing & Validation

### Run Tests
```bash
# All marine engineering tests
pytest tests/marine_engineering/ -v

# Integration tests only
pytest tests/marine_engineering/integration/ -v

# Performance benchmarks
pytest tests/marine_engineering/test_performance.py --benchmark-only

# With coverage report
pytest tests/marine_engineering/ --cov=marine_engineering --cov-report=html
```

### Validation Against Excel
```python
# Component database validation
from marine_engineering.mooring_analysis import ComponentDatabase

db = ComponentDatabase()
chain = db.get_chain_properties(diameter=120, grade=ChainGrade.R4)

# Validate against Excel reference
assert abs(chain.minimum_breaking_load - 10080e3) < 1e3  # ±1 kN
assert abs(chain.weight_in_air - 256.0) < 0.1            # ±0.1 kg/m
```

---

## 🐛 Common Issues & Solutions

### Issue 1: Import Errors
```python
# ❌ Wrong
from marine_engineering import mooring_analysis

# ✅ Correct
from marine_engineering.mooring_analysis import ComponentDatabase
```

### Issue 2: Catenary Convergence Failure
```python
# Solution: Adjust initial guess
result = solver.solve(input_params, initial_guess_factor=1.5)

# Or use simplified solver
from marine_engineering.catenary import SimplifiedCatenarySolver
simple_solver = SimplifiedCatenarySolver()
result = simple_solver.solve(input_params)
```

### Issue 3: OCIMF Database Not Found
```python
# ✅ Use absolute path
import os
db_path = os.path.join(
    os.path.dirname(__file__),
    "data/marine_engineering/ocimf_database.csv"
)
db = OCIMFDatabase(db_path)
```

### Issue 4: Hydro Coefficient Interpolation Warning
```python
# Extrapolation warning - use clipping
db.get_added_mass(omega=2.5, extrapolate=False)  # Returns nearest value
```

---

## 📚 API Reference Summary

### Mooring Analysis
```python
ComponentDatabase()
  .get_chain_properties(diameter, grade) -> ChainProperties
  .get_wire_rope_properties(diameter, construction) -> WireRopeProperties
  .get_synthetic_line_properties(diameter, material) -> SyntheticLineProperties

CatenarySolver()
  .solve(CatenaryInput) -> CatenaryResult

LazyWaveSolver()
  .solve(LazyWaveInput) -> LazyWaveResult
```

### Wave Spectra
```python
JONSWAPSpectrum(WaveSpectrumParameters)
  .compute_spectrum() -> np.ndarray
  .spectral_moments() -> dict
  .generate_time_series(duration, dt) -> (time, elevation)

PiersonMoskowitzSpectrum(WaveSpectrumParameters)
  .compute_spectrum() -> np.ndarray
```

### Environmental Loading
```python
OCIMFDatabase(file_path)
  .calculate_forces(vessel_id, conditions) -> EnvironmentalForces
  .get_coefficients(vessel_id, heading, displacement) -> dict

EnvironmentalForces
  .wind_surge, .wind_sway, .wind_yaw
  .current_surge, .current_sway, .current_yaw
  .total_surge, .total_sway, .total_yaw
```

### Hydrodynamic Coefficients
```python
CoefficientDatabase()
  .load_from_directory(path)
  .get_added_mass(omega) -> np.ndarray (6×6)
  .get_damping(omega) -> np.ndarray (6×6)
  .interpolate(omega) -> tuple[np.ndarray, np.ndarray]
```

---

## 🎯 Best Practices

### 1. Always Validate Inputs
```python
def analyze_mooring_line(params: CatenaryInput) -> CatenaryResult:
    # Validate
    if params.horizontal_distance <= 0:
        raise ValueError("Horizontal distance must be positive")
    if params.line_length < params.horizontal_distance:
        raise ValueError("Line length must exceed horizontal distance")

    # Solve
    solver = CatenarySolver()
    return solver.solve(params)
```

### 2. Use Type Hints
```python
from typing import List, Tuple
import numpy as np

def calculate_mooring_tensions(
    forces: EnvironmentalForces,
    num_lines: int = 8
) -> List[float]:
    """Calculate individual line tensions."""
    force_per_line = forces.total_surge / num_lines
    return [force_per_line] * num_lines
```

### 3. Handle Errors Gracefully
```python
try:
    result = solver.solve(catenary_input)
except ConvergenceError as e:
    print(f"Convergence failed: {e}")
    # Fallback to simplified solver
    result = simplified_solver.solve(catenary_input)
```

### 4. Document Units
```python
def calculate_tension(
    force: float,  # N (Newtons)
    area: float,   # m² (square meters)
) -> float:        # Pa (Pascals)
    """
    Calculate stress from force and area.

    Parameters
    ----------
    force : float
        Applied force in Newtons
    area : float
        Cross-sectional area in square meters

    Returns
    -------
    stress : float
        Stress in Pascals
    """
    return force / area
```

---

## 📈 Performance Tuning

### For Large-Scale Analysis

#### Parallel Processing
```python
from concurrent.futures import ProcessPoolExecutor
import numpy as np

def analyze_single_case(params):
    solver = CatenarySolver()
    return solver.solve(params)

# Generate parameter sets
param_sets = [...]  # 1000 cases

# Parallel execution
with ProcessPoolExecutor(max_workers=8) as executor:
    results = list(executor.map(analyze_single_case, param_sets))
```

#### Memory Optimization
```python
# Use generators for large datasets
def generate_wave_spectra(Hs_range, Tp_range):
    for Hs in Hs_range:
        for Tp in Tp_range:
            params = WaveSpectrumParameters(Hs=Hs, Tp=Tp)
            yield JONSWAPSpectrum(params)

# Process one at a time
for spectrum in generate_wave_spectra(Hs_range, Tp_range):
    S = spectrum.compute_spectrum()
    # Process S...
```

---

## 🔗 Related Resources

### Documentation Files
- **Main Summary:** `docs/MARINE_ENGINEERING_PHASE_1-3_SUMMARY.md`
- **Performance Report:** `docs/performance_optimization_report.md`
- **Integration Guide:** `tests/marine_engineering/integration/README.md`
- **Implementation Details:** `docs/marine_engineering/IMPLEMENTATION_SUMMARY.md`

### Example Scripts
- **Integration Example:** `tests/marine_engineering/integration/integration_example.py`
- **FPSO Analysis:** `examples/fpso_mooring_analysis.py`
- **Lazy Wave Example:** `examples/lazy_wave_example.py`

### Industry Standards
- DNV-OS-E301: Position Mooring
- API RP 2SK: Stationkeeping Systems
- ISO 19901-7: Stationkeeping
- OCIMF MEG4: Environmental Loads
- DNV-RP-C205: Wave Analysis

---

## 💡 Tips & Tricks

### Quick Debugging
```python
# Enable detailed logging
import logging
logging.basicConfig(level=logging.DEBUG)

# Solver will print convergence details
result = solver.solve(params)
```

### Visualize Results
```python
import matplotlib.pyplot as plt

# Plot catenary shape
plt.figure(figsize=(10, 6))
plt.plot(result.x_coords, result.y_coords)
plt.xlabel('Horizontal Distance (m)')
plt.ylabel('Depth (m)')
plt.title('Mooring Line Configuration')
plt.grid(True)
plt.show()
```

### Export to OrcaFlex
```python
# Prepare OrcaFlex input data
orcaflex_data = {
    'LineName': 'Mooring_Line_1',
    'Length': catenary_input.line_length,
    'TargetSegmentLength': 10.0,
    'MassPerUnitLength': catenary_input.weight_per_length / 9.81,
    'EA': 1e9,  # Axial stiffness
    'EndAConnection': 'Anchored',
    'EndBConnection': 'Vessel'
}

# Save as JSON
import json
with open('orcaflex_mooring.json', 'w') as f:
    json.dump(orcaflex_data, f, indent=2)
```

---

## 📞 Getting Help

### Quick Checks
1. ✅ Check import paths (use absolute imports)
2. ✅ Verify data file locations
3. ✅ Validate input parameters
4. ✅ Review error messages carefully

### Resources
- **GitHub Issues:** [Report bugs]
- **Documentation:** `docs/` directory
- **Email:** vamsee.achanta@aceengineer.com

### Contribution
Pull requests welcome! See `CONTRIBUTING.md` for guidelines.

---

**Last Updated:** October 3, 2025
**Version:** 1.0
**Status:** ✅ Production Ready
