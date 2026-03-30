# pyLife Fatigue Library Evaluation

**Issue:** vamseeachanta/workspace-hub#1458
**Date:** 2026-03-29
**Status:** Evaluation complete — recommended for integration

## Libraries Evaluated

| Property | pyLife (Bosch Research) | py-fatigue (OWI-Lab) |
|---|---|---|
| Version | 2.2.1 | 2.1.0 |
| License | Apache-2.0 | GPL-3.0 |
| GitHub Stars | ~165 | ~30 |
| Contributors | ~20 | ~5 |
| Python | 3.9+ | 3.9+ |
| Key Deps | pandas, numpy, scipy | pandas, numpy, numba |

## pyLife — Summary

Bosch Research's general-purpose fatigue and reliability library. Automotive heritage but fully applicable to offshore/marine.

### Key Capabilities
- **S-N Curves (WoehlerCurve):** Bi-linear with configurable k_1/k_2 slopes, SD/ND knee point, scatter bands (TN/TS), failure probability
- **Miner's Rule:** Three variants — Elementary, Haibach, Original
- **Woehler Fitting:** Four methods — Elementary, MaxLikelihood, Bayesian, Probit
- **Rainflow Counting:** C extension for performance
- **FKM Nonlinear:** Full FKM guideline implementation for notched components
- **Mesh Integration:** Hotspot detection, gradient-based methods, mesh mapping
- **Notch Approximation:** Neuber/Seeger-Beste methods
- **Failure Probability:** Statistical fatigue assessment
- **pandas Deep Integration:** Custom accessors on Series/DataFrame

### API Example — S-N Curve + Miner's Rule
```python
from pylife.materiallaws.woehlercurve import WoehlerCurve

# DNV-RP-C203 Category F
params = pd.Series({'k_1': 3.0, 'SD': 52.63, 'ND': 1e7, 'k_2': 5.0})
wc = WoehlerCurve(params)

# Allowable cycles at 100 MPa stress range
N = wc.cycles(np.array([100]))  # → 1,457,807

# Miner's damage = sum(n_i / N_i)
damage = applied_cycles / wc.cycles(stress_ranges)
total_D = damage.sum()
```

### Limitations
- No crack growth (Paris' law)
- No damage equivalent moment/stress (DEM/DES) calculations
- Automotive documentation style — offshore engineers need to map DNV parameters
- Woehler fitting API expects specific DataFrame column names (not well documented)

## py-fatigue — Summary

OWI-Lab's fatigue library developed for offshore wind structural health monitoring.

### Key Capabilities
- **S-N Curves (SNCurve):** Single-slope segments with log10(a) intercept convention
- **Palmgren-Miner:** Damage accumulation via pandas accessor
- **Rainflow Counting:** Numba-accelerated
- **Crack Growth:** Paris' law implementation (unique to py-fatigue)
- **Mean Stress Corrections:** Goodman, Gerber, SWT, Walker
- **DEM/DES:** Damage equivalent moment and stress calculations (offshore-specific)
- **Geometry:** Cylinder and generic geometry helpers

### API Example — S-N Curve
```python
from py_fatigue.material.sn_curve import SNCurve

# DNV F: N = 10^11.855 / S^3
sn = SNCurve(slope=3.0, intercept=11.855, endurance=1e7,
             curve="F", norm="DNV-RP-C203")
N = sn.get_cycles(100)  # → 716,143
```

### Limitations
- No Woehler curve fitting
- No FEM mesh integration
- No notch approximation methods
- No FKM guideline implementation
- GPL-3.0 license (copyleft — may restrict commercial use)
- Smaller community and contributor base

## Head-to-Head Comparison

| Feature | pyLife | py-fatigue |
|---|---|---|
| S-N Curves | Bi-linear, scatter bands | Single-segment, simpler API |
| Miner's Rule | 3 variants (Elementary/Haibach/Original) | Palmgren-Miner |
| Woehler Fitting | 4+ methods | Not available |
| Rainflow | C extension | Numba-accelerated |
| Crack Growth | Not available | Paris' law |
| Mean Stress | FKM methods | Goodman/Gerber/SWT/Walker |
| FEM Post-proc | Hotspot, mesh mapping | Not available |
| Notch Approx. | Neuber/Seeger-Beste | Not available |
| Failure Prob. | Yes | Not available |
| DEM/DES | Not available | Yes (offshore-specific) |
| License | Apache-2.0 (permissive) | GPL-3.0 (copyleft) |

## PoC Results

The proof-of-concept script (`scripts/integrations/pylife_poc.py`) demonstrates:

1. **DNV-RP-C203 Category F S-N curve** — both libraries can define it, pyLife matches DNV tables exactly with bi-linear knee-point parameterization
2. **Miner's damage** — D = 0.072 for a typical offshore stress histogram (design life factor ~14x)
3. **Woehler fitting** — pyLife's Elementary method needs specific data format; manual log-log regression recovered m=2.96 vs true 3.0
4. **Comparison** — py-fatigue uses a different intercept convention (log10 vs linear) and single-slope segments

## Recommendation

**Primary engine: pyLife** — broader API, Apache-2.0 license, mature community, covers 80%+ of digitalmodel's fatigue needs (S-N curves, Miner's rule, Woehler fitting, FEM post-processing).

**Complement with py-fatigue when:**
- Crack growth analysis is needed (Paris' law)
- DEM/DES calculations for offshore wind fatigue
- GPL-3.0 licensing is acceptable for the use case

**Integration path:**
1. Add `pylife>=2.2` to digitalmodel's dependencies
2. Create a thin wrapper in `digitalmodel/fatigue/` that maps DNV curve parameters to pyLife's WoehlerCurve
3. Implement Miner's rule damage calculator using pyLife's built-in methods
4. Optionally add py-fatigue for crack growth workflows

## References
- pyLife docs: https://pylife.readthedocs.io
- py-fatigue docs: https://owi-lab.github.io/py_fatigue/
- DNV-RP-C203 (2021): Fatigue Design of Offshore Steel Structures
- Related issue: workspace-hub#1457 (py-fatigue standalone evaluation)
