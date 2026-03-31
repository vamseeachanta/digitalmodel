# Fluids Piping Design Library — Evaluation

**Issue:** vamseeachanta/workspace-hub#1450
**Date:** 2026-03-31
**Version tested:** 1.3.0

## Overview

[fluids](https://github.com/CalebBell/fluids) is an MIT-licensed pure-Python library for fluid dynamics and piping design calculations. Part of the ChEDL (Chemical Engineering Design Library) ecosystem, it implements 29+ friction factor correlations, Crane TP-410 fitting loss coefficients, API 520/526 relief valve sizing, and single-phase pipe pressure drop — all without compiled dependencies.

## Installation

```bash
uv add fluids   # adds to pyproject.toml
```

No binary wheels or system libraries required. Installs `fluids` + `scipy` dependency.

## Key API Patterns

```python
from fluids.friction import friction_factor, one_phase_dP
from fluids.core import Reynolds, dP_from_K
from fluids.fittings import K_gate_valve_Crane, K_globe_valve_Crane, bend_rounded
from fluids.safety_valve import API520_A_g, API520_round_size

# Friction factor — Clamond exact solution (default)
fd = friction_factor(Re=1e5, eD=1e-4)  # → 0.01851

# Reynolds number
Re = Reynolds(V=2.0, D=0.2032, rho=1025, mu=0.001)  # → 416560

# Single-phase pipe pressure drop
dP = one_phase_dP(m=66.5, rho=1025, mu=0.001, D=0.2032, roughness=4.57e-5, L=100)

# Fitting K-factors (Crane TP-410)
K = K_globe_valve_Crane(D1=0.2032, D2=0.2032)  # → 4.77
dP = dP_from_K(K=K, rho=1025, V=2.0)           # → 9780 Pa

# API 520 relief valve sizing
A = API520_A_g(m=1.0, T=350, Z=0.95, MW=28.97, k=1.4, P1=1e6, P2=101325)
A_std = API520_round_size(A)  # next standard orifice size
```

## Capabilities Evaluated

| Feature | API | Status |
|---|---|---|
| Darcy friction factor | `friction_factor()` — 29 correlations + Clamond exact | Verified against 64/Re laminar and Colebrook |
| Pipe pressure drop | `one_phase_dP()` — mass-flow based | Verified against hand-calc (Darcy-Weisbach) |
| Fitting K-factors | `K_gate_valve_Crane()`, `K_globe_valve_Crane()`, `bend_rounded()` | Crane TP-410 method, physically reasonable |
| Relief valve sizing | `API520_A_g()`, `API520_round_size()` | API 520/526 gas sizing verified |

## Offshore Engineering Relevance

- **Seawater injection lines:** Pressure drop for pipe sizing and pump selection
- **Process piping:** Fitting losses for system hydraulic analysis
- **Relief systems:** API 520 sizing for safety valve specification
- **Material roughness:** Built-in roughness database for common pipe materials

## Gotchas

- `bend_rounded()` requires either `fd` (friction factor) or `Re` parameter — will raise `ValueError` if neither provided
- `one_phase_dP()` takes mass flow rate `m` (kg/s), not velocity
- `API520_round_size()` returns an area (m²), not an orifice letter designation
- Primarily single-phase — for two-phase flow, look at separate correlations

## Recommendation

**Recommended for adoption.** fluids covers the core piping design calculations needed for offshore engineering workflows. Pure Python with no compiled dependencies makes it easy to deploy. Well-maintained (425+ stars, active development), good documentation, and MIT license.

Pairs with `ht` (heat transfer) from the same ecosystem for thermal-hydraulic analysis.

## References

- PoC script: `scripts/integrations/fluids_evaluation.py`
- Tests: `tests/test_fluids_integration.py`
- Upstream docs: https://fluids.readthedocs.io
- GitHub: https://github.com/CalebBell/fluids
