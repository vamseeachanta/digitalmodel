# Catenary Module Consolidation Plan

**Version:** 1.0
**Date:** 2025-10-03
**Status:** Analysis & Planning Phase

---

## Executive Summary

The repository contains **two distinct catenary solver implementations** serving different use cases:

1. **Phase 1 NEW Implementation** (`src/marine_engineering/mooring_analysis/catenary_solver.py`)
   - Advanced Newton-Raphson BVP solver
   - Validated against Excel "Poly Mooring" reference (±1% accuracy)
   - Comprehensive test coverage (445 lines of tests)
   - Modern dataclass-based API

2. **Legacy Implementation** (`src/digitalmodel/modules/catenary/`)
   - Simplified analytical formulas
   - Riser geometry and lazy-wave configuration
   - Dict-based API with plotting capabilities
   - Integrated with OrcaFlex model building

**Recommendation:** **Option A - Extend Phase 1 Implementation** with legacy functionality preserved as utility methods.

---

## Current State Analysis

### Phase 1 Implementation (NEW) - `src/marine_engineering/mooring_analysis/catenary_solver.py`

**Location:** `src/marine_engineering/mooring_analysis/catenary_solver.py` (306 lines)

**Mathematical Approach:**
- **General 2D catenary BVP solver** using system of 3 equations:
  1. `x2 - x1 = X` (horizontal span)
  2. `h2 - h1 = Y` (vertical span)
  3. `s2 - s1 + elongation = L` (total length)
- **Newton-Raphson iteration** with `scipy.optimize.fsolve`
- **Fallback solver:** Simplified formulation with `scipy.optimize.brentq`
- **Elastic elongation:** `elongation = H * L / EA` (average tension approximation)

**Features:**
- ✅ Elastic elongation with axial stiffness EA
- ✅ Tension distribution along line
- ✅ Touchdown point calculation
- ✅ Shape coordinates (100 points)
- ✅ Convergence diagnostics
- ✅ Dataclass-based API (type-safe)
- ✅ Comprehensive test suite (445 lines)
- ✅ Excel reference validation (±1% accuracy)

**API Pattern:**
```python
from marine_engineering.mooring_analysis.catenary_solver import (
    CatenaryInput, CatenaryResults, CatenarySolver
)

params = CatenaryInput(
    length=1000,
    horizontal_span=800,
    vertical_span=100,
    weight_per_length=1962,
    ea_stiffness=64e9,
    water_depth=150,  # Optional
    seabed_friction=0.0  # Optional
)

solver = CatenarySolver(tolerance=1e-6, max_iterations=200)
results = solver.solve(params)

# Access results
print(results.horizontal_tension)  # 785000 N
print(results.total_tension_fairlead)  # 809000 N
print(results.elongation)  # 12.3 m
print(results.shape_x, results.shape_y)  # Line geometry
```

**Use Cases:**
- Mooring line analysis
- Boundary value problems with specified endpoints
- High-accuracy engineering calculations
- Multi-segment mooring systems

**Strengths:**
- Rigorous mathematical formulation
- Validated against industry standards
- Type-safe API with dataclasses
- Excellent test coverage
- Handles complex boundary conditions

**Limitations:**
- No lazy-wave configuration support
- No plotting capabilities
- No buoyancy section modeling
- No OrcaFlex integration
- No angle-based input methods

---

### Legacy Implementation - `src/digitalmodel/modules/catenary/`

**Location:** `src/digitalmodel/modules/catenary/` (4 modules)

**Files:**
1. **`catenary.py`** (36 lines) - Router class
2. **`catenary_equation.py`** (82 lines) - CatenaryCalculator class
3. **`catenaryMethods.py`** (471 lines) - Specialized functions
4. **`catenary_riser.py`** (168 lines) - Riser analysis workflow

**Mathematical Approach:**
- **Simplified analytical formulas** based on:
  - Force-based: `S = d * (2F/w - d)`, `X = ((F/w) - d) * log(...)`
  - Angle-based: `BendRadius = d * cos(q) / (1 - cos(q))`
  - Lazy-wave: Segmented analysis (hang-off → sag → buoyancy → hog → TDP)
- **No iterative solvers** - direct closed-form solutions
- **Buoyancy modeling:** Uniform and discrete buoyancy sections

**Features:**
- ✅ Three input modes: Force (F), Horizontal distance (X), Angle (q)
- ✅ Lazy-wave catenary equation (sag-hog-buoyancy)
- ✅ Buoyancy properties calculation
- ✅ Matplotlib plotting (shape visualization)
- ✅ OrcaFlex model building integration
- ✅ Pipe properties integration
- ✅ Range analysis for layback calculations
- ✅ Multiple fluid fill conditions

**API Pattern:**
```python
from digitalmodel.subsea.catenary.catenary_equation import CatenaryCalculator

calc = CatenaryCalculator()

# Method 1: Angle-based
data = {"d": 100, "q": 45, "F": None, "X": None}
result = calc.calculate(data)
# Returns: {"S": length, "X": horiz_dist, "BendRadius": radius}

# Method 2: Force-based
data = {"d": 100, "F": 50000, "w": 1962, "q": None, "X": None}
result = calc.calculate(data)

# Lazy-wave via catenary_riser
from digitalmodel.subsea.catenary.catenary_riser import catenary_riser
cfg = {...}  # Complex config dict
result_cfg = catenary_riser(cfg)
```

**Use Cases:**
- SCR (Steel Catenary Riser) design
- SLWR (Lazy-Wave Riser) configuration
- Quick geometry checks
- OrcaFlex model preparation
- Riser hang-off angle calculations

**Strengths:**
- Simple, fast closed-form solutions
- Supports lazy-wave configurations
- Integrated plotting
- OrcaFlex workflow integration
- Multiple input methods (F, X, q)
- Buoyancy modeling

**Limitations:**
- No elastic elongation support
- No BVP solver (can't specify both endpoints)
- Dict-based API (not type-safe)
- Limited test coverage
- No validation against industry references
- Hardcoded assumptions (low point at anchor)

---

## Comparison Matrix

| Feature | Phase 1 (NEW) | Legacy | Recommendation |
|---------|---------------|--------|----------------|
| **Mathematical Rigor** | ✅ BVP solver | ⚠️ Simplified | **Keep Phase 1** |
| **Elastic Elongation** | ✅ EA stiffness | ❌ None | **Keep Phase 1** |
| **Validation** | ✅ Excel ±1% | ❌ None | **Keep Phase 1** |
| **Test Coverage** | ✅ 445 lines | ⚠️ Minimal | **Keep Phase 1** |
| **API Design** | ✅ Dataclasses | ⚠️ Dicts | **Keep Phase 1** |
| **Tension Distribution** | ✅ Full array | ❌ Endpoints only | **Keep Phase 1** |
| **Convergence Diagnostics** | ✅ Yes | ❌ No | **Keep Phase 1** |
| **Lazy-Wave Config** | ❌ None | ✅ Full support | **Port to Phase 1** |
| **Buoyancy Modeling** | ❌ None | ✅ Yes | **Port to Phase 1** |
| **Plotting** | ❌ None | ✅ Matplotlib | **Add to Phase 1** |
| **Angle Input (q)** | ❌ None | ✅ Yes | **Add to Phase 1** |
| **Force Input (F)** | ❌ None | ✅ Yes | **Add to Phase 1** |
| **OrcaFlex Integration** | ❌ None | ✅ Yes | **Port to Phase 1** |
| **Pipe Properties** | ❌ None | ✅ Yes | **Keep separate module** |

**Score:** Phase 1 (9/13), Legacy (7/13)
**Verdict:** Phase 1 provides superior foundation - extend with legacy features

---

## Consolidation Strategy

### **Option A: Extend Phase 1 (RECOMMENDED)**

**Approach:**
1. **Keep Phase 1 `CatenarySolver` as primary solver**
2. **Add utility functions** for legacy simplified methods (angle-based, force-based)
3. **Port lazy-wave solver** to Phase 1 API style
4. **Add plotting module** using Phase 1 results
5. **Create migration guide** for legacy code

**Advantages:**
- ✅ Maintains validated, rigorous solver
- ✅ Backward compatibility via utility functions
- ✅ Gradual migration path
- ✅ Best long-term maintainability
- ✅ Type-safe API for new code

**Risks:**
- ⚠️ Requires porting lazy-wave logic (moderate complexity)
- ⚠️ Breaking changes for legacy code (mitigated by utilities)
- ⚠️ Need to update OrcaFlex integration

---

### **Option B: Dual API (NOT RECOMMENDED)**

**Approach:**
- Keep both implementations separate
- Create unified router module
- Document use cases for each

**Disadvantages:**
- ❌ Code duplication
- ❌ Two APIs to maintain
- ❌ Confusion for users
- ❌ Divergent evolution risk

---

## Migration Plan

### Phase 1: Create Unified Module Structure

**New directory:** `src/marine_engineering/catenary/`

```
src/marine_engineering/catenary/
├── __init__.py               # Public API exports
├── solver.py                 # Phase 1 CatenarySolver (moved)
├── simplified.py             # Legacy simplified methods (NEW)
├── lazy_wave.py              # Lazy-wave solver (NEW, ported)
├── plotting.py               # Matplotlib plotting (NEW, ported)
└── utils.py                  # Helper functions
```

**Test structure:**
```
tests/marine_engineering/catenary/
├── test_solver.py            # Phase 1 solver tests (moved)
├── test_simplified.py        # Simplified method tests (NEW)
├── test_lazy_wave.py         # Lazy-wave tests (NEW)
└── test_plotting.py          # Plotting tests (NEW)
```

---

### Phase 2: Port Legacy Functionality

#### 2.1 Create `simplified.py` - Simplified Analytical Methods

**Purpose:** Quick calculations using closed-form formulas (legacy compatibility)

```python
"""Simplified catenary calculations using analytical formulas."""

from dataclasses import dataclass
from typing import Optional
import math

@dataclass
class SimplifiedInput:
    """Input for simplified catenary calculations."""
    vertical_distance: float  # d [m]
    force: Optional[float] = None  # F [N]
    weight_per_length: Optional[float] = None  # w [N/m]
    angle: Optional[float] = None  # q [degrees]
    horizontal_distance: Optional[float] = None  # X [m]

@dataclass
class SimplifiedResult:
    """Results from simplified catenary."""
    arc_length: float  # S [m]
    horizontal_distance: float  # X [m]
    bend_radius: Optional[float] = None  # BendRadius [m]

def calculate_from_force(d: float, F: float, w: float) -> SimplifiedResult:
    """
    Calculate catenary from force parameters.

    Formula: S = d * (2F/w - d)
             X = ((F/w) - d) * log((S + F/w) / ((F/w) - d))
    """
    S = d * (2 * F / w - d)
    X = ((F / w) - d) * math.log((S + (F / w)) / ((F / w) - d))
    return SimplifiedResult(arc_length=S, horizontal_distance=X)

def calculate_from_angle(d: float, q: float) -> SimplifiedResult:
    """
    Calculate catenary from declination angle.

    Formula: BendRadius = d * cos(q) / (1 - cos(q))
             S = BendRadius * tan(q)
             X = BendRadius * asinh(tan(q))
    """
    angle_comp = 90 - q
    tan_q = math.tan(math.radians(angle_comp))
    cos_q = math.cos(math.radians(angle_comp))

    bend_radius = d * cos_q / (1 - cos_q)
    S = bend_radius * tan_q
    X = bend_radius * math.asinh(tan_q)

    return SimplifiedResult(arc_length=S, horizontal_distance=X, bend_radius=bend_radius)

# Legacy API wrapper for backward compatibility
class CatenaryCalculator:
    """Legacy API wrapper (backward compatibility)."""
    def calculate(self, data: dict) -> dict:
        """Calculate using legacy dict API."""
        if data.get("q") is not None:
            result = calculate_from_angle(data["d"], data["q"])
            return {"S": result.arc_length, "X": result.horizontal_distance,
                    "BendRadius": result.bend_radius}
        elif data.get("F") is not None:
            result = calculate_from_force(data["d"], data["F"], data["w"])
            return {"S": result.arc_length, "X": result.horizontal_distance}
        else:
            raise NotImplementedError("X-based calculation not implemented")
```

---

#### 2.2 Create `lazy_wave.py` - Lazy-Wave Catenary Solver

**Purpose:** Multi-segment riser analysis (sag-hog-buoyancy configuration)

```python
"""Lazy-wave catenary solver for buoyancy-supported risers."""

from dataclasses import dataclass
import numpy as np
from .solver import CatenarySolver, CatenaryInput

@dataclass
class LazyWaveInput:
    """Input for lazy-wave catenary analysis."""
    weight_without_buoyancy: float  # N/m
    weight_with_buoyancy: float  # N/m (can be negative)
    sag_elevation: float  # m above seabed
    hog_elevation: float  # m above seabed
    hang_off_height: float  # m
    declination_angle: float  # degrees
    ea_stiffness: float  # N

@dataclass
class LazyWaveSegment:
    """Results for one lazy-wave segment."""
    arc_length: float
    horizontal_distance: float
    vertical_distance: float
    shape_x: np.ndarray
    shape_y: np.ndarray

@dataclass
class LazyWaveResults:
    """Complete lazy-wave catenary results."""
    hang_off_to_buoyancy: LazyWaveSegment
    buoyancy_section: LazyWaveSegment
    buoyancy_to_tdp: LazyWaveSegment
    total_arc_length: float
    total_horizontal_distance: float
    horizontal_force: float
    vertical_force: float
    total_buoyancy_force: float

class LazyWaveSolver:
    """Lazy-wave catenary solver using segmented analysis."""

    def __init__(self):
        self.solver = CatenarySolver()

    def solve(self, params: LazyWaveInput) -> LazyWaveResults:
        """
        Solve lazy-wave catenary with buoyancy sections.

        Segments:
        1. Hang-off to Sag (weight without buoyancy)
        2. Sag to Buoyancy to Hog (weight with buoyancy, can be negative)
        3. Buoyancy to Touchdown (weight without buoyancy)
        """
        # Implementation ported from catenaryMethods.py
        # Uses Phase 1 CatenarySolver for each segment
        ...
```

---

#### 2.3 Create `plotting.py` - Visualization Module

**Purpose:** Matplotlib plotting for catenary results

```python
"""Plotting utilities for catenary visualization."""

import matplotlib.pyplot as plt
import numpy as np
from .solver import CatenaryResults
from .lazy_wave import LazyWaveResults

def plot_catenary_shape(
    results: CatenaryResults,
    title: str = "Catenary Mooring Line Shape",
    save_path: str = None,
    dpi: int = 800
):
    """
    Plot catenary line shape.

    Parameters
    ----------
    results : CatenaryResults
        Catenary solution from CatenarySolver
    title : str
        Plot title
    save_path : str, optional
        Path to save figure
    dpi : int
        Figure resolution
    """
    plt.figure(figsize=(10, 6))
    plt.plot(results.shape_x, results.shape_y, 'b-', linewidth=2, label='Catenary')

    # Mark endpoints
    plt.plot(results.shape_x[0], results.shape_y[0], 'go', markersize=8, label='Anchor')
    plt.plot(results.shape_x[-1], results.shape_y[-1], 'ro', markersize=8, label='Fairlead')

    plt.xlabel('Horizontal distance [m]', fontsize=12, fontweight='bold')
    plt.ylabel('Distance from seabed [m]', fontsize=12, fontweight='bold')
    plt.title(title, fontsize=14, fontweight='bold')
    plt.grid(True)
    plt.legend()

    if save_path:
        plt.savefig(save_path, dpi=dpi, bbox_inches='tight')
    plt.show()

def plot_tension_distribution(results: CatenaryResults, save_path: str = None):
    """Plot tension distribution along catenary."""
    plt.figure(figsize=(10, 6))
    plt.plot(results.shape_x, results.tension_distribution / 1e6, 'b-', linewidth=2)
    plt.axhline(results.horizontal_tension / 1e6, color='r', linestyle='--',
                label=f'H = {results.horizontal_tension/1e6:.1f} MN')

    plt.xlabel('Distance along line [m]', fontsize=12, fontweight='bold')
    plt.ylabel('Tension [MN]', fontsize=12, fontweight='bold')
    plt.title('Tension Distribution', fontsize=14, fontweight='bold')
    plt.grid(True)
    plt.legend()

    if save_path:
        plt.savefig(save_path, dpi=800, bbox_inches='tight')
    plt.show()

def plot_lazy_wave(results: LazyWaveResults, save_path: str = None):
    """Plot lazy-wave catenary with buoyancy sections."""
    # Implementation ported from catenaryMethods.py lazyWavePlot()
    ...
```

---

### Phase 3: Update Public API (`__init__.py`)

```python
"""
Marine Engineering Catenary Module
===================================

Comprehensive catenary analysis for mooring lines and risers.

Main Solver (recommended):
    CatenarySolver - Advanced BVP solver with elastic elongation

Simplified Methods (quick calculations):
    calculate_from_force - Force-based catenary
    calculate_from_angle - Angle-based catenary

Lazy-Wave Analysis:
    LazyWaveSolver - Multi-segment buoyancy-supported risers

Visualization:
    plot_catenary_shape - Line geometry
    plot_tension_distribution - Tension along line
    plot_lazy_wave - Lazy-wave configuration

Legacy Compatibility:
    CatenaryCalculator - Dict-based API (deprecated, use CatenarySolver)
"""

# Primary solver (Phase 1)
from .solver import CatenarySolver, CatenaryInput, CatenaryResults

# Simplified methods
from .simplified import (
    calculate_from_force,
    calculate_from_angle,
    SimplifiedInput,
    SimplifiedResult
)

# Lazy-wave
from .lazy_wave import LazyWaveSolver, LazyWaveInput, LazyWaveResults

# Plotting
from .plotting import (
    plot_catenary_shape,
    plot_tension_distribution,
    plot_lazy_wave
)

# Legacy compatibility (deprecated)
from .simplified import CatenaryCalculator

__all__ = [
    # Primary API
    'CatenarySolver', 'CatenaryInput', 'CatenaryResults',

    # Simplified methods
    'calculate_from_force', 'calculate_from_angle',
    'SimplifiedInput', 'SimplifiedResult',

    # Lazy-wave
    'LazyWaveSolver', 'LazyWaveInput', 'LazyWaveResults',

    # Plotting
    'plot_catenary_shape', 'plot_tension_distribution', 'plot_lazy_wave',

    # Legacy (deprecated)
    'CatenaryCalculator',
]
```

---

### Phase 4: Deprecation & Migration

#### 4.1 Mark Legacy Modules as Deprecated

Add deprecation warnings to `src/digitalmodel/modules/catenary/`:

```python
# catenary.py, catenary_equation.py, catenaryMethods.py
import warnings

warnings.warn(
    "digitalmodel.subsea.catenary is deprecated. "
    "Use marine_engineering.catenary instead. "
    "See migration guide: docs/catenary_migration.md",
    DeprecationWarning,
    stacklevel=2
)
```

#### 4.2 Create Migration Guide (`docs/catenary_migration.md`)

```markdown
# Catenary Module Migration Guide

## Quick Start

**Old (Legacy):**
```python
from digitalmodel.subsea.catenary.catenary_equation import CatenaryCalculator

calc = CatenaryCalculator()
result = calc.calculate({"d": 100, "q": 45, "F": None, "X": None})
```

**New (Phase 1 Extended):**
```python
from marine_engineering.catenary import calculate_from_angle

result = calculate_from_angle(d=100, q=45)
print(result.arc_length, result.horizontal_distance, result.bend_radius)
```

## Full BVP Solver (Recommended)

**Old:** Not available in legacy

**New:**
```python
from marine_engineering.catenary import CatenarySolver, CatenaryInput

params = CatenaryInput(
    length=1000,
    horizontal_span=800,
    vertical_span=100,
    weight_per_length=1962,
    ea_stiffness=64e9
)

solver = CatenarySolver()
results = solver.solve(params)

print(results.horizontal_tension)  # Validated ±1% vs Excel
print(results.elongation)  # Elastic stretch
```

[... more examples ...]
```

#### 4.3 Update Import Locations

**Files to update (search results):**

No direct imports found! (Good news - minimal breaking changes)

**Tools/examples to update:**
- `tools/doc_tools/modules/catenary_*.py` - Reference implementations
- `tests/modules/catenary_riser/` - Test suite

---

### Phase 5: Testing Strategy

#### 5.1 Legacy Compatibility Tests

```python
# tests/marine_engineering/catenary/test_legacy_compatibility.py

def test_simplified_matches_legacy():
    """Verify simplified methods match legacy CatenaryCalculator."""
    from marine_engineering.catenary import calculate_from_angle
    from digitalmodel.subsea.catenary.catenary_equation import CatenaryCalculator

    # Test angle-based calculation
    legacy_calc = CatenaryCalculator()
    legacy_result = legacy_calc.calculate({"d": 100, "q": 45, "F": None, "X": None})

    new_result = calculate_from_angle(d=100, q=45)

    assert new_result.arc_length == pytest.approx(legacy_result["S"])
    assert new_result.horizontal_distance == pytest.approx(legacy_result["X"])
```

#### 5.2 Integration Tests

```python
# tests/marine_engineering/catenary/test_integration.py

def test_lazy_wave_workflow():
    """Test complete lazy-wave riser analysis workflow."""
    from marine_engineering.catenary import LazyWaveSolver, LazyWaveInput
    from marine_engineering.catenary import plot_lazy_wave

    params = LazyWaveInput(...)
    solver = LazyWaveSolver()
    results = solver.solve(params)

    # Should produce same geometry as legacy implementation
    assert results.total_arc_length > 0
    assert results.horizontal_force > 0
```

---

## Implementation Steps (Detailed)

### Step 1: Create New Directory Structure (1 day)

```bash
# Create new module directory
mkdir -p src/marine_engineering/catenary
mkdir -p tests/marine_engineering/catenary

# Move Phase 1 solver
mv src/marine_engineering/mooring_analysis/catenary_solver.py \
   src/marine_engineering/catenary/solver.py

# Move tests
mv tests/marine_engineering/test_catenary_solver.py \
   tests/marine_engineering/catenary/test_solver.py
```

### Step 2: Port Simplified Methods (2 days)

**File:** `src/marine_engineering/catenary/simplified.py`

**Tasks:**
- [ ] Port `calculate_from_force()` from `catenaryMethods.py`
- [ ] Port `calculate_from_angle()` from `catenary_equation.py`
- [ ] Create dataclass inputs/outputs
- [ ] Add `CatenaryCalculator` wrapper for legacy API
- [ ] Write unit tests (compare vs legacy)
- [ ] Validate against legacy test cases

### Step 3: Port Lazy-Wave Solver (5 days)

**File:** `src/marine_engineering/catenary/lazy_wave.py`

**Tasks:**
- [ ] Port `lazyWaveCatenaryEquation()` logic
- [ ] Port `sagHogEquation()` calculations
- [ ] Convert to dataclass API
- [ ] Use Phase 1 `CatenarySolver` for segments
- [ ] Add buoyancy force calculations
- [ ] Write comprehensive tests
- [ ] Validate against legacy lazy-wave examples

### Step 4: Create Plotting Module (2 days)

**File:** `src/marine_engineering/catenary/plotting.py`

**Tasks:**
- [ ] Port `simple_catenary_plot()` to use `CatenaryResults`
- [ ] Port `lazyWavePlot()` to use `LazyWaveResults`
- [ ] Add tension distribution plot
- [ ] Add configuration comparison plots
- [ ] Write plotting tests (image comparison)

### Step 5: Update Public API (1 day)

**File:** `src/marine_engineering/catenary/__init__.py`

**Tasks:**
- [ ] Export all public classes/functions
- [ ] Add comprehensive docstrings
- [ ] Create usage examples in docstring
- [ ] Generate API documentation

### Step 6: Deprecate Legacy Modules (1 day)

**Files:** `src/digitalmodel/modules/catenary/*.py`

**Tasks:**
- [ ] Add `DeprecationWarning` to all legacy modules
- [ ] Update imports to point to new module
- [ ] Create migration guide documentation
- [ ] Add "DEPRECATED" to docstrings

### Step 7: Update Tests (2 days)

**Tasks:**
- [ ] Create legacy compatibility test suite
- [ ] Run all existing tests against new API
- [ ] Add integration tests (lazy-wave + plotting)
- [ ] Performance benchmarks (Phase 1 vs legacy)

### Step 8: Update Documentation (2 days)

**Tasks:**
- [ ] Create migration guide (`docs/catenary_migration.md`)
- [ ] Update API reference
- [ ] Add usage examples for all methods
- [ ] Create comparison table (when to use which solver)

### Step 9: Update Dependent Code (3 days)

**Files to check:**
- `catenary_riser.py` - Update imports
- `tools/doc_tools/modules/` - Reference implementations
- OrcaFlex integration - Update API calls
- Example scripts - Update to new API

---

## Risk Assessment

### Breaking Changes

| Change | Impact | Mitigation |
|--------|--------|------------|
| Import paths change | **HIGH** - All code using catenary | Deprecation warnings + backward compatibility wrappers |
| Dict API → Dataclass API | **MEDIUM** - Legacy code expects dicts | Keep `CatenaryCalculator` wrapper |
| Function signatures | **LOW** - Internal functions | Not exposed in public API |

### Backward Compatibility Strategy

1. **Keep legacy modules** with deprecation warnings (remove in v2.0)
2. **Provide wrapper classes** (`CatenaryCalculator`) that translate dict ↔ dataclass
3. **Gradual migration:** Legacy code continues working with warnings
4. **Migration guide** with 1:1 code examples

### Testing Requirements

**Mandatory tests before consolidation:**
- [ ] All legacy tests pass using new API via wrappers
- [ ] Phase 1 tests continue passing (no regression)
- [ ] Lazy-wave results match legacy within 0.1%
- [ ] Plotting produces equivalent visualizations
- [ ] Performance benchmarks (new API ≥ legacy speed)

### Rollback Plan

If consolidation fails:
1. **Keep Phase 1 in `marine_engineering.catenary.solver`**
2. **Keep legacy in `digitalmodel.subsea.catenary`**
3. **Document use cases for each**
4. **Accept dual API temporarily**

---

## Success Metrics

### Code Quality
- [ ] Test coverage ≥ 90% for new modules
- [ ] All type hints for public API
- [ ] Zero deprecation warnings in new code
- [ ] Documentation for all public functions

### Performance
- [ ] Simplified methods: <0.1ms per calculation
- [ ] Phase 1 solver: <10ms per solve
- [ ] Lazy-wave solver: <100ms per solve
- [ ] Memory usage: <10MB for typical analysis

### Validation
- [ ] Phase 1 solver: ±1% vs Excel reference
- [ ] Lazy-wave: ±0.1% vs legacy results
- [ ] Simplified methods: Exact match vs legacy

### Migration
- [ ] Migration guide covers 100% of legacy use cases
- [ ] All examples updated to new API
- [ ] Zero breaking changes for deprecated API
- [ ] Deprecation warnings in all legacy modules

---

## Timeline Estimate

| Phase | Duration | Dependencies |
|-------|----------|--------------|
| 1. Directory structure | 1 day | None |
| 2. Port simplified methods | 2 days | Phase 1 |
| 3. Port lazy-wave solver | 5 days | Phase 2 |
| 4. Create plotting module | 2 days | Phase 3 |
| 5. Update public API | 1 day | Phases 2-4 |
| 6. Deprecate legacy | 1 day | Phase 5 |
| 7. Update tests | 2 days | Phase 6 |
| 8. Update documentation | 2 days | Phase 7 |
| 9. Update dependent code | 3 days | Phase 8 |
| **TOTAL** | **19 days** | |

**Buffer for testing/validation:** +5 days
**Total with buffer:** **24 days (~1 month)**

---

## Post-Consolidation Roadmap

### Short-term (v1.1)
- [ ] Add multi-segment mooring solver (arbitrary segments)
- [ ] Add dynamic analysis (time-domain catenary)
- [ ] Integration with wave loading module

### Medium-term (v1.5)
- [ ] 3D catenary solver (out-of-plane effects)
- [ ] Seabed interaction modeling (friction, trenching)
- [ ] Soil stiffness integration

### Long-term (v2.0)
- [ ] Remove legacy modules (breaking release)
- [ ] Unified mooring analysis framework
- [ ] FEA integration (ANSYS, OrcaFlex, etc.)

---

## Appendix A: File Inventory

### Phase 1 Implementation
- `src/marine_engineering/mooring_analysis/catenary_solver.py` (306 lines)
- `tests/marine_engineering/test_catenary_solver.py` (445 lines)
- `src/marine_engineering/tests/test_mooring_catenary.py` (531 lines)

### Legacy Implementation
- `src/digitalmodel/modules/catenary/catenary.py` (36 lines)
- `src/digitalmodel/modules/catenary/catenary_equation.py` (82 lines)
- `src/digitalmodel/modules/catenary/catenaryMethods.py` (471 lines)
- `src/digitalmodel/modules/catenary/catenary_riser.py` (168 lines)
- `tests/modules/catenary_riser/test_catenary.py`
- `tests/modules/catenary_riser/test_catenary_riser.py`

### Reference/Examples
- `tools/doc_tools/modules/catenary_solver.py` (143 lines)
- `tools/doc_tools/modules/catenary_equation.py`
- `tools/doc_tools/modules/simple_catenary_*.py`
- `scripts/validate_catenary_solver.py`

### Backup/Experimental
- `src/marine_engineering/mooring_analysis/catenary_solver_backup.py`
- `src/marine_engineering/mooring_analysis/catenary_solver_fixed.py`
- `src/marine_engineering/mooring_analysis/catenary_solver_v2.py`
- `src/marine_engineering/mooring_analysis/catenary_solver_final.py`

**Total files:** 26+ catenary-related files
**Recommendation:** Archive backup/experimental files after consolidation

---

## Appendix B: Import Analysis

**Current import patterns:**

```python
# Phase 1 (NEW)
from src.marine_engineering.mooring_analysis.catenary_solver import (
    CatenaryInput, CatenaryResults, CatenarySolver
)

# Legacy
from digitalmodel.subsea.catenary.catenary_equation import CatenaryCalculator
from digitalmodel.subsea.catenary.catenaryMethods import (
    lazyWaveCatenaryEquation, simple_catenary_plot, ...
)
from digitalmodel.subsea.catenary.catenary_riser import catenary_riser
```

**No active imports found** in grep search - indicates low coupling risk!

Most usage appears to be in:
- Test files (isolated)
- Example scripts (easy to update)
- Tools/documentation (reference only)

**Conclusion:** Minimal breaking change risk for consolidation.

---

## Appendix C: Mathematical Formulation Comparison

### Phase 1: General BVP Formulation

**System of equations:**
```
1. x2 - x1 = X                    (horizontal span constraint)
2. h2 - h1 = Y                    (vertical span constraint)
3. s2 - s1 + ε = L                (total length constraint)

where:
  xi = horizontal distance from low point
  hi = a * cosh(xi/a)             (height above low point)
  si = a * sinh(xi/a)             (arc length from low point)
  ε = H * L / EA                  (elastic elongation)
  a = H / w                       (catenary parameter)
```

**Solved via:** `scipy.optimize.fsolve` (Newton-Raphson on 3 unknowns: H, x1, x2)

---

### Legacy: Simplified Formulations

**Method 1: Force-based**
```
S = d * (2F/w - d)
X = ((F/w) - d) * log((S + F/w) / ((F/w) - d))
```
*Assumes low point at anchor, no elastic elongation*

**Method 2: Angle-based**
```
BendRadius = d * cos(q) / (1 - cos(q))
S = BendRadius * tan(q)
X = BendRadius * asinh(tan(q))
```
*Geometric approach from declination angle*

**Method 3: Lazy-wave (segmented)**
```
Each segment solved independently with weight adjustment:
  - Hang-off to Sag: w_pipe
  - Sag to Hog: w_pipe + w_buoy (can be negative)
  - Hog to TDP: w_pipe
```
*Multi-segment chain with different weights per section*

---

## Appendix D: Validation Strategy

### Phase 1 Validation
- **Excel "Poly Mooring"** reference: 695 array formulas
- **Tolerance:** ±1% for tensions, ±5% for elongation
- **Test coverage:** 445 lines covering 12+ scenarios

### Legacy Validation
- **No formal validation** against standards
- **Engineering judgment** - results appear reasonable
- **OrcaFlex comparison** - informal checks

### Consolidation Validation Plan
1. **Run all legacy test cases** using new API wrappers
2. **Compare numerical results** (lazy-wave vs legacy: ±0.1%)
3. **Visual comparison** of plots (overlay old vs new)
4. **Performance benchmarks** (speed regression tests)
5. **Industry peer review** (optional - marine engineering experts)

---

## Recommendations Summary

### Immediate Actions (Week 1)
1. ✅ **Approve consolidation plan** (this document)
2. ⚠️ **Create feature branch** `feature/catenary-consolidation`
3. ⚠️ **Set up new directory** `src/marine_engineering/catenary/`
4. ⚠️ **Move Phase 1 solver** to new location

### Priority Tasks (Weeks 2-3)
1. 🔴 **Port simplified methods** with tests
2. 🔴 **Port lazy-wave solver** with validation
3. 🟡 **Create plotting module**
4. 🟡 **Update public API** and documentation

### Final Steps (Week 4)
1. 🟢 **Add deprecation warnings** to legacy modules
2. 🟢 **Create migration guide**
3. 🟢 **Update all examples**
4. 🟢 **Merge to main** after full validation

---

**Document Status:** ✅ **READY FOR REVIEW**
**Next Step:** Present to project stakeholders for approval
**Contact:** Digital Model Project Team
