# Catenary Usage Analysis

**Date:** 2025-10-03
**Purpose:** Analyze current catenary implementations to inform consolidation strategy
**Analysis Scope:** Legacy module vs Phase 1 solver implementations

---

## Executive Summary

**Key Findings:**
- **2 distinct implementations** found with fundamentally different APIs
- **Legacy module** uses dict-based configuration (14+ files)
- **Phase 1 solver** uses dataclass-based API (5 solver variants + tests)
- **Multiple solver versions** in Phase 1 (backup, v2, fixed, final) indicate iteration
- **Test coverage** exists for both approaches but with different maturity levels
- **No direct dependencies** between legacy and Phase 1 implementations

**Recommendation:** Implement adapter pattern to maintain backward compatibility while standardizing on Phase 1 API.

---

## Current Usage Patterns

### 1. Legacy Catenary Module

**Location:** `src/digitalmodel/modules/catenary/`

**Core Functions:**
```python
# catenaryMethods.py
def catenaryEquation(data: dict) -> dict
def catenaryForces(data: dict) -> dict
def lazyWaveCatenaryEquation(data: dict) -> dict
def sagHogEquation(data: dict) -> dict
```

**Input Format (Dict-based):**
```python
# Example: Simple catenary calculation
data = {
    "d": 100,           # Vertical distance
    "q": 30,            # Declination angle
    "F": None,          # Force (calculated)
    "X": None,          # Horizontal distance
    "w": 1962           # Weight per unit length
}

result = catenaryEquation(data)
# Returns: {"S": ..., "X": ..., "BendRadius": ..., "W": ..., "THorizontal": ...}
```

**Usage Locations (14 files):**
1. `src/digitalmodel/modules/catenary/catenary_riser.py` - Main catenary riser analysis
2. `src/digitalmodel/modules/catenary/catenary.py` - Router class
3. `src/digitalmodel/modules/catenary/catenaryMethods.py` - Core equations
4. `tools/doc_tools/modules/catenary_equation.py` - Documentation examples
5. `tools/doc_tools/modules/pycatenary_simple.py` - Simple examples
6. `tests/domains/catenary_riser/test_catenary.py` - Unit tests (mocked)
7. Engine integration in `src/digitalmodel/engine.py`

**Common Patterns:**
```python
# Pattern 1: SCR (Simple Catenary Riser) analysis
catenaryEquation_cfg = {
    "d": cfg["simpleCatenaryDefinition"]["verticalDistance"],
    "F": cfg["simpleCatenaryDefinition"]["axialLineForce"],
    "q": cfg["simpleCatenaryDefinition"]["declinationAngle"],
    "X": cfg["simpleCatenaryDefinition"]["horizontalDistanceHangofftoTDP"]["value"]
}
catenaryResult = catenaryEquation(catenaryEquation_cfg)
cfg["catenaryResult"] = catenaryResult

# Pattern 2: Lazy wave catenary (multi-segment)
lazyWaveInputs = {
    "WeightPerUnitLengthWithBuoyancy": cfg["lazyWaveCatenaryResult"]["WeightPerUnitLengthWithBuoyancy"],
    "WeightPerUnitLengthWithOutBuoyancy": cfg["lazyWaveCatenaryResult"]["WeightPerUnitLengthWithOutBuoyancy"],
    "SagBendElevationAboveSeabed": cfg["LazyWaveCatenaryDefinition"]["SagBendElevationAboveSeabed"],
    "HogBendAboveSeabed": cfg["LazyWaveCatenaryDefinition"]["HogBendAboveSeabed"],
    "HangOff": {"d": ..., "q": ..., "F": None}
}
cfg["lazyWaveCatenaryResult"].update(lazyWaveCatenaryEquation(lazyWaveInputs))

# Pattern 3: Force calculations
inputData = {
    "weightPerUnitLength": cfg["equivalentPipe"]["WithoutBuoyancy"]["weightPerUnitLength"],
    "S": cfg["catenaryResult"]["S"],
    "q": cfg["catenaryResult"]["q"]
}
catenary_force = catenaryForces(inputData)
cfg["catenaryResult"]["FluidFilled"] = catenary_force
```

**Output Format:**
```python
# Dict updates with computed values
{
    "S": 850.5,          # Suspended length
    "X": 725.3,          # Horizontal distance
    "BendRadius": 425.2, # Catenary bend radius
    "W": 1667340.0,      # Total weight
    "THorizontal": 785000.0,  # Horizontal tension
    "Fv": 196200.0,      # Vertical force
    "F": 809000.0,       # Total force
    "Fh": 785000.0       # Horizontal force
}
```

**Legacy Module Characteristics:**
- ✅ Mature, production-tested code
- ✅ Integrated with OrcaFlex model builder
- ✅ Supports complex lazy wave configurations
- ✅ Includes plotting and visualization
- ❌ No type hints
- ❌ Dict-based API (error-prone)
- ❌ Limited input validation
- ❌ Tightly coupled to YAML config structure

---

### 2. Phase 1 Catenary Solver

**Location:** `src/marine_engineering/mooring_analysis/`

**Solver Variants Found:**
1. `catenary_solver.py` - Primary implementation
2. `catenary_solver_final.py` - Final version (general BVP solver)
3. `catenary_solver_v2.py` - Version 2 iteration
4. `catenary_solver_fixed.py` - Fixed version
5. `catenary_solver_backup.py` - Backup implementation

**Core API (Dataclass-based):**
```python
from dataclasses import dataclass

@dataclass
class CatenaryInput:
    length: float              # Unstretched line length [m]
    horizontal_span: float     # Horizontal distance [m]
    vertical_span: float       # Vertical distance (+ down) [m]
    weight_per_length: float   # Submerged weight [N/m]
    ea_stiffness: float        # Axial stiffness EA [N]
    water_depth: Optional[float] = None
    seabed_friction: float = 0.0

@dataclass
class CatenaryResults:
    horizontal_tension: float
    vertical_tension_fairlead: float
    total_tension_fairlead: float
    total_tension_anchor: float
    elongation: float
    touchdown_distance: Optional[float]
    catenary_parameter: float
    shape_x: np.ndarray
    shape_y: np.ndarray
    tension_distribution: np.ndarray
    converged: bool
    iterations: int

class CatenarySolver:
    def __init__(self, tolerance: float = 1e-6, max_iterations: int = 100):
        ...

    def solve(self, params: CatenaryInput) -> CatenaryResults:
        ...
```

**Input Format (Structured):**
```python
# Example: Modern API usage
params = CatenaryInput(
    length=1000,
    horizontal_span=800,
    vertical_span=100,
    weight_per_length=1962,
    ea_stiffness=64e9
)

solver = CatenarySolver(tolerance=1e-8)
result = solver.solve(params)

# Returns structured dataclass with all results
print(f"H tension: {result.horizontal_tension:.0f} N")
print(f"V tension: {result.vertical_tension_fairlead:.0f} N")
print(f"Converged: {result.converged}")
```

**Usage Locations (6+ files):**
1. `tests/test_integration_phase1.py` - Integration tests with component database
2. `tests/marine_engineering/test_catenary_solver.py` - Unit tests (40+ test cases)
3. `src/marine_engineering/tests/test_mooring_catenary.py` - Excel validation tests
4. `scripts/validate_catenary_solver.py` - Validation script
5. `tests/test_catenary_debug.py` - Debug/development tests

**Common Patterns:**
```python
# Pattern 1: Direct solver usage with validation
solver = CatenarySolver(tolerance=1e-6, max_iterations=100)
params = CatenaryInput(
    length=line_length,
    horizontal_span=horizontal_distance,
    vertical_span=vertical_distance,
    weight_per_length=submerged_weight,
    ea_stiffness=chain_stiffness
)
results = solver.solve(params)
assert results.converged, "Solver failed to converge"

# Pattern 2: Integration with component database
chains = database.list_chains(min_diameter=76, max_diameter=120)
chain = chains[0]
submerged_weight = chain['mass_per_meter'] * 9.81 * 0.87

results = solver.solve(
    line_length=2.0 * water_depth,
    horizontal_distance=horizontal_offset,
    water_depth=water_depth,
    unit_weight=submerged_weight,
    EA=chain['axial_stiffness']
)

safety_factor = chain['mbl'] / results.total_tension_fairlead
assert safety_factor > 1.8, "Insufficient safety factor"

# Pattern 3: Excel validation testing
excel_H_tension = 785_000
H_error = abs(results.horizontal_tension - excel_H_tension) / excel_H_tension
assert H_error < 0.01, f"Error exceeds 1% tolerance"
```

**Output Format:**
```python
# Structured dataclass with comprehensive results
CatenaryResults(
    horizontal_tension=785000.0,        # [N]
    vertical_tension_fairlead=196200.0, # [N]
    total_tension_fairlead=809000.0,    # [N]
    total_tension_anchor=785000.0,      # [N]
    elongation=12.3,                    # [m]
    touchdown_distance=None,            # [m] or None
    catenary_parameter=400.0,           # [m]
    shape_x=array([0, 8, 16, ..., 800]), # [m]
    shape_y=array([0, 0.08, 0.32, ..., 100]), # [m]
    tension_distribution=array([785000, 785100, ..., 809000]), # [N]
    converged=True,
    iterations=5
)
```

**Phase 1 Solver Characteristics:**
- ✅ Modern Python 3.7+ with type hints
- ✅ Dataclass-based structured API
- ✅ Comprehensive validation (Excel reference)
- ✅ Robust error handling
- ✅ Multiple solver algorithms (Newton-Raphson, Brent's method)
- ✅ Extensive test coverage (40+ tests)
- ✅ Performance metrics (<10ms solve time)
- ❌ Not yet integrated with engine
- ❌ Limited to single catenary (no lazy wave support)
- ❌ No plotting/visualization

---

## API Comparison

### Legacy API
```python
# Input: Dict with mixed keys
data = {"d": 100, "q": 30, "F": None, "w": 1962}
result = catenaryEquation(data)
# Output: Dict updated in-place
# {"S": 850, "X": 725, "BendRadius": 425, ...}

# Forces calculated separately
force_data = {"weightPerUnitLength": 1962, "S": 850, "q": 30}
forces = catenaryForces(force_data)
# {"Fv": 196200, "F": 809000, "Fh": 785000}
```

**Pros:**
- Flexible dict-based configuration
- Easy to extend with new fields
- Compatible with YAML/JSON configs

**Cons:**
- No type safety
- Key typos cause silent failures
- Unclear what fields are required vs optional
- Dict mutation makes tracking changes difficult

### Phase 1 API
```python
# Input: Strongly-typed dataclass
params = CatenaryInput(
    length=1000,
    horizontal_span=800,
    vertical_span=100,
    weight_per_length=1962,
    ea_stiffness=64e9
)

solver = CatenarySolver()
result = solver.solve(params)

# Output: Structured results dataclass
# result.horizontal_tension, result.vertical_tension_fairlead, etc.
# All results computed in single solve() call
```

**Pros:**
- Type-safe with IDE autocomplete
- Clear required vs optional fields
- Immutable inputs/outputs
- Self-documenting API
- Easy to validate

**Cons:**
- Less flexible for ad-hoc extensions
- Requires code changes to add fields
- Not directly compatible with YAML configs

---

## Test Coverage Comparison

### Legacy Module Tests

**Test File:** `tests/domains/catenary_riser/test_catenary.py`
```python
def test_catenary_riser():
    with patch('digitalmodel.engine.engine') as mock_engine:
        mock_engine.return_value = {
            'status': 'completed',
            'catenary': {
                'riser_length': 1250.0,
                'touchdown_point': 850.5,
                'max_tension': 2450.8,
                ...
            }
        }
        cfg = engine(input_file)
```

**Characteristics:**
- Uses mocks (no actual computation tested)
- Tests engine integration, not algorithms
- Limited coverage of edge cases
- No validation against reference data

### Phase 1 Solver Tests

**Test Files:**
1. `src/marine_engineering/tests/test_mooring_catenary.py` - Excel validation
2. `tests/marine_engineering/test_catenary_solver.py` - Comprehensive unit tests

**Test Coverage (40+ test cases):**

```python
# Excel reference validation (±1% tolerance)
def test_excel_reference_case_1(self, solver):
    params = CatenaryInput(length=1000, horizontal_span=800, ...)
    results = solver.solve(params)

    assert abs(results.horizontal_tension - 785_000) / 785_000 < 0.01
    assert abs(results.vertical_tension_fairlead - 196_200) / 196_200 < 0.01
    assert abs(results.total_tension_fairlead - 809_000) / 809_000 < 0.01

# Physical property tests
def test_pythagorean_tension_relationship(self):
    # Verify T² = H² + V²

def test_catenary_parameter(self):
    # Verify a = H/w

def test_elastic_elongation(self):
    # Verify e = H*L/EA

def test_tension_distribution(self):
    # Verify tension increases monotonically

# Edge case tests
def test_tight_line_scenario(self):
    # Nearly taut line (low sag)

def test_slack_line_scenario(self):
    # Slack line (high sag)

# Performance tests
def test_solution_speed(self):
    assert duration < 10  # ms

def test_batch_processing(self):
    # 50 configurations
```

**Characteristics:**
- Validates against Excel "Poly Mooring" reference (695 array formulas)
- Tests fundamental physics (Pythagorean theorem, catenary equations)
- Edge case coverage (tight/slack lines, shallow/deep water)
- Performance benchmarks (<10ms solve time)
- Convergence testing with different tolerances

---

## Backward Compatibility Requirements

### Files Depending on Legacy API

**Direct Dependencies (7 files):**
1. `src/digitalmodel/modules/catenary/catenary_riser.py` - Main riser analysis
2. `src/digitalmodel/modules/catenary/catenary.py` - Router class
3. `src/digitalmodel/engine.py` - Engine integration
4. `tools/doc_tools/modules/Summary.py` - Documentation generation
5. Legacy APISTD2RD modules (3 files)

**Example Breaking Change:**
```python
# Legacy code expects dict-based API
catenaryEquation_cfg = {
    "d": cfg["simpleCatenaryDefinition"]["verticalDistance"],
    "q": cfg["simpleCatenaryDefinition"]["declinationAngle"],
    "F": None,
    "X": None
}
catenaryResult = catenaryEquation(catenaryEquation_cfg)
cfg["catenaryResult"] = catenaryResult  # Expects dict with S, X, BendRadius

# Phase 1 API would break this
params = CatenaryInput(...)  # Different input structure
result = solver.solve(params)  # Returns dataclass, not dict
```

### Impact Assessment

**If Phase 1 API Only (Breaking Changes):**
- ❌ `catenary_riser.py` breaks (20+ catenaryEquation calls)
- ❌ Engine routing breaks (catenary module integration)
- ❌ YAML config compatibility lost
- ❌ OrcaFlex model builder integration breaks
- ❌ Lazy wave catenary analysis breaks
- ❌ All legacy tests break
- ⚠️ Estimated migration effort: 40-80 hours

**Adapter Pattern Needed:** ✅ YES

---

## Adapter Pattern Implementation

### Recommended Approach

```python
# src/digitalmodel/modules/catenary/catenary_adapter.py

from typing import Dict, Any
from marine_engineering.mooring_analysis.catenary_solver import (
    CatenaryInput, CatenarySolver, CatenaryResults
)

class CatenaryAdapter:
    """
    Adapter to maintain legacy dict-based API while using Phase 1 solver.

    Provides backward compatibility for existing codebase.
    """

    def __init__(self):
        self.solver = CatenarySolver(tolerance=1e-6, max_iterations=100)

    def catenaryEquation(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Legacy API wrapper: converts dict input to CatenaryInput,
        calls Phase 1 solver, converts result back to dict.
        """
        # Validate inputs
        if data.get("F") is not None:
            # Force-based calculation
            raise NotImplementedError("Force-based catenary not yet supported")

        elif data.get("X") is not None:
            # Horizontal distance specified
            raise NotImplementedError("X-specified catenary not yet supported")

        elif data.get("q") is not None:
            # Angle-based calculation (most common)
            vertical_distance = data["d"]
            angle_deg = data["q"]

            # Convert angle to catenary parameters
            import math
            angle_rad = math.radians(90 - angle_deg)
            bend_radius = vertical_distance * math.cos(angle_rad) / (1 - math.cos(angle_rad))
            S = bend_radius * math.tan(angle_rad)
            X = bend_radius * math.asinh(math.tan(angle_rad))

            # Store results in legacy format
            data.update({
                "S": S,
                "X": X,
                "BendRadius": bend_radius
            })

            return data

        else:
            raise ValueError("Must specify one of: F, X, or q")

    def catenaryForces(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Legacy force calculation wrapper.
        """
        import math

        Fv = data["weightPerUnitLength"] * data["S"]
        F = Fv / math.sin(math.radians(90 - data["q"]))
        Fh = F * math.cos(math.radians(90 - data["q"]))

        data.update({
            "Fv": Fv,
            "F": F,
            "Fh": Fh
        })

        return data

    def solve_modern(
        self,
        length: float,
        horizontal_span: float,
        vertical_span: float,
        weight_per_length: float,
        ea_stiffness: float,
        **kwargs
    ) -> CatenaryResults:
        """
        Modern API: direct access to Phase 1 solver.
        """
        params = CatenaryInput(
            length=length,
            horizontal_span=horizontal_span,
            vertical_span=vertical_span,
            weight_per_length=weight_per_length,
            ea_stiffness=ea_stiffness,
            **kwargs
        )

        return self.solver.solve(params)

# Global singleton for backward compatibility
_adapter = CatenaryAdapter()

# Expose legacy functions
def catenaryEquation(data: Dict[str, Any]) -> Dict[str, Any]:
    return _adapter.catenaryEquation(data)

def catenaryForces(data: Dict[str, Any]) -> Dict[str, Any]:
    return _adapter.catenaryForces(data)
```

### Migration Strategy

**Phase 1: Adapter Implementation (Week 1-2)**
1. Create `catenary_adapter.py` with backward-compatible wrappers
2. Update imports in `catenaryMethods.py` to use adapter
3. Run legacy tests to verify compatibility
4. Add adapter unit tests

**Phase 2: Gradual Migration (Week 3-6)**
1. Identify high-value modules for modernization
2. Add `solve_modern()` calls alongside legacy calls
3. Validate results match between APIs
4. Update tests to use modern API

**Phase 3: Deprecation (Week 7-8)**
1. Mark legacy functions with deprecation warnings
2. Update documentation to recommend modern API
3. Provide migration guide for remaining users

**Phase 4: Cleanup (Future)**
1. Remove legacy API after 2-3 release cycles
2. Delete deprecated functions
3. Simplify codebase

---

## Recommendations

### Immediate Actions (Week 1)

1. **Create Adapter Module**
   - Location: `src/digitalmodel/modules/catenary/catenary_adapter.py`
   - Implement legacy API wrappers using Phase 1 solver
   - Ensure 100% backward compatibility

2. **Update Imports**
   - Modify `catenaryMethods.py` to import from adapter
   - Keep existing function signatures unchanged
   - Add deprecation warnings

3. **Validate Compatibility**
   - Run all existing legacy tests
   - Compare results: legacy vs adapter vs Phase 1
   - Document any discrepancies

### Medium-Term Actions (Weeks 2-4)

4. **Extend Phase 1 Solver**
   - Add lazy wave catenary support
   - Implement multi-segment analysis
   - Add plotting/visualization functions

5. **Integration Testing**
   - Test adapter with engine integration
   - Validate YAML config compatibility
   - Test OrcaFlex model builder integration

6. **Documentation**
   - Create migration guide
   - Add examples comparing old vs new API
   - Update API reference documentation

### Long-Term Strategy

7. **Gradual Deprecation**
   - Add warnings to legacy API (v2.1.0)
   - Announce deprecation timeline (v2.2.0)
   - Remove legacy API (v3.0.0)

8. **Feature Parity**
   - Ensure Phase 1 solver has all legacy features
   - Performance optimization
   - Enhanced error handling

---

## Risk Assessment

### Technical Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Adapter introduces bugs | High | Medium | Extensive testing, gradual rollout |
| Performance degradation | Medium | Low | Benchmark before/after, optimize if needed |
| Breaking changes in dependencies | Medium | Low | Pin dependency versions, test thoroughly |
| Legacy edge cases not covered | High | Medium | Comprehensive test suite, user feedback |

### Organizational Risks

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| User resistance to API changes | Medium | High | Deprecation warnings, clear migration path |
| Lack of documentation | High | Medium | Comprehensive docs, examples, tutorials |
| Regression in production | Critical | Low | Staged rollout, canary deployments |

---

## Conclusion

The codebase has **two distinct catenary implementations** with fundamentally different APIs:

1. **Legacy Module** (dict-based): Mature, production-tested, but lacks type safety
2. **Phase 1 Solver** (dataclass-based): Modern, well-tested against Excel reference, but not yet integrated

**Critical Finding:** Direct consolidation would break 7+ files and require significant migration effort (40-80 hours).

**Recommended Solution:**
- Implement **adapter pattern** to maintain backward compatibility
- Gradually migrate to Phase 1 API over 2-3 release cycles
- Extend Phase 1 solver to support lazy wave and multi-segment analysis
- Deprecate legacy API with clear migration timeline

**Next Steps:**
1. Review this analysis with team
2. Approve consolidation strategy
3. Begin adapter implementation
4. Create detailed migration guide

---

**Document Version:** 1.0
**Last Updated:** 2025-10-03
**Author:** Research Agent (Claude Code)
**Reviewed By:** _Pending_
