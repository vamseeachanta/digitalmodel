# Catenary Solver Specification

**Module**: Mooring Analysis
**Component**: Catenary Solver
**Status**: Planned 📋
**Source**: Excel "Poly Mooring" (695 array formulas)
**Priority**: P1 - Critical Path

---

## Overview

Advanced quasi-static catenary solver for mooring line analysis implementing analytical catenary equations with numerical iteration for boundary value problems. Converts Excel array formula implementation to vectorized Python with multi-segment support and elastic elongation.

### Purpose
Calculate mooring line shape, tension distribution, and restoring forces for suspended cables under self-weight and pretension, supporting both infinite and finite water depth scenarios.

### Scope
- Single-segment uniform catenary analysis
- Multi-segment composite mooring lines
- Elastic elongation integration
- Touchdown point calculations
- Stiffness matrix for coupled dynamics

---

## Mathematical Foundation

### Classical Catenary Equation

For a uniform cable suspended between two points under its own weight:

**Catenary Shape:**
```
y(x) = a * cosh(x/a) - a
```

Where:
- `a = H / w` (catenary parameter)
- `H` = horizontal tension component [N]
- `w` = submerged weight per unit length [N/m]
- `x` = horizontal distance from low point [m]
- `y` = vertical distance from low point [m]

**Arc Length:**
```
s(x) = a * sinh(x/a)
```

**Tension at Point:**
```
T(x) = w * sqrt(a² + y(x)²) = sqrt(H² + (w*y)²)
```

### Boundary Value Problem

Given:
- Total line length `L` (unstretched)
- Horizontal span `X_span`
- Vertical span `Y_span` (fairlead depth - anchor depth)
- Weight per length `w`
- Axial stiffness `EA`

Find: Horizontal tension `H` such that:
```
s(X_span) + elongation(H) = L
```

Where:
```
elongation = H * L / EA
```

### Excel Implementation (Extracted)

The Excel "Poly Mooring" sheet uses array formulas with iterative solvers:

```excel
// Catenary parameter
a = H / w

// Arc length at horizontal span
s = a * SINH(X_span / a)

// Elastic elongation
e = H * L / EA

// Length error for iteration
error = s + e - L

// Solver: Goal Seek or VBA iteration to minimize error
```

**Excel Convergence:** Uses Excel Solver or Goal Seek with tolerance 1e-6

---

## Python Implementation Design

### Core Solver Class

```python
from dataclasses import dataclass
from typing import Optional, Tuple
import numpy as np
from scipy.optimize import newton, brentq

@dataclass
class CatenaryInput:
    """Input parameters for catenary analysis."""
    length: float              # Unstretched line length [m]
    horizontal_span: float     # Horizontal distance [m]
    vertical_span: float       # Vertical distance (+ down) [m]
    weight_per_length: float   # Submerged weight [N/m]
    ea_stiffness: float        # Axial stiffness EA [N]
    water_depth: float = None  # Optional for touchdown analysis [m]
    seabed_friction: float = 0.0  # Seabed friction coefficient [-]

@dataclass
class CatenaryResults:
    """Results from catenary analysis."""
    horizontal_tension: float       # H at anchor [N]
    vertical_tension_fairlead: float  # V at fairlead [N]
    total_tension_fairlead: float    # T at fairlead [N]
    total_tension_anchor: float      # T at anchor [N]
    elongation: float                # Elastic stretch [m]
    touchdown_distance: float        # Distance to touchdown [m]
    catenary_parameter: float        # a = H/w [m]
    shape_x: np.ndarray             # Line shape coordinates
    shape_y: np.ndarray
    tension_distribution: np.ndarray  # Tension along line
    converged: bool                  # Solver convergence flag
    iterations: int                  # Solver iteration count

class CatenarySolver:
    """
    Advanced catenary solver for mooring line analysis.

    Implements analytical catenary equations with numerical
    iteration for boundary value problems. Handles elastic
    elongation and multi-segment lines.
    """

    def __init__(self, tolerance: float = 1e-6, max_iterations: int = 100):
        """
        Initialize catenary solver.

        Parameters
        ----------
        tolerance : float
            Convergence tolerance for length error [m]
        max_iterations : int
            Maximum iterations for Newton-Raphson solver
        """
        self.tolerance = tolerance
        self.max_iterations = max_iterations
        self.gravity = 9.8065  # m/s²

    def solve(self, params: CatenaryInput) -> CatenaryResults:
        """
        Solve catenary boundary value problem.

        Finds horizontal tension H that satisfies length constraint
        using Newton-Raphson iteration.

        Parameters
        ----------
        params : CatenaryInput
            Catenary input parameters

        Returns
        -------
        results : CatenaryResults
            Complete catenary solution
        """
        # Define length error function
        def length_error(H: float) -> float:
            """Difference between calculated and target length."""
            a = H / params.weight_per_length

            # Arc length from catenary equation
            s = a * np.sinh(params.horizontal_span / a)

            # Elastic elongation
            elongation = H * params.length / params.ea_stiffness

            # Total length error
            return (s + elongation) - params.length

        # Define derivative for Newton-Raphson (improves convergence)
        def length_error_derivative(H: float) -> float:
            """Derivative of length error w.r.t. H."""
            a = H / params.weight_per_length
            x = params.horizontal_span

            # d(arc_length)/dH
            ds_dH = (x / params.weight_per_length) * (
                np.cosh(x/a) - (x/(2*a)) * np.sinh(x/a)
            )

            # d(elongation)/dH
            de_dH = params.length / params.ea_stiffness

            return ds_dH + de_dH

        # Initial guess for horizontal tension
        # Use simple estimation: H ≈ w * X_span / 2
        H_initial = params.weight_per_length * params.horizontal_span / 2

        # Solve using Newton-Raphson
        try:
            H_solution = newton(
                length_error,
                x0=H_initial,
                fprime=length_error_derivative,
                tol=self.tolerance,
                maxiter=self.max_iterations,
                full_output=False
            )
            converged = True
            iterations = None  # Newton doesn't return this

        except RuntimeError:
            # Fallback to Brent's method if Newton fails
            # Brent's method is more robust but slower
            H_min = 1.0  # Minimum realistic tension
            H_max = params.length * params.weight_per_length * 10

            H_solution, result = brentq(
                length_error,
                a=H_min,
                b=H_max,
                xtol=self.tolerance,
                maxiter=self.max_iterations,
                full_output=True
            )
            converged = result.converged
            iterations = result.iterations

        # Calculate all results from solution
        return self._compute_results(params, H_solution, converged, iterations)

    def _compute_results(
        self,
        params: CatenaryInput,
        H: float,
        converged: bool,
        iterations: Optional[int]
    ) -> CatenaryResults:
        """Compute all catenary results from horizontal tension."""

        # Catenary parameter
        a = H / params.weight_per_length

        # Vertical tension at fairlead
        V_fairlead = H * np.sinh(params.horizontal_span / a)

        # Total tension at fairlead
        T_fairlead = np.sqrt(H**2 + V_fairlead**2)

        # Total tension at anchor (just horizontal component)
        T_anchor = H

        # Elastic elongation
        elongation = H * params.length / params.ea_stiffness

        # Touchdown distance (if line reaches seabed)
        if params.water_depth is not None:
            touchdown = self._calculate_touchdown(
                H, params.weight_per_length, params.water_depth
            )
        else:
            touchdown = None

        # Calculate line shape
        n_points = 100
        x = np.linspace(0, params.horizontal_span, n_points)
        y = a * (np.cosh(x / a) - 1)

        # Tension distribution along line
        tension_dist = np.sqrt(H**2 + (params.weight_per_length * y)**2)

        return CatenaryResults(
            horizontal_tension=H,
            vertical_tension_fairlead=V_fairlead,
            total_tension_fairlead=T_fairlead,
            total_tension_anchor=T_anchor,
            elongation=elongation,
            touchdown_distance=touchdown,
            catenary_parameter=a,
            shape_x=x,
            shape_y=y,
            tension_distribution=tension_dist,
            converged=converged,
            iterations=iterations if iterations else 0
        )

    def _calculate_touchdown(
        self,
        H: float,
        w: float,
        water_depth: float
    ) -> float:
        """
        Calculate horizontal distance to seabed touchdown point.

        Solves for x where y(x) = water_depth
        """
        a = H / w

        # Invert catenary equation: x = a * acosh(y/a + 1)
        if water_depth / a + 1 > 1:
            x_touchdown = a * np.arccosh(water_depth / a + 1)
            return x_touchdown
        else:
            return 0.0  # Line doesn't reach seabed
```

---

## Multi-Segment Catenary Analysis

### Composite Mooring Line

Realistic mooring lines consist of multiple segments with different properties:
- Chain at fairlead
- Wire rope midspan
- Chain at anchor

### Multi-Segment Algorithm

```python
@dataclass
class LineSegment:
    """Individual segment of composite mooring line."""
    length: float               # Unstretched length [m]
    weight_per_length: float    # Submerged weight [N/m]
    ea_stiffness: float        # Axial stiffness [N]
    segment_type: str          # "chain", "wire", "synthetic"

class MultiSegmentCatenary:
    """Solver for composite multi-segment mooring lines."""

    def __init__(self):
        self.single_solver = CatenarySolver()

    def solve_composite_line(
        self,
        segments: list[LineSegment],
        horizontal_span: float,
        vertical_span: float
    ) -> dict:
        """
        Solve composite mooring line with multiple segments.

        Approach:
        1. Assume horizontal tension H (constant along line)
        2. Solve each segment's catenary with known H
        3. Sum total horizontal and vertical spans
        4. Iterate H until spans match boundary conditions

        This is more complex than single segment and requires
        iterative solution of system of equations.
        """

        def span_error(H: float) -> Tuple[float, float]:
            """Calculate error in horizontal and vertical spans."""
            total_x = 0.0
            total_y = 0.0

            for segment in segments:
                # Solve individual segment catenary
                # This gets complex - see detailed implementation
                pass

            x_error = total_x - horizontal_span
            y_error = total_y - vertical_span

            return x_error, y_error

        # Use 2D Newton-Raphson or least squares optimization
        # to find H that satisfies both span constraints
        pass
```

**Note:** Multi-segment implementation is complex and planned for Phase 2.

---

## Validation Against Excel

### Test Cases from Excel

Excel "Poly Mooring" sheet provides reference cases:

**Test Case 1: Single Chain Segment**
```yaml
input:
  length: 1000          # m
  horizontal_span: 800  # m
  vertical_span: 100    # m (fairlead depth)
  weight_per_length: 1962  # N/m (76mm chain in water)
  ea_stiffness: 64e9    # N (studlink chain)

expected_excel_results:
  horizontal_tension: 785_000  # N (±1%)
  vertical_tension: 196_200    # N
  total_tension: 809_000       # N
  elongation: 12.3             # m
```

**Validation Criteria:**
```python
def test_against_excel_case_1():
    """Validate Python solver against Excel reference."""
    params = CatenaryInput(
        length=1000,
        horizontal_span=800,
        vertical_span=100,
        weight_per_length=1962,
        ea_stiffness=64e9
    )

    solver = CatenarySolver()
    results = solver.solve(params)

    # Excel reference values
    assert abs(results.horizontal_tension - 785_000) / 785_000 < 0.01
    assert abs(results.vertical_tension_fairlead - 196_200) / 196_200 < 0.01
    assert results.converged == True
```

---

## Performance Requirements

- **Single Segment**: <10ms solution time
- **Convergence**: <10 iterations typical
- **Accuracy**: Within 0.1% of Excel results
- **Robustness**: Handle extreme geometries (shallow/steep lines)

---

## Integration Points

### Ship Dynamics Module
- Provides mooring restoring forces vs vessel offset
- Linearized stiffness matrix for coupled analysis

### OrcaFlex Export
- Line shape initial conditions
- Pretension values for dynamic analysis

### Component Database
- Load segment properties (weight, EA) by specification

---

## Future Enhancements

- **Dynamic Catenary**: Time-varying tension analysis
- **Seabed Interaction**: Friction and embedment models
- **Current Effects**: Hydrodynamic drag on suspended line
- **3D Catenary**: Out-of-plane effects for spread moorings

---

*Catenary solver specification based on Excel "Poly Mooring" with 695 formulas converted to Python analytical solution.*
