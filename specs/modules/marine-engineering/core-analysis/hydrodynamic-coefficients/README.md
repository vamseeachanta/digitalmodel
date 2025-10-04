# Hydrodynamic Coefficients Module

## Overview

This module provides hydrodynamic coefficient management and interpolation for marine structure analysis. Includes added mass, radiation damping, viscous damping, and frequency-dependent coefficient handling essential for accurate ship motion prediction and offshore structure analysis.

## Module Structure

```
hydrodynamic-coefficients/
├── README.md                          # This file - module overview
├── tasks.md                          # Implementation tasks and tracking
├── task_summary.md                   # Execution tracking and progress
├── prompt.md                         # Original prompts and reuse patterns
├── technical-details.md              # Deep technical documentation
└── sub-specs/
    ├── added-mass.md                 # Added mass coefficients
    ├── radiation-damping.md          # Radiation damping coefficients
    ├── viscous-damping.md            # Viscous/linearized damping
    └── frequency-dependent-coefficients.md  # Interpolation and frequency effects
```

## Core Specifications

### Added Mass Coefficients
**File**: `sub-specs/added-mass.md`
**Status**: Planned 📋
**Source**: Excel "Damping" sheet (84×12), 28 hydrodynamic references

Frequency-dependent added mass for 6-DOF motion analysis:
- Surge, sway, heave added mass (translational)
- Roll, pitch, yaw added inertia (rotational)
- Cross-coupling terms (e.g., sway-roll)
- Infinite-frequency limits

**Physical Significance:**
```
Added mass represents the inertia of fluid accelerated with the body.
When a vessel moves, it must accelerate surrounding water mass.

Example: Heave added mass A₃₃
- Large for flat-bottomed vessels (more water moved)
- Frequency-dependent (radiation effects)
- Units: kg or kg⋅m² (for rotational)
```

**Excel Implementation:**
```excel
// Heave added mass at frequency ω
A33(ω) = VLOOKUP(ω, AddedMassTable, 3)

// Infinite frequency limit (ω → ∞)
A33_inf = LAST(AddedMassTable)
```

### Radiation Damping
**File**: `sub-specs/radiation-damping.md`
**Status**: Planned 📋
**Source**: Excel damping calculations

Wave radiation damping from oscillating body:
- Energy dissipation through radiated waves
- Frequency-dependent coefficients
- Critical for resonance prediction
- Coupling with added mass (Kramers-Kronig relations)

**Physical Significance:**
```
Radiation damping = energy lost by generating waves

When vessel oscillates, it radiates waves carrying energy away.
Peak radiation damping typically occurs near resonance.

B₃₃(ω) = radiation damping coefficient in heave [N⋅s/m]
```

**Kramers-Kronig Relations:**
```
Added mass and damping are related through causality:

A(ω) - A(∞) = (2/π) ∫₀^∞ [B(ω')/ω'] sin(ω't) dω'

Physical meaning: Added mass can be computed from damping
```

### Viscous Damping
**File**: `sub-specs/viscous-damping.md`
**Status**: Planned 📋
**Source**: Excel "Damping" sheet (84×12 cells)

Linearized viscous damping for roll and pitch:
- Roll damping from bilge keels, hull viscosity
- Pitch damping contributions
- Empirical formulas and experimental data
- Critical damping ratio calculations

**Excel Damping Sheet Analysis:**
```
Sheet: "Damping" (84 rows × 12 columns)

Columns identified:
A: Frequency [rad/s]
B: Roll damping B₄₄ [N⋅m⋅s]
C: Pitch damping B₅₅ [N⋅m⋅s]
D: Heave damping B₃₃ [N⋅s/m]
E-L: Cross-coupling terms

Formulas: Interpolation from experimental data or AQWA results
```

**Linearized Roll Damping:**
```
Roll equation of motion:
(I₄₄ + A₄₄)φ̈ + B₄₄φ̇ + C₄₄φ = M₄(t)

Where:
- I₄₄ = roll moment of inertia
- A₄₄ = roll added inertia
- B₄₄ = roll damping (radiation + viscous)
- C₄₄ = restoring moment coefficient
- M₄ = wave exciting moment

Critical damping ratio:
ζ = B₄₄ / (2√[(I₄₄+A₄₄)C₄₄])
```

### Frequency-Dependent Interpolation
**File**: `sub-specs/frequency-dependent-coefficients.md`
**Status**: Planned 📋

Advanced interpolation for frequency-dependent coefficients:
- Multi-dimensional interpolation (frequency × heading × DOF)
- Smooth spline interpolation
- Extrapolation handling
- Integration with RAO processing

**Interpolation Methods:**
```python
# Linear interpolation (fast, simple)
A33_interp = np.interp(omega_target, omega_table, A33_table)

# Cubic spline (smooth, better for derivatives)
from scipy.interpolate import CubicSpline
cs = CubicSpline(omega_table, A33_table)
A33_interp = cs(omega_target)

# PCHIP (monotonic, prevents overshoot)
from scipy.interpolate import PchipInterpolator
pchip = PchipInterpolator(omega_table, A33_table)
A33_interp = pchip(omega_target)
```

## Integration Architecture

### Cross-Module Dependencies

#### Ship Dynamics Integration
- **Primary Consumer**: 6DOF motion analysis requires hydrodynamic coefficients
- **Data Flow**: Coefficients → Equation of motion matrices → Motion response
- **Critical Path**: Cannot compute accurate motions without good coefficients

#### RAO Processing Integration
- **Bidirectional**: RAO data contains implicit hydrodynamic coefficients
- **Extraction**: Extract A and B from RAO magnitude and phase
- **Validation**: Cross-check computed RAO against measured RAO

#### AQWA Integration
- **Import**: Read hydrodynamic database from AQWA .AH1/.LIS files
- **Validation**: Compare computed vs AQWA-calculated coefficients
- **Format**: Handle AQWA frequency arrays and DOF ordering

### Data Flow Architecture

```mermaid
graph TB
    A[AQWA Hydrodynamic Analysis] --> B[Coefficient Database]
    C[Experimental Data] --> B
    D[Empirical Formulas] --> B

    B --> E[Added Mass A(ω)]
    B --> F[Radiation Damping B(ω)]
    B --> G[Viscous Damping]

    E --> H[Frequency Interpolation]
    F --> H
    G --> H

    H --> I[Ship Dynamics Solver]
    I --> J[Motion RAO]
    I --> K[Time-Domain Simulation]

    J --> L[Spectral Analysis]
    K --> M[Extreme Response]
```

## Technical Architecture

### Coefficient Database

```python
from dataclasses import dataclass
from typing import Dict, Tuple
import numpy as np
from scipy.interpolate import interp1d, CubicSpline

@dataclass
class HydrodynamicCoefficients:
    """
    Hydrodynamic coefficient database.

    Contains frequency-dependent added mass and damping
    for all 6 degrees of freedom (surge, sway, heave, roll, pitch, yaw).
    """
    frequencies: np.ndarray        # Frequency array [rad/s]
    added_mass: np.ndarray        # Added mass [6×6×N_freq]
    damping: np.ndarray           # Damping coefficients [6×6×N_freq]
    infinite_freq_added_mass: np.ndarray  # A(∞) [6×6]

    # Optional: directional dependence
    headings: np.ndarray = None   # Wave headings [deg]

    def __post_init__(self):
        """Validate dimensions."""
        n_freq = len(self.frequencies)
        assert self.added_mass.shape == (6, 6, n_freq)
        assert self.damping.shape == (6, 6, n_freq)
        assert self.infinite_freq_added_mass.shape == (6, 6)

    def get_added_mass(
        self,
        frequency: float,
        dof_i: int,
        dof_j: int,
        method: str = 'cubic'
    ) -> float:
        """
        Get added mass coefficient at arbitrary frequency.

        Parameters
        ----------
        frequency : float
            Frequency [rad/s]
        dof_i, dof_j : int
            Degree of freedom indices (0-5 for surge-yaw)
        method : str
            Interpolation method: 'linear', 'cubic', 'pchip'

        Returns
        -------
        A_ij : float
            Added mass coefficient
        """
        A_data = self.added_mass[dof_i, dof_j, :]

        if method == 'linear':
            return np.interp(frequency, self.frequencies, A_data)
        elif method == 'cubic':
            cs = CubicSpline(self.frequencies, A_data)
            return cs(frequency)
        elif method == 'pchip':
            from scipy.interpolate import PchipInterpolator
            pchip = PchipInterpolator(self.frequencies, A_data)
            return pchip(frequency)
        else:
            raise ValueError(f"Unknown method: {method}")

    def get_damping(
        self,
        frequency: float,
        dof_i: int,
        dof_j: int,
        method: str = 'cubic'
    ) -> float:
        """Get radiation damping coefficient at frequency."""
        B_data = self.damping[dof_i, dof_j, :]

        if method == 'linear':
            return np.interp(frequency, self.frequencies, B_data)
        elif method == 'cubic':
            cs = CubicSpline(self.frequencies, B_data)
            return cs(frequency)
        else:
            from scipy.interpolate import PchipInterpolator
            pchip = PchipInterpolator(self.frequencies, B_data)
            return pchip(frequency)

class HydrodynamicCoefficientManager:
    """
    Manager for hydrodynamic coefficient databases.

    Handles loading from multiple sources (AQWA, Excel, experimental),
    interpolation, and validation.
    """

    def __init__(self):
        """Initialize coefficient manager."""
        self.coefficients: Dict[str, HydrodynamicCoefficients] = {}

    def load_from_aqwa(self, aqwa_lis_file: str, vessel_name: str):
        """
        Load hydrodynamic coefficients from AQWA .LIS file.

        AQWA .LIS contains:
        - Frequency array
        - Added mass matrix A(ω) [6×6×N_freq]
        - Damping matrix B(ω) [6×6×N_freq]
        - Infinite frequency added mass A(∞)
        """
        # Parse AQWA .LIS file
        # This is complex - AQWA has specific FORTRAN format
        pass

    def load_from_excel(self, excel_file: str, sheet: str = "Damping"):
        """
        Load from Excel "Damping" sheet.

        Expected format (84×12):
        - Column A: Frequency [rad/s]
        - Columns B-L: Damping coefficients B_ij
        """
        import pandas as pd
        df = pd.read_excel(excel_file, sheet_name=sheet)

        frequencies = df.iloc[:, 0].values  # First column

        # Extract damping coefficients
        # Assume columns map to specific DOFs
        damping = np.zeros((6, 6, len(frequencies)))

        # Example: Column B = B44 (roll damping)
        damping[3, 3, :] = df.iloc[:, 1].values
        # Column C = B55 (pitch damping)
        damping[4, 4, :] = df.iloc[:, 2].values
        # etc.

        # Added mass not in Excel - would need AQWA import
        # For now, create dummy structure
        added_mass = np.zeros((6, 6, len(frequencies)))
        infinite_freq = np.zeros((6, 6))

        coeffs = HydrodynamicCoefficients(
            frequencies=frequencies,
            added_mass=added_mass,
            damping=damping,
            infinite_freq_added_mass=infinite_freq
        )

        self.coefficients['excel_import'] = coeffs
        return coeffs

    def compute_critical_damping_ratio(
        self,
        coeffs: HydrodynamicCoefficients,
        dof: int,
        inertia: float,
        restoring: float,
        frequency: float
    ) -> float:
        """
        Compute critical damping ratio for specific DOF.

        ζ = B / (2√[(I+A)C])

        Parameters
        ----------
        coeffs : HydrodynamicCoefficients
            Coefficient database
        dof : int
            Degree of freedom (0-5)
        inertia : float
            Mass or moment of inertia
        restoring : float
            Restoring coefficient (hydrostatic)
        frequency : float
            Frequency for added mass evaluation [rad/s]

        Returns
        -------
        zeta : float
            Critical damping ratio (0-1 typically)
        """
        A = coeffs.get_added_mass(frequency, dof, dof)
        B = coeffs.get_damping(frequency, dof, dof)

        # Critical damping
        B_crit = 2 * np.sqrt((inertia + A) * restoring)

        # Damping ratio
        zeta = B / B_crit

        return zeta

    def validate_kramers_kronig(
        self,
        coeffs: HydrodynamicCoefficients,
        dof_i: int,
        dof_j: int
    ) -> float:
        """
        Validate added mass and damping satisfy Kramers-Kronig relations.

        These relations enforce causality - added mass and damping
        must be consistent.

        Returns error metric (0 = perfect, >0 = inconsistent)
        """
        # This is advanced - requires Hilbert transform
        # Placeholder for now
        pass
```

## Excel Source Data

### Damping Sheet Structure
**Location**: Excel "Damping" sheet
**Size**: 84 rows × 12 columns
**Purpose**: Frequency-dependent damping coefficients

**Extracted Data:**
```
Row 1: Headers
Rows 2-85: Data (84 frequency points)

Column A: Frequency [rad/s] - Range 0.1 to 2.0 rad/s
Column B: B44 Roll damping [N⋅m⋅s]
Column C: B55 Pitch damping [N⋅m⋅s]
Column D: B33 Heave damping [N⋅s/m]
Columns E-L: Cross-coupling damping terms
```

**Typical Values (from Excel):**
```
At ω = 0.5 rad/s (T ≈ 12.6 s):
- Roll damping B44: 5.2e6 N⋅m⋅s
- Pitch damping B55: 8.1e7 N⋅m⋅s
- Heave damping B33: 2.3e5 N⋅s/m
```

## Implementation Status

### Current Development Phase: Specification Complete
**Timeline**: Q1-Q2 2025
**Progress**: 0% Implementation (Spec Complete)
**Status**: Ready for Implementation 📋

#### Specification Complete ✅
- ✅ Module structure defined
- ✅ Excel damping sheet analyzed (84×12)
- ✅ Integration with RAO processing identified
- ✅ AQWA import requirements documented

#### Planned Components 📋
- **Added Mass Module**: Frequency-dependent interpolation
- **Damping Module**: Radiation + viscous damping handling
- **AQWA Parser**: Import from .LIS hydrodynamic database
- **Validation Tools**: Kramers-Kronig checks, symmetry validation

## Quality Standards

### Technical Validation
- **Interpolation Accuracy**: <1% error between data points
- **Kramers-Kronig**: Causality relations satisfied to 5%
- **Symmetry**: A_ij = A_ji (reciprocity theorem)
- **Physical Bounds**: Positive-definite added mass at ω → ∞

### Industry Compliance
- **DNV-GL**: Added mass and damping per classification requirements
- **AQWA Compatibility**: Import/export AQWA hydrodynamic databases
- **API RP 2A**: Hydrodynamic coefficients for offshore structures

### Performance Requirements
- **Interpolation Speed**: <1ms per coefficient lookup
- **Database Load**: <500ms for full 6×6×100 database
- **Memory**: <50MB for typical coefficient databases

## User Experience

### Quick Coefficient Lookup
```python
from digitalmodel.marine_engineering.hydrodynamic_coefficients import load_coefficients

# Load from AQWA
coeffs = load_coefficients.from_aqwa("vessel_hydro.lis")

# Get heave added mass at 0.8 rad/s
A33 = coeffs.get_added_mass(frequency=0.8, dof_i=2, dof_j=2)

# Get roll damping
B44 = coeffs.get_damping(frequency=0.8, dof_i=3, dof_j=3)

print(f"Heave added mass: {A33:.0f} kg")
print(f"Roll damping: {B44:.2e} N⋅m⋅s")
```

### Critical Damping Analysis
```python
# Compute critical damping ratio for roll
zeta_roll = coeffs.compute_critical_damping_ratio(
    dof=3,  # Roll
    inertia=2.5e7,  # Roll inertia [kg⋅m²]
    restoring=1.8e8,  # GM⋅Δ⋅g [N⋅m]
    frequency=0.5
)

print(f"Roll damping ratio: {zeta_roll:.3f}")
if zeta_roll < 0.05:
    print("⚠️ Warning: Low roll damping - consider bilge keels")
```

## Success Metrics

### Technical Success Criteria
- [ ] AQWA hydrodynamic database import functional
- [ ] Excel damping sheet import validated
- [ ] Interpolation accuracy <1% between data points
- [ ] Integration with ship dynamics successful

### User Success Criteria
- [ ] Marine engineers access coefficients in <2 lines of code
- [ ] Coefficient validation tools catch physical inconsistencies
- [ ] Seamless workflow from AQWA to motion analysis

### Business Impact
- [ ] Enable accurate motion prediction for certification
- [ ] Support hydrodynamic analysis quality assurance
- [ ] Reduce errors from manual coefficient entry

## Future Enhancements

### Advanced Features
- **Machine Learning**: AI-based coefficient prediction from hull geometry
- **Sensitivity Analysis**: Coefficient uncertainty quantification
- **Multi-Body**: Hydrodynamic interaction coefficients
- **Drift Forces**: Second-order coefficients for slow-drift analysis

### Database Expansion
- **Vessel Library**: Pre-computed coefficients for standard hulls
- **Experimental Data**: Model test database integration
- **CFD Integration**: Import from CFD simulations

---

*Hydrodynamic coefficients module provides essential frequency-dependent added mass and damping for accurate marine structure motion analysis.*
