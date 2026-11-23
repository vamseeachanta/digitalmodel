# Phase 2 Data Extraction Plan

**Generated:** 2025-10-03
**Excel Source:** `D:/workspace-hub/_temp/marine_analysis_data.xlsm`
**Target Modules:** Hydrodynamic Coefficients & OCIMF Environmental Loading
**Total Formulas:** 1,244 (17.6% of Excel workbook)

---

## Executive Summary

Phase 2 focuses on extracting two critical marine engineering datasets:

1. **OCIMF Environmental Loading** - 186 rows of wind/current coefficients (1,244 formulas)
2. **Hydrodynamic Coefficients** - Frequency-dependent added mass and damping matrices

This document provides exact cell locations, formula analysis, data structure, and Python implementation strategy for both modules.

---

## 1. OCIMF Environmental Loading

### 1.1 Excel Location & Structure

#### Primary Data Sheet: "OCIMF (raw)"
- **Dimensions:** 186 rows × 38 columns
- **Formula Count:** 743 formulas (data extraction and cell linking)
- **Data Rows:** 175 non-empty rows with coefficient data

#### Processed Data Sheet: "AQWA OCIMF Inputs"
- **Dimensions:** 84 rows × 41 columns
- **Formula Count:** 501 formulas (wind/current force calculations)
- **Integration:** AQWA-ready coefficient format

### 1.2 Data Structure Analysis

#### OCIMF (raw) Sheet Organization

**Four Coefficient Sections:**

**Section 1: CXw (Wind Force - Longitudinal) - Rows 1-17**
- Row 1: Header "CXw"
- Row 2: Vessel types - "Loaded Tanker | Ballasted Tanker | LNGC Carrier"
- Row 3: Draft ratios - "WD/t" (Wind Draft / Total Draft)
- Row 4: Column headers - Draft ratio values [1.02, 1.05, 1.1, 3.0]
- Rows 5-17: Heading angles 0° to 180° (13 rows)
  - Column A: Heading angle (deg)
  - Columns B-K: CXw coefficients for different vessel types and draft ratios

**Sample Data (CXw):**
```
Row 5 (0°):   [0, 0.071, 0.043, -0.034, 0.022, 0.071, 0.042, 0.022, 0.005, 0.013, 0.023]
Row 8 (45°):  [45, 0.244, 0.205, 0.130, -0.001, 0.244, 0.112, -0.001, 0.205, 0.130, -0.001]
Row 11 (90°): [90, 0.091, 0.086, 0.050, 0.022, 0.091, 0.056, 0.022, 0.086, 0.050, 0.022]
Row 17 (180°): [180, -0.055, -0.034, -0.023, -0.014, -0.055, -0.036, -0.014, 0.114, -0.222, -0.024]
```

**Section 2: CYw (Wind Force - Lateral) - Rows 19-36**
- Row 19: Header "CYw"
- Row 20: Vessel types
- Row 21: Draft ratios
- Row 22: Column headers - Draft ratio values [1.02, 1.05, 1.1, 3.0, 6.0]
- Rows 23-35: Heading angles 0° to 180° (13 rows)

**Sample Data (CYw):**
```
Row 23 (0°):   [0, 0.105, 0.000, 0.009, 0.015, 0.000, 0.105, 0.000, 0.015, 0.000, 0.612, 0.000, 0.000]
Row 26 (45°):  [45, 1.621, 1.314, 1.108, 0.597, 0.365, 1.621, 1.646, 0.597, 0.328, 2.970, 2.521, 0.978]
Row 29 (90°):  [90, 2.914, 2.584, 1.961, 1.062, 0.581, 2.914, 2.842, 1.062, 0.500, 2.628, 2.212, 0.943]
```

**Section 3: CMw (Wind Moment - Yaw) - Rows 38-55**
- Row 41: Column headers - "Angle (°)" + draft ratios
- Rows 42-54: Heading angles 0° to 180° (13 rows)

**Sample Data (CMw):**
```
Row 42 (0°):   [0, -0.003, 0.000, -0.001, -0.004, -0.003, 0.000, -0.004, -0.143, -0.091]
Row 45 (45°):  [45, -0.211, -0.211, -0.221, -0.085, -0.211, -0.325, -0.085, -0.211, -0.221]
Row 51 (135°): [135, 0.147, 0.167, 0.247, 0.073, 0.147, 0.240, 0.073, 0.167, 0.247]
```

**Section 4: Current Coefficients (CXc, CYc, CMc) - Rows 57-162**
- Similar structure to wind coefficients
- Rows 59-72: CXc (Current force - longitudinal)
- Rows 109-122: CYc (Current force - lateral)
- Rows 148-161: CMc (Current moment - yaw)

**Column Structure (Example for Loaded Tanker):**
- Column A: Heading angle (0°, 15°, 30°, 45°, 60°, 75°, 90°, 105°, 120°, 135°, 150°, 165°, 180°)
- Columns B-E: Coefficients for draft ratios 1.02, 1.05, 1.1, 3.0
- Pattern repeats for Ballasted Tanker (cols F-I) and LNGC (cols J-M)

### 1.3 AQWA OCIMF Inputs Sheet

**Purpose:** Convert raw OCIMF coefficients to AQWA-compatible format

**Key Formulas:**

**Row 6 (Displacement Correction):**
```excel
Cell C6: =499.253664+(D-t)*L
```
Where:
- D = Vessel depth (named range from 'Condition 1'!$G$32)
- t = Draft
- L = Vessel length (named range from 'Condition 1'!$G$28)

**Row 8 (Wind Speed Reference):**
```excel
Cell C8: ='Condition 1'!G71
```

**Rows 9+ (Frequency Scaling):**
```excel
Cell P9: =F23*$C$8^2    # Surge wind force scaled by wind speed²
Cell S9: =G23*$C$8^2    # Sway wind force scaled
Cell V9: =H23*$C$8^2    # Yaw moment scaled
```

**Wind Coefficient Section (Rows 12-38):**
- Row 12-13: Headers "Environment Heading (°) | CXw | CYw | CMw | AQWA Coefficients"
- Rows 14-38: Heading angles 0° to 360° with interpolated coefficients

**Current Coefficient Section (Rows 41-84):**
- Similar structure for current loading
- Row 51: Current speed reference
- Rows 55-83: Current coefficients by heading

### 1.4 Data Extraction Strategy

#### Step 1: Extract Raw OCIMF Database

**Target:** CSV file with all coefficient data

**Python Implementation:**
```python
import openpyxl
import pandas as pd
import numpy as np

def extract_ocimf_coefficients(excel_path):
    """
    Extract all OCIMF wind and current coefficients from Excel

    Returns
    -------
    dict
        {
            'wind': DataFrame with CXw, CYw, CMw by heading and draft ratio
            'current': DataFrame with CXc, CYc, CMc by heading and draft ratio
        }
    """
    wb = openpyxl.load_workbook(excel_path, data_only=True)
    sheet = wb['OCIMF (raw)']

    # Extract wind coefficients
    wind_data = {
        'CXw': extract_coefficient_section(sheet, start_row=5, end_row=17,
                                           heading_col='A', data_cols='B:K'),
        'CYw': extract_coefficient_section(sheet, start_row=23, end_row=35,
                                           heading_col='A', data_cols='B:M'),
        'CMw': extract_coefficient_section(sheet, start_row=42, end_row=54,
                                           heading_col='A', data_cols='B:J')
    }

    # Extract current coefficients
    current_data = {
        'CXc': extract_coefficient_section(sheet, start_row=60, end_row=72,
                                           heading_col='A', data_cols='B:E'),
        'CYc': extract_coefficient_section(sheet, start_row=110, end_row=122,
                                           heading_col='A', data_cols='B:E'),
        'CMc': extract_coefficient_section(sheet, start_row=149, end_row=161,
                                           heading_col='A', data_cols='B:E')
    }

    return {
        'wind': pd.DataFrame(wind_data),
        'current': pd.DataFrame(current_data)
    }

def extract_coefficient_section(sheet, start_row, end_row, heading_col, data_cols):
    """Extract a single coefficient section (CXw, CYw, etc.)"""
    headings = []
    coefficients = []

    for row_idx in range(start_row, end_row + 1):
        row = list(sheet.rows)[row_idx - 1]

        # Heading angle from column A
        heading = row[0].value
        if heading is None or not isinstance(heading, (int, float)):
            continue

        headings.append(heading)

        # Extract coefficient values
        row_coeffs = []
        col_start = ord(data_cols.split(':')[0]) - ord('A')
        col_end = ord(data_cols.split(':')[1]) - ord('A')

        for col_idx in range(col_start, col_end + 1):
            value = row[col_idx].value
            row_coeffs.append(value if value is not None else np.nan)

        coefficients.append(row_coeffs)

    return {
        'heading': headings,
        'coefficients': np.array(coefficients)
    }
```

**Output CSV Structure:**
```csv
# ocimf_wind_coefficients.csv
vessel_type,draft_ratio,heading,CXw,CYw,CMw
Loaded_Tanker,1.02,0,0.071,0.105,-0.003
Loaded_Tanker,1.02,15,0.061,0.563,-0.198
Loaded_Tanker,1.02,30,0.200,0.837,-0.235
...
LNGC,3.0,180,-0.024,0.000,0.000
```

#### Step 2: Create Python OCIMF Database Class

**File:** `modules/marine_analysis/ocimf_coefficients.py`

```python
import pandas as pd
import numpy as np
from scipy.interpolate import interp1d, interp2d

class OCIMFDatabase:
    """
    OCIMF wind and current coefficient database with interpolation

    Attributes
    ----------
    wind_coeffs : pd.DataFrame
        Wind coefficients (CXw, CYw, CMw) by heading and draft ratio
    current_coeffs : pd.DataFrame
        Current coefficients (CXc, CYc, CMc) by heading and draft ratio

    Methods
    -------
    get_wind_coefficients(heading, draft_ratio, vessel_type='Loaded_Tanker')
        Returns interpolated CXw, CYw, CMw for given conditions
    get_current_coefficients(heading, draft_ratio, vessel_type='Loaded_Tanker')
        Returns interpolated CXc, CYc, CMc for given conditions
    calculate_wind_force(wind_speed, heading, draft_ratio, vessel_props)
        Calculate wind forces and moments using OCIMF methodology
    calculate_current_force(current_speed, heading, draft_ratio, vessel_props)
        Calculate current forces and moments
    """

    def __init__(self, data_path='data/ocimf_coefficients.csv'):
        """Load OCIMF coefficient database"""
        self.data = pd.read_csv(data_path)

        # Separate wind and current coefficients
        self.wind_coeffs = self.data[self.data['type'] == 'wind']
        self.current_coeffs = self.data[self.data['type'] == 'current']

    def get_wind_coefficients(self, heading, draft_ratio,
                             vessel_type='Loaded_Tanker'):
        """
        Get wind coefficients for given conditions using 2D interpolation

        Parameters
        ----------
        heading : float
            Heading angle in degrees (0-360)
        draft_ratio : float
            Wind draft to total draft ratio (WD/t)
        vessel_type : str
            'Loaded_Tanker', 'Ballasted_Tanker', or 'LNGC_Carrier'

        Returns
        -------
        CXw, CYw, CMw : float
            Wind force/moment coefficients

        Notes
        -----
        Wind forces calculated as:
        Fx = 0.5 * ρ_air * V² * A_lateral * CXw
        Fy = 0.5 * ρ_air * V² * A_frontal * CYw
        Mz = 0.5 * ρ_air * V² * A_lateral * L * CMw

        Where:
        - ρ_air = 1.225 kg/m³
        - V = wind speed (m/s)
        - A_lateral = lateral projected area (m²)
        - A_frontal = frontal projected area (m²)
        - L = vessel length (m)
        """
        # Filter for vessel type
        vessel_data = self.wind_coeffs[
            self.wind_coeffs['vessel_type'] == vessel_type
        ]

        # Get unique headings and draft ratios
        headings = sorted(vessel_data['heading'].unique())
        draft_ratios = sorted(vessel_data['draft_ratio'].unique())

        # Normalize heading to 0-180 (symmetry assumption)
        heading_norm = heading % 360
        if heading_norm > 180:
            heading_norm = 360 - heading_norm

        # Create 2D interpolators for each coefficient
        CXw_interp = self._create_2d_interpolator(
            vessel_data, headings, draft_ratios, 'CXw'
        )
        CYw_interp = self._create_2d_interpolator(
            vessel_data, headings, draft_ratios, 'CYw'
        )
        CMw_interp = self._create_2d_interpolator(
            vessel_data, headings, draft_ratios, 'CMw'
        )

        # Interpolate
        CXw = float(CXw_interp(heading_norm, draft_ratio))
        CYw = float(CYw_interp(heading_norm, draft_ratio))
        CMw = float(CMw_interp(heading_norm, draft_ratio))

        # Apply symmetry for headings > 180
        if heading % 360 > 180:
            CYw = -CYw  # Lateral force changes sign
            CMw = -CMw  # Moment changes sign

        return CXw, CYw, CMw

    def _create_2d_interpolator(self, data, headings, draft_ratios, coeff_name):
        """Create 2D interpolator for a coefficient"""
        # Pivot data to 2D grid
        grid = data.pivot_table(
            values=coeff_name,
            index='heading',
            columns='draft_ratio',
            aggfunc='mean'
        )

        # Use cubic spline interpolation
        from scipy.interpolate import RectBivariateSpline
        interpolator = RectBivariateSpline(
            headings, draft_ratios, grid.values,
            kx=3, ky=1  # Cubic in heading, linear in draft ratio
        )

        return interpolator

    def calculate_wind_force(self, wind_speed, heading, draft_ratio,
                            vessel_props, vessel_type='Loaded_Tanker'):
        """
        Calculate wind forces and moments using OCIMF methodology

        Parameters
        ----------
        wind_speed : float
            Wind speed in m/s
        heading : float
            Relative wind heading in degrees
        draft_ratio : float
            WD/t ratio
        vessel_props : dict
            {
                'A_lateral': lateral projected area (m²),
                'A_frontal': frontal projected area (m²),
                'length': vessel length (m)
            }
        vessel_type : str
            Vessel type

        Returns
        -------
        dict
            {
                'Fx': Surge force (N),
                'Fy': Sway force (N),
                'Mz': Yaw moment (N·m)
            }
        """
        # Get coefficients
        CXw, CYw, CMw = self.get_wind_coefficients(
            heading, draft_ratio, vessel_type
        )

        # Air properties
        rho_air = 1.225  # kg/m³

        # Dynamic pressure
        q = 0.5 * rho_air * wind_speed**2

        # Calculate forces
        Fx = q * vessel_props['A_lateral'] * CXw
        Fy = q * vessel_props['A_frontal'] * CYw
        Mz = q * vessel_props['A_lateral'] * vessel_props['length'] * CMw

        return {
            'Fx': Fx,
            'Fy': Fy,
            'Mz': Mz,
            'CXw': CXw,
            'CYw': CYw,
            'CMw': CMw
        }

    def get_current_coefficients(self, heading, draft_ratio,
                                vessel_type='Loaded_Tanker'):
        """
        Get current coefficients (similar to wind coefficients)

        Returns
        -------
        CXc, CYc, CMc : float
            Current force/moment coefficients
        """
        # Similar implementation to get_wind_coefficients
        # Using current_coeffs DataFrame instead
        pass  # Implementation similar to wind coefficients
```

#### Step 3: Validation Against Excel

**Validation Test:**
```python
def test_ocimf_against_excel():
    """
    Validate Python OCIMF database against Excel calculations

    Test cases from AQWA OCIMF Inputs sheet:
    - Row 14: Heading 0°, wind speed from C8
    - Row 20: Heading 45°
    - Row 29: Heading 90°
    """
    import pandas as pd
    import numpy as np

    # Load Excel results
    excel_results = pd.read_excel(
        'D:/workspace-hub/_temp/marine_analysis_data.xlsm',
        sheet_name='AQWA OCIMF Inputs',
        skiprows=12,
        nrows=25,
        usecols='B:H'
    )

    # Initialize Python OCIMF database
    ocimf = OCIMFDatabase('data/ocimf_coefficients.csv')

    # Test parameters
    wind_speed = 20.0  # m/s (from Excel C8)
    draft_ratio = 1.05
    vessel_type = 'Loaded_Tanker'

    vessel_props = {
        'A_lateral': 1500,  # m²
        'A_frontal': 800,   # m²
        'length': 180       # m
    }

    # Compare for multiple headings
    test_headings = [0, 15, 30, 45, 60, 90, 120, 135, 150, 165, 180]

    errors = []
    for heading in test_headings:
        # Python calculation
        python_result = ocimf.calculate_wind_force(
            wind_speed, heading, draft_ratio, vessel_props, vessel_type
        )

        # Excel result (extract from DataFrame)
        excel_row = excel_results[excel_results['Heading'] == heading]
        excel_Fx = excel_row['Surge_Force'].values[0]
        excel_Fy = excel_row['Sway_Force'].values[0]
        excel_Mz = excel_row['Yaw_Moment'].values[0]

        # Calculate relative errors
        err_Fx = abs(python_result['Fx'] - excel_Fx) / abs(excel_Fx + 1e-10)
        err_Fy = abs(python_result['Fy'] - excel_Fy) / abs(excel_Fy + 1e-10)
        err_Mz = abs(python_result['Mz'] - excel_Mz) / abs(excel_Mz + 1e-10)

        errors.append({
            'heading': heading,
            'err_Fx': err_Fx,
            'err_Fy': err_Fy,
            'err_Mz': err_Mz
        })

        print(f"Heading {heading}°:")
        print(f"  Fx error: {err_Fx*100:.2f}%")
        print(f"  Fy error: {err_Fy*100:.2f}%")
        print(f"  Mz error: {err_Mz*100:.2f}%")

    # Assert all errors < 1%
    errors_df = pd.DataFrame(errors)
    assert errors_df[['err_Fx', 'err_Fy', 'err_Mz']].max().max() < 0.01, \
        "Validation failed: errors exceed 1% tolerance"

    print("\n✓ All validation tests passed (errors < 1%)")
```

### 1.5 Output Files

**Data Files:**
1. `data/ocimf_wind_coefficients.csv` - Wind coefficient database
2. `data/ocimf_current_coefficients.csv` - Current coefficient database
3. `data/ocimf_vessel_types.json` - Vessel type definitions

**Module Files:**
1. `modules/marine_analysis/ocimf_coefficients.py` - Main module
2. `modules/marine_analysis/environmental_loading.py` - Force calculation wrapper
3. `tests/test_ocimf_validation.py` - Validation tests

---

## 2. Hydrodynamic Coefficients

### 2.1 Excel Location & Structure

#### Primary Sheet: "Damping"
- **Dimensions:** 84 rows × 12 columns
- **Formula Count:** 19 formulas (mostly cell references)
- **Data Type:** Damping guidance and natural period calculations

**NOTE:** The Excel file does NOT contain full hydrodynamic coefficient matrices (added mass A_ij and damping B_ij). The "Damping" sheet provides:
1. Natural period calculations for roll and pitch
2. Damping ratio guidance (10% for roll, 8% for pitch)
3. Added damping calculations to achieve desired damping ratios

**Actual hydrodynamic coefficient data must come from AQWA output files (.LIS format).**

### 2.2 Damping Sheet Content

**Structure:**

**Section 1: Roll Damping (Rows 16-28)**
- Row 17: Headers - "Item | Value | Units"
- Row 18: Natural Period (seconds)
- Row 19: Natural Frequency (rad/s)
- Row 21: Damping % (% of critical) - Currently 0%
- Row 22: Damping Value (N·m/(rad/s))
- Row 24: **Desired Damping %** - **10% (user input)**
- Row 26: **Added Damping** - Calculated value to achieve 10%
- Row 27: Added Damping in alternative units (N·m/(°/s))

**Formula (Row 26 - Added Damping):**
```excel
C26: =#DIV/0!   (Formula present but gives error due to missing natural frequency)
```

Expected formula structure:
```excel
= (Desired_Damping_% - Current_Damping_%) * Critical_Damping
= (0.10 - 0) * 2 * sqrt(K * I_xx)
```
Where:
- K = Roll restoring stiffness (N·m/rad)
- I_xx = Roll moment of inertia (kg·m²)

**Section 2: Pitch Damping (Rows 31-43)**
- Row 32: Headers
- Row 33: Natural Period (seconds)
- Row 34: Natural Frequency (rad/s)
- Row 36: Damping % - Currently unknown
- Row 39: **Desired Damping %** - **8% (user input)**
- Row 41: **Added Damping** - Calculated value

**Section 3: Heave Damping (Rows 46+)**
- Similar structure for heave motion

### 2.3 Data Extraction Strategy

**For Damping Guidance:**

```python
def extract_damping_guidance(excel_path):
    """
    Extract damping ratio guidance from Excel

    Returns
    -------
    dict
        {
            'roll': {
                'desired_damping_ratio': 0.10,  # 10% of critical
                'natural_period': None,  # User must provide or calculate
                'unit': '% of Critical'
            },
            'pitch': {
                'desired_damping_ratio': 0.08,  # 8% of critical
                'natural_period': None,
                'unit': '% of Critical'
            },
            'heave': {
                'desired_damping_ratio': 0.08,  # Typical value
                'natural_period': None,
                'unit': '% of Critical'
            }
        }
    """
    wb = openpyxl.load_workbook(excel_path, data_only=True)
    sheet = wb['Damping']

    # Extract roll damping (Row 24, Column B)
    roll_damping = sheet['B24'].value  # Should be 10

    # Extract pitch damping (Row 39, Column B)
    pitch_damping = sheet['B39'].value  # Should be 8

    return {
        'roll': {
            'desired_damping_ratio': roll_damping / 100.0,
            'natural_period': sheet['B18'].value,  # May be None
            'guidance': 'Typical for moored vessels: 5-15% of critical'
        },
        'pitch': {
            'desired_damping_ratio': pitch_damping / 100.0,
            'natural_period': sheet['B33'].value,
            'guidance': 'Typical for moored vessels: 5-10% of critical'
        },
        'heave': {
            'desired_damping_ratio': 0.08,  # Default if not specified
            'guidance': 'Typical for moored vessels: 5-10% of critical'
        }
    }
```

**For Full Hydrodynamic Coefficients (from AQWA .LIS file):**

The Excel file references AQWA hydrodynamic databases but does not store the full coefficient matrices. These must be extracted from AQWA output files.

**AQWA .LIS File Structure:**
```
HYDRODYNAMIC DATABASE
=====================

FREQUENCY-DEPENDENT ADDED MASS AND DAMPING

Frequency (rad/s): 0.1000

Added Mass Matrix A(ω) [kg or kg·m or kg·m²]:
       Surge      Sway      Heave      Roll       Pitch      Yaw
Surge  1.234e6    0.0       0.0        0.0        0.0        0.0
Sway   0.0        2.456e6   0.0        0.0        0.0        1.234e5
Heave  0.0        0.0       3.678e6    0.0        0.0        0.0
Roll   0.0        0.0       0.0        4.890e7    0.0        0.0
Pitch  0.0        0.0       0.0        0.0        5.012e7    0.0
Yaw    0.0        1.234e5   0.0        0.0        0.0        6.134e8

Damping Matrix B(ω) [N·s/m or N·m·s or N·m·s]:
       Surge      Sway      Heave      Roll       Pitch      Yaw
Surge  1.123e4    0.0       0.0        0.0        0.0        0.0
Sway   0.0        2.234e4   0.0        0.0        0.0        3.345e3
...
```

### 2.4 Hydrodynamic Coefficient Module

**File:** `modules/marine_analysis/hydrodynamic_coefficients.py`

```python
import numpy as np
import pandas as pd
from scipy.interpolate import interp1d

class HydrodynamicCoefficients:
    """
    Frequency-dependent hydrodynamic coefficient database

    Manages added mass and damping matrices from AQWA or WAMIT

    Attributes
    ----------
    frequencies : np.ndarray
        Array of wave frequencies (rad/s)
    added_mass : dict
        Added mass matrices A_ij(ω) for each frequency
        Keys: frequency (float)
        Values: 6x6 numpy array
    damping : dict
        Radiation damping matrices B_ij(ω) for each frequency
        Keys: frequency (float)
        Values: 6x6 numpy array
    hydrostatic : np.ndarray
        Hydrostatic restoring matrix C (6x6, frequency-independent)

    Methods
    -------
    from_aqwa_lis(filepath)
        Load coefficients from AQWA .LIS file
    from_wamit(filepath)
        Load coefficients from WAMIT output
    from_csv(filepath)
        Load from CSV export
    get_added_mass(frequency)
        Get added mass matrix at specific frequency (with interpolation)
    get_damping(frequency)
        Get damping matrix at specific frequency
    get_total_mass(frequency)
        Get total mass matrix (body + added mass)
    apply_viscous_damping(damping_ratios)
        Add viscous damping using guidance from Excel
    """

    def __init__(self):
        self.frequencies = np.array([])
        self.added_mass = {}
        self.damping = {}
        self.hydrostatic = np.zeros((6, 6))

        # Damping guidance from Excel
        self.damping_guidance = {
            'roll': 0.10,   # 10% of critical (from Excel)
            'pitch': 0.08,  # 8% of critical (from Excel)
            'heave': 0.08   # Typical value
        }

    @classmethod
    def from_aqwa_lis(cls, filepath):
        """
        Load hydrodynamic coefficients from AQWA .LIS file

        Parameters
        ----------
        filepath : str
            Path to AQWA output .LIS file

        Returns
        -------
        HydrodynamicCoefficients
            Loaded coefficient database
        """
        hydro = cls()

        # Parse AQWA .LIS file
        with open(filepath, 'r') as f:
            lines = f.readlines()

        # Find sections with added mass and damping
        # (Parsing logic for AQWA format)
        # This is complex - typically 50-100 lines of parsing code

        current_freq = None
        reading_added_mass = False
        reading_damping = False

        for line in lines:
            # Detect frequency
            if 'Frequency (rad/s):' in line:
                current_freq = float(line.split(':')[1].strip())
                hydro.frequencies = np.append(hydro.frequencies, current_freq)
                hydro.added_mass[current_freq] = np.zeros((6, 6))
                hydro.damping[current_freq] = np.zeros((6, 6))

            # Detect matrix sections
            if 'Added Mass Matrix' in line:
                reading_added_mass = True
                reading_damping = False
                row_idx = 0
            elif 'Damping Matrix' in line:
                reading_added_mass = False
                reading_damping = True
                row_idx = 0

            # Parse matrix rows
            elif reading_added_mass or reading_damping:
                # Extract numerical values
                # (Detailed parsing implementation)
                pass

        return hydro

    def get_added_mass(self, frequency):
        """
        Get added mass matrix at specified frequency using interpolation

        Parameters
        ----------
        frequency : float
            Wave frequency in rad/s

        Returns
        -------
        A : np.ndarray (6, 6)
            Added mass matrix [kg, kg·m, kg·m²]
        """
        if frequency in self.added_mass:
            return self.added_mass[frequency]

        # Interpolation
        A_interp = np.zeros((6, 6))

        for i in range(6):
            for j in range(6):
                # Extract A_ij(ω) for all frequencies
                A_ij_values = np.array([
                    self.added_mass[freq][i, j]
                    for freq in self.frequencies
                ])

                # Cubic spline interpolation
                interpolator = interp1d(
                    self.frequencies, A_ij_values,
                    kind='cubic', fill_value='extrapolate'
                )

                A_interp[i, j] = interpolator(frequency)

        return A_interp

    def get_damping(self, frequency):
        """
        Get radiation damping matrix at specified frequency

        Parameters
        ----------
        frequency : float
            Wave frequency in rad/s

        Returns
        -------
        B : np.ndarray (6, 6)
            Radiation damping matrix [N·s/m, N·m·s, N·m·s]
        """
        # Similar to get_added_mass
        # Implementation with interpolation
        pass

    def apply_viscous_damping(self, body_mass, natural_frequencies):
        """
        Add viscous damping to achieve desired damping ratios from Excel

        Parameters
        ----------
        body_mass : dict
            Body mass properties:
            {
                'mass': mass (kg),
                'I_xx': roll moment of inertia (kg·m²),
                'I_yy': pitch moment of inertia (kg·m²),
                'I_zz': yaw moment of inertia (kg·m²)
            }
        natural_frequencies : dict
            Natural frequencies for each DOF:
            {
                'heave': ω_n (rad/s),
                'roll': ω_n (rad/s),
                'pitch': ω_n (rad/s)
            }

        Returns
        -------
        B_viscous : np.ndarray (6, 6)
            Viscous damping matrix to add to radiation damping

        Notes
        -----
        Uses Excel guidance:
        - Roll: 10% of critical damping
        - Pitch: 8% of critical damping
        - Heave: 8% of critical damping

        Critical damping: C_crit = 2 * sqrt(K * M)
        Where K = stiffness, M = mass (or moment of inertia)
        """
        B_viscous = np.zeros((6, 6))

        # Heave (DOF 3)
        if 'heave' in natural_frequencies:
            omega_n = natural_frequencies['heave']
            m = body_mass['mass']
            C_crit = 2 * m * omega_n
            B_viscous[2, 2] = self.damping_guidance['heave'] * C_crit

        # Roll (DOF 4)
        if 'roll' in natural_frequencies:
            omega_n = natural_frequencies['roll']
            I_xx = body_mass['I_xx']
            C_crit = 2 * I_xx * omega_n
            B_viscous[3, 3] = self.damping_guidance['roll'] * C_crit

        # Pitch (DOF 5)
        if 'pitch' in natural_frequencies:
            omega_n = natural_frequencies['pitch']
            I_yy = body_mass['I_yy']
            C_crit = 2 * I_yy * omega_n
            B_viscous[4, 4] = self.damping_guidance['pitch'] * C_crit

        return B_viscous
```

### 2.5 Data Storage Format

**Option 1: HDF5 (Recommended for large datasets)**
```python
import h5py

# Save
with h5py.File('hydrodynamic_coefficients.h5', 'w') as f:
    f.create_dataset('frequencies', data=hydro.frequencies)

    # Store added mass as 3D array (freq × 6 × 6)
    A_3d = np.stack([hydro.added_mass[freq] for freq in hydro.frequencies])
    f.create_dataset('added_mass', data=A_3d)

    # Store damping
    B_3d = np.stack([hydro.damping[freq] for freq in hydro.frequencies])
    f.create_dataset('damping', data=B_3d)

    # Hydrostatic (frequency-independent)
    f.create_dataset('hydrostatic', data=hydro.hydrostatic)

# Load
with h5py.File('hydrodynamic_coefficients.h5', 'r') as f:
    frequencies = f['frequencies'][:]
    A_3d = f['added_mass'][:]
    B_3d = f['damping'][:]
    C = f['hydrostatic'][:]
```

**Option 2: CSV (for smaller datasets, easier inspection)**
```csv
# added_mass.csv (long format)
frequency,DOF_i,DOF_j,value
0.1,1,1,1234567.89
0.1,1,2,0.00
0.1,2,2,2456789.01
...

# damping.csv
frequency,DOF_i,DOF_j,value
0.1,1,1,11234.56
0.1,2,2,22345.67
...
```

### 2.6 Integration with Excel Damping Guidance

**Combined Usage:**

```python
# Load AQWA hydrodynamic coefficients
hydro = HydrodynamicCoefficients.from_aqwa_lis('vessel_hydro.lis')

# Apply Excel damping guidance
body_mass = {
    'mass': 50000000,  # 50,000 tonnes
    'I_xx': 1.5e9,      # kg·m²
    'I_yy': 5.0e10,
    'I_zz': 5.0e10
}

natural_frequencies = {
    'heave': 0.5,   # rad/s
    'roll': 0.3,
    'pitch': 0.4
}

B_viscous = hydro.apply_viscous_damping(body_mass, natural_frequencies)

# Total damping at frequency ω
omega = 0.5
B_total = hydro.get_damping(omega) + B_viscous

print(f"Radiation damping (roll): {hydro.get_damping(omega)[3,3]:.2e} N·m·s")
print(f"Viscous damping (roll):   {B_viscous[3,3]:.2e} N·m·s")
print(f"Total damping (roll):     {B_total[3,3]:.2e} N·m·s")
```

---

## 3. Formula Analysis

### 3.1 OCIMF Wind Force Formulas

**From AQWA OCIMF Inputs Sheet:**

**Wind Force - Longitudinal (Surge):**
```
Fx = 0.5 * ρ_air * V_wind² * A_lateral * CXw
```
Excel implementation (Row 14+, Column P):
```excel
P14: =F23*$C$8^2
```
Where:
- F23: CXw coefficient (from interpolation)
- C8: Wind speed (m/s)

**Wind Force - Lateral (Sway):**
```
Fy = 0.5 * ρ_air * V_wind² * A_frontal * CYw
```
Excel implementation (Row 14+, Column S):
```excel
S14: =G23*$C$8^2
```

**Wind Moment - Yaw:**
```
Mz = 0.5 * ρ_air * V_wind² * A_lateral * L * CMw
```
Excel implementation (Row 14+, Column V):
```excel
V14: =H23*$C$8^2
```

**Constants:**
- ρ_air = 1.225 kg/m³ (air density at sea level, 15°C)
- V_wind: Wind speed (m/s)
- A_lateral: Lateral projected area (m²)
- A_frontal: Frontal projected area (m²)
- L: Vessel length between perpendiculars (m)

### 3.2 OCIMF Current Force Formulas

**Current Force - Longitudinal:**
```
Fx = 0.5 * ρ_water * V_current² * A_underwater_lateral * CXc
```

**Current Force - Lateral:**
```
Fy = 0.5 * ρ_water * V_current² * A_underwater_frontal * CYc
```

**Current Moment - Yaw:**
```
Mz = 0.5 * ρ_water * V_current² * A_underwater_lateral * L * CMc
```

**Constants:**
- ρ_water = 1025 kg/m³ (seawater density)
- V_current: Current speed (m/s)

### 3.3 Displacement Correction Formula

**From AQWA OCIMF Inputs, Cell C6:**
```excel
=499.253664+(D-t)*L
```

**Purpose:** Correct displacement for draft changes

**Variables:**
- 499.253664: Base displacement (unknown units, possibly metric tons × 1000 = kg)
- D: Vessel depth (m) - from 'Condition 1'!$G$32
- t: Draft (m)
- L: Length (m) - from 'Condition 1'!$G$28

**Physical Interpretation:**
```
Δ_corrected = Δ_base + (D - t) * L * correction_factor
```
This approximates the change in displacement when draft changes, assuming a rectangular waterplane area approximation.

### 3.4 Frequency Scaling Formulas

**Excel formulas show quadratic scaling with frequency:**

```excel
Row 9, Column P: =F23*$C$8^2
```

This represents the dynamic pressure term in force calculations:
```
q = 0.5 * ρ * V²
```

Where V (velocity) is proportional to frequency in wave-induced motions.

---

## 4. Python Implementation Preview

### 4.1 OCIMF Database Module

**Class Structure:**

```python
# modules/marine_analysis/ocimf_coefficients.py

class OCIMFDatabase:
    """OCIMF coefficient database with 2D interpolation"""

    def __init__(self, csv_path):
        self.data = pd.read_csv(csv_path)

    def get_wind_coefficients(self, heading, draft_ratio, vessel_type):
        """2D interpolation (heading × draft_ratio)"""
        return CXw, CYw, CMw

    def get_current_coefficients(self, heading, draft_ratio, vessel_type):
        """2D interpolation for current coefficients"""
        return CXc, CYc, CMc

    def calculate_wind_force(self, wind_speed, heading, vessel_props):
        """Complete wind force calculation"""
        return {'Fx': ..., 'Fy': ..., 'Mz': ...}

    def calculate_current_force(self, current_speed, heading, vessel_props):
        """Complete current force calculation"""
        return {'Fx': ..., 'Fy': ..., 'Mz': ...}
```

### 4.2 Hydrodynamic Coefficients Module

**Class Structure:**

```python
# modules/marine_analysis/hydrodynamic_coefficients.py

class HydrodynamicCoefficients:
    """Frequency-dependent hydrodynamic database"""

    def __init__(self):
        self.frequencies = []
        self.added_mass = {}  # freq: 6×6 matrix
        self.damping = {}     # freq: 6×6 matrix

    @classmethod
    def from_aqwa_lis(cls, filepath):
        """Load from AQWA output"""
        pass

    def get_added_mass(self, frequency):
        """Interpolate added mass at frequency"""
        return A_matrix_6x6

    def get_damping(self, frequency):
        """Interpolate damping at frequency"""
        return B_matrix_6x6

    def apply_viscous_damping(self, body_mass, natural_frequencies):
        """Add viscous damping per Excel guidance"""
        return B_viscous_6x6
```

### 4.3 Integration Example

**Complete environmental loading calculation:**

```python
from modules.marine_analysis import OCIMFDatabase, VesselProperties

# Initialize
ocimf = OCIMFDatabase('data/ocimf_coefficients.csv')

# Define vessel
vessel = VesselProperties(
    length=180,
    beam=30,
    draft=10,
    A_lateral=1500,
    A_frontal=800
)

# Environmental conditions
wind_speed = 20  # m/s
wind_heading = 45  # degrees
current_speed = 1.0  # m/s
current_heading = 90  # degrees

# Calculate forces
wind_force = ocimf.calculate_wind_force(
    wind_speed, wind_heading, vessel.to_dict()
)

current_force = ocimf.calculate_current_force(
    current_speed, current_heading, vessel.to_dict()
)

# Total environmental force
F_total = {
    'Fx': wind_force['Fx'] + current_force['Fx'],
    'Fy': wind_force['Fy'] + current_force['Fy'],
    'Mz': wind_force['Mz'] + current_force['Mz']
}

print(f"Total surge force:  {F_total['Fx']/1e6:.2f} MN")
print(f"Total sway force:   {F_total['Fy']/1e6:.2f} MN")
print(f"Total yaw moment:   {F_total['Mz']/1e9:.2f} GN·m")
```

---

## 5. Validation Requirements

### 5.1 OCIMF Coefficient Validation

**Test Cases:**

1. **Interpolation Accuracy**
   - Test at known data points (should match exactly)
   - Test at midpoints (compare with manual linear interpolation)
   - Test extrapolation limits

2. **Force Calculation Validation**
   - Compare Python vs Excel for 50+ test cases
   - Tolerance: ±0.5% for all coefficients
   - Tolerance: ±1.0% for calculated forces

3. **Symmetry Checks**
   - CYw(heading) = -CYw(360° - heading)
   - CMw(heading) = -CMw(360° - heading)
   - CXw should be symmetric about 90°

4. **Physical Bounds**
   - All coefficients should be reasonable (-5 < C < 5)
   - Wind force increases with wind speed²
   - Forces should be zero at certain symmetric headings

### 5.2 Hydrodynamic Coefficient Validation

**Test Cases:**

1. **Matrix Properties**
   - Added mass matrices should be symmetric: A_ij = A_ji
   - Damping matrices should be symmetric: B_ij = B_ji
   - All diagonal terms should be positive
   - Off-diagonal coupling terms should be reasonable

2. **Frequency Dependence**
   - Added mass should decrease with increasing frequency
   - Damping should increase with frequency (typically)
   - Interpolation should be smooth (no discontinuities)

3. **Damping Ratio Validation**
   - Calculated viscous damping should achieve desired ratios
   - Roll damping: 10% of critical (from Excel)
   - Pitch damping: 8% of critical (from Excel)

4. **Unit Consistency**
   - Added mass: [kg], [kg·m], [kg·m²]
   - Damping: [N·s/m], [N·m·s], [N·m²·s]
   - Forces: [N], Moments: [N·m]

---

## 6. Data Quality Issues & Resolutions

### 6.1 OCIMF Data Issues Identified

**Issue 1: Inconsistent Draft Ratio Coverage**
- Loaded Tanker: 4 draft ratios (1.02, 1.05, 1.1, 3.0)
- Ballasted Tanker: 3 draft ratios (1.05, 1.1, 3.0)
- LNGC: 3 draft ratios (1.05, 1.1, 3.0)

**Resolution:**
- Use nearest neighbor for out-of-range draft ratios
- Document valid interpolation ranges
- Warning if user requests extrapolation

**Issue 2: Small Numerical Errors (Floating Point)**
- Some zero values represented as 5.55e-17 (machine epsilon)
- Example: Row 23, Col C: `5.551115123125783e-17` should be `0.0`

**Resolution:**
- Apply threshold: `if abs(value) < 1e-10: value = 0`
- Document this cleaning step

**Issue 3: Missing Heading Angles**
- Data has: 0°, 15°, 30°, 45°, 60°, 75°, 90°, 105°, 120°, 135°, 150°, 165°, 180°
- Missing: 5°, 10°, 20°, etc. for finer resolution

**Resolution:**
- Use cubic spline interpolation for smooth curves
- Validate interpolation against industry data
- Consider adding more reference points from OCIMF publications

### 6.2 Damping Sheet Issues

**Issue 1: Missing Natural Frequencies**
- Cells for natural period/frequency contain #DIV/0! errors
- Added damping calculations fail due to missing inputs

**Resolution:**
- Natural frequencies must be calculated from:
  1. AQWA modal analysis, OR
  2. Analytical formulas using vessel properties, OR
  3. User input from stability analysis

**Issue 2: Incomplete Data**
- Only roll and pitch damping guidance provided
- Surge, sway, heave, yaw guidance missing

**Resolution:**
- Use industry standard values:
  - Surge/Sway: 5% of critical (typical for moored vessels)
  - Heave: 8% of critical (similar to pitch)
  - Yaw: 10% of critical (similar to roll)

---

## 7. File Organization

### 7.1 Data Files

```
digitalmodel/
├── data/
│   ├── ocimf/
│   │   ├── wind_coefficients.csv         # Extracted wind coefficients
│   │   ├── current_coefficients.csv      # Extracted current coefficients
│   │   ├── vessel_types.json             # Vessel type definitions
│   │   └── README.md                     # Data source documentation
│   ├── hydrodynamic/
│   │   ├── damping_guidance.json         # From Excel Damping sheet
│   │   ├── example_hydro_coeffs.h5       # Sample AQWA data
│   │   └── README.md
│   └── validation/
│       ├── ocimf_excel_reference.csv     # Excel calculation results
│       └── hydrodynamic_test_cases.csv
```

### 7.2 Module Files

```
digitalmodel/
├── modules/
│   └── marine_analysis/
│       ├── __init__.py
│       ├── ocimf_coefficients.py         # OCIMF database class
│       ├── environmental_loading.py      # Combined wind/current forces
│       ├── hydrodynamic_coefficients.py  # Hydro coefficient management
│       ├── aqwa_parser.py                # Parse AQWA .LIS files
│       └── utils.py                      # Helper functions
```

### 7.3 Test Files

```
digitalmodel/
├── tests/
│   ├── modules/
│   │   └── marine_analysis/
│   │       ├── test_ocimf_database.py           # Unit tests
│   │       ├── test_ocimf_validation.py         # Excel validation
│   │       ├── test_environmental_loading.py
│   │       ├── test_hydrodynamic_coeffs.py
│   │       ├── test_aqwa_parser.py
│   │       └── fixtures/
│   │           ├── sample_aqwa_output.lis
│   │           └── sample_vessel_data.json
```

### 7.4 Documentation Files

```
digitalmodel/
├── docs/
│   ├── phase2_data_extraction_plan.md    # This document
│   ├── ocimf_methodology.md              # OCIMF calculation methodology
│   ├── hydrodynamic_theory.md            # Theory background
│   └── validation_reports/
│       ├── ocimf_validation_report.md
│       └── hydro_validation_report.md
```

---

## 8. Implementation Schedule

### Week 1: OCIMF Data Extraction
- **Day 1-2:** Extract raw OCIMF data to CSV
- **Day 3:** Implement `OCIMFDatabase` class
- **Day 4:** Implement interpolation methods
- **Day 5:** Unit tests and documentation

### Week 2: OCIMF Validation
- **Day 1-2:** Implement force calculation methods
- **Day 3:** Excel validation test suite
- **Day 4:** Fix any validation failures
- **Day 5:** Performance optimization and edge case handling

### Week 3: Hydrodynamic Coefficients
- **Day 1-2:** Implement AQWA .LIS parser
- **Day 3:** Implement `HydrodynamicCoefficients` class
- **Day 4:** Implement interpolation and viscous damping
- **Day 5:** Unit tests

### Week 4: Integration & Documentation
- **Day 1:** Integrate OCIMF + Hydro modules
- **Day 2:** Example notebooks and tutorials
- **Day 3:** Complete API documentation
- **Day 4:** Validation report generation
- **Day 5:** Code review and finalization

---

## 9. Success Criteria

### 9.1 OCIMF Module
- ✅ All 186 rows of coefficient data extracted
- ✅ 2D interpolation works for heading and draft ratio
- ✅ Python calculations match Excel within ±1%
- ✅ All validation tests pass
- ✅ API documentation complete
- ✅ Performance: <10ms per force calculation

### 9.2 Hydrodynamic Coefficients Module
- ✅ AQWA .LIS parser handles all standard formats
- ✅ Frequency interpolation accurate to ±0.5%
- ✅ Viscous damping achieves desired ratios (10% roll, 8% pitch)
- ✅ Matrix symmetry validated
- ✅ All unit tests pass
- ✅ HDF5 storage efficient (<10MB for typical vessel)

### 9.3 Integration
- ✅ Modules work together seamlessly
- ✅ Example scripts demonstrate usage
- ✅ Documentation clear and comprehensive
- ✅ Code coverage > 90%
- ✅ All validation reports generated

---

## 10. Risk Assessment & Mitigation

### 10.1 Data Extraction Risks

**Risk 1: Excel formula errors**
- **Likelihood:** Low
- **Impact:** High
- **Mitigation:** Extensive validation against known results, cross-check with OCIMF publications

**Risk 2: Missing or incomplete data**
- **Likelihood:** Medium
- **Impact:** Medium
- **Mitigation:** Document gaps, provide default values, user warnings

**Risk 3: Unit conversion errors**
- **Likelihood:** Medium
- **Impact:** High
- **Mitigation:** Explicit unit tracking, dimensional analysis, extensive testing

### 10.2 Implementation Risks

**Risk 1: Interpolation accuracy**
- **Likelihood:** Low
- **Impact:** Medium
- **Mitigation:** Compare multiple interpolation methods (linear, cubic, 2D), validate against Excel

**Risk 2: AQWA parser compatibility**
- **Likelihood:** Medium (multiple AQWA versions exist)
- **Impact:** Medium
- **Mitigation:** Test with multiple AQWA version outputs, provide format documentation

**Risk 3: Performance issues with large datasets**
- **Likelihood:** Low
- **Impact:** Low
- **Mitigation:** Use efficient data structures (numpy, pandas), profile and optimize

---

## 11. References

### 11.1 OCIMF Publications
1. OCIMF (1994). "Prediction of Wind and Current Loads on VLCCs"
2. OCIMF (2008). "Mooring Equipment Guidelines (MEG3)"
3. OCIMF (2018). "Mooring Equipment Guidelines (MEG4)"

### 11.2 Hydrodynamic Theory
1. Newman, J.N. (1977). "Marine Hydrodynamics"
2. Faltinsen, O.M. (1990). "Sea Loads on Ships and Offshore Structures"
3. ANSYS AQWA User Manual (v2023 R2)

### 11.3 Standards
1. DNV-RP-H103: "Modelling and Analysis of Marine Operations"
2. API RP 2SK: "Design and Analysis of Stationkeeping Systems for Floating Structures"
3. ISO 19901-7: "Stationkeeping systems for floating offshore structures"

---

## 12. Appendices

### Appendix A: Excel Cell Reference Map

**OCIMF (raw) Sheet:**
```
CXw Section:
  Headers: Row 1-4
  Data: Rows 5-17, Columns A-K
  Total cells: 13 rows × 11 columns = 143 data cells

CYw Section:
  Headers: Row 19-22
  Data: Rows 23-35, Columns A-M
  Total cells: 13 rows × 13 columns = 169 data cells

CMw Section:
  Headers: Row 38-41
  Data: Rows 42-54, Columns A-J
  Total cells: 13 rows × 10 columns = 130 data cells

Current Coefficients:
  CXc: Rows 60-72
  CYc: Rows 110-122
  CMc: Rows 149-161
```

**AQWA OCIMF Inputs Sheet:**
```
Wind Coefficients: Rows 12-38, Columns B-H
Current Coefficients: Rows 55-83, Columns B-H
Key Formulas: C6, C8, P9-V9, etc.
```

### Appendix B: Data Types and Precision

**OCIMF Coefficients:**
- Data type: `float64` (double precision)
- Precision: 4 decimal places for display
- Range: -5.0 to +5.0 (typical)
- Special values: 0.0 (not NaN)

**Hydrodynamic Coefficients:**
- Added mass: `float64`, range 1e3 to 1e10
- Damping: `float64`, range 1e2 to 1e8
- Frequency: `float64`, range 0.1 to 3.0 rad/s

### Appendix C: Validation Test Matrix

| Test Case | Input | Expected Output | Tolerance | Status |
|-----------|-------|----------------|-----------|--------|
| OCIMF-01 | Heading=0°, DR=1.05 | CXw=0.071 | ±0.001 | ⏳ Pending |
| OCIMF-02 | Heading=45°, DR=1.05 | CXw=0.244 | ±0.001 | ⏳ Pending |
| OCIMF-03 | Heading=22.5°, DR=1.05 | CXw≈0.130 | ±0.01 | ⏳ Pending |
| WIND-01 | V=20m/s, θ=0° | Fx from Excel | ±1% | ⏳ Pending |
| WIND-02 | V=20m/s, θ=45° | Fx,Fy,Mz | ±1% | ⏳ Pending |
| HYDRO-01 | ω=0.5rad/s | A(0.5) vs AQWA | ±0.5% | ⏳ Pending |
| HYDRO-02 | Natural freq | Damping 10% | ±2% | ⏳ Pending |

---

**Document Version:** 1.0
**Status:** Draft for Implementation
**Next Review:** After Week 1 implementation
**Contact:** Marine Analysis Team
