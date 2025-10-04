# Component Database Specification

**Module**: Mooring Analysis
**Component**: Component Database
**Status**: Planned 📋
**Source**: Excel sheets "Chain Data", "Wire Data", "Mooring Line Data"
**Priority**: P1 - Critical Path

---

## Overview

Comprehensive mooring component database management system containing specifications for chains, wire ropes, and synthetic lines. Extracted from Excel with 336 total components and 2,434 formulas defining material properties, breaking loads, and stiffness characteristics.

### Database Contents
- **Chain Database**: 60 components (R3, R3S, R4, R4S, R5 grades)
- **Wire Rope Database**: 24 configurations (6×36, 6×19, 8×19 constructions)
- **Synthetic Line Database**: 252 rope types (polyester, nylon, HMPE, aramid)

### Purpose
Provide standardized component property lookup for mooring system design with industry-certified specifications, breaking load calculations, and material characteristics.

---

## Excel Source Analysis

### Chain Data Sheet
**Size**: 60 rows × 13 columns
**Formulas**: 473 formulas
**Excel Location**: "Chain Data" sheet

#### Column Structure:
```
A: Link Type        (Stud Link / Studless)
B: Grade            (R3, R3S, R4, R4S, R5)
C: Diameter [mm]    (16mm to 162mm range)
D: MBL [kN]         = Formula based on grade
E: Stiffness [kN]   = Formula based on link type
F: Weight Air [kg/m]
G: Weight Water [kg/m]
H: Bend Radius [m]
I: Fatigue Category
J: Certification
K: Manufacturer
L: Part Number
M: Notes
```

#### Key Excel Formulas Extracted:

**Minimum Breaking Load (MBL):**
```excel
// Stud Link Chain
=IF(B2="Stud Link", 21900*(C2^2)/1000000, 19900*(C2^2)/1000000)

// Grade adjustment
=D2 * VLOOKUP(B2, GradeTable, 2)  // R3=1.0, R4=1.12, R5=1.25
```

**Axial Stiffness (EA):**
```excel
// Stud Link: 64 GPa
// Studless: 54.4 GPa
=IF(B2="Stud Link", 64000000, 54400000)
```

**Weight Calculations:**
```excel
// Weight in air (kg/m)
=7850 * PI() * (C2/1000)^2 / 4 * LinkFactor

// Weight in water (kg/m) - buoyancy correction
=F2 * (1 - 1025/7850)
```

### Wire Rope Data Sheet
**Size**: 24 rows × 13 columns
**Formulas**: 144 formulas
**Excel Location**: "Wire Data" sheet

#### Wire Rope Constructions:
- 6×36 IWRC (Independent Wire Rope Core)
- 6×19 Seale
- 8×19 Warrington
- Spiral strand

#### Key Excel Formulas:

**Breaking Strength:**
```excel
// Wire rope MBL depends on construction and grade
=IF(Construction="6x36", 1770*Diameter^2,
   IF(Construction="6x19", 1570*Diameter^2,
      1420*Diameter^2))  // kN
```

**Effective Modulus (EA):**
```excel
// Wire rope has lower effective modulus than solid steel
// due to rope construction effects
=80000000 * ConstructionFactor  // kN
// 6x36 IWRC: factor = 1.0
// 6x19: factor = 0.9
```

### Mooring Line Data Sheet
**Size**: 252 rows × 17 columns
**Formulas**: 1,817 formulas
**Excel Location**: "Mooring Line Data" sheet

#### Synthetic Rope Types:
- Polyester (most common for deepwater)
- Nylon (high elongation)
- HMPE (Dyneema, high strength-to-weight)
- Aramid (Kevlar, low elongation)

#### Key Properties:
```excel
// MBL from manufacturer data
=VLOOKUP(RopeType, ManufacturerTable, 3)

// Stiffness (highly nonlinear for synthetics)
// Excel uses multi-point lookup table
=INTERPOLATE(Strain, StiffnessTable)

// Weight in water (very light, buoyant for some)
=DryWeight * (1 - WaterAbsorption) - BuoyancyForce
```

---

## Python Database Design

### Data Model

```python
from dataclasses import dataclass
from enum import Enum
from typing import Optional
import pandas as pd
import numpy as np

class ChainGrade(Enum):
    """Chain grades per offshore standards."""
    R3 = "R3"      # Grade 3 (basic offshore)
    R3S = "R3S"    # Grade 3 Stud Link
    R4 = "R4"      # Grade 4 (high strength)
    R4S = "R4S"    # Grade 4 Stud Link
    R5 = "R5"      # Grade 5 (ultra high strength)

class LinkType(Enum):
    """Chain link construction."""
    STUD_LINK = "Stud Link"
    STUDLESS = "Studless"

@dataclass
class ChainProperties:
    """
    Chain component specification.

    Extracted from Excel "Chain Data" (60 components, 473 formulas)
    """
    diameter: float          # Nominal diameter [mm]
    grade: ChainGrade       # Material grade
    link_type: LinkType     # Stud link or studless
    mbl: float              # Minimum Breaking Load [kN]
    stiffness: float        # EA axial stiffness [kN]
    weight_air: float       # Weight in air [kg/m]
    weight_water: float     # Submerged weight in seawater [kg/m]
    bend_radius: float      # Minimum bend radius [m]
    fatigue_category: str   # Fatigue design category
    certification: str      # ABS/DNV/etc certification
    manufacturer: str       # Manufacturer name
    part_number: str        # Manufacturer part number
    notes: str = ""         # Additional notes

    @classmethod
    def from_excel_formula(
        cls,
        diameter: float,
        grade: ChainGrade,
        link_type: LinkType
    ) -> 'ChainProperties':
        """
        Calculate properties using Excel formulas.

        Implements Excel formulas from "Chain Data" sheet:
        - MBL = 21900*(D^2)/1e6 for stud link [kN]
        - MBL = 19900*(D^2)/1e6 for studless [kN]
        - Grade factors: R3=1.0, R4=1.12, R5=1.25
        - EA = 64 GPa for stud link, 54.4 GPa for studless
        """
        # Base MBL calculation (diameter in mm)
        if link_type == LinkType.STUD_LINK:
            mbl_base = 21.9 * (diameter ** 2) / 1000  # kN
            ea = 64_000_000  # kN (64 GPa)
            link_factor = 5.0  # Volume factor for weight calc
        else:
            mbl_base = 19.9 * (diameter ** 2) / 1000  # kN
            ea = 54_400_000  # kN (54.4 GPa)
            link_factor = 4.5

        # Apply grade factor
        grade_factors = {
            ChainGrade.R3: 1.00,
            ChainGrade.R3S: 1.00,
            ChainGrade.R4: 1.12,
            ChainGrade.R4S: 1.12,
            ChainGrade.R5: 1.25
        }
        mbl = mbl_base * grade_factors[grade]

        # Weight calculations (steel density = 7850 kg/m³)
        # Approximate volume per meter based on link geometry
        volume_per_meter = np.pi * (diameter/1000)**2 / 4 * link_factor
        weight_air = volume_per_meter * 7850  # kg/m

        # Submerged weight (seawater density = 1025 kg/m³)
        weight_water = weight_air * (1 - 1025/7850)  # kg/m

        # Minimum bend radius (typically 4-6× diameter)
        bend_radius = 5.0 * diameter / 1000  # m

        return cls(
            diameter=diameter,
            grade=grade,
            link_type=link_type,
            mbl=mbl,
            stiffness=ea,
            weight_air=weight_air,
            weight_water=weight_water,
            bend_radius=bend_radius,
            fatigue_category="T-curve",  # Typical for offshore chain
            certification="DNV/ABS",
            manufacturer="Generic",
            part_number=f"{diameter}mm-{grade.value}-{link_type.value}",
            notes=f"Calculated from Excel formulas"
        )

    @property
    def weight_force_water(self) -> float:
        """Submerged weight force per meter [N/m]."""
        return self.weight_water * 9.8065

@dataclass
class WireRopeProperties:
    """Wire rope specification from Excel "Wire Data"."""
    diameter: float            # Nominal diameter [mm]
    construction: str          # e.g., "6x36 IWRC"
    grade: str                # Wire grade (e.g., "1960 MPa")
    mbl: float                # Minimum Breaking Load [kN]
    stiffness: float          # EA effective modulus [kN]
    weight_air: float         # Weight in air [kg/m]
    weight_water: float       # Submerged weight [kg/m]

    @classmethod
    def from_excel_formula(
        cls,
        diameter: float,
        construction: str = "6x36 IWRC",
        grade: str = "1960 MPa"
    ) -> 'WireRopeProperties':
        """Calculate properties from Excel formulas."""

        # MBL formulas by construction (from Excel)
        construction_mbl_factors = {
            "6x36 IWRC": 1770,  # kN/mm²
            "6x19 Seale": 1570,
            "8x19 Warrington": 1420,
            "Spiral Strand": 1650
        }

        factor = construction_mbl_factors.get(construction, 1570)
        mbl = factor * (diameter ** 2) / 1000  # kN

        # Effective modulus (lower than steel due to rope construction)
        construction_ea_factors = {
            "6x36 IWRC": 80_000_000,  # kN (80 GPa effective)
            "6x19 Seale": 72_000_000,
            "8x19 Warrington": 70_000_000,
            "Spiral Strand": 85_000_000
        }
        ea = construction_ea_factors.get(construction, 80_000_000)

        # Weight (slightly less than solid steel due to voids)
        # Metallic area ≈ 0.55 * nominal area for 6x36
        metal_fill_factor = 0.55
        volume_per_meter = np.pi * (diameter/1000)**2 / 4 * metal_fill_factor
        weight_air = volume_per_meter * 7850  # kg/m
        weight_water = weight_air * (1 - 1025/7850)

        return cls(
            diameter=diameter,
            construction=construction,
            grade=grade,
            mbl=mbl,
            stiffness=ea,
            weight_air=weight_air,
            weight_water=weight_water
        )

@dataclass
class SyntheticRopeProperties:
    """Synthetic rope specification from Excel "Mooring Line Data"."""
    diameter: float              # Nominal diameter [mm]
    material: str                # Polyester, Nylon, HMPE, Aramid
    construction: str            # 12-strand, 8-strand, parallel, etc.
    mbl: float                  # Minimum Breaking Load [kN]
    stiffness_table: np.ndarray  # Nonlinear stiffness [strain, EA]
    weight_dry: float           # Dry weight [kg/m]
    weight_wet: float           # Wet weight [kg/m]
    elongation_at_break: float  # Ultimate elongation [%]

    def get_stiffness(self, load_fraction: float) -> float:
        """
        Get effective stiffness at load level.

        Synthetic ropes have highly nonlinear stiffness.
        Excel uses multi-point interpolation table.

        Parameters
        ----------
        load_fraction : float
            Load as fraction of MBL (0.0 to 1.0)

        Returns
        -------
        ea : float
            Effective axial stiffness at this load [kN]
        """
        # Interpolate from stiffness table
        # Format: [[strain_1, EA_1], [strain_2, EA_2], ...]
        strains = self.stiffness_table[:, 0]
        eas = self.stiffness_table[:, 1]

        # Convert load fraction to approximate strain
        # This is simplified - real implementation needs iteration
        strain_approx = load_fraction * self.elongation_at_break / 100

        return np.interp(strain_approx, strains, eas)
```

### Database Manager

```python
from pathlib import Path
import pandas as pd
from typing import List, Optional

class ComponentDatabase:
    """
    Mooring component database manager.

    Loads and manages component libraries from Excel extracts:
    - Chain: 60 components (473 formulas)
    - Wire: 24 components (144 formulas)
    - Synthetic: 252 components (1,817 formulas)

    Total: 336 components, 2,434 formulas
    """

    def __init__(self, data_dir: Optional[Path] = None):
        """
        Initialize component database.

        Parameters
        ----------
        data_dir : Path, optional
            Directory containing CSV database files.
            Defaults to module data directory.
        """
        if data_dir is None:
            # Default to module data directory
            data_dir = Path(__file__).parent.parent / "data"

        self.data_dir = Path(data_dir)

        # Load databases from CSV (extracted from Excel)
        self.chain_db = pd.read_csv(self.data_dir / "chain_properties.csv")
        self.wire_db = pd.read_csv(self.data_dir / "wire_properties.csv")
        self.line_db = pd.read_csv(self.data_dir / "line_properties.csv")

        print(f"Loaded {len(self.chain_db)} chain components")
        print(f"Loaded {len(self.wire_db)} wire rope components")
        print(f"Loaded {len(self.line_db)} synthetic line components")

    def get_chain(
        self,
        diameter: float,
        grade: str = "R3",
        link_type: str = "Stud Link"
    ) -> ChainProperties:
        """
        Query chain database by specification.

        Parameters
        ----------
        diameter : float
            Nominal chain diameter [mm]
        grade : str
            Chain grade: R3, R3S, R4, R4S, R5
        link_type : str
            "Stud Link" or "Studless"

        Returns
        -------
        chain : ChainProperties
            Chain component specification

        Raises
        ------
        ValueError
            If component not found in database
        """
        query = (
            (self.chain_db['diameter'] == diameter) &
            (self.chain_db['grade'] == grade) &
            (self.chain_db['link_type'] == link_type)
        )

        matches = self.chain_db[query]

        if len(matches) == 0:
            raise ValueError(
                f"Chain not found: {diameter}mm {grade} {link_type}"
            )

        if len(matches) > 1:
            print(f"Warning: Multiple matches, using first")

        row = matches.iloc[0]
        return ChainProperties(**row.to_dict())

    def find_chain_by_mbl(
        self,
        required_mbl: float,
        grade: str = "R4",
        safety_factor: float = 1.0
    ) -> ChainProperties:
        """
        Find smallest chain meeting MBL requirement.

        Parameters
        ----------
        required_mbl : float
            Required minimum breaking load [kN]
        grade : str
            Desired chain grade
        safety_factor : float
            Safety factor to apply (default 1.0 = no extra margin)

        Returns
        -------
        chain : ChainProperties
            Smallest chain meeting requirement
        """
        target_mbl = required_mbl * safety_factor

        # Filter by grade
        candidates = self.chain_db[self.chain_db['grade'] == grade]

        # Filter by MBL
        suitable = candidates[candidates['mbl'] >= target_mbl]

        if len(suitable) == 0:
            raise ValueError(
                f"No chain found with MBL >= {target_mbl:.0f} kN"
            )

        # Return smallest (by diameter)
        best = suitable.loc[suitable['diameter'].idxmin()]
        return ChainProperties(**best.to_dict())

    def list_available_chains(self, grade: Optional[str] = None) -> pd.DataFrame:
        """List all available chain sizes."""
        if grade:
            return self.chain_db[self.chain_db['grade'] == grade]
        return self.chain_db
```

---

## Data Extraction from Excel

### CSV Export Format

**chain_properties.csv:**
```csv
diameter,grade,link_type,mbl,stiffness,weight_air,weight_water,bend_radius,fatigue_category,certification,manufacturer,part_number,notes
16,R3,Stud Link,5.6,64000000,2.1,1.93,0.08,T-curve,DNV/ABS,Generic,16mm-R3-StudLink,
19,R3,Stud Link,7.9,64000000,3.0,2.75,0.095,T-curve,DNV/ABS,Generic,19mm-R3-StudLink,
...
162,R5,Stud Link,721.5,64000000,217.5,199.8,0.81,T-curve,DNV/ABS,Generic,162mm-R5-StudLink,
```

### Extraction Script

```python
import openpyxl
import pandas as pd

def extract_chain_database_from_excel(excel_path: str, output_csv: str):
    """
    Extract chain database from Excel "Chain Data" sheet.

    Reads 60 rows × 13 columns with 473 formulas.
    Evaluates formulas and exports to CSV.
    """
    wb = openpyxl.load_workbook(excel_path, data_only=True)
    ws = wb["Chain Data"]

    data = []
    for row in ws.iter_rows(min_row=2, max_row=61, values_only=True):
        data.append({
            'link_type': row[0],
            'grade': row[1],
            'diameter': row[2],
            'mbl': row[3],
            'stiffness': row[4],
            'weight_air': row[5],
            'weight_water': row[6],
            'bend_radius': row[7],
            'fatigue_category': row[8],
            'certification': row[9],
            'manufacturer': row[10],
            'part_number': row[11],
            'notes': row[12] or ""
        })

    df = pd.DataFrame(data)
    df.to_csv(output_csv, index=False)
    print(f"Exported {len(df)} chain components to {output_csv}")
```

---

## Validation

### Test Against Excel

```python
def test_chain_mbl_formula():
    """Validate MBL calculation against Excel."""

    # Test case from Excel row 30: 76mm R4 Stud Link
    chain = ChainProperties.from_excel_formula(
        diameter=76,
        grade=ChainGrade.R4,
        link_type=LinkType.STUD_LINK
    )

    # Excel calculated MBL: 141.8 kN
    excel_mbl = 141.8

    assert abs(chain.mbl - excel_mbl) / excel_mbl < 0.01  # Within 1%
    assert chain.stiffness == 64_000_000  # EA for stud link
```

---

## Performance Requirements

- Database load time: <100ms for all 336 components
- Query time: <1ms per lookup
- Memory usage: <10MB for complete database

---

*Component database specification based on 336 Excel components with 2,434 formulas extracted and converted to Python.*
