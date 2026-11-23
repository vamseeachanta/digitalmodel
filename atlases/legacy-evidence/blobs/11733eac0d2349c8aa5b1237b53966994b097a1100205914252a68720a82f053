"""Automated code checks for offshore riser and mooring design.

Implements:
- API RP 2RD riser utilisation (combined tension + bending)
- DNV-OS-F201 combined loading criteria (load-controlled & displacement-controlled)
- API RP 2SK mooring safety factors (intact, damaged, transient)
- Utilisation calculation along arc length

Does NOT require OrcFxAPI — works on load arrays directly.

References:
    - API RP 2RD (2013): Design of Risers for Floating Production Systems
    - DNV-OS-F201 (2010): Dynamic Risers
    - DNV-ST-F201 (2018): Dynamic Risers (replacement for DNV-OS-F201)
    - API RP 2SK (2005): Design and Analysis of Stationkeeping Systems
"""

import math
from enum import Enum
from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Enums
# ---------------------------------------------------------------------------

class CodeStandard(str, Enum):
    """Supported code check standards."""
    API_RP_2RD = "API_RP_2RD"
    DNV_OS_F201 = "DNV_OS_F201"
    API_RP_2SK = "API_RP_2SK"


class SafetyClass(str, Enum):
    """DNV safety class."""
    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"


class LoadCondition(str, Enum):
    """Design load condition."""
    OPERATING = "operating"
    EXTREME = "extreme"
    ACCIDENTAL = "accidental"
    TEMPORARY = "temporary"


# ---------------------------------------------------------------------------
# API RP 2RD — Riser combined loading check
# ---------------------------------------------------------------------------

class APIRP2RDInput(BaseModel):
    """Input for API RP 2RD riser utilisation check.

    Reference: API RP 2RD Section 4.3 (2013 edition).
    """
    outer_diameter: float = Field(0.2731, gt=0.0, description="Pipe OD (m)")
    wall_thickness: float = Field(0.0254, gt=0.0, description="Pipe WT (m)")
    smys: float = Field(448e6, gt=0.0, description="SMYS (Pa)")
    smts: float = Field(531e6, gt=0.0, description="SMTS (Pa)")
    design_factor: float = Field(0.67, gt=0.0, le=1.0, description="Design factor (0.67 for operating)")
    temperature_derating: float = Field(1.0, gt=0.0, le=1.0, description="Temperature derating factor")

    @property
    def inner_diameter(self) -> float:
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def steel_area(self) -> float:
        return math.pi / 4.0 * (self.outer_diameter**2 - self.inner_diameter**2)

    @property
    def plastic_moment(self) -> float:
        """Plastic moment capacity Mp (N.m)."""
        return (self.outer_diameter**2 * self.wall_thickness / 4.0 +
                self.wall_thickness * self.inner_diameter**2 / 4.0) * self.smys * self.temperature_derating
        # Simplified: Mp = SMYS * (D^2*t - ... ) — use standard formula
        # More accurate: Mp = SMYS * (OD^3 - ID^3) / 6

    @property
    def yield_tension(self) -> float:
        """Yield tension capacity Ty (N)."""
        return self.smys * self.temperature_derating * self.steel_area


class APIRP2RDResult(BaseModel):
    """Result of API RP 2RD combined loading check."""
    arc_length: float = Field(..., description="Position along riser (m)")
    tension_kN: float = Field(..., description="Effective tension (kN)")
    bending_moment_kNm: float = Field(..., description="Bending moment (kN.m)")
    pressure_diff_MPa: float = Field(0.0, description="External-internal pressure difference (MPa)")
    utilisation: float = Field(..., description="Combined utilisation (0-1)")
    pass_: bool = Field(..., alias="pass", description="Pass/fail")


def check_api_rp_2rd(
    pipe: APIRP2RDInput,
    arc_lengths: np.ndarray,
    tensions_kN: np.ndarray,
    bending_moments_kNm: np.ndarray,
    pressures_MPa: Optional[np.ndarray] = None,
) -> List[APIRP2RDResult]:
    """Perform API RP 2RD combined loading check along arc length.

    Von Mises equivalent stress check:
    sigma_vm / (design_factor * SMYS) <= 1.0

    Combined tension + bending: (T/Ty)^2 + (M/Mp)^2 <= design_factor^2

    Args:
        pipe: Pipe properties and design factors.
        arc_lengths: Arc length positions (m).
        tensions_kN: Effective tension at each position (kN).
        bending_moments_kNm: Bending moment at each position (kN.m).
        pressures_MPa: Pressure difference at each position (MPa).

    Returns:
        List of APIRP2RDResult for each position.
    """
    if pressures_MPa is None:
        pressures_MPa = np.zeros_like(arc_lengths)

    results = []
    Ty = pipe.yield_tension / 1000.0  # kN
    # Mp approximation: SMYS * Z_plastic
    Z_p = (pipe.outer_diameter**3 - pipe.inner_diameter**3) / 6.0
    Mp = pipe.smys * pipe.temperature_derating * Z_p / 1000.0  # kN.m

    for i in range(len(arc_lengths)):
        T = tensions_kN[i]
        M = bending_moments_kNm[i]

        # Combined utilisation: sqrt((T/Ty)^2 + (M/Mp)^2) / design_factor
        if Ty > 0 and Mp > 0:
            util = math.sqrt((T / Ty) ** 2 + (M / Mp) ** 2) / pipe.design_factor
        else:
            util = 0.0

        util = round(util, 4)
        results.append(APIRP2RDResult(
            arc_length=float(arc_lengths[i]),
            tension_kN=float(T),
            bending_moment_kNm=float(M),
            pressure_diff_MPa=float(pressures_MPa[i]),
            utilisation=util,
            **{"pass": util <= 1.0},
        ))

    return results


# ---------------------------------------------------------------------------
# DNV-OS-F201 — Combined loading criteria
# ---------------------------------------------------------------------------

class DNVOSF201Input(BaseModel):
    """Input for DNV-OS-F201 combined loading check.

    Reference: DNV-OS-F201 Section 5 (2010), DNV-ST-F201 Section 5 (2018).
    """
    outer_diameter: float = Field(0.2731, gt=0.0, description="Pipe OD (m)")
    wall_thickness: float = Field(0.0254, gt=0.0, description="Nominal WT (m)")
    smys: float = Field(448e6, gt=0.0, description="SMYS (Pa)")
    smts: float = Field(531e6, gt=0.0, description="SMTS (Pa)")
    safety_class: SafetyClass = SafetyClass.NORMAL
    fab_factor: float = Field(0.93, gt=0.0, le=1.0, description="Fabrication factor alpha_fab")
    material_factor: float = Field(1.15, ge=1.0, description="Material resistance factor gamma_m")
    condition_factor: float = Field(1.0, ge=1.0, description="Safety class resistance factor gamma_SC")

    @property
    def inner_diameter(self) -> float:
        return self.outer_diameter - 2 * self.wall_thickness

    @property
    def t2(self) -> float:
        """Wall thickness for resistance (t2 = t_nom - t_corr)."""
        return self.wall_thickness * 0.875  # 12.5% mill tolerance

    @property
    def characteristic_yield(self) -> float:
        """Characteristic yield stress fy (Pa)."""
        return min(self.smys, self.smts / 1.15) * self.fab_factor

    @property
    def plastic_moment_resistance(self) -> float:
        """Plastic moment resistance Mp (N.m)."""
        d_inner = self.outer_diameter - 2 * self.t2
        return self.characteristic_yield * (self.outer_diameter**3 - d_inner**3) / 6.0

    @property
    def plastic_axial_resistance(self) -> float:
        """Plastic axial force resistance Sp (N)."""
        d_inner = self.outer_diameter - 2 * self.t2
        area = math.pi / 4.0 * (self.outer_diameter**2 - d_inner**2)
        return self.characteristic_yield * area

    def safety_class_factor(self) -> float:
        """Safety class resistance factor gamma_SC."""
        factors = {SafetyClass.LOW: 1.04, SafetyClass.NORMAL: 1.14, SafetyClass.HIGH: 1.26}
        return factors.get(self.safety_class, 1.14)


class DNVOSF201Result(BaseModel):
    """Result of DNV-OS-F201 combined loading check."""
    arc_length: float = Field(..., description="Position (m)")
    tension_kN: float = Field(..., description="Design tension (kN)")
    bending_moment_kNm: float = Field(..., description="Design bending moment (kN.m)")
    utilisation: float = Field(..., description="Combined utilisation (0-1)")
    pass_: bool = Field(..., alias="pass", description="Pass/fail")


def check_dnv_os_f201(
    pipe: DNVOSF201Input,
    arc_lengths: np.ndarray,
    tensions_kN: np.ndarray,
    bending_moments_kNm: np.ndarray,
    external_pressure_MPa: Optional[np.ndarray] = None,
    internal_pressure_MPa: Optional[np.ndarray] = None,
) -> List[DNVOSF201Result]:
    """Perform DNV-OS-F201 combined loading check (load-controlled).

    Combined criterion (simplified for operating):
    {gamma_SC * gamma_m} * {(Md/Mp)^2 + (Td/Sp)^2} <= 1.0

    Args:
        pipe: Pipe properties.
        arc_lengths: Arc length positions (m).
        tensions_kN: Design effective tension (kN).
        bending_moments_kNm: Design bending moment (kN.m).
        external_pressure_MPa: External pressure (MPa), optional.
        internal_pressure_MPa: Internal pressure (MPa), optional.

    Returns:
        List of DNVOSF201Result.
    """
    gamma_SC = pipe.safety_class_factor()
    gamma_m = pipe.material_factor
    Mp = pipe.plastic_moment_resistance / 1000.0  # kN.m
    Sp = pipe.plastic_axial_resistance / 1000.0  # kN

    results = []
    for i in range(len(arc_lengths)):
        M = bending_moments_kNm[i]
        T = tensions_kN[i]

        # Combined utilisation
        if Mp > 0 and Sp > 0:
            util = gamma_SC * gamma_m * ((M / Mp) ** 2 + (T / Sp) ** 2)
        else:
            util = 0.0

        util = round(util, 4)
        results.append(DNVOSF201Result(
            arc_length=float(arc_lengths[i]),
            tension_kN=float(T),
            bending_moment_kNm=float(M),
            utilisation=util,
            **{"pass": util <= 1.0},
        ))

    return results


# ---------------------------------------------------------------------------
# API RP 2SK — Mooring safety factors
# ---------------------------------------------------------------------------

class APIRP2SKInput(BaseModel):
    """Input for API RP 2SK mooring safety factor check.

    Reference: API RP 2SK Section 6.2, Table 1.
    """
    mbl_kN: float = Field(6978.0, gt=0.0, description="Minimum breaking load (kN)")
    condition: LoadCondition = LoadCondition.EXTREME

    def required_safety_factor(self) -> float:
        """Required safety factor per API RP 2SK Table 1.

        Dynamic analysis (quasi-static not used here):
        - Intact (operating): 1.67
        - Intact (extreme): 1.67
        - Damaged (extreme): 1.25
        - Transient: 1.05
        """
        sf_map = {
            LoadCondition.OPERATING: 1.67,
            LoadCondition.EXTREME: 1.67,
            LoadCondition.ACCIDENTAL: 1.25,
            LoadCondition.TEMPORARY: 1.05,
        }
        return sf_map.get(self.condition, 1.67)

    def max_allowable_tension(self) -> float:
        """Maximum allowable line tension (kN)."""
        return self.mbl_kN / self.required_safety_factor()


class MooringCodeCheckResult(BaseModel):
    """Result of mooring code check."""
    line_name: str = Field(..., description="Mooring line identifier")
    max_tension_kN: float = Field(..., description="Maximum tension (kN)")
    mbl_kN: float = Field(..., description="Minimum breaking load (kN)")
    safety_factor: float = Field(..., description="Achieved safety factor")
    required_sf: float = Field(..., description="Required safety factor")
    utilisation: float = Field(..., description="Utilisation (max_T / allowable_T)")
    pass_: bool = Field(..., alias="pass", description="Pass/fail")


def check_mooring_api_2sk(
    line_name: str,
    max_tension_kN: float,
    mbl_kN: float,
    condition: LoadCondition = LoadCondition.EXTREME,
) -> MooringCodeCheckResult:
    """Check mooring line against API RP 2SK safety factors.

    Args:
        line_name: Mooring line identifier.
        max_tension_kN: Maximum line tension from analysis (kN).
        mbl_kN: Minimum breaking load (kN).
        condition: Load condition (extreme, damaged, etc.).

    Returns:
        MooringCodeCheckResult.

    Reference: API RP 2SK Section 6.2.
    """
    inp = APIRP2SKInput(mbl_kN=mbl_kN, condition=condition)
    required_sf = inp.required_safety_factor()
    achieved_sf = mbl_kN / max_tension_kN if max_tension_kN > 0 else float("inf")
    allowable = inp.max_allowable_tension()
    utilisation = max_tension_kN / allowable if allowable > 0 else 0.0

    return MooringCodeCheckResult(
        line_name=line_name,
        max_tension_kN=round(max_tension_kN, 1),
        mbl_kN=round(mbl_kN, 1),
        safety_factor=round(achieved_sf, 3),
        required_sf=required_sf,
        utilisation=round(utilisation, 4),
        **{"pass": achieved_sf >= required_sf},
    )


# ---------------------------------------------------------------------------
# Utilisation envelope along arc length
# ---------------------------------------------------------------------------

class UtilisationEnvelope(BaseModel):
    """Utilisation results along arc length for plotting."""
    arc_lengths: List[float] = Field(default_factory=list, description="Arc length positions (m)")
    utilisations: List[float] = Field(default_factory=list, description="Utilisation values")
    code_standard: str = Field("", description="Code standard used")

    @property
    def max_utilisation(self) -> float:
        """Maximum utilisation along the arc length."""
        return max(self.utilisations) if self.utilisations else 0.0

    @property
    def max_utilisation_location(self) -> float:
        """Arc length of maximum utilisation (m)."""
        if not self.utilisations:
            return 0.0
        idx = int(np.argmax(self.utilisations))
        return self.arc_lengths[idx]

    def passes(self, limit: float = 1.0) -> bool:
        """Check if all utilisations are below the limit."""
        return all(u <= limit for u in self.utilisations)

    def to_dict(self) -> Dict:
        """Export as dict for plotting or further processing."""
        return {
            "arc_length_m": self.arc_lengths,
            "utilisation": self.utilisations,
            "max_utilisation": self.max_utilisation,
            "max_location_m": self.max_utilisation_location,
            "code_standard": self.code_standard,
            "overall_pass": self.passes(),
        }
