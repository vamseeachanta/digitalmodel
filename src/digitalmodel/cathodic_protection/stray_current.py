"""Stray-current interference: DC potential shift and AC corrosion likelihood.

Re-modelled for issue #2247 (owner direction 2026-09-27) from open
literature. The governing standards (EN 50162, ISO 18086, ISO 15589-1,
NACE SP0169) are **not on file**; every threshold and default constant in
this module is a :class:`~digitalmodel.cathodic_protection._provisional.ProvisionalValue`
naming the literature it was read from and the standard clause that must
confirm it (see ``PROVISIONAL_VALUES`` and the standards-verification
checklist in ``docs/domains/cathodic_protection/standards-inventory.md``).
All public calculations stay behind ``experimental=True``.

DC interference (method shape of an EN 50162-style assessment)
    The assessed quantity is the pipe-to-soil potential shift ``dE`` caused
    by the interference (positive = anodic = current discharge), compared
    with a limit that depends on the soil-resistivity band. ``dE`` is either
    measured (preferred) or computed from a point current source with the
    pipeline treated as a leaky transmission line in the imposed earth
    potential. The remote-earth potential of the source is **not** the pipe
    shift: the pipe metal takes an attenuation-weighted average of the earth
    potential along its length, and only the difference appears across the
    coating.

AC corrosion (criteria form reported for ISO 18086)
    Coupon AC current density from the spread resistance of a circular
    holiday, ``i_ac = 8 V_ac / (rho pi d)``, compared with the AC voltage
    target, the ``i_ac`` limit, the ``i_dc`` limit and the ``i_ac/i_dc``
    ratio limit. ``V_ac`` is a measured (or separately computed) input: no
    induction model is carried here.

Drainage bond
    Ohm's-law bond sizing from the required drainage current and the
    available driving voltage.

Sources (full references in the module constants ``SRC_*``)
    Lynch (2016) CEOCOR, reproducing the EN 50162 shift table; Brenna,
    Beretta & Ormellese (2020) Materials 13:2158, reporting the ISO 18086
    criteria and the coupon-density equation; Newman (1966) J. Electrochem.
    Soc. 113:501, disk spread resistance; Sunde (1968) and Peabody (2001),
    point-source earth potential and leaky-line pipeline attenuation.
"""

from __future__ import annotations

import math
from enum import Enum
from typing import Callable, Final, Optional

from pydantic import BaseModel, Field
from scipy import integrate, optimize

from digitalmodel.cathodic_protection._evidence import BRENNA, CROSSWALK, EN_TABLE_1, PATERLINI
from digitalmodel.cathodic_protection._experimental import require_experimental
from digitalmodel.cathodic_protection._provisional import (
    ProvisionalValue,
    render_provisional,
)

# ---------------------------------------------------------------------------
# Literature sources
# ---------------------------------------------------------------------------

SRC_LYNCH_2016: Final = (
    "C. Lynch, 'dc Stray Currents - Current Practices for Corrosion Protection', "
    "CEOCOR Congress 2016, Ljubljana, slide 12 'Standard - 50162' (reproduces the "
    "EN 50162 maximum positive potential-shift table), "
    "https://ceocor.lu/download/2016_slovenia/2016-LYNCH-DC-Stray-Current.pdf"
)
SRC_BRENNA_2020: Final = (
    "A. Brenna, S. Beretta, M. Ormellese, 'AC Corrosion of Carbon Steel under "
    "Cathodic Protection Condition: Assessment, Criteria and Mechanism. A Review', "
    "Materials 13(9):2158, 2020, doi:10.3390/ma13092158"
)
SRC_NEWMAN_1966: Final = (
    "J. Newman, 'Resistance for Flow of Current to a Disk', J. Electrochem. Soc. "
    "113(5):501-502, 1966, doi:10.1149/1.2424003"
)
SRC_SUNDE_1968: Final = (
    "E. D. Sunde, 'Earth Conduction Effects in Transmission Systems', Dover, "
    "New York, 1968 (point-source earth potential; buried conductor in an imposed "
    "earth potential)"
)
SRC_PEABODY_2001: Final = (
    "A. W. Peabody, 'Peabody's Control of Pipeline Corrosion', 2nd ed., "
    "R. L. Bianchetti (ed.), NACE International, 2001, ISBN 1-57590-092-0 "
    "(pipeline attenuation constant alpha = sqrt(r*g); resistance drainage bonds)"
)
SRC_F103_2010: Final = (
    "DNV-RP-F103 (2010) Sec. 5.6.10, on file; same value as "
    "digitalmodel.cathodic_protection.dnv_rp_f103.STEEL_RESISTIVITY"
)

_PENDING_EN50162_T1: Final = "EN 50162:2004 Table 1 (maximum permissible potential shifts)"
_PENDING_ISO18086: Final = "ISO 18086:2019 AC-corrosion protection criteria clause"

# ---------------------------------------------------------------------------
# Provisional constants (every default used by this module)
# ---------------------------------------------------------------------------

DC_RHO_BAND_LOW_OHM_M: Final = ProvisionalValue(
    15.0, "ohm-m", SRC_LYNCH_2016,
    note="lower soil-resistivity band edge ('< 15' / '15 - 200')",
    pending_standard=_PENDING_EN50162_T1,
    evidence_class="confirmed-by-official-preview", evidence_source=EN_TABLE_1,
)
DC_RHO_BAND_HIGH_OHM_M: Final = ProvisionalValue(
    200.0, "ohm-m", SRC_LYNCH_2016,
    note="middle band includes 200; upper band is strictly above 200 ohm-m",
    pending_standard=_PENDING_EN50162_T1,
    evidence_class="confirmed-by-official-preview", evidence_source=EN_TABLE_1,
)
DC_SHIFT_LIMIT_LOW_RHO_MV: Final = ProvisionalValue(
    20.0, "mV", SRC_LYNCH_2016,
    note="max positive shift including IR drop, rho < 15 ohm-m",
    pending_standard=_PENDING_EN50162_T1,
    evidence_class="confirmed-by-official-preview", evidence_source=EN_TABLE_1,
)
DC_SHIFT_SLOPE_MV_PER_OHM_M: Final = ProvisionalValue(
    1.5, "mV/(ohm-m)", SRC_LYNCH_2016,
    note=(
        "max positive shift including IR drop = 1.5 x rho for 15 <= rho <= 200 ohm-m; "
        "official-preview observation, without-CP scope; extraction rights and "
        "full-edition applicability remain unresolved"
    ),
    pending_standard=_PENDING_EN50162_T1,
    evidence_class="confirmed-by-official-preview", evidence_source=EN_TABLE_1,
)
DC_SHIFT_LIMIT_HIGH_RHO_MV: Final = ProvisionalValue(
    300.0, "mV", SRC_LYNCH_2016,
    note="max positive shift including IR drop, rho > 200 ohm-m",
    pending_standard=_PENDING_EN50162_T1,
    evidence_class="confirmed-by-official-preview", evidence_source=EN_TABLE_1,
)
DC_SHIFT_LIMIT_IR_FREE_MV: Final = ProvisionalValue(
    20.0, "mV", SRC_LYNCH_2016,
    note="max positive shift excluding IR drop, all resistivity bands",
    pending_standard=_PENDING_EN50162_T1,
    evidence_class="confirmed-by-official-preview", evidence_source=EN_TABLE_1,
)
AC_VOLTAGE_TARGET_V: Final = ProvisionalValue(
    15.0, "V rms", SRC_BRENNA_2020,
    note="first step: reduce pipeline AC voltage below 15 V rms (as reported for ISO 18086)",
    pending_standard=_PENDING_ISO18086,
    evidence_class="reproduced-by-secondary",
    evidence_source=BRENNA + "; s2.1, PDF p3",
)
AC_CURRENT_DENSITY_LIMIT_A_M2: Final = ProvisionalValue(
    30.0, "A/m2", SRC_BRENNA_2020,
    note="i_ac < 30 A/m2 on a 1 cm2 coupon over a representative time (e.g. 24 h)",
    pending_standard=_PENDING_ISO18086,
    evidence_class="reproduced-by-secondary",
    evidence_source=PATERLINI,
)
DC_CURRENT_DENSITY_LIMIT_A_M2: Final = ProvisionalValue(
    1.0, "A/m2", SRC_BRENNA_2020,
    note="alternatively, average cathodic i_dc < 1 A/m2 when i_ac > 30 A/m2",
    pending_standard=_PENDING_ISO18086,
    evidence_class="reproduced-by-secondary",
    evidence_source=PATERLINI,
)
AC_DC_RATIO_LIMIT: Final = ProvisionalValue(
    3.0, "dimensionless", SRC_BRENNA_2020,
    note="alternatively, i_ac / i_dc < 3 over a representative time",
    pending_standard=_PENDING_ISO18086,
    evidence_class="reproduced-by-secondary",
    evidence_source=BRENNA + "; s2.3 / s3; " + PATERLINI,
)
COUPON_AREA_M2: Final = ProvisionalValue(
    1.0e-4, "m2", SRC_BRENNA_2020,
    note="criteria are defined on a 1 cm2 coupon or probe",
    pending_standard=_PENDING_ISO18086,
    evidence_class="reproduced-by-secondary",
    evidence_source=BRENNA + "; s2.2 / s3, PDF pp3 and 10",
)
STEEL_RESISTIVITY_OHM_M: Final = ProvisionalValue(
    2.0e-7, "ohm-m", SRC_F103_2010,
    note="carbon-steel line-pipe default; replace with the project linepipe value",
    pending_standard="project linepipe specification (material resistivity)",
    evidence_class="inferred", evidence_source=CROSSWALK,
)

#: Every provisional default of this module, keyed by constant name.
PROVISIONAL_VALUES: Final[dict[str, ProvisionalValue]] = {
    "DC_RHO_BAND_LOW_OHM_M": DC_RHO_BAND_LOW_OHM_M,
    "DC_RHO_BAND_HIGH_OHM_M": DC_RHO_BAND_HIGH_OHM_M,
    "DC_SHIFT_LIMIT_LOW_RHO_MV": DC_SHIFT_LIMIT_LOW_RHO_MV,
    "DC_SHIFT_SLOPE_MV_PER_OHM_M": DC_SHIFT_SLOPE_MV_PER_OHM_M,
    "DC_SHIFT_LIMIT_HIGH_RHO_MV": DC_SHIFT_LIMIT_HIGH_RHO_MV,
    "DC_SHIFT_LIMIT_IR_FREE_MV": DC_SHIFT_LIMIT_IR_FREE_MV,
    "AC_VOLTAGE_TARGET_V": AC_VOLTAGE_TARGET_V,
    "AC_CURRENT_DENSITY_LIMIT_A_M2": AC_CURRENT_DENSITY_LIMIT_A_M2,
    "DC_CURRENT_DENSITY_LIMIT_A_M2": DC_CURRENT_DENSITY_LIMIT_A_M2,
    "AC_DC_RATIO_LIMIT": AC_DC_RATIO_LIMIT,
    "COUPON_AREA_M2": COUPON_AREA_M2,
    "STEEL_RESISTIVITY_OHM_M": STEEL_RESISTIVITY_OHM_M,
}

_GATE_REASON: Final = (
    "re-modelled from open literature (#2247); every threshold is provisional "
    "until the standard is on file"
)
_GATE_STANDARD: Final = "EN 50162 Table 1 (DC) and ISO 18086 (AC)"


# ---------------------------------------------------------------------------
# Enums and I/O models
# ---------------------------------------------------------------------------


class InterferenceType(str, Enum):
    """Type of stray current interference."""

    DC_TRANSIT = "dc_transit"
    DC_HVDC = "dc_hvdc"
    DC_MINING = "dc_mining"
    AC_POWERLINE = "ac_powerline"
    AC_RAILROAD = "ac_railroad"
    TELLURIC = "telluric"


class MitigationType(str, Enum):
    """Stray current mitigation methods."""

    DRAINAGE_BOND = "drainage_bond"
    POLARIZATION_CELL = "polarization_cell"
    GALVANIC_ANODE = "galvanic_anode"
    COATING_IMPROVEMENT = "coating_improvement"
    INSULATING_JOINT = "insulating_joint"
    GROUNDING_CELL = "grounding_cell"


class ShiftBasis(str, Enum):
    """Whether a DC potential shift includes the IR drop."""

    INCLUDING_IR = "including_ir"
    EXCLUDING_IR = "excluding_ir"


_AC_TYPES: Final = frozenset({InterferenceType.AC_POWERLINE, InterferenceType.AC_RAILROAD})


class StrayCurrentInput(BaseModel):
    """Input for a stray-current assessment.

    DC types need either ``measured_shift_mV`` (preferred) or the source
    model inputs ``leakage_current_A``, ``separation_distance_m``,
    ``pipeline_od_m``, ``wall_thickness_m`` and ``coating_resistance_ohm_m2``.
    AC types need ``ac_voltage_V``.
    """

    interference_type: InterferenceType = Field(..., description="Type of interference source")
    soil_resistivity_ohm_m: float = Field(..., gt=0, description="Soil resistivity [ohm-m]")
    pipeline_od_m: Optional[float] = Field(default=None, gt=0, description="Pipeline outer diameter [m]")

    # --- DC ---
    measured_shift_mV: Optional[float] = Field(
        default=None,
        description="Measured pipe-to-soil potential shift due to the interference [mV] "
        "(positive = anodic)",
    )
    shift_basis: ShiftBasis = Field(
        default=ShiftBasis.INCLUDING_IR,
        description="Whether the (measured) shift includes the IR drop",
    )
    leakage_current_A: Optional[float] = Field(
        default=None,
        description="Source leakage current into earth [A]; positive = current leaves the "
        "source into the soil, negative = current collected by the source",
    )
    separation_distance_m: Optional[float] = Field(
        default=None, gt=0, description="Perpendicular distance from source to pipeline [m]"
    )
    wall_thickness_m: Optional[float] = Field(default=None, gt=0, description="Pipe wall thickness [m]")
    coating_resistance_ohm_m2: Optional[float] = Field(
        default=None, gt=0, description="Specific coating resistance [ohm-m2]"
    )
    steel_resistivity_ohm_m: float = Field(
        default=STEEL_RESISTIVITY_OHM_M.value, gt=0,
        description="Pipe steel resistivity [ohm-m] (provisional default)",
    )
    cathodically_protected: bool = Field(
        default=False, description="Structure is under cathodic protection"
    )
    allowable_shift_mV: Optional[float] = Field(
        default=None, gt=0,
        description="Allowable positive shift [mV]; required for cathodically protected "
        "structures, overrides the provisional table otherwise",
    )

    # --- AC ---
    ac_voltage_V: Optional[float] = Field(
        default=None, ge=0, description="Measured pipe-to-soil AC voltage [V rms]"
    )
    holiday_diameter_m: Optional[float] = Field(
        default=None, gt=0,
        description="Circular holiday / coupon diameter [m]; default is the 1 cm2 coupon",
    )
    dc_current_density_A_m2: Optional[float] = Field(
        default=None, ge=0, description="Measured cathodic coupon DC current density [A/m2]"
    )


class StrayCurrentResult(BaseModel):
    """Result of a stray-current assessment."""

    interference_type: str
    method: str = Field(..., description="Calculation route used")
    passes: bool = Field(..., description="All applicable (provisional) criteria met")

    # DC
    potential_shift_mV: Optional[float] = Field(
        default=None, description="Governing (maximum positive, anodic) potential shift [mV]"
    )
    max_cathodic_shift_mV: Optional[float] = Field(
        default=None, description="Most negative shift along the pipe [mV] (source model only)"
    )
    anodic_peak_offset_m: Optional[float] = Field(
        default=None, description="Distance along the pipe of the anodic peak from the closest point [m]"
    )
    shift_basis: Optional[str] = None
    shift_limit_mV: Optional[float] = None
    shift_ratio: Optional[float] = Field(default=None, description="shift / limit")

    # AC
    ac_voltage_V: Optional[float] = None
    ac_current_density_A_m2: Optional[float] = None
    dc_current_density_A_m2: Optional[float] = None
    ac_dc_ratio: Optional[float] = None
    ac_voltage_ok: Optional[bool] = None
    criteria_met: list[str] = Field(default_factory=list)

    recommended_mitigation: list[str] = Field(default_factory=list)
    provenance: dict[str, str] = Field(
        default_factory=dict, description="Provisional values and equation sources used"
    )
    notes: list[str] = Field(default_factory=list)


class MitigationDesign(BaseModel):
    """Drainage-bond sizing output."""

    mitigation_type: str = Field(..., description="Selected mitigation method")
    bond_resistance_ohm: float = Field(..., description="Bond (resistor) resistance [ohm]")
    bond_current_A: float = Field(..., description="Bond current at the design voltage [A]")
    bond_power_W: float = Field(..., description="Power dissipated in the bond resistor [W]")
    provenance: dict[str, str] = Field(default_factory=dict)
    notes: str = Field(default="", description="Design notes")


# ---------------------------------------------------------------------------
# DC: limit table
# ---------------------------------------------------------------------------


def dc_shift_limit_mV(
    soil_resistivity_ohm_m: float,
    basis: ShiftBasis = ShiftBasis.INCLUDING_IR,
    *,
    experimental: bool = False,
) -> float:
    """Maximum positive potential shift for a structure without CP [mV].

    Provisional band table (EN 50162 official preview, Table 1, printed p10):

    ============================  =====================  =====================
    Soil resistivity rho [ohm-m]  incl. IR drop [mV]     excl. IR drop [mV]
    ============================  =====================  =====================
    rho < 15                      20                     20
    15 <= rho <= 200              1.5 * rho              20
    rho > 200                     300                    20
    ============================  =====================  =====================
    """
    require_experimental(
        experimental, model="stray_current.dc_shift_limit_mV",
        reason=_GATE_REASON, standard=_GATE_STANDARD,
    )
    rho = _positive(soil_resistivity_ohm_m, "soil_resistivity_ohm_m")
    if ShiftBasis(basis) is ShiftBasis.EXCLUDING_IR:
        return DC_SHIFT_LIMIT_IR_FREE_MV.value
    if rho < DC_RHO_BAND_LOW_OHM_M.value:
        return DC_SHIFT_LIMIT_LOW_RHO_MV.value
    if rho <= DC_RHO_BAND_HIGH_OHM_M.value:
        return DC_SHIFT_SLOPE_MV_PER_OHM_M.value * rho
    return DC_SHIFT_LIMIT_HIGH_RHO_MV.value


# ---------------------------------------------------------------------------
# DC: point source + leaky transmission line
# ---------------------------------------------------------------------------


def pipe_attenuation_constant(
    pipeline_od_m: float,
    wall_thickness_m: float,
    coating_resistance_ohm_m2: float,
    steel_resistivity_ohm_m: float = STEEL_RESISTIVITY_OHM_M.value,
    *,
    experimental: bool = False,
) -> float:
    """Attenuation constant ``alpha = sqrt(r' g')`` of a coated pipe [1/m].

    ``r' = rho_steel / (pi t (D - t))`` [ohm/m] (longitudinal resistance) and
    ``g' = pi D / R_c`` [S/m] (coating leakage conductance), Peabody (2001).
    """
    require_experimental(
        experimental, model="stray_current.pipe_attenuation_constant",
        reason=_GATE_REASON, standard=_GATE_STANDARD,
    )
    return _alpha(pipeline_od_m, wall_thickness_m, coating_resistance_ohm_m2,
                  steel_resistivity_ohm_m)


def _alpha(od: float, wt: float, rc: float, rho_steel: float) -> float:
    od = _positive(od, "pipeline_od_m")
    wt = _positive(wt, "wall_thickness_m")
    if wt >= od / 2.0:
        raise ValueError(f"wall_thickness_m ({wt}) must be less than half the OD ({od})")
    r_long = _positive(rho_steel, "steel_resistivity_ohm_m") / (math.pi * wt * (od - wt))
    g_leak = math.pi * od / _positive(rc, "coating_resistance_ohm_m2")
    return math.sqrt(r_long * g_leak)


def _unit_shift(xi: float, a: float) -> float:
    """Dimensionless shift ``F(xi, a)`` for a unit point source.

    With ``V_e(x) = K / sqrt(x^2 + d^2)``, ``K = rho I / (2 pi)``, ``xi = x/d``
    and ``a = alpha d``, the pipe-to-soil shift is ``dE = (K/d) F(xi, a)``,

        F = 1/2 int_0^inf e^-v [g(xi + v/a) + g(xi - v/a)] dv - g(xi),
        g(u) = 1 / sqrt(u^2 + 1),

    i.e. the infinite leaky line's Green's-function average of the earth
    potential minus the local earth potential.
    """

    def g(u: float) -> float:
        return 1.0 / math.sqrt(u * u + 1.0)

    def integrand(v: float) -> float:
        return math.exp(-v) * (g(xi + v / a) + g(xi - v / a))

    upper = 60.0
    breaks = [p for p in (a * abs(xi),) if 0.0 < p < upper]
    val, _err = integrate.quad(integrand, 0.0, upper, points=breaks or None, limit=400,
                               epsabs=1e-12, epsrel=1e-10)
    return 0.5 * float(val) - g(xi)


def point_source_shift_mV(
    offset_m: float,
    leakage_current_A: float,
    separation_distance_m: float,
    soil_resistivity_ohm_m: float,
    attenuation_constant_per_m: float,
    *,
    experimental: bool = False,
) -> float:
    """Pipe-to-soil shift [mV] at ``offset_m`` along the pipe from the closest point.

    Earth potential of a surface point source in a uniform half-space
    (Sunde 1968): ``V_e(r) = rho I / (2 pi r)``. Pipeline as an infinite
    leaky transmission line (Sunde 1968; Peabody 2001):
    ``V_p'' = alpha^2 (V_p - V_e)``, bounded solution
    ``V_p(x) = (alpha/2) int V_e(s) exp(-alpha |x - s|) ds``; the shift is
    ``dE = V_p - V_e`` (positive = anodic, current discharge). The pipe is a
    passive probe (its own leakage does not perturb ``V_e``).
    """
    require_experimental(
        experimental, model="stray_current.point_source_shift_mV",
        reason=_GATE_REASON, standard=_GATE_STANDARD,
    )
    d = _positive(separation_distance_m, "separation_distance_m")
    rho = _positive(soil_resistivity_ohm_m, "soil_resistivity_ohm_m")
    a = _positive(attenuation_constant_per_m, "attenuation_constant_per_m") * d
    k = rho * leakage_current_A / (2.0 * math.pi)
    return 1000.0 * k / d * _unit_shift(offset_m / d, a)


def _profile_extrema(a: float) -> tuple[tuple[float, float], tuple[float, float]]:
    """Return ((xi_max, F_max), (xi_min, F_min)) over xi >= 0."""
    f: Callable[[float], float] = lambda xi: _unit_shift(xi, a)  # noqa: E731
    hi = max(1.0e3, 40.0 / a)
    n = 120
    grid = [0.0] + [1.0e-3 * (hi / 1.0e-3) ** (i / (n - 1)) for i in range(n)]
    vals = [f(x) for x in grid]

    def refine(idx: int, sign: float) -> tuple[float, float]:
        lo = grid[max(idx - 1, 0)]
        up = grid[min(idx + 1, len(grid) - 1)]
        if up <= lo:
            return grid[idx], vals[idx]
        res = optimize.minimize_scalar(lambda x: -sign * f(x), bounds=(lo, up),
                                       method="bounded", options={"xatol": 1e-6 * up})
        x_best, v_best = float(res.x), float(-sign * res.fun)
        if sign * v_best < sign * vals[idx]:
            return grid[idx], vals[idx]
        return x_best, v_best

    i_max = max(range(len(vals)), key=vals.__getitem__)
    i_min = min(range(len(vals)), key=vals.__getitem__)
    return refine(i_max, 1.0), refine(i_min, -1.0)


# ---------------------------------------------------------------------------
# AC: coupon current density
# ---------------------------------------------------------------------------


def coupon_diameter_m(area_m2: float = COUPON_AREA_M2.value) -> float:
    """Diameter of a circular coupon of the given area [m]."""
    return math.sqrt(4.0 * _positive(area_m2, "area_m2") / math.pi)


def ac_current_density_A_m2(
    ac_voltage_V: float,
    soil_resistivity_ohm_m: float,
    holiday_diameter_m: float,
    pipeline_od_m: Optional[float] = None,
    *,
    experimental: bool = False,
) -> float:
    """AC current density through a circular holiday / coupon [A/m2].

    ``i_ac = 8 V_ac / (rho pi d)`` (Brenna et al. 2020, eq. for a circular
    coating defect). It follows from the spread resistance of a disk of
    diameter ``d`` flush with an insulating plane on a uniform half-space,
    ``R = rho / (2 d)`` (Newman 1966), over the disk area ``pi d^2 / 4``.

    Raises
    ------
    ValueError
        ``ac_voltage_V`` negative, non-positive resistivity or diameter, or a
        holiday not smaller than the pipe OD (the flat-disk idealisation does
        not hold).
    """
    require_experimental(
        experimental, model="stray_current.ac_current_density_A_m2",
        reason=_GATE_REASON, standard=_GATE_STANDARD,
    )
    if not math.isfinite(ac_voltage_V) or ac_voltage_V < 0.0:
        raise ValueError(f"ac_voltage_V must be a finite rms value >= 0, got {ac_voltage_V}")
    rho = _positive(soil_resistivity_ohm_m, "soil_resistivity_ohm_m")
    d = _positive(holiday_diameter_m, "holiday_diameter_m")
    if pipeline_od_m is not None and d >= _positive(pipeline_od_m, "pipeline_od_m"):
        raise ValueError(
            f"holiday_diameter_m ({d}) must be smaller than the pipe OD ({pipeline_od_m}): "
            "the disk spread-resistance formula assumes a small flat holiday"
        )
    return 8.0 * ac_voltage_V / (rho * math.pi * d)


# ---------------------------------------------------------------------------
# Assessment
# ---------------------------------------------------------------------------


def assess_stray_current(
    input_params: StrayCurrentInput,
    *,
    experimental: bool = False,
) -> StrayCurrentResult:
    """Assess DC interference or AC corrosion likelihood on a buried pipeline.

    Experimental
    ------------
    Re-modelled from open literature for issue #2247; every threshold is a
    provisional value (see ``PROVISIONAL_VALUES``) pending EN 50162 Table 1
    (DC) and ISO 18086 (AC). Calling without ``experimental=True`` raises
    :class:`~digitalmodel.cathodic_protection._experimental.ExperimentalModelError`.

    Raises
    ------
    ExperimentalModelError
        If ``experimental`` is false.
    ValueError
        Missing inputs for the selected route, or inputs outside the
        validity range of the model.
    """
    require_experimental(
        experimental, model="stray_current.assess_stray_current",
        reason=_GATE_REASON, standard=_GATE_STANDARD,
    )
    if input_params.interference_type in _AC_TYPES:
        return _assess_ac(input_params)
    return _assess_dc(input_params)


def _assess_dc(p: StrayCurrentInput) -> StrayCurrentResult:
    provenance: dict[str, str] = {}
    notes: list[str] = []

    # Limit
    if p.allowable_shift_mV is not None:
        limit = p.allowable_shift_mV
        provenance["shift_limit"] = "user input allowable_shift_mV"
    elif p.cathodically_protected:
        raise ValueError(
            "allowable_shift_mV is required for a cathodically protected structure: the "
            "open literature gives no tabulated shift limit (the acceptance is that the "
            "protection criterion is still met under interference); supply the margin"
        )
    else:
        limit = dc_shift_limit_mV(p.soil_resistivity_ohm_m, p.shift_basis, experimental=True)
        for key in _dc_limit_keys(p.soil_resistivity_ohm_m, p.shift_basis):
            provenance[key] = render_provisional(PROVISIONAL_VALUES[key], key)

    max_cathodic: Optional[float] = None
    peak_offset: Optional[float] = None
    if p.measured_shift_mV is not None:
        method = "measured_shift"
        shift = p.measured_shift_mV
    else:
        if p.interference_type is InterferenceType.TELLURIC:
            raise ValueError("telluric interference needs measured_shift_mV (no source model)")
        missing = [
            name for name in ("leakage_current_A", "separation_distance_m", "pipeline_od_m",
                              "wall_thickness_m", "coating_resistance_ohm_m2")
            if getattr(p, name) is None
        ]
        if missing:
            raise ValueError(
                "DC assessment needs measured_shift_mV or the source-model inputs; missing: "
                + ", ".join(missing)
            )
        if p.shift_basis is ShiftBasis.EXCLUDING_IR:
            raise ValueError(
                "the source model returns a shift including the coating IR drop; "
                "use shift_basis=including_ir"
            )
        assert p.separation_distance_m is not None and p.pipeline_od_m is not None
        assert p.wall_thickness_m is not None and p.coating_resistance_ohm_m2 is not None
        assert p.leakage_current_A is not None
        d = p.separation_distance_m
        if d <= p.pipeline_od_m:
            raise ValueError(
                f"separation_distance_m ({d}) must exceed the pipe OD ({p.pipeline_od_m}) "
                "for the line-conductor idealisation"
            )
        alpha = _alpha(p.pipeline_od_m, p.wall_thickness_m, p.coating_resistance_ohm_m2,
                       p.steel_resistivity_ohm_m)
        (xi_max, f_max), (xi_min, f_min) = _profile_extrema(alpha * d)
        scale = 1000.0 * p.soil_resistivity_ohm_m * p.leakage_current_A / (2.0 * math.pi * d)
        if scale >= 0.0:
            shift, peak_offset, max_cathodic = scale * f_max, xi_max * d, scale * f_min
        else:
            shift, peak_offset, max_cathodic = scale * f_min, xi_min * d, scale * f_max
        method = "point_source_transmission_line"
        provenance["earth_potential"] = f"V_e = rho I / (2 pi r): {SRC_SUNDE_1968}"
        provenance["pipe_response"] = (
            "V_p'' = alpha^2 (V_p - V_e), alpha = sqrt(r' g'): "
            f"{SRC_SUNDE_1968}; {SRC_PEABODY_2001}"
        )
        if p.steel_resistivity_ohm_m == STEEL_RESISTIVITY_OHM_M.value:
            provenance["STEEL_RESISTIVITY_OHM_M"] = render_provisional(
                STEEL_RESISTIVITY_OHM_M, "STEEL_RESISTIVITY_OHM_M")
        notes.append(
            f"attenuation constant alpha = {alpha:.4g} 1/m; uniform half-space, infinite "
            "pipe, pipe does not perturb the source field"
        )

    ratio = shift / limit
    passes = shift <= limit
    if not passes:
        mitigation = [MitigationType.DRAINAGE_BOND.value, MitigationType.INSULATING_JOINT.value,
                      MitigationType.COATING_IMPROVEMENT.value]
    else:
        mitigation = []
    return StrayCurrentResult(
        interference_type=p.interference_type.value,
        method=method,
        passes=passes,
        potential_shift_mV=shift,
        max_cathodic_shift_mV=max_cathodic,
        anodic_peak_offset_m=peak_offset,
        shift_basis=p.shift_basis.value,
        shift_limit_mV=limit,
        shift_ratio=ratio,
        recommended_mitigation=mitigation,
        provenance=provenance,
        notes=notes,
    )


def _dc_limit_keys(rho: float, basis: ShiftBasis) -> list[str]:
    if basis is ShiftBasis.EXCLUDING_IR:
        return ["DC_SHIFT_LIMIT_IR_FREE_MV"]
    if rho < DC_RHO_BAND_LOW_OHM_M.value:
        return ["DC_RHO_BAND_LOW_OHM_M", "DC_SHIFT_LIMIT_LOW_RHO_MV"]
    if rho <= DC_RHO_BAND_HIGH_OHM_M.value:
        return ["DC_RHO_BAND_LOW_OHM_M", "DC_RHO_BAND_HIGH_OHM_M", "DC_SHIFT_SLOPE_MV_PER_OHM_M"]
    return ["DC_RHO_BAND_HIGH_OHM_M", "DC_SHIFT_LIMIT_HIGH_RHO_MV"]


def _assess_ac(p: StrayCurrentInput) -> StrayCurrentResult:
    if p.ac_voltage_V is None:
        raise ValueError(
            "AC assessment needs ac_voltage_V (measured, or from a separate induction "
            "study): no simplified induction model is carried"
        )
    notes: list[str] = []
    provenance: dict[str, str] = {
        "i_ac": f"i_ac = 8 V_ac / (rho pi d): {SRC_BRENNA_2020}; R = rho/(2d): {SRC_NEWMAN_1966}",
    }
    if p.holiday_diameter_m is None:
        d = coupon_diameter_m()
        provenance["COUPON_AREA_M2"] = render_provisional(COUPON_AREA_M2, "COUPON_AREA_M2")
    else:
        d = p.holiday_diameter_m
        if not math.isclose(d, coupon_diameter_m(), rel_tol=1e-3):
            notes.append(
                "criteria are defined on a 1 cm2 coupon; the density at this holiday "
                "diameter is not directly comparable"
            )
    i_ac = ac_current_density_A_m2(p.ac_voltage_V, p.soil_resistivity_ohm_m, d,
                                   p.pipeline_od_m, experimental=True)

    for key in ("AC_VOLTAGE_TARGET_V", "AC_CURRENT_DENSITY_LIMIT_A_M2"):
        provenance[key] = render_provisional(PROVISIONAL_VALUES[key], key)
    voltage_ok = p.ac_voltage_V < AC_VOLTAGE_TARGET_V.value
    met: list[str] = []
    if i_ac < AC_CURRENT_DENSITY_LIMIT_A_M2.value:
        met.append("i_ac < limit")
    ratio: Optional[float] = None
    i_dc = p.dc_current_density_A_m2
    if i_dc is None:
        notes.append("i_dc not given: the i_dc and i_ac/i_dc alternatives were not evaluated")
    else:
        for key in ("DC_CURRENT_DENSITY_LIMIT_A_M2", "AC_DC_RATIO_LIMIT"):
            provenance[key] = render_provisional(PROVISIONAL_VALUES[key], key)
        if i_dc < DC_CURRENT_DENSITY_LIMIT_A_M2.value:
            met.append("i_dc < limit")
        if i_dc > 0.0:
            ratio = i_ac / i_dc
            if ratio < AC_DC_RATIO_LIMIT.value:
                met.append("i_ac/i_dc < limit")
    passes = voltage_ok and bool(met)
    mitigation: list[str] = []
    if not passes:
        mitigation = [MitigationType.GROUNDING_CELL.value, MitigationType.POLARIZATION_CELL.value]
    return StrayCurrentResult(
        interference_type=p.interference_type.value,
        method="coupon_spread_resistance",
        passes=passes,
        ac_voltage_V=p.ac_voltage_V,
        ac_current_density_A_m2=i_ac,
        dc_current_density_A_m2=i_dc,
        ac_dc_ratio=ratio,
        ac_voltage_ok=voltage_ok,
        criteria_met=met,
        recommended_mitigation=mitigation,
        provenance=provenance,
        notes=notes,
    )


# ---------------------------------------------------------------------------
# Drainage bond
# ---------------------------------------------------------------------------


def design_drainage_bond(
    required_drainage_current_A: float,
    driving_voltage_V: float,
    circuit_resistance_ohm: float = 0.0,
    *,
    experimental: bool = False,
) -> MitigationDesign:
    """Size a resistance drainage bond by Ohm's law.

    ``R_bond = V_drive / I_drain - R_circuit`` and ``P = I_drain^2 R_bond``
    (resistance bond, Peabody 2001). ``V_drive`` is the measured open-circuit
    voltage between the pipeline and the stray-current return at the bond
    location; ``R_circuit`` is the rest of the loop (cables, contacts).
    The required drainage current comes from a survey or an interference
    test, not from this function.

    Raises
    ------
    ExperimentalModelError
        If ``experimental`` is false.
    ValueError
        Non-positive current or voltage, negative circuit resistance, or a
        driving voltage too low to pass the current through the circuit.
    """
    require_experimental(
        experimental, model="stray_current.design_drainage_bond",
        reason=_GATE_REASON, standard="EN 50162 Table 1",
    )
    i_d = _positive(required_drainage_current_A, "required_drainage_current_A")
    v_d = _positive(driving_voltage_V, "driving_voltage_V")
    if not math.isfinite(circuit_resistance_ohm) or circuit_resistance_ohm < 0.0:
        raise ValueError(f"circuit_resistance_ohm must be >= 0, got {circuit_resistance_ohm}")
    r_bond = v_d / i_d - circuit_resistance_ohm
    if r_bond <= 0.0:
        raise ValueError(
            f"driving voltage {v_d} V cannot pass {i_d} A through a circuit of "
            f"{circuit_resistance_ohm} ohm; no bond resistance is needed or the bond "
            "must be forced (rectifier)"
        )
    return MitigationDesign(
        mitigation_type=MitigationType.DRAINAGE_BOND.value,
        bond_resistance_ohm=r_bond,
        bond_current_A=i_d,
        bond_power_W=i_d * i_d * r_bond,
        provenance={"bond": f"R = V/I - R_circuit, P = I^2 R: {SRC_PEABODY_2001}"},
        notes=(
            "Verify the bond current direction under all traffic/operating states; "
            "a fluctuating source needs a unidirectional (diode) bond."
        ),
    )


def _positive(value: float, name: str) -> float:
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{name} must be a finite positive number, got {value}")
    return float(value)
