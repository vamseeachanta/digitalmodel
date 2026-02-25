"""Pipeline pressure containment checks — DNV-ST-F101 and API RP 1111 (WRK-355).

Implements core functions:
- Wall thickness sizing (minimum required wall thickness)
- System pressure test validation
- MAOP (Maximum Allowable Operating Pressure)
- DNV-ST-F101 burst resistance and pressure containment
- API RP 1111 external collapse and propagating buckle

Extended functions (imported for backward-compatibility):
- DNV-ST-F101 collapse, propagating buckle, combined loading: see pipeline_pressure_dnv
- API RP 1111 internal pressure, combined loading, workflow: see pipeline_pressure_workflow

All pressures in MPa; all dimensions in metres (SI throughout).
"""
from __future__ import annotations

import math
from dataclasses import dataclass


# ---------------------------------------------------------------------------
# Design constants — DNV-ST-F101 (2021)
# ---------------------------------------------------------------------------

#: Safety class resistance factors (DNV-ST-F101 Table 5-3)
GAMMA_SC: dict[str, float] = {
    "low": 1.046,
    "normal": 1.138,
    "high": 1.308,
}

#: Material resistance factor (DNV-ST-F101 Section 5.3)
GAMMA_M: float = 1.15

#: Incidental-to-design pressure ratio (DNV-ST-F101 Section 5.2)
GAMMA_INC: float = 1.1

#: Material strength / usage factor (DNV-ST-F101 Section 5.3)
ALPHA_U: float = 0.96

# ---------------------------------------------------------------------------
# Design constants — API RP 1111 (4th Edition)
# ---------------------------------------------------------------------------

#: Collapse design factor for external pressure (API RP 1111 Section 4.3.3)
DF_COLLAPSE: float = 0.7

#: Safety factor against propagating buckle initiation (API RP 1111 Section 4.3.2)
DF_PROPAGATION: float = 1.3

# ---------------------------------------------------------------------------
# Material constants
# ---------------------------------------------------------------------------

#: Steel Young's modulus [MPa]
_E_STEEL: float = 207000.0

#: Steel Poisson's ratio
_NU_STEEL: float = 0.3

#: Seawater density [kg/m3]
_RHO_SW: float = 1025.0

#: Gravitational acceleration [m/s2]
_G: float = 9.81

_VALID_SAFETY_CLASSES = frozenset(GAMMA_SC)


# ---------------------------------------------------------------------------
# Data structures
# ---------------------------------------------------------------------------

@dataclass
class PipeGeometry:
    """Pipe cross-section geometry.

    Args:
        OD:  Nominal outside diameter [m]. Must be > 0.
        WT:  Nominal wall thickness [m]. Must be > 0 and < OD/2.
    """

    OD: float
    WT: float

    def __post_init__(self) -> None:
        if self.OD <= 0.0:
            raise ValueError(f"OD must be > 0, got {self.OD}")
        if self.WT <= 0.0:
            raise ValueError(f"WT must be > 0, got {self.WT}")
        if self.WT >= self.OD / 2.0:
            raise ValueError(
                f"WT ({self.WT}) must be < OD/2 ({self.OD / 2.0:.6f})"
            )

    @property
    def D_over_t(self) -> float:
        """Outside diameter to wall-thickness ratio."""
        return self.OD / self.WT


@dataclass
class PipeMaterial:
    """Pipeline material properties.

    Args:
        name:  Material grade label (e.g. ``"X65"``).
        SMYS:  Specified Minimum Yield Strength [MPa]. Must be > 0.
        SMTS:  Specified Minimum Tensile Strength [MPa]. Must be >= SMYS.
    """

    name: str
    SMYS: float
    SMTS: float

    def __post_init__(self) -> None:
        if self.SMYS <= 0.0:
            raise ValueError(f"SMYS must be > 0, got {self.SMYS}")
        if self.SMTS < self.SMYS:
            raise ValueError(
                f"SMTS ({self.SMTS}) must be >= SMYS ({self.SMYS})"
            )


# ---------------------------------------------------------------------------
# Material library
# ---------------------------------------------------------------------------

#: Standard pipeline material grades: name -> PipeMaterial
MATERIAL_LIBRARY: dict[str, PipeMaterial] = {
    "X52": PipeMaterial(name="X52", SMYS=358.0, SMTS=455.0),
    "X60": PipeMaterial(name="X60", SMYS=413.0, SMTS=517.0),
    "X65": PipeMaterial(name="X65", SMYS=448.0, SMTS=531.0),
    "X70": PipeMaterial(name="X70", SMYS=482.0, SMTS=565.0),
}


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

def _validate_safety_class(safety_class: str) -> None:
    """Raise ValueError for unrecognised safety class strings."""
    if safety_class not in _VALID_SAFETY_CLASSES:
        raise ValueError(
            f"safety_class must be one of {sorted(_VALID_SAFETY_CLASSES)}, "
            f"got {safety_class!r}"
        )


# ---------------------------------------------------------------------------
# DNV-ST-F101: wall thickness sizing
# ---------------------------------------------------------------------------

def dnv_wall_thickness(
    p_d: float,
    p_e: float,
    D: float,
    mat: PipeMaterial,
    safety_class: str = "normal",
    t_corr: float = 0.001,
    fab_tol_pct: float = 12.5,
) -> float:
    """Minimum required wall thickness per DNV-ST-F101 pressure containment limit state.

    Derived from the limit state check p_d <= p_b / (gamma_inc * gamma_m * gamma_SC)
    where p_b = 2 * (t/D) * SMYS_T * alpha_U::

        t_nom_bare = (p_d - p_e) * D * gamma_inc * gamma_m * gamma_SC
                     / (2 * SMYS * alpha_U)
        t_min      = t_nom_bare / (1 - fab_tol_pct/100) + t_corr

    A higher safety class (larger gamma_SC) requires a thicker wall.

    Args:
        p_d:           Design pressure [MPa].
        p_e:           External (hydrostatic) pressure [MPa].
        D:             Nominal outside diameter [m].
        mat:           Pipeline material.
        safety_class:  ``"low"``, ``"normal"``, or ``"high"``.
        t_corr:        Corrosion allowance [m]. Default 0.001 m (1 mm).
        fab_tol_pct:   Negative fabrication tolerance [%]. Default 12.5 %.

    Returns:
        Minimum nominal wall thickness t_min [m].

    Raises:
        ValueError: If safety_class is invalid.
    """
    _validate_safety_class(safety_class)
    gsc = GAMMA_SC[safety_class]

    numerator = (p_d - p_e) * D * GAMMA_INC * GAMMA_M * gsc
    t_bare = numerator / (2.0 * mat.SMYS * ALPHA_U)
    # Account for fabrication tolerance and corrosion allowance
    t_min = t_bare / (1.0 - fab_tol_pct / 100.0) + t_corr
    return t_min


# ---------------------------------------------------------------------------
# DNV-ST-F101: burst resistance
# ---------------------------------------------------------------------------

def dnv_burst_resistance(
    D: float,
    t_nom: float,
    mat: PipeMaterial,
    t_fab_neg: float = 0.0,
    t_corr: float = 0.0,
    temp_derating: float = 0.0,
) -> float:
    """Burst resistance p_b(t1) per DNV-ST-F101 Eq. (5.4).

    Formula::

        smys_t = min(SMYS, 0.9 * SMTS) - temp_derating
        t1     = t_nom * (1 - t_fab_neg/100) - t_corr
        p_b    = 2 * (t1 / D) * smys_t

    Args:
        D:             Nominal outside diameter [m].
        t_nom:         Nominal wall thickness [m].
        mat:           Pipeline material.
        t_fab_neg:     Negative fabrication tolerance [% of t_nom]. Default 0.0.
        t_corr:        Corrosion/erosion allowance [m]. Default 0.0.
        temp_derating: Temperature de-rating applied to SMYS and SMTS [MPa]. Default 0.0.

    Returns:
        Burst resistance p_b [MPa].
    """
    smys_derating = mat.SMYS - temp_derating
    smts_derating = mat.SMTS - temp_derating
    smys_t = min(smys_derating, 0.9 * smts_derating)

    t1 = t_nom * (1.0 - t_fab_neg / 100.0) - t_corr
    return 2.0 * (t1 / D) * smys_t


# ---------------------------------------------------------------------------
# DNV-ST-F101: pressure containment check
# ---------------------------------------------------------------------------

def dnv_pressure_containment_check(
    p_li: float,
    D: float,
    t_nom: float,
    mat: PipeMaterial,
    safety_class: str = "normal",
    **kwargs: float,
) -> dict[str, object]:
    """Check local incidental pressure against allowable burst resistance.

    Condition (DNV-ST-F101 Section 5.4.2.1)::

        p_li <= p_b(t1) / gamma_inc

    Args:
        p_li:          Local incidental pressure [MPa].
        D:             Nominal outside diameter [m].
        t_nom:         Nominal wall thickness [m].
        mat:           Pipeline material.
        safety_class:  One of ``"low"``, ``"normal"``, ``"high"``.
        **kwargs:      Forwarded to :func:`dnv_burst_resistance`
                       (``t_fab_neg``, ``t_corr``, ``temp_derating``).

    Returns:
        Dict with keys:
        - ``pass`` (bool): True when criterion is satisfied.
        - ``utilization`` (float): p_li / p_b_allowable.
        - ``p_b`` (float): Burst resistance [MPa].
        - ``p_b_allowable`` (float): p_b / gamma_inc [MPa].
    """
    _validate_safety_class(safety_class)

    p_b = dnv_burst_resistance(D=D, t_nom=t_nom, mat=mat, **kwargs)
    p_b_allowable = p_b / GAMMA_INC
    utilization = p_li / p_b_allowable if p_b_allowable > 0.0 else float("inf")

    return {
        "pass": utilization <= 1.0,
        "utilization": utilization,
        "p_b": p_b,
        "p_b_allowable": p_b_allowable,
    }


# ---------------------------------------------------------------------------
# DNV-ST-F101: system pressure test
# ---------------------------------------------------------------------------

def dnv_system_pressure_test(p_d: float, factor: float = 1.25) -> float:
    """Minimum system pressure test requirement per DNV-ST-F101 Eq. (5.16).

    p_test >= factor * p_d   (minimum factor = 1.25 for offshore pipelines)

    Args:
        p_d:     Design pressure [MPa]. Must be > 0.
        factor:  Test-to-design pressure ratio. Default 1.25.

    Returns:
        Minimum required test pressure p_test [MPa].

    Raises:
        ValueError: If p_d is not positive.
    """
    if p_d <= 0.0:
        raise ValueError(f"p_d must be > 0, got {p_d}")
    return factor * p_d


# ---------------------------------------------------------------------------
# DNV-ST-F101: MAOP
# ---------------------------------------------------------------------------

def dnv_maop(
    D: float,
    t_nom: float,
    mat: PipeMaterial,
    safety_class: str = "normal",
    **kwargs: float,
) -> float:
    """Maximum Allowable Operating Pressure per DNV-ST-F101 Section 5.

    MAOP = p_b / (gamma_inc * gamma_m * gamma_sc)

    Args:
        D:             Nominal outside diameter [m].
        t_nom:         Nominal wall thickness [m].
        mat:           Pipeline material.
        safety_class:  One of ``"low"``, ``"normal"``, ``"high"``.
        **kwargs:      Forwarded to :func:`dnv_burst_resistance`.

    Returns:
        MAOP [MPa].

    Raises:
        ValueError: If safety_class is invalid.
    """
    _validate_safety_class(safety_class)
    gsc = GAMMA_SC[safety_class]

    p_b = dnv_burst_resistance(D=D, t_nom=t_nom, mat=mat, **kwargs)
    return p_b / (GAMMA_INC * GAMMA_M * gsc)


# ---------------------------------------------------------------------------
# API RP 1111: hydrostatic water pressure
# ---------------------------------------------------------------------------

def api_water_pressure(
    depth: float,
    rho_sw: float = _RHO_SW,
    g: float = _G,
) -> float:
    """Hydrostatic pressure at a given water depth.

    p = rho_sw * g * depth / 1e6

    Args:
        depth:   Water depth [m]. Zero or negative values return 0.0.
        rho_sw:  Seawater density [kg/m3]. Default 1025 kg/m3.
        g:       Gravitational acceleration [m/s2]. Default 9.81 m/s2.

    Returns:
        Hydrostatic pressure [MPa].
    """
    if depth <= 0.0:
        return 0.0
    return rho_sw * g * depth / 1.0e6


# ---------------------------------------------------------------------------
# API RP 1111: elastic collapse pressure
# ---------------------------------------------------------------------------

def api_elastic_collapse_pressure(
    D: float,
    t: float,
    E: float = _E_STEEL,
    nu: float = _NU_STEEL,
) -> float:
    """Elastic collapse pressure per Timoshenko ring formula.

    p_e = 2 * E * (t/D)^3 / (1 - nu^2)

    Used in API RP 1111 Section 4.3.3 interaction equation.

    Args:
        D:   Outside diameter [m].
        t:   Wall thickness [m].
        E:   Young's modulus [MPa]. Default 207000 MPa.
        nu:  Poisson's ratio. Default 0.3.

    Returns:
        Elastic collapse pressure p_e [MPa].
    """
    return 2.0 * E * (t / D) ** 3 / (1.0 - nu ** 2)


# ---------------------------------------------------------------------------
# API RP 1111: collapse pressure (interaction equation Eq. 4.3.3)
# ---------------------------------------------------------------------------

def api_collapse_pressure(
    D: float,
    t: float,
    SMYS: float,
    f_o: float = 1.0,
    E: float = _E_STEEL,
    nu: float = _NU_STEEL,
) -> float:
    """Collapse pressure per API RP 1111 Section 4.3.3 interaction equation.

    Two-term harmonic (plastic + elastic) interaction::

        p_plastic = f_o * 0.45 * SMYS * (2 * t / D)
        p_e       = api_elastic_collapse_pressure(D, t, E, nu)
        p_c       = p_plastic * p_e / (p_plastic + p_e)

    Args:
        D:     Outside diameter [m].
        t:     Wall thickness [m].
        SMYS:  Specified Minimum Yield Strength [MPa].
        f_o:   Ovality factor (0, 1]. Default 1.0 (perfectly round).
        E:     Young's modulus [MPa]. Default 207000 MPa.
        nu:    Poisson's ratio. Default 0.3.

    Returns:
        Collapse pressure p_c [MPa].
    """
    p_plastic = f_o * 0.45 * SMYS * (2.0 * t / D)
    p_e = api_elastic_collapse_pressure(D=D, t=t, E=E, nu=nu)
    return p_plastic * p_e / (p_plastic + p_e)


# ---------------------------------------------------------------------------
# API RP 1111: external collapse check
# ---------------------------------------------------------------------------

def api_external_collapse_check(
    depth: float,
    D: float,
    t: float,
    SMYS: float,
    f_o: float = 1.0,
    E: float = _E_STEEL,
    nu: float = _NU_STEEL,
    rho_sw: float = _RHO_SW,
) -> dict[str, object]:
    """Check external hydrostatic pressure against allowable collapse pressure.

    Design criterion (API RP 1111 Section 4.3.3)::

        p_water <= DF_COLLAPSE * p_c

    Args:
        depth:   Water depth [m].
        D:       Outside diameter [m].
        t:       Wall thickness [m].
        SMYS:    SMYS at temperature [MPa].
        f_o:     Ovality factor. Default 1.0.
        E:       Young's modulus [MPa]. Default 207000 MPa.
        nu:      Poisson's ratio. Default 0.3.
        rho_sw:  Seawater density [kg/m3]. Default 1025 kg/m3.

    Returns:
        Dict with keys:
        - ``pass`` (bool): True when criterion is satisfied.
        - ``utilization`` (float): p_water / p_allowable.
        - ``p_water`` (float): Hydrostatic pressure at depth [MPa].
        - ``p_c`` (float): Collapse pressure [MPa].
        - ``p_allowable`` (float): DF_COLLAPSE * p_c [MPa].
    """
    p_water = api_water_pressure(depth=depth, rho_sw=rho_sw)
    p_c = api_collapse_pressure(D=D, t=t, SMYS=SMYS, f_o=f_o, E=E, nu=nu)
    p_allowable = DF_COLLAPSE * p_c
    utilization = p_water / p_allowable if p_allowable > 0.0 else float("inf")

    return {
        "pass": utilization <= 1.0,
        "utilization": utilization,
        "p_water": p_water,
        "p_c": p_c,
        "p_allowable": p_allowable,
    }


# ---------------------------------------------------------------------------
# API RP 1111: propagating buckle pressure
# ---------------------------------------------------------------------------

def api_propagating_buckle_pressure(
    D: float,
    t: float,
    SMYS: float,
) -> float:
    """Propagating buckle initiation pressure per API RP 1111 Eq. 4.3.2-1.

    p_p = 24 * SMYS * (t/D)^2.4

    Args:
        D:     Outside diameter [m].
        t:     Wall thickness [m].
        SMYS:  Specified Minimum Yield Strength [MPa].

    Returns:
        Propagating buckle pressure p_p [MPa].
    """
    return 24.0 * SMYS * (t / D) ** 2.4


# ---------------------------------------------------------------------------
# API RP 1111: propagating buckle check
# ---------------------------------------------------------------------------

def api_propagating_buckle_check(
    depth: float,
    D: float,
    t: float,
    SMYS: float,
    rho_sw: float = _RHO_SW,
) -> dict[str, object]:
    """Check external pressure against propagating buckle initiation pressure.

    Design criterion (API RP 1111 Section 4.3.2)::

        p_water <= p_p / DF_PROPAGATION

    If this check fails, buckle arrestors are required at intervals along
    the pipeline to limit propagation length.

    Args:
        depth:   Water depth [m].
        D:       Outside diameter [m].
        t:       Wall thickness [m].
        SMYS:    SMYS at temperature [MPa].
        rho_sw:  Seawater density [kg/m3]. Default 1025 kg/m3.

    Returns:
        Dict with keys:
        - ``pass`` (bool): True when criterion is satisfied.
        - ``utilization`` (float): p_water / p_p_allowable.
        - ``p_water`` (float): Hydrostatic pressure at depth [MPa].
        - ``p_p`` (float): Propagating buckle pressure [MPa].
        - ``p_p_allowable`` (float): p_p / DF_PROPAGATION [MPa].
        - ``arrestors_required`` (bool): True when check fails.
    """
    p_water = api_water_pressure(depth=depth, rho_sw=rho_sw)
    p_p = api_propagating_buckle_pressure(D=D, t=t, SMYS=SMYS)
    p_p_allowable = p_p / DF_PROPAGATION
    utilization = p_water / p_p_allowable if p_p_allowable > 0.0 else float("inf")
    check_passes = utilization <= 1.0

    return {
        "pass": check_passes,
        "utilization": utilization,
        "p_water": p_water,
        "p_p": p_p,
        "p_p_allowable": p_p_allowable,
        "arrestors_required": not check_passes,
    }


# ---------------------------------------------------------------------------
# Extended checks — re-exported for backward compatibility
# ---------------------------------------------------------------------------
# DNV-ST-F101 collapse / propagating buckle / combined loading
from digitalmodel.subsea.pipeline.pipeline_pressure_dnv import (  # noqa: E402
    dnv_collapse_pressure,
    dnv_external_collapse_check,
    dnv_propagating_buckle_pressure,
    dnv_propagating_buckle_check,
    dnv_combined_loading_check,
)

# API RP 1111 internal pressure / combined loading + config workflow
from digitalmodel.subsea.pipeline.pipeline_pressure_workflow import (  # noqa: E402
    DF_BURST,
    api_burst_pressure,
    api_internal_pressure_check,
    api_combined_loading_check,
    WallThicknessSizingWorkflow,
    run_sizing_from_yaml,
)

__all__ = [
    # Constants
    "GAMMA_SC", "GAMMA_M", "GAMMA_INC", "ALPHA_U",
    "DF_COLLAPSE", "DF_PROPAGATION", "DF_BURST",
    # Data classes
    "PipeGeometry", "PipeMaterial",
    # Material library
    "MATERIAL_LIBRARY",
    # DNV-ST-F101
    "dnv_wall_thickness", "dnv_burst_resistance",
    "dnv_pressure_containment_check", "dnv_system_pressure_test", "dnv_maop",
    "dnv_collapse_pressure", "dnv_external_collapse_check",
    "dnv_propagating_buckle_pressure", "dnv_propagating_buckle_check",
    "dnv_combined_loading_check",
    # API RP 1111
    "api_water_pressure", "api_elastic_collapse_pressure",
    "api_collapse_pressure", "api_external_collapse_check",
    "api_propagating_buckle_pressure", "api_propagating_buckle_check",
    "api_burst_pressure", "api_internal_pressure_check",
    "api_combined_loading_check",
    # Workflow
    "WallThicknessSizingWorkflow", "run_sizing_from_yaml",
]
