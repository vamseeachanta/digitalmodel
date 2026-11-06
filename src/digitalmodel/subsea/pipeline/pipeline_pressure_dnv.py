"""DNV-ST-F101 extended pressure checks — collapse, propagating buckle, combined loading.

WRK-355: Pipeline and flexibles module — pressure containment checks.

Implements:
- Characteristic collapse pressure (Section 5.4.4)
- External collapse check
- Propagating buckle pressure and check (Section 5.4.6)
- Combined loading check (Section 5.4.3)

All pressures in MPa; all dimensions in metres (SI throughout).
"""
from __future__ import annotations

import math

from digitalmodel.subsea.pipeline.pipeline_pressure import (
    GAMMA_SC,
    GAMMA_M,
    GAMMA_INC,
    MATERIAL_LIBRARY,
    PipeMaterial,
    _E_STEEL,
    _NU_STEEL,
    _RHO_SW,
    _validate_safety_class,
    api_water_pressure,
)


# DNV-ST-F101 (2021), System collapse: qualification remains unavailable.
# These omissions apply even when the legacy numerical utilization passes.
_COLLAPSE_NOT_EVALUATED = (
    "fabrication_factor",
    "strength_derating",
    "ovality_and_thickness_basis",
    "design_criterion_and_factors",
)


# ---------------------------------------------------------------------------
# DNV-ST-F101: collapse pressure (characteristic)
# ---------------------------------------------------------------------------

def dnv_collapse_pressure(
    D: float,
    t: float,
    mat: PipeMaterial,
    f_0: float = 0.005,
    E: float = _E_STEEL,
    nu: float = _NU_STEEL,
) -> float:
    """Solve the existing DNV-type collapse interaction equation.

    Public equation source: EPRG, Deeper, Longer and Thicker: Challenges
    for the Design of Subsea Pipelines (June 2022), section 3.1, Eq. (3):
    https://www.eprg.net/fileadmin/EPRG_Dokumente/00222_Position_Paper_-_Final__EPRG__49__2_29_.pdf
    Component coefficients and powers: TU Delft, Local Buckling and
    Collapse of Thick Walled pipe (2016), Eqs. (3.9), (3.10):
    https://repository.tudelft.nl/file/File_09e16762-ff32-49eb-bff8-85cffa889063

    This numerical solution retains the existing plastic component model,
    without fabrication or material-strength reduction factors. Legacy
    ovality and elastic-property defaults are assumed, based on previous
    experience; they are not qualified project inputs. This function alone
    leaves compliance with DNV-ST-F101 (2021), System collapse, NOT_EVALUATED.

    Solved numerically from the cubic interaction equation::

        (p_c - p_el) * (p_c^2 - p_p^2) = p_c * p_el * p_p * f_0 * (D/t)

    where::
        p_el = 2 * E * (t/D)^3 / (1 - nu^2)
        p_p  = SMYS * 2 * t / D

    Args:
        D:    Outside diameter [m].
        t:    Wall thickness [m].
        mat:  Pipeline material (SMYS used).
        f_0:  Initial ovality (D_max - D_min)/D. Default 0.005.
        E:    Young's modulus [MPa]. Default 207000 MPa.
        nu:   Poisson's ratio. Default 0.3.

    Returns:
        Characteristic collapse pressure p_c [MPa].

    Raises:
        ValueError: Inputs are nonfinite or outside the mathematical domain.
    """
    values = (D, t, mat.SMYS, E, nu, f_0)
    if not all(math.isfinite(value) for value in values):
        raise ValueError("Collapse inputs must be finite")
    if D <= 0.0 or not 0.0 < t < D / 2.0:
        raise ValueError("Collapse geometry requires D > 0 and 0 < t < D/2")
    if E <= 0.0 or mat.SMYS <= 0.0 or not -1.0 < nu < 1.0 or f_0 < 0.0:
        raise ValueError("Collapse requires E, SMYS > 0, |nu| < 1 and f_0 >= 0")

    # Component definitions: TU Delft (2016), Eqs. (3.9), (3.10), cited above.
    p_el = 2.0 * E * (t / D) ** 3 / (1.0 - nu ** 2)
    p_p = mat.SMYS * 2.0 * t / D
    coeff = p_el * p_p * f_0 * (D / t)

    if f_0 == 0.0 or coeff == 0.0:
        return min(p_el, p_p)

    def _g(pc: float) -> float:
        return (pc - p_el) * (pc ** 2 - p_p ** 2) - pc * coeff

    # EPRG (2022), Eq. (3): g(0)>0 and g(min(p_el,p_p))<0 for f_0>0.
    # On this interval (p_el-p)*(p_p**2-p**2)/p decreases strictly,
    # so it contains exactly one root. Larger roots are not collapse onset.
    lo, hi = 0.0, min(p_el, p_p)
    # Numerical budget only: 64 halvings reduce the original bracket by 2**-64,
    # beyond binary64's 53-bit significand. Stop sooner on rounded stagnation.
    for _ in range(64):
        mid = (lo + hi) / 2.0
        if mid == lo or mid == hi:
            return mid
        if _g(mid) > 0:
            lo = mid
        else:
            hi = mid
    return (lo + hi) / 2.0


# ---------------------------------------------------------------------------
# DNV-ST-F101: external collapse check
# ---------------------------------------------------------------------------

def dnv_external_collapse_check(
    depth: float,
    D: float,
    t: float,
    mat: PipeMaterial,
    safety_class: str = "normal",
    f_0: float = 0.005,
    E: float = _E_STEEL,
    nu: float = _NU_STEEL,
    rho_sw: float = _RHO_SW,
) -> dict[str, object]:
    """Screen external hydrostatic pressure using the legacy collapse model.

    ``pass`` reports the arithmetic criterion below only. Complete compliance
    with DNV-ST-F101 (2021), System collapse, is NOT_EVALUATED: fabrication,
    strength derating, geometry basis and design factors are not qualified.
    Defaults are assumed, based on previous experience; no standard-qualified
    ovality, elastic properties or seawater density is inferred from them.

    Criterion (DNV-ST-F101 Section 5.4.4)::

        p_water <= p_c / (gamma_inc * gamma_m * gamma_sc)

    Args:
        depth:         Water depth [m].
        D:             Outside diameter [m].
        t:             Wall thickness [m].
        mat:           Pipeline material.
        safety_class:  One of ``"low"``, ``"normal"``, ``"high"``.
        f_0:           Initial ovality. Default 0.005.
        E:             Young's modulus [MPa]. Default 207000 MPa.
        nu:            Poisson's ratio. Default 0.3.
        rho_sw:        Seawater density [kg/m3]. Default 1025 kg/m3.

    Returns:
        Dict with keys: ``pass``, ``utilization``, ``p_water``, ``p_c``,
        ``p_allowable``, ``standard_compliance`` and ``not_evaluated``.
    """
    _validate_safety_class(safety_class)
    gsc = GAMMA_SC[safety_class]

    p_water = api_water_pressure(depth=depth, rho_sw=rho_sw)
    p_c = dnv_collapse_pressure(D=D, t=t, mat=mat, f_0=f_0, E=E, nu=nu)
    p_allowable = p_c / (GAMMA_INC * GAMMA_M * gsc)
    utilization = p_water / p_allowable if p_allowable > 0.0 else float("inf")

    return {
        "pass": utilization <= 1.0,
        "utilization": utilization,
        "p_water": p_water,
        "p_c": p_c,
        "p_allowable": p_allowable,
        "standard_compliance": "NOT_EVALUATED",
        "not_evaluated": _COLLAPSE_NOT_EVALUATED,
    }


# ---------------------------------------------------------------------------
# DNV-ST-F101: propagating buckle pressure and check
# ---------------------------------------------------------------------------

def dnv_propagating_buckle_pressure(
    D: float,
    t: float,
    mat: PipeMaterial,
) -> float:
    """Propagating buckle pressure per DNV-ST-F101 Section 5.4.6.

    p_pr = 35 * SMYS * (t/D)^2.5

    Args:
        D:    Outside diameter [m].
        t:    Wall thickness [m].
        mat:  Pipeline material (SMYS used).

    Returns:
        Propagating buckle pressure p_pr [MPa].
    """
    return 35.0 * mat.SMYS * (t / D) ** 2.5


def dnv_propagating_buckle_check(
    depth: float,
    D: float,
    t: float,
    mat: PipeMaterial,
    safety_class: str = "normal",
    rho_sw: float = _RHO_SW,
) -> dict[str, object]:
    """Check external pressure against DNV-ST-F101 propagating buckle pressure.

    Criterion::

        p_water <= p_pr / (gamma_inc * gamma_m * gamma_sc)

    Args:
        depth:         Water depth [m].
        D:             Outside diameter [m].
        t:             Wall thickness [m].
        mat:           Pipeline material.
        safety_class:  One of ``"low"``, ``"normal"``, ``"high"``.
        rho_sw:        Seawater density [kg/m3]. Default 1025 kg/m3.

    Returns:
        Dict with keys: ``pass``, ``utilization``, ``p_water``, ``p_pr``,
        ``p_pr_allowable``, ``arrestors_required``.
    """
    _validate_safety_class(safety_class)
    gsc = GAMMA_SC[safety_class]

    p_water = api_water_pressure(depth=depth, rho_sw=rho_sw)
    p_pr = dnv_propagating_buckle_pressure(D=D, t=t, mat=mat)
    p_pr_allowable = p_pr / (GAMMA_INC * GAMMA_M * gsc)
    utilization = p_water / p_pr_allowable if p_pr_allowable > 0.0 else float("inf")
    check_passes = utilization <= 1.0

    return {
        "pass": check_passes,
        "utilization": utilization,
        "p_water": p_water,
        "p_pr": p_pr,
        "p_pr_allowable": p_pr_allowable,
        "arrestors_required": not check_passes,
    }


# ---------------------------------------------------------------------------
# DNV-ST-F101: combined loading check
# ---------------------------------------------------------------------------

def dnv_combined_loading_check(
    D: float,
    t: float,
    mat: PipeMaterial,
    M_Sd: float,
    S_Sd: float,
    p_li: float,
    p_e: float,
    safety_class: str = "normal",
    f_0: float = 0.005,
    *,
    E: float = _E_STEEL,
    nu: float = _NU_STEEL,
) -> dict[str, object]:
    """Screen combined loading using the legacy interaction criterion.

    ``pass`` reports numerical utilization only. The interaction criterion and
    factors have not been qualified against DNV-ST-F101 (2021), Local buckling.
    Standard compliance is NOT_EVALUATED even for a passing screen. Collapse
    defaults are assumed, based on previous experience; supply elastic properties
    explicitly to use the same input basis as the external-collapse screen.

    Interaction equation::

        U = sqrt( (gamma_SC * gamma_m * M_Sd / M_p)^2
                 + (gamma_SC * gamma_m * S_Sd / S_p)^2
                 + |(p_li - p_e) / p_c|^2 )  <= 1

    Plastic reference quantities::

        M_p = SMYS [Pa] * (D - t)^2 * t
        S_p = SMYS [Pa] * pi * (D - t) * t

    Args:
        D:             Outside diameter [m].
        t:             Wall thickness [m].
        mat:           Pipeline material (SMYS in MPa).
        M_Sd:          Design bending moment [N·m].
        S_Sd:          Design effective tension [N].
        p_li:          Local incidental pressure (internal) [MPa].
        p_e:           External (hydrostatic) pressure [MPa].
        safety_class:  One of ``"low"``, ``"normal"``, ``"high"``.
        f_0:           Pipe ovality for collapse calculation. Default 0.005.
        E:             Young's modulus [MPa]; legacy assumed default.
        nu:            Poisson's ratio; legacy assumed default.

    Returns:
        Dict with keys: ``pass``, ``utilization``, ``M_p``, ``S_p``,
        ``p_c``, ``pressure_ratio``, ``standard_compliance`` and
        ``not_evaluated``.
    """
    _validate_safety_class(safety_class)
    gsc = GAMMA_SC[safety_class]

    smys_pa = mat.SMYS * 1.0e6
    M_p = smys_pa * (D - t) ** 2 * t
    S_p = smys_pa * math.pi * (D - t) * t

    p_c = dnv_collapse_pressure(D=D, t=t, mat=mat, f_0=f_0, E=E, nu=nu)
    pressure_ratio = (p_li - p_e) / p_c if p_c > 0.0 else 0.0

    if M_p > 0.0 and S_p > 0.0:
        moment_term = (gsc * GAMMA_M * abs(M_Sd) / M_p) ** 2
        tension_term = (gsc * GAMMA_M * abs(S_Sd) / S_p) ** 2
        pressure_term = abs(pressure_ratio) ** 2
        utilization = math.sqrt(moment_term + tension_term + pressure_term)
    else:
        utilization = float("inf")

    return {
        "pass": utilization <= 1.0,
        "utilization": utilization,
        "M_p": M_p,
        "S_p": S_p,
        "p_c": p_c,
        "pressure_ratio": pressure_ratio,
        "standard_compliance": "NOT_EVALUATED",
        "not_evaluated": _COLLAPSE_NOT_EVALUATED,
    }
