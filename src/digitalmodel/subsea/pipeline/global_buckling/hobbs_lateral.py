"""
Hobbs (1984) closed-form lateral (snaking) post-buckling of a heated pipeline.

Reference
---------
Hobbs, R.E. (1984). "In-Service Buckling of Heated Pipelines."
ASCE Journal of Transportation Engineering 110(2), 175-189.
Lateral modes 1-4: Table 1 with Eqs. 26-29.  Periodic mode: Eqs. 20-25.

Equations implemented
---------------------
With ``q_A = phi_A w`` and ``q_L = phi_L w`` (fully-mobilised axial and
lateral soil resistance per unit length), for the finite modes 1-4::

    P    = k1 EI / L^2
    z    = k2 EA q_L^2 L^5 / (q_A (EI)^2)
    P0   = P + k3 q_A L [sqrt(1 + z) - 1]
    yhat = k4 q_L L^4 / EI
    Mhat = k5 q_L L^2

and for the periodic ("infinite") mode, where there is no axial feed-in and
therefore no k3 slip term::

    P    = 4 pi^2 EI / L^2
    P0   = P + k2 EA q_L^2 L^6 / (EI)^2
    slope_max = k6 q_L L^3 / EI

The single-friction form quoted in most textbooks (``phi_A = phi_L = phi``)
is recovered exactly: ``z`` collapses to ``k2 EA q L^5 / (EI)^2``.

Shape of the equilibrium path
-----------------------------
``P0(L)`` is not monotonic.  It falls as the buckle lengthens (the ``k1 EI/L^2``
term) and then rises as the feed-in term grows, so it has a **minimum**.  That
minimum is the classical Hobbs "safe" / snap-through force: below it no
equilibrium buckle of that mode exists, and above it there are two roots -- a
short, high-curvature branch and the long post-snap branch.

Limitations
-----------
Small-slope elastic equilibrium with idealised fully-mobilised Coulomb
friction and an initially straight line.  This predicts equilibrium paths, not
the *initiation* temperature of a real line with out-of-straightness -- for
that, imperfection methods (Taylor & Gan 1986) or FE per DNV-RP-F110 are
required.  Coating bending stiffness, cyclic soil memory, residual lay
tension, and code acceptance checks are outside this module.
"""
from __future__ import annotations

import math

from scipy.optimize import brentq, minimize_scalar

from .models import (
    MODE_CONSTANTS,
    HobbsMode,
    LateralBuckleState,
    ModeSusceptibility,
    PipeSection,
    SoilResistance,
)

__all__ = [
    "critical_state",
    "effective_driving_force",
    "equilibria_at_temperature",
    "governing_mode",
    "lateral_equilibrium",
    "screen_modes",
]

# Decades of buckle length scanned either side of the dimensional anchor when
# bracketing the minimum of P0(L).  Ten decades comfortably covers every
# realistic combination of section stiffness and soil resistance.
_SCAN_DECADES = 5.0
_SCAN_POINTS = 121


def _as_mode(mode: HobbsMode | str | int) -> HobbsMode:
    if isinstance(mode, HobbsMode):
        return mode
    try:
        return HobbsMode(str(mode))
    except ValueError:
        raise ValueError(
            f"mode must be 1, 2, 3, 4 or 'infinite', got {mode!r}"
        ) from None


def _check_positive(name: str, value: float) -> None:
    if not math.isfinite(value) or value <= 0.0:
        raise ValueError(f"{name} must be finite and positive, got {value!r}")


def lateral_equilibrium(
    pipe: PipeSection,
    soil: SoilResistance,
    buckle_length_m: float,
    mode: HobbsMode | str | int = HobbsMode.MODE_3,
) -> LateralBuckleState:
    """Evaluate one point on the lateral post-buckling equilibrium path.

    Parameters
    ----------
    pipe             section, weight and thermal properties
    soil             axial and lateral friction coefficients
    buckle_length_m  L, the Hobbs characteristic lobe length [m]
    mode             Hobbs mode 1-4 or ``'infinite'``

    Returns
    -------
    LateralBuckleState with force, amplitude, moment and stress.
    """
    _check_positive("buckle_length_m", buckle_length_m)
    mode = _as_mode(mode)
    k = MODE_CONSTANTS[mode]

    length = buckle_length_m
    weight = pipe.submerged_weight_N_m
    q_axial = soil.axial_resistance_N_m(weight)
    q_lateral = soil.lateral_resistance_N_m(weight)
    EI = pipe.EI

    buckle_force = k.k1 * EI / length**2

    if mode is HobbsMode.INFINITE:
        release = k.k2 * pipe.EA * q_lateral**2 * length**6 / EI**2
        max_slope: float | None = k.k6 * q_lateral * length**3 / EI  # type: ignore[operator]
    else:
        z = k.k2 * pipe.EA * q_lateral**2 * length**5 / (q_axial * EI**2)
        # sqrt(1+z) - 1 loses precision for small z; the rationalised form
        # z / (sqrt(1+z) + 1) is algebraically identical and stable.
        release = k.k3 * q_axial * length * z / (math.sqrt(1.0 + z) + 1.0)
        max_slope = None

    far_field_force = buckle_force + release
    amplitude = k.k4 * q_lateral * length**4 / EI
    moment = k.k5 * q_lateral * length**2

    axial_stress = buckle_force / pipe.area_m2
    bending_stress = moment * pipe.outer_radius_m / pipe.inertia_m4

    return LateralBuckleState(
        mode=mode,
        buckle_length_m=length,
        buckle_force_N=buckle_force,
        far_field_force_N=far_field_force,
        temperature_rise_K=far_field_force
        / (pipe.EA * pipe.thermal_expansion_per_K),
        amplitude_m=amplitude,
        max_moment_Nm=moment,
        axial_stress_pa=axial_stress,
        bending_stress_pa=bending_stress,
        combined_stress_pa=axial_stress + bending_stress,
        max_slope=max_slope,
    )


def _length_anchor(pipe: PipeSection, soil: SoilResistance) -> float:
    """Dimensional length scale (EI^3 / (EA q_A q_L))^(1/8) used to seed scans.

    For ``phi_A = phi_L`` this is the exact scale that makes the periodic-mode
    minimum a pure function of the mode constants, so it lands within a decade
    or so of the turning point for every realistic input.
    """
    weight = pipe.submerged_weight_N_m
    q_axial = soil.axial_resistance_N_m(weight)
    q_lateral = soil.lateral_resistance_N_m(weight)
    return (pipe.EI**3 / (pipe.EA * q_axial * q_lateral)) ** 0.125


def critical_state(
    pipe: PipeSection,
    soil: SoilResistance,
    mode: HobbsMode | str | int = HobbsMode.MODE_3,
) -> LateralBuckleState:
    """Minimum of ``P0(L)`` -- the snap-through / "safe" force for this mode.

    The periodic mode is minimised analytically::

        L_min = (k1 EI^3 / (3 k2 EA q_L^2))^(1/8)

    The finite modes have no closed-form minimum, so ``P0`` is bracketed on a
    geometric scan around the dimensional anchor and then refined.

    Notes
    -----
    This is the lowest far-field force at which an equilibrium buckle of the
    given mode can exist.  It is **not** an initiation prediction and it is not
    a design allowable: a real line with imperfections buckles at a lower force
    (see DNV-RP-F110 and Taylor & Gan 1986).
    """
    mode = _as_mode(mode)
    k = MODE_CONSTANTS[mode]
    weight = pipe.submerged_weight_N_m
    q_lateral = soil.lateral_resistance_N_m(weight)

    if mode is HobbsMode.INFINITE:
        length = (
            k.k1 * pipe.EI**3 / (3.0 * k.k2 * pipe.EA * q_lateral**2)
        ) ** 0.125
        return lateral_equilibrium(pipe, soil, length, mode)

    anchor = _length_anchor(pipe, soil)
    exponents = [
        -_SCAN_DECADES + 2.0 * _SCAN_DECADES * i / (_SCAN_POINTS - 1)
        for i in range(_SCAN_POINTS)
    ]
    lengths = [anchor * 10.0**e for e in exponents]
    forces = [
        lateral_equilibrium(pipe, soil, length, mode).far_field_force_N
        for length in lengths
    ]

    index = min(range(len(forces)), key=forces.__getitem__)
    if index in (0, len(forces) - 1):
        raise RuntimeError(
            f"could not bracket the P0 minimum for mode {mode.value} within "
            f"{_SCAN_DECADES} decades of L = {anchor:.4g} m"
        )

    result = minimize_scalar(
        lambda log_length: math.log(
            lateral_equilibrium(
                pipe, soil, math.exp(log_length), mode
            ).far_field_force_N
        ),
        bounds=(math.log(lengths[index - 1]), math.log(lengths[index + 1])),
        method="bounded",
        options={"xatol": 1e-12},
    )
    if not result.success:
        raise RuntimeError(
            f"P0 minimum refinement failed for mode {mode.value}: {result.message}"
        )
    return lateral_equilibrium(pipe, soil, math.exp(result.x), mode)


def equilibria_at_temperature(
    pipe: PipeSection,
    soil: SoilResistance,
    temperature_rise_K: float,
    mode: HobbsMode | str | int = HobbsMode.MODE_3,
) -> tuple[LateralBuckleState, ...]:
    """Equilibrium buckles that satisfy ``P0 = EA alpha dT``, shortest first.

    Returns an empty tuple below the critical force, a single state at the
    turning point, and two states above it: the short unstable branch and the
    long post-snap branch of Hobbs Fig. 7.

    The two-root structure is geometric, not a dynamic stability analysis --
    the short branch is not physically held by a real line.
    """
    mode = _as_mode(mode)
    target = pipe.fully_restrained_thermal_force(temperature_rise_K)
    turning_point = critical_state(pipe, soil, mode)

    tolerance = 1e-10 * max(target, turning_point.far_field_force_N)
    difference = target - turning_point.far_field_force_N
    if difference < -tolerance:
        return ()
    if abs(difference) <= tolerance:
        return (turning_point,)

    def residual(length: float) -> float:
        return (
            lateral_equilibrium(pipe, soil, length, mode).far_field_force_N / target
            - 1.0
        )

    lower = turning_point.buckle_length_m
    for _ in range(200):
        lower /= 2.0
        if residual(lower) > 0.0:
            break
    else:  # pragma: no cover - unreachable for finite positive inputs
        raise RuntimeError("could not bracket the short equilibrium branch")

    upper = turning_point.buckle_length_m
    for _ in range(200):
        upper *= 2.0
        if residual(upper) > 0.0:
            break
    else:  # pragma: no cover
        raise RuntimeError("could not bracket the long equilibrium branch")

    roots = (
        brentq(residual, lower, turning_point.buckle_length_m, xtol=1e-12),
        brentq(residual, turning_point.buckle_length_m, upper, xtol=1e-12),
    )
    return tuple(lateral_equilibrium(pipe, soil, root, mode) for root in roots)


def effective_driving_force(
    pipe: PipeSection,
    *,
    temperature_rise_K: float,
    internal_pressure_pa: float = 0.0,
    internal_area_m2: float = 0.0,
    poisson_ratio: float = 0.3,
    residual_lay_tension_N: float = 0.0,
) -> float:
    """Fully-restrained compressive effective axial force S_eff [N], positive.

        S_eff = EA alpha dT + p_i A_i (1 - 2 nu) - H

    ``H`` is the residual (as-laid) tension, which relieves compression.  The
    result is clipped at zero: a line in net tension cannot buckle globally.
    """
    if internal_pressure_pa < 0.0:
        raise ValueError("internal_pressure_pa must be non-negative")
    if internal_area_m2 < 0.0:
        raise ValueError("internal_area_m2 must be non-negative")
    thermal = pipe.fully_restrained_thermal_force(temperature_rise_K)
    pressure = internal_pressure_pa * internal_area_m2 * (1.0 - 2.0 * poisson_ratio)
    return max(0.0, thermal + pressure - residual_lay_tension_N)


def screen_modes(
    pipe: PipeSection,
    soil: SoilResistance,
    driving_force_N: float,
    modes: tuple[HobbsMode, ...] | None = None,
) -> tuple[ModeSusceptibility, ...]:
    """Compare a driving force against every mode's critical force.

    Returned in ascending order of critical force, so the first entry is the
    governing (most easily triggered) mode.
    """
    _check_positive("driving_force_N", driving_force_N)
    selected = modes if modes is not None else tuple(HobbsMode)
    results = []
    for mode in selected:
        state = critical_state(pipe, soil, mode)
        utilisation = driving_force_N / state.far_field_force_N
        results.append(
            ModeSusceptibility(
                mode=mode,
                critical_state=state,
                driving_force_N=driving_force_N,
                utilisation=utilisation,
                susceptible=utilisation >= 1.0,
            )
        )
    return tuple(sorted(results, key=lambda r: r.critical_state.far_field_force_N))


def governing_mode(
    pipe: PipeSection,
    soil: SoilResistance,
    modes: tuple[HobbsMode, ...] | None = None,
) -> LateralBuckleState:
    """Critical state of the mode with the lowest snap-through force."""
    selected = modes if modes is not None else tuple(HobbsMode)
    states = [critical_state(pipe, soil, mode) for mode in selected]
    return min(states, key=lambda state: state.far_field_force_N)
