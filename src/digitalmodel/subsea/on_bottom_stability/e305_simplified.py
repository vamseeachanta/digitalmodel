# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""SI simplified stability primitives with explicit parity conventions.

The weight-demand equation follows the supplied calculation basis. Geometry
and Morison algebra follow the existing on-bottom stability implementation.
No licensed tables or coefficient defaults are distributed. Edition-specific
E305 equivalence and the OS-F101 comparator are not established; see the
workflow README. A demand ratio alone is not overall qualification.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, fields, replace
from collections.abc import Callable, Sequence

from digitalmodel.citations.schema import Citation


def _finite(name: str, value: float, *, positive: bool = False) -> None:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} must be a finite number")
    if not math.isfinite(value) or value < 0 or (positive and value == 0):
        raise ValueError(f"{name} must be finite and {'positive' if positive else 'nonnegative'}")


@dataclass(frozen=True)
class Pipe:
    """Length-weighted layers; inside corrosion and mass-fraction absorption."""

    steel_od_m: float
    wall_m: float
    corrosion_allowance_m: float
    steel_density_kg_m3: float
    corrosion_coating_m: float
    corrosion_coating_density_kg_m3: float
    concrete_m: float
    concrete_density_kg_m3: float
    absorption_mass_fraction: float
    marine_growth_m: float
    marine_growth_density_kg_m3: float
    field_joint_fraction: float
    field_joint_m: float
    field_joint_density_kg_m3: float
    contents_density_kg_m3: float
    added_mass_kg_m: float
    water_density_kg_m3: float

    def __post_init__(self) -> None:
        """Validate finite inputs and the declared physical domain."""
        for item in fields(self):
            _finite(item.name, getattr(self, item.name))
        for name in ('steel_od_m', 'wall_m', 'steel_density_kg_m3', 'water_density_kg_m3'):
            _finite(name, getattr(self, name), positive=True)
        if self.wall_m * 2 >= self.steel_od_m:
            raise ValueError('wall must leave a positive bore')
        if self.corrosion_allowance_m >= self.wall_m:
            raise ValueError('corrosion allowance must be less than wall')
        if self.field_joint_fraction > 1:
            raise ValueError('field_joint_fraction must be at most one')


@dataclass(frozen=True)
class WeightResult:
    """Mass and submerged weight per metre with local-diameter buoyancy."""

    component_mass_kg_m: tuple[tuple[str, float], ...]
    mass_kg_m: float
    weight_n_m: float
    buoyancy_n_m: float
    submerged_weight_n_m: float
    hydrodynamic_diameter_m: float
    g_m_s2: float


def _annulus(outer: float, inner: float) -> float:
    return math.pi * (outer**2 - inner**2) / 4


def weight_build_up(pipe: Pipe, *, g: float) -> WeightResult:
    """Compute layer masses, contents, added mass and displaced-water weight."""
    _finite('g', g, positive=True)
    bore = pipe.steel_od_m - 2 * (pipe.wall_m - pipe.corrosion_allowance_m)
    corrosion_od = pipe.steel_od_m + 2 * pipe.corrosion_coating_m
    concrete_od = corrosion_od + 2 * pipe.concrete_m
    joint_od = corrosion_od + 2 * pipe.field_joint_m
    grown_od = concrete_od + 2 * pipe.marine_growth_m
    grown_joint_od = joint_od + 2 * pipe.marine_growth_m
    joint = pipe.field_joint_fraction
    body = 1 - joint
    masses = (
        ('steel', _annulus(pipe.steel_od_m, bore) * pipe.steel_density_kg_m3),
        ('corrosion_coating', _annulus(corrosion_od, pipe.steel_od_m) * pipe.corrosion_coating_density_kg_m3),
        ('concrete', body * _annulus(concrete_od, corrosion_od) * pipe.concrete_density_kg_m3 * (1 + pipe.absorption_mass_fraction)),
        ('field_joint', joint * _annulus(joint_od, corrosion_od) * pipe.field_joint_density_kg_m3),
        ('marine_growth', (body * _annulus(grown_od, concrete_od) + joint * _annulus(grown_joint_od, joint_od)) * pipe.marine_growth_density_kg_m3),
        ('contents', _annulus(bore, 0) * pipe.contents_density_kg_m3),
        ('added', pipe.added_mass_kg_m),
    )
    mass = sum(value for _, value in masses)
    buoyancy = (body * _annulus(grown_od, 0) + joint * _annulus(grown_joint_od, 0)) * pipe.water_density_kg_m3 * g
    diameter = max(grown_od if body > 0 else 0, grown_joint_od if joint > 0 else 0)
    return WeightResult(masses, mass, mass * g, buoyancy, mass * g - buoyancy, diameter, g)


@dataclass(frozen=True)
class Case:
    """Explicit regular-wave parity inputs, with no empirical defaults."""

    role: str
    wave_velocity_m_s: float
    current_m_s: float
    period_s: float
    R: float
    Fw: float
    cd: float
    cm: float
    cl: float
    mu: float
    target_safety_factor: float
    gamma_st: float
    reduction_application: str
    duplicate_current: bool = False
    phase_rad: float | None = None
    phase_count: int = 720  # Numerical angular resolution, not a standard value.

    def __post_init__(self) -> None:
        """Validate finite inputs and the declared physical domain."""
        for name in ('wave_velocity_m_s', 'current_m_s', 'cd', 'cm', 'cl'):
            _finite(name, getattr(self, name))
        for name in ('period_s', 'R', 'Fw', 'mu', 'target_safety_factor', 'gamma_st'):
            _finite(name, getattr(self, name), positive=True)
        if self.role not in ('extreme_wave', 'extreme_current'):
            raise ValueError('case role must be extreme_wave or extreme_current')
        if self.reduction_application not in ('wave_velocity', 'wave_and_current'):
            raise ValueError('an explicit reduction_application is required')
        if not isinstance(self.duplicate_current, bool):
            raise ValueError('duplicate_current must be boolean')
        if type(self.phase_count) is not int or self.phase_count < 4:
            raise ValueError('phase_count must be an integer of at least four')
        if self.phase_rad is not None and (
            isinstance(self.phase_rad, bool)
            or not isinstance(self.phase_rad, (int, float))
            or not math.isfinite(self.phase_rad)
        ):
            raise ValueError('phase_rad must be finite or None')


@dataclass(frozen=True)
class CaseResult:
    """Governing simultaneous demand; qualification remains incomplete."""

    role: str
    required_weight_n_m: float
    unfactored_weight_n_m: float
    available_weight_n_m: float
    utilization: float | None
    safety_factor: float | None
    demand_pass: bool
    phase_rad: float
    drag_n_m: float
    inertia_n_m: float
    lift_n_m: float
    K: float
    M: float | None
    R: float
    Fw: float
    period_s: float
    reduction_application: str
    duplicate_current: bool
    factor_source: str
    gamma_st_requested: float
    citations: tuple[Citation, ...] = ()
    os_f101_status: str = 'not_established'
    qualified: bool = False


def dimensionless_ratios(wave_velocity_m_s: float, period_s: float,
                         current_m_s: float, diameter_m: float) -> tuple[float, float | None]:
    """K=Uw*T/D and M=Uc/Uw, as documented by the legacy repository tests."""
    _finite('wave_velocity_m_s', wave_velocity_m_s)
    _finite('current_m_s', current_m_s)
    _finite('period_s', period_s, positive=True)
    _finite('diameter_m', diameter_m, positive=True)
    return (wave_velocity_m_s * period_s / diameter_m,
            current_m_s / wave_velocity_m_s if wave_velocity_m_s > 0 else None)


def _phase_demand(case: Case, phase: float, diameter: float, rho: float) -> tuple:
    wave = case.wave_velocity_m_s * case.R
    current = case.current_m_s * (2 if case.duplicate_current else 1)
    if case.reduction_application == 'wave_and_current':
        current *= case.R
    velocity = current + wave * math.cos(phase)
    acceleration = -wave * (2 * math.pi / case.period_s) * math.sin(phase)
    drag = 0.5 * rho * case.cd * diameter * velocity * abs(velocity)
    inertia = rho * case.cm * math.pi / 4 * diameter**2 * acceleration
    lift = 0.5 * rho * case.cl * diameter * velocity**2
    demand = (abs(drag + inertia) + case.mu * lift) / case.mu * case.Fw
    return demand, phase, drag, inertia, lift


def evaluate_case(case: Case, *, diameter_m: float, rho_water_kg_m3: float,
                  available_weight_n_m: float, citations: tuple[Citation, ...] = (),
                  factor_source: str = 'typed_parity') -> CaseResult:
    """Maximize simultaneous demand, then apply the supplied safety factor.

    FD+FI is signed before taking its absolute lateral magnitude. Lift belongs
    to the same phase. No independent component maxima are combined. The
    requested gamma_st is retained without inventing an OS-F101 comparison.
    """
    _finite('diameter_m', diameter_m, positive=True)
    _finite('rho_water_kg_m3', rho_water_kg_m3, positive=True)
    if not math.isfinite(available_weight_n_m):
        raise ValueError('available_weight_n_m must be finite')
    phases = (case.phase_rad,) if case.phase_rad is not None else (
        2 * math.pi * i / case.phase_count for i in range(case.phase_count))
    demand, phase, drag, inertia, lift = max(
        (_phase_demand(case, p, diameter_m, rho_water_kg_m3) for p in phases),
        key=lambda row: row[0],
    )
    required = demand * case.target_safety_factor
    K, M = dimensionless_ratios(case.wave_velocity_m_s, case.period_s,
                                case.current_m_s, diameter_m)
    return CaseResult(
        role=case.role, required_weight_n_m=required, unfactored_weight_n_m=demand,
        available_weight_n_m=available_weight_n_m,
        utilization=required / available_weight_n_m if available_weight_n_m > 0 else None,
        safety_factor=available_weight_n_m / demand if demand > 0 else None,
        demand_pass=available_weight_n_m > 0 and required <= available_weight_n_m,
        phase_rad=phase, drag_n_m=drag, inertia_n_m=inertia, lift_n_m=lift,
        K=K, M=M, R=case.R, Fw=case.Fw, period_s=case.period_s,
        reduction_application=case.reduction_application,
        duplicate_current=case.duplicate_current, factor_source=factor_source,
        gamma_st_requested=case.gamma_st, citations=citations,
    )


def diameter_averaged_current(reference_current_m_s: float, *, reference_height_m: float,
                              diameter_m: float, roughness_m: float) -> float:
    """Integrate the repository's shifted log profile from bed to pipe top.

    U(z)/Uref=ln(1+z/z0)/ln(1+href/z0). This is an explicit profile
    convention; equivalence with E305 Appendix A remains unverified.
    """
    _finite('reference_current_m_s', reference_current_m_s)
    for name, value in (('reference_height_m', reference_height_m),
                        ('diameter_m', diameter_m), ('roughness_m', roughness_m)):
        _finite(name, value, positive=True)
    ratio = diameter_m / roughness_m
    integral = (diameter_m + roughness_m) * math.log1p(ratio) - diameter_m
    return reference_current_m_s * integral / diameter_m / math.log1p(reference_height_m / roughness_m)


def wave_from_sea_state(*, Hs_m: float, Tp_s: float, depth_m: float,
                        elevation_m: float, g: float) -> tuple[float, float]:
    """Use the existing spectral helper only with its supported gravity."""
    from digitalmodel.subsea.pipeline.free_span import wave_velocity

    _finite('Hs_m', Hs_m)
    _finite('Tp_s', Tp_s, positive=True)
    _finite('depth_m', depth_m, positive=True)
    _finite('elevation_m', elevation_m)
    _finite('g', g, positive=True)
    if elevation_m >= depth_m:
        raise ValueError('pipe elevation must be below water depth')
    if g != wave_velocity._G:
        raise ValueError('spectral helper has fixed gravity; use typed kinematics for another gravity')
    if Hs_m == 0:
        return 0.0, Tp_s
    result = wave_velocity.compute_wave_velocity(Hs_m, Tp_s, depth_m, elevation_m)
    return result.Uw_ms, result.Tu_s


@dataclass(frozen=True)
class InverseResult:
    """Bracket-limited solution; no global minimum or OS-F101 qualification."""

    thickness_m: float | None
    status: str
    converged: bool
    residual_n_m: float | None
    bracket_m: tuple[float, float]
    iterations: int
    qualified: bool = False


def _forward_margin(pipe: Pipe, builder: Callable, *, g: float, thickness: float) -> float:
    updated = replace(pipe, concrete_m=thickness)
    weight = weight_build_up(updated, g=g)
    cases = tuple(builder(updated, weight))
    if len(cases) != 2 or {case.role for case in cases} != {'extreme_wave', 'extreme_current'}:
        raise ValueError('both distinct load-case roles are required')
    required = max(evaluate_case(case, diameter_m=weight.hydrodynamic_diameter_m,
                                 rho_water_kg_m3=updated.water_density_kg_m3,
                                 available_weight_n_m=weight.submerged_weight_n_m).required_weight_n_m
                   for case in cases)
    margin = weight.submerged_weight_n_m - required
    if not math.isfinite(margin):
        raise ValueError('forward margin must be finite')
    return margin


def inverse_concrete_thickness(pipe: Pipe,
                               case_builder: Callable[[Pipe, WeightResult], Sequence[Case]], *,
                               g: float, lower_m: float, upper_m: float,
                               tolerance_m: float = 1e-6, max_iterations: int = 100) -> InverseResult:
    """Bisect a caller bracket, rebuilding both cases at each trial diameter.

    The positive-margin endpoint is retained. Convergence bounds thickness
    error within the supplied bracket; it does not establish a smallest root
    when the diameter-dependent response is nonmonotone.
    """
    for name, value in (('g', g), ('tolerance_m', tolerance_m)):
        _finite(name, value, positive=True)
    _finite('lower_m', lower_m)
    _finite('upper_m', upper_m, positive=True)
    if upper_m <= lower_m or type(max_iterations) is not int or max_iterations < 1:
        raise ValueError('an increasing bracket and positive iteration count are required')
    low = _forward_margin(pipe, case_builder, g=g, thickness=lower_m)
    high = _forward_margin(pipe, case_builder, g=g, thickness=upper_m)
    bracket = (lower_m, upper_m)
    if low >= 0:
        status = 'lower_endpoint' if low == 0 else 'lower_feasible'
        return InverseResult(lower_m, status, True, low, bracket, 0)
    if high < 0:
        return InverseResult(None, 'unbracketed', False, high, bracket, 0)
    if high == 0:
        return InverseResult(upper_m, 'upper_endpoint', True, high, bracket, 0)
    for iteration in range(1, max_iterations + 1):
        midpoint = (lower_m + upper_m) / 2
        margin = _forward_margin(pipe, case_builder, g=g, thickness=midpoint)
        if margin >= 0:
            upper_m, high = midpoint, margin
        else:
            lower_m = midpoint
        if upper_m - lower_m <= tolerance_m:
            return InverseResult(upper_m, 'converged', True, high,
                                 (lower_m, upper_m), iteration)
    return InverseResult(None, 'not_converged', False, high,
                         (lower_m, upper_m), max_iterations)
