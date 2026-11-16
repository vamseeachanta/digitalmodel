"""Static riser weight and tension arithmetic from a legacy engineering workbook.

Source reference: ``API-RP-16Q``; clause not stated in source. No standard
edition is identified. Factors are required inputs, never code defaults.
See ``docs/drilling_riser/stackup_calculation.md`` for scope and Input gaps.
"""

from __future__ import annotations

from dataclasses import dataclass, fields
from math import isfinite
from numbers import Real


def _finite(name: str, value: float, minimum: float | None = None) -> None:
    if isinstance(value, bool) or not isinstance(value, Real) or not isfinite(value):
        raise ValueError(f"{name} must be a finite real number")
    if minimum is not None and value < minimum:
        raise ValueError(f"{name} must be >= {minimum}")


@dataclass(frozen=True)
class JointWeightInput:
    """Force inputs in N; buoyancy submerged weight is signed (uplift < 0).

    A combined assembly supplied as buoyancy weights can be represented by
    zero base weights and explicit neutral factors. Fractions/ratios are
    dimensionless. Base submerged weight can also be negative.
    """

    dry_weight_n: float
    submerged_weight_n: float
    buoyancy_dry_weight_n: float
    buoyancy_submerged_weight_n: float
    steel_tolerance_fraction: float
    buoyancy_factor_1: float

    def __post_init__(self) -> None:
        for name in ("dry_weight_n", "buoyancy_dry_weight_n",
                     "steel_tolerance_fraction", "buoyancy_factor_1"):
            _finite(name, getattr(self, name), 0.0)
        _finite("submerged_weight_n", self.submerged_weight_n)
        _finite("buoyancy_submerged_weight_n", self.buoyancy_submerged_weight_n)
        if self.buoyancy_factor_1 > 1.0:
            raise ValueError("buoyancy_factor_1 must be <= 1")


def adjusted_joint_weights(inputs: JointWeightInput) -> tuple[float, float]:
    """Return (dry, submerged) forces [N] after explicit allowances.

    Wdry = Wbase,dry*(1+t) + Wbuoy,dry;
    Wwet = Wbase,wet*(1+t) + Wbuoy,wet*fbt.
    Source names API-RP-16Q; clause not stated in source.
    """
    multiplier = 1.0 + inputs.steel_tolerance_fraction
    dry = inputs.dry_weight_n * multiplier + inputs.buoyancy_dry_weight_n
    wet = (inputs.submerged_weight_n * multiplier
           + inputs.buoyancy_submerged_weight_n * inputs.buoyancy_factor_1)
    _finite("adjusted dry weight", dry)
    _finite("adjusted submerged weight", wet)
    return dry, wet


@dataclass(frozen=True)
class HydrostaticSectionInput:
    """Uniform section with weights measured for a water-filled bore.

    Joint forces include their specified allowances. Auxiliary forces describe
    an external attached line, distinct from fluids in the auxiliary bores.
    Installed length can be a partial joint or several joints. Fluid mass in
    auxiliary bores is supplied separately to allow different fluid densities.
    """

    joint_length_m: float
    installed_length_m: float
    dry_joint_weight_n: float
    wet_joint_weight_n: float
    bore_area_m2: float
    auxiliary_bore_area_m2: float
    auxiliary_dry_weight_n_m: float
    auxiliary_wet_weight_n_m: float
    auxiliary_fluid_mass_kg_m: float
    fluid_density_kg_m3: float
    water_density_kg_m3: float
    gravity_m_s2: float

    def __post_init__(self) -> None:
        for name in ("joint_length_m", "water_density_kg_m3", "gravity_m_s2"):
            _finite(name, getattr(self, name), 0.0)
            if getattr(self, name) == 0.0:
                raise ValueError(f"{name} must be > 0")
        for name in ("installed_length_m", "dry_joint_weight_n", "bore_area_m2",
                     "auxiliary_bore_area_m2", "auxiliary_dry_weight_n_m",
                     "auxiliary_fluid_mass_kg_m", "fluid_density_kg_m3"):
            _finite(name, getattr(self, name), 0.0)
        for name in ("wet_joint_weight_n", "auxiliary_wet_weight_n_m"):
            _finite(name, getattr(self, name))


@dataclass(frozen=True)
class SectionWeights:
    """Per-length forces, fluid/displacement masses and installed forces."""

    dry_weight_n_m: float
    reference_wet_weight_n_m: float
    internal_water_mass_kg_m: float
    displaced_water_mass_kg_m: float
    internal_fluid_mass_kg_m: float
    filled_air_weight_n: float
    filled_submerged_weight_n: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _finite(field.name, getattr(self, field.name))


def section_weights(inputs: HydrostaticSectionInput) -> SectionWeights:
    """Compute distributed weights with internal fluid counted once.

    wd=Wd/L+wa,d; ww=Ww/L+wa,w; mi,w=(Ai+Aa)*rho,w;
    md=(wd-ww)/g+mi,w; mi=Ai*rho,i+ma,i;
    Wair=Li*(wd+mi*g); Wsub=Li*(wd+(mi-md)*g).
    The equivalent displacement is inferred from the supplied flooded wet
    weight, rather than from an assumed exterior diameter.
    Source names API-RP-16Q; clause not stated in source.
    """
    p = inputs
    dry = p.dry_joint_weight_n / p.joint_length_m + p.auxiliary_dry_weight_n_m
    wet = p.wet_joint_weight_n / p.joint_length_m + p.auxiliary_wet_weight_n_m
    water = (p.bore_area_m2 + p.auxiliary_bore_area_m2) * p.water_density_kg_m3
    displaced = (dry - wet) / p.gravity_m_s2 + water
    fluid = p.bore_area_m2 * p.fluid_density_kg_m3 + p.auxiliary_fluid_mass_kg_m
    return SectionWeights(
        dry, wet, water, displaced, fluid,
        p.installed_length_m * (dry + fluid * p.gravity_m_s2),
        p.installed_length_m * (dry + (fluid - displaced) * p.gravity_m_s2),
    )


@dataclass(frozen=True)
class TensionInput:
    """Aggregated force terms [N], explicit ratios and whole unit counts.

    Flooded steel and buoyancy are separate terms. Additional supported weight
    is unfactored; internal fluid includes auxiliary fluid. The displaced
    internal water term is distinct from exterior component displacement.
    The lower package is excluded from the tensioned string's weight sum.
    """

    flooded_steel_weight_n: float
    steel_weight_factor_1: float
    additional_supported_weight_n: float
    buoyancy_uplift_n: float
    buoyancy_factor_1: float
    internal_fluid_weight_n: float
    displaced_internal_water_weight_n: float
    tensioner_count: int
    failed_tensioner_count: int
    lower_package_submerged_weight_n: float
    minimum_lower_package_overpull_n: float
    recoil_overpull_n: float
    dynamic_tension_limit_n: float
    setting_fraction: float

    def __post_init__(self) -> None:
        for name in ("flooded_steel_weight_n", "buoyancy_uplift_n",
                     "internal_fluid_weight_n", "displaced_internal_water_weight_n",
                     "minimum_lower_package_overpull_n", "recoil_overpull_n",
                     "dynamic_tension_limit_n", "buoyancy_factor_1", "setting_fraction"):
            _finite(name, getattr(self, name), 0.0)
        for name in ("additional_supported_weight_n", "lower_package_submerged_weight_n"):
            _finite(name, getattr(self, name))
        _finite("steel_weight_factor_1", self.steel_weight_factor_1, 1.0)
        if self.buoyancy_factor_1 > 1.0 or self.setting_fraction > 1.0:
            raise ValueError("buoyancy_factor_1 and setting_fraction must be <= 1")
        for name in ("tensioner_count", "failed_tensioner_count"):
            value = getattr(self, name)
            if isinstance(value, bool) or not isinstance(value, int):
                raise ValueError(f"{name} must be an integer count")
        if not 0 <= self.failed_tensioner_count < self.tensioner_count:
            raise ValueError("require 0 <= failed_tensioner_count < tensioner_count")


@dataclass(frozen=True)
class TensionResult:
    """Algebraic static results [N]; capacity compliance is a separate check."""

    factored_steel_weight_n: float
    factored_buoyancy_n: float
    fluid_correction_n: float
    slip_ring_tension_n: float
    minimum_top_tension_n: float
    riser_overpull_n: float
    package_overpull_n: float
    additional_overpull_n: float
    overall_top_tension_n: float
    maximum_setting_n: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _finite(field.name, getattr(self, field.name))


def minimum_tension(inputs: TensionInput) -> TensionResult:
    """Compute the source's static demand and lower-package overpull chain.

    Tsr=Ws*fwt+Wextra-Bn*fbt+Wfluid-Wwater;
    Tmin=Tsr*N/(N-n); OPriser=Tmin-Tsr; OPpackage=OPriser-Wpackage;
    OPextra=max(OPmin-OPpackage,0); Toverall=Tmin+OPextra+OPrecoil.
    Maximum setting = DTL*setting_fraction. Mechanical efficiency and fleet
    angle reduction are omitted as in the source, using explicit unity factors.
    Source names API-RP-16Q; clause not stated in source.
    """
    from digitalmodel.drilling_riser.assembly import minimum_top_tension_16q
    from digitalmodel.drilling_riser.stackup import minimum_slip_ring_tension

    p = inputs
    steel = p.flooded_steel_weight_n * p.steel_weight_factor_1 + p.additional_supported_weight_n
    buoyancy = p.buoyancy_uplift_n * p.buoyancy_factor_1
    fluid = p.internal_fluid_weight_n - p.displaced_internal_water_weight_n
    # Reuse the existing mechanical split with zero fluid head; integrated
    # fluid forces (different bores/densities) are then added explicitly.
    mechanical = minimum_slip_ring_tension(
        p.flooded_steel_weight_n / 1000.0, p.buoyancy_uplift_n / 1000.0,
        0.0, 0.0, 0.0, 0.0, 0.0,
        f_wt=p.steel_weight_factor_1, f_bt=p.buoyancy_factor_1,
    ) * 1000.0
    slip = mechanical + p.additional_supported_weight_n + fluid
    top = minimum_top_tension_16q(
        slip / 1000.0, n_units=p.tensioner_count,
        n_fail=p.failed_tensioner_count, efficiency=1.0, fleet_angle_factor=1.0,
    ) * 1000.0
    overpull = top - slip
    package = overpull - p.lower_package_submerged_weight_n
    additional = max(p.minimum_lower_package_overpull_n - package, 0.0)
    return TensionResult(
        steel, buoyancy, fluid, slip, top, overpull, package, additional,
        top + additional + p.recoil_overpull_n,
        p.dynamic_tension_limit_n * p.setting_fraction,
    )


@dataclass(frozen=True)
class TubularCapacityInput:
    """Axial material screen: diameter/thickness [m], yield stress [Pa]."""

    outer_diameter_m: float
    wall_thickness_m: float
    yield_stress_pa: float
    allowable_factor_1: float

    def __post_init__(self) -> None:
        for name in ("outer_diameter_m", "wall_thickness_m", "yield_stress_pa",
                     "allowable_factor_1"):
            _finite(name, getattr(self, name), 0.0)
            if getattr(self, name) == 0.0:
                raise ValueError(f"{name} must be > 0")
        if 2.0 * self.wall_thickness_m >= self.outer_diameter_m:
            raise ValueError("wall thickness must be less than the outer radius")
        if self.allowable_factor_1 > 1.0:
            raise ValueError("allowable_factor_1 must be <= 1")


def axial_wall_capacity_n(inputs: TubularCapacityInput) -> float:
    """Return f*Sy*pi/4*(OD**2-(OD-2*t)**2) [N].

    f is a required input. This is the source's material-area screen only,
    without bending, pressure, connectors, fatigue or dynamic response.
    Source names API-RP-16Q; clause not stated in source.
    """
    from digitalmodel.drilling_riser.section import cross_sectional_area

    p = inputs
    area = cross_sectional_area(p.outer_diameter_m,
                              p.outer_diameter_m - 2.0 * p.wall_thickness_m)
    return p.allowable_factor_1 * p.yield_stress_pa * area


def flooded_material_weight_n(
    dry_weight_n: float, material_density_kg_m3: float, water_density_kg_m3: float,
) -> float:
    """Return Wdry*(1-rho,water/rho,material) [N] for flooded material.

    Source names API-RP-16Q; clause not stated in source. This density-ratio
    approximation excludes trapped volumes and separately attached buoyancy.
    """
    _finite("dry_weight_n", dry_weight_n, 0.0)
    _finite("material_density_kg_m3", material_density_kg_m3, 0.0)
    _finite("water_density_kg_m3", water_density_kg_m3, 0.0)
    if material_density_kg_m3 == 0.0:
        raise ValueError("material_density_kg_m3 must be > 0")
    return dry_weight_n * (1.0 - water_density_kg_m3 / material_density_kg_m3)


def governing_tension_n(minimum_top_tension_n: float, overpull_top_tension_n: float) -> float:
    """Return max(Tmin, Toverpull) [N] for two supplied tension requirements.

    This selects requirements; it does not regenerate a stored density curve.
    Source names API-RP-16Q; clause not stated in source.
    """
    _finite("minimum_top_tension_n", minimum_top_tension_n, 0.0)
    _finite("overpull_top_tension_n", overpull_top_tension_n, 0.0)
    return max(minimum_top_tension_n, overpull_top_tension_n)
