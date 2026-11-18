"""Validated riser line properties and static weight balance in SI units.

Method extracted from a legacy engineering calculation. This module reuses
``section.section_properties`` for A=pi*(Do**2-Di**2)/4,
I=pi*(Do**4-Di**4)/64, J=2*I, EA=E*A, EI=E*I and
GJ=E*J/(2*(1+nu)). Count multiplies joint length and mass; mass per length
uses one joint. Equivalent buoyancy diameter satisfies
rho*pi*(Db**2-Di**2)/4 = dry_mass_per_length - submerged_mass_per_length.
The submerged mass is apparent weight divided by gravity; it may be negative.
It is not inertial mass. A geometric buoyancy diameter can instead be supplied
directly by a caller; it need not agree with measured apparent weights.

Static balance is Tbottom=Ttop-m_service*g. The caller supplies dry or apparent
service mass and the top tension at each boundary, including externally imposed
loads. No continuity, hydrostatic pressure correction, dynamic equilibrium,
fatigue life, wall thickness sizing or acceptance criterion is inferred.
Soil resistance normalization is pressure=line_load/reference_diameter; it uses
the diameter associated with the supplied p-y data, not a different target
pile diameter. No soil constitutive relation or interpolation is evaluated.

Inputs and outputs use m, kg, s, N and Pa; units are in field names. Boundary
constructors use exact length definitions (inch to m, foot to m) and kPa to Pa.
Unit basis: NIST SP 811 (2008), Guide for the Use of the International System
of Units, Appendix B.8: 1 ft = 0.3048 m and 1 in = 0.0254 m, both printed in
boldface as exact; Sec. 4.3 Table 5: kilo = 10**3 (kPa to Pa, kN to N).
Mass and force conversion coefficients are required: exact definitions can be
supplied (NIST SP 811 Appendix B footnote 22: 1 lb = 0.45359237 kg; footnote
23: 1 lbf = 4.4482216152605 N for g_n = 9.80665 m/s2), or historical rounding
can be reproduced explicitly without silently changing the engineering data.
No source-specific coefficient is a default.

Joint dry and apparent submerged masses are on one load basis: the same
contents, coatings and attachments. On that basis the apparent submerged mass
is the dry mass less the displaced water mass, which cannot be negative, so a
submerged mass above the dry mass is rejected. A caller whose two weights
include different contents or loads must reconcile them before construction.

The Poisson ratio interval -1 < nu < 0.5 is derived, not sourced: for linear
isotropic elasticity with E > 0, the shear modulus G = E/(2*(1+nu)) is positive
only for nu > -1 and the bulk modulus K = E/(3*(1-2*nu)) only for nu < 0.5.

Standards as stated in the source: API-1111; clause not stated in source.
That name appears in a cover subject, without an edition or a demonstrated
connection to the formulas above. These equations are an arithmetic method,
not a claim of API-1111 compliance. Existing section-module references are not
additional source citations for this extraction.

Input gaps: standard edition, clause and applicability; material properties;
water density; gravity; basis of measured apparent weights and mass ratios;
any supplied linear mass coefficient; mass/force conversion precision; validity
of externally supplied tensions and component load paths; reference diameter
and suitability of empirical soil data. These quantities require caller evidence.
Limits and remaining work: linear isotropic circular sections only; every
component requires a positive length and count. Very thin sections can lose
floating-point precision in the reused difference-of-powers formulas. Solver
execution, generated solver scripts, plotting, project-specific grouping of
components, current profiles and raw empirical tables are outside this module.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite, pi, sqrt
from numbers import Integral, Real

from digitalmodel.drilling_riser.section import INCH_TO_METER, section_properties
from digitalmodel.drilling_riser.stackup import effective_tension

FOOT_TO_METER = 0.3048  # NIST SP 811 (2008) Appendix B.8, boldface (exact).
KILO = 1000.0  # NIST SP 811 (2008) Sec. 4.3 Table 5: kilo = 10**3.


def _finite(value: float, name: str) -> None:
    if isinstance(value, bool) or not isinstance(value, Real) or not isfinite(value):
        raise ValueError(f'{name} must be a finite real number')


def _positive(value: float, name: str) -> None:
    _finite(value, name)
    if value <= 0:
        raise ValueError(f'{name} must be positive')


def _nonnegative(value: float, name: str) -> None:
    _finite(value, name)
    if value < 0:
        raise ValueError(f'{name} must be nonnegative')


@dataclass(frozen=True)
class RiserJointInput:
    """One repeated joint: SI geometry, elastic data and mass equivalents.

    ``from_imperial`` converts imperial lengths and masses; the modulus is accepted in kPa.
    The joint count accepts any integral number except a boolean (for example
    a NumPy integer) and is stored as a Python int.
    """

    joint_count_dimensionless: int
    joint_length_m: float
    outer_diameter_m: float
    wall_thickness_m: float
    dry_mass_per_joint_kg: float
    submerged_mass_per_joint_kg: float
    youngs_modulus_pa: float
    poisson_ratio_dimensionless: float

    def __post_init__(self) -> None:
        count = self.joint_count_dimensionless
        if isinstance(count, bool) or not isinstance(count, Integral) or count < 1:
            raise ValueError('joint_count_dimensionless must be a positive integer')
        object.__setattr__(self, 'joint_count_dimensionless', int(count))
        for name in ('joint_length_m', 'outer_diameter_m', 'wall_thickness_m',
                     'youngs_modulus_pa'):
            _positive(getattr(self, name), name)
        _nonnegative(self.dry_mass_per_joint_kg, 'dry_mass_per_joint_kg')
        _finite(self.submerged_mass_per_joint_kg, 'submerged_mass_per_joint_kg')
        _finite(self.poisson_ratio_dimensionless, 'poisson_ratio_dimensionless')
        if 2 * self.wall_thickness_m > self.outer_diameter_m:
            raise ValueError('wall thickness exceeds the outer radius')
        if not -1 < self.poisson_ratio_dimensionless < 0.5:
            # Derived in the module docstring from G > 0 and K > 0.
            raise ValueError('isotropic elasticity requires -1 < Poisson ratio < 0.5')
        if self.submerged_mass_per_joint_kg > self.dry_mass_per_joint_kg:
            # One load basis: displaced water mass = dry - submerged >= 0.
            raise ValueError('submerged mass exceeds dry mass: negative displacement')

    @classmethod
    def from_imperial(
        cls, *, joint_count_dimensionless: int, joint_length_ft: float,
        outer_diameter_in: float, wall_thickness_in: float,
        dry_mass_per_joint_lb: float, submerged_mass_per_joint_lb: float,
        youngs_modulus_kpa: float, poisson_ratio_dimensionless: float,
        mass_conversion_kg_per_lb: float,
    ) -> RiserJointInput:
        """Convert lengths exactly; masses use the explicit kg/lb coefficient.

        Labels in lb are interpreted as mass or apparent mass, not force.
        The modulus is supplied in kPa, independent of the imperial lengths.
        """
        _positive(mass_conversion_kg_per_lb, 'mass_conversion_kg_per_lb')
        for name, value in (
            ('joint_length_ft', joint_length_ft),
            ('outer_diameter_in', outer_diameter_in),
            ('wall_thickness_in', wall_thickness_in),
            ('dry_mass_per_joint_lb', dry_mass_per_joint_lb),
            ('submerged_mass_per_joint_lb', submerged_mass_per_joint_lb),
            ('youngs_modulus_kpa', youngs_modulus_kpa),
        ):
            _finite(value, name)
        return cls(
            joint_count_dimensionless, joint_length_ft * FOOT_TO_METER,
            outer_diameter_in * INCH_TO_METER, wall_thickness_in * INCH_TO_METER,
            dry_mass_per_joint_lb * mass_conversion_kg_per_lb,
            submerged_mass_per_joint_lb * mass_conversion_kg_per_lb,
            youngs_modulus_kpa * KILO, poisson_ratio_dimensionless,
        )


@dataclass(frozen=True)
class JointProperties:
    """SI results; stiffness uses the stress diameter, not buoyancy diameter."""

    inner_diameter_m: float
    area_m2: float
    second_moment_m4: float
    polar_moment_m4: float
    axial_stiffness_n: float
    bending_stiffness_n_m2: float
    torsional_stiffness_n_m2: float
    total_length_m: float
    total_dry_mass_kg: float
    total_submerged_mass_kg: float
    dry_mass_kg_per_m: float
    submerged_mass_kg_per_m: float


def compute_joint_properties(inputs: RiserJointInput) -> JointProperties:
    """Compute Di=Do-2t, section properties, n*L, n*m and m/L.

    The section equations are given in the module method and delegated to the
    existing section implementation. No material or weight factor is assumed.
    """
    bore = inputs.outer_diameter_m - 2 * inputs.wall_thickness_m
    props = section_properties(
        inputs.outer_diameter_m, bore, inputs.youngs_modulus_pa,
        inputs.poisson_ratio_dimensionless,
    )
    return JointProperties(
        bore, props.area, props.moment_of_inertia, props.polar_moment,
        props.axial_stiffness, props.bending_stiffness, props.torsional_stiffness,
        inputs.joint_count_dimensionless * inputs.joint_length_m,
        inputs.joint_count_dimensionless * inputs.dry_mass_per_joint_kg,
        inputs.joint_count_dimensionless * inputs.submerged_mass_per_joint_kg,
        inputs.dry_mass_per_joint_kg / inputs.joint_length_m,
        inputs.submerged_mass_per_joint_kg / inputs.joint_length_m,
    )


@dataclass(frozen=True)
class BuoyancyInput:
    """Apparent mass loss and bore diameter used to infer displaced annulus."""

    dry_mass_kg_per_m: float
    submerged_mass_kg_per_m: float
    bore_diameter_m: float
    water_density_kg_per_m3: float

    def __post_init__(self) -> None:
        _nonnegative(self.dry_mass_kg_per_m, 'dry_mass_kg_per_m')
        _finite(self.submerged_mass_kg_per_m, 'submerged_mass_kg_per_m')
        _nonnegative(self.bore_diameter_m, 'bore_diameter_m')
        _positive(self.water_density_kg_per_m3, 'water_density_kg_per_m3')
        if self.submerged_mass_kg_per_m > self.dry_mass_kg_per_m:
            raise ValueError('submerged mass exceeds dry mass: negative displacement')


def compute_buoyancy_diameter_m(inputs: BuoyancyInput) -> float:
    """Return sqrt(4*(m_dry-m_sub)/(pi*rho)+Di**2), with masses per metre."""
    loss = inputs.dry_mass_kg_per_m - inputs.submerged_mass_kg_per_m
    return sqrt(loss * 4 / pi / inputs.water_density_kg_per_m3
                + inputs.bore_diameter_m**2)


def apparent_mass_from_ratio(
    dry_mass_kg: float, submerged_to_dry_ratio_dimensionless: float,
) -> float:
    """Return m_sub=m_dry*r with caller-supplied apparent-mass ratio r <= 1.

    Negative r represents net uplift. The ratio is an input gap until its
    engineering basis is supplied; it is not a material constant.
    """
    _nonnegative(dry_mass_kg, 'dry_mass_kg')
    _finite(submerged_to_dry_ratio_dimensionless, 'submerged_to_dry_ratio_dimensionless')
    if submerged_to_dry_ratio_dimensionless > 1:
        raise ValueError('apparent mass ratio cannot exceed one')
    return dry_mass_kg * submerged_to_dry_ratio_dimensionless


def mass_from_linear_density(linear_mass_kg_per_m: float, length_m: float) -> float:
    """Return m=mu*L with an explicit, nonnegative linear mass coefficient."""
    _nonnegative(linear_mass_kg_per_m, 'linear_mass_kg_per_m')
    _positive(length_m, 'length_m')
    return linear_mass_kg_per_m * length_m


@dataclass(frozen=True)
class TensionInput:
    """Top force, signed in-service mass equivalent and required gravity."""

    top_tension_n: float
    inservice_mass_kg: float
    gravity_m_per_s2: float

    def __post_init__(self) -> None:
        _finite(self.top_tension_n, 'top_tension_n')
        _finite(self.inservice_mass_kg, 'inservice_mass_kg')
        _positive(self.gravity_m_per_s2, 'gravity_m_per_s2')


@dataclass(frozen=True)
class TensionBalance:
    """Positive tension is tensile; negative bottom tension denotes compression."""

    inservice_weight_n: float
    bottom_tension_n: float


def compute_tension_balance(inputs: TensionInput) -> TensionBalance:
    """Return W=m_service*g and Tbottom=Ttop-W, without a pressure correction.

    The existing ``stackup.effective_tension`` function is reused for the balance.
    Its contract is in kN, so forces are converted N -> kN at the call and the
    result kN -> N on return.
    """
    weight = inputs.inservice_mass_kg * inputs.gravity_m_per_s2
    bottom_kn = effective_tension(inputs.top_tension_n / KILO, weight / KILO, 1.0)
    return TensionBalance(weight, bottom_kn * KILO)


@dataclass(frozen=True)
class SoilResistanceInput:
    """One supplied p-y point; deflection is magnitude and line load is signed."""

    deflection_m: float
    line_load_n_per_m: float
    reference_diameter_m: float

    def __post_init__(self) -> None:
        _nonnegative(self.deflection_m, 'deflection_m')
        _finite(self.line_load_n_per_m, 'line_load_n_per_m')
        _positive(self.reference_diameter_m, 'reference_diameter_m')

    @classmethod
    def from_imperial(
        cls, *, deflection_in: float, line_load_lbf_per_in: float,
        reference_diameter_in: float, force_conversion_n_per_lbf: float,
    ) -> SoilResistanceInput:
        """Convert y and D using exact inches; force uses an explicit N/lbf factor."""
        _positive(force_conversion_n_per_lbf, 'force_conversion_n_per_lbf')
        for name, value in (
            ('deflection_in', deflection_in),
            ('line_load_lbf_per_in', line_load_lbf_per_in),
            ('reference_diameter_in', reference_diameter_in),
        ):
            _finite(value, name)
        return cls(deflection_in * INCH_TO_METER,
                   line_load_lbf_per_in * force_conversion_n_per_lbf / INCH_TO_METER,
                   reference_diameter_in * INCH_TO_METER)


def compute_soil_pressure_pa(inputs: SoilResistanceInput) -> float:
    """Return q=p/D_ref in Pa for supplied line resistance p in N/m."""
    return inputs.line_load_n_per_m / inputs.reference_diameter_m
