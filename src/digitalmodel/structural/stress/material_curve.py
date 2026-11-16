"""Two-region true and engineering stress–strain curves, with SI inputs.

Method source: a legacy engineering workbook, citing ``ASME VIII DIV 2 with
Addenda (2010)``, ``Section 3.D.3``. No equation numbers are stated in source.
This module reconstructs that source's equations; agreement with that source
does not independently establish standards conformance or material suitability.

Inputs: engineering yield and ultimate stresses and elastic modulus in Pa;
offset strain, proportional strain and material hardening coefficient are
dimensionless ratios. All are required. Other stress units shall be converted
at the caller boundary using the existing ``digitalmodel.units.Q_`` registry.

With R = yield/ultimate, offset o, proportional strain p, and coefficient c:
    m1 = (ln(R) + p - o) / ln(ln(1+p) / ln(1+o))
    m2 = c (1-R)
    A1 = yield (1+o) / ln(1+o)**m1
    A2 = ultimate exp(m2) / m2**m2
    K = 1.5 R**1.5 - 0.5 R**2.5 - R**3.5
    H = 2 (stress - yield - K (ultimate-yield)) / (K (ultimate-yield))
    e1 = (stress/A1)**(1/m1); e2 = (stress/A2)**(1/m2)
    true_strain = stress/E + e1 (1-tanh(H))/2 + e2 (1+tanh(H))/2
    engineering_strain = exp(true_strain)-1
    engineering_stress = true_stress/(1+engineering_strain)

The existing LinearElasticModel supplies stress/E. Each power branch is
blended once; no inverse solver, fitted experimental data or strain grid is
inferred. Logarithms and exponentials use log1p/expm1 to retain small strains.
The strength-gap ratio and factored K = R**1.5 (1-R) (1.5+R) avoid subtracting
nearly equal polynomial terms when the two strengths approach each other.

Input gaps: the proportional strain and hardening coefficient are described
as ASME table lookups, but material classification, table identification and
table-specific clause are not stated in source (clause not stated in source
for those lookups). They shall be independently supplied and qualified.
Temperature, material applicability, and permissible strain range are also
unspecified. No material lookup values or defaults are provided here.

Limits and what is left: nonnegative uniaxial tensile stress only, with
0 < yield < ultimate and 0 < proportional strain < offset strain. Numerical
continuation beyond the true ultimate is allowed to reproduce the algebra;
post-necking validity, unloading, compression, cyclic response, temperature
derating, material selection, plots and solver export are not established.
Unrepresentable parameters or results raise ValueError.
"""

from dataclasses import dataclass, fields
import math
from numbers import Real

from .stress_strain import LinearElasticModel


def _validate_number(name: str, value: float, *, positive: bool) -> None:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real number")
    try:
        valid = math.isfinite(value) and (value > 0 if positive else value >= 0)
    except OverflowError:
        valid = False
    if not valid:
        bound = "positive" if positive else "nonnegative"
        raise ValueError(f"{name} must be finite and {bound}")


@dataclass(frozen=True)
class MaterialCurveInputs:
    """Engineering material properties; Pa and dimensionless strain ratios."""

    yield_stress_pa: float
    ultimate_stress_pa: float
    elastic_modulus_pa: float
    yield_offset_strain_ratio: float
    proportional_strain_ratio: float
    hardening_coefficient_ratio: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _validate_number(field.name, getattr(self, field.name), positive=True)
        if self.yield_stress_pa >= self.ultimate_stress_pa:
            raise ValueError("yield_stress_pa must be below ultimate_stress_pa")
        if self.proportional_strain_ratio >= self.yield_offset_strain_ratio:
            raise ValueError("proportional strain must be below yield offset strain")


@dataclass(frozen=True)
class CurveParameters:
    """Derived scales in Pa; exponents and transition parameter are ratios."""

    strength_ratio: float
    micro_exponent_ratio: float
    macro_exponent_ratio: float
    micro_scale_pa: float
    macro_scale_pa: float
    blend_parameter_ratio: float
    true_ultimate_stress_pa: float


@dataclass(frozen=True)
class TrueStrainComponents:
    """Dimensionless true-strain terms and the dimensionless blend argument."""

    elastic_strain_ratio: float
    blend_argument_ratio: float
    micro_power_strain_ratio: float
    macro_power_strain_ratio: float
    micro_strain_ratio: float
    macro_strain_ratio: float

    @property
    def total_strain_ratio(self) -> float:
        """Return the elastic term plus both weighted plastic terms."""
        return self.elastic_strain_ratio + self.micro_strain_ratio + self.macro_strain_ratio


@dataclass(frozen=True)
class EngineeringPoint:
    """Engineering stress in Pa and dimensionless engineering strain."""

    stress_pa: float
    strain_ratio: float


def calculate_curve_parameters(material: MaterialCurveInputs) -> CurveParameters:
    """Compute R, m1, m2, A1, A2, K and ultimate*exp(m2).

    Equations are given in the module docstring. Source attribution:
    ASME VIII DIV 2 with Addenda (2010), Section 3.D.3.
    """
    try:
        ratio = material.yield_stress_pa / material.ultimate_stress_pa
        gap_ratio = (material.ultimate_stress_pa - material.yield_stress_pa) / (
            material.ultimate_stress_pa
        )
        offset = material.yield_offset_strain_ratio
        proportional = material.proportional_strain_ratio
        micro = (math.log(ratio) + proportional - offset) / math.log(
            math.log1p(proportional) / math.log1p(offset)
        )
        macro = material.hardening_coefficient_ratio * gap_ratio
        ultimate_true = material.ultimate_stress_pa * math.exp(macro)
        micro_scale = material.yield_stress_pa * (1 + offset) / math.log1p(offset) ** micro
        macro_scale = ultimate_true / macro ** macro
        blend = ratio ** 1.5 * gap_ratio * (1.5 + ratio)
        result = CurveParameters(ratio, micro, macro, micro_scale, macro_scale,
                                 blend, ultimate_true)
        if any(not math.isfinite(getattr(result, field.name))
               or getattr(result, field.name) <= 0 for field in fields(result)):
            raise ValueError("nonpositive or nonfinite derived parameter")
        return result
    except (OverflowError, ZeroDivisionError, ValueError) as exc:
        raise ValueError("curve parameters are not representable for these inputs") from exc


def calculate_true_strain_components(
    true_stress_pa: float, material: MaterialCurveInputs,
) -> TrueStrainComponents:
    """Compute stress/E and tanh-weighted (stress/Ai)**(1/mi) terms.

    H = 2*(stress-yield-K*(ultimate-yield))/(K*(ultimate-yield)).
    Source: ASME VIII DIV 2 with Addenda (2010), Section 3.D.3.
    The scalar stress is an evaluation coordinate in Pa, not a stress limit.
    """
    _validate_number("true_stress_pa", true_stress_pa, positive=False)
    parameters = calculate_curve_parameters(material)
    try:
        width_pa = parameters.blend_parameter_ratio * (
            material.ultimate_stress_pa - material.yield_stress_pa
        )
        h = 2 * (true_stress_pa - (material.yield_stress_pa + width_pa)) / width_pa
        micro = (true_stress_pa / parameters.micro_scale_pa) ** (
            1 / parameters.micro_exponent_ratio
        )
        macro = (true_stress_pa / parameters.macro_scale_pa) ** (
            1 / parameters.macro_exponent_ratio
        )
        transition = math.tanh(h)
        elastic = LinearElasticModel().calculate_strain(
            true_stress_pa, elastic_modulus=material.elastic_modulus_pa
        )
        result = TrueStrainComponents(elastic, h, micro, macro,
                                     micro / 2 * (1 - transition),
                                     macro / 2 * (1 + transition))
        if any(not math.isfinite(getattr(result, field.name)) for field in fields(result)):
            raise ValueError("nonfinite strain component")
        if not math.isfinite(result.total_strain_ratio):
            raise ValueError("nonfinite total strain")
        return result
    except (OverflowError, ZeroDivisionError, ValueError) as exc:
        raise ValueError("true strain is not representable for these inputs") from exc


def engineering_stress_strain(true_stress_pa: float, true_strain_ratio: float) -> EngineeringPoint:
    """Return engineering strain = exp(true strain)-1 and stress = true/exp(strain).

    These are uniform-deformation conversions as used by a legacy engineering
    workbook citing ASME VIII DIV 2 with Addenda (2010), Section 3.D.3.
    No separate conversion-equation clause is stated in source.
    """
    _validate_number("true_stress_pa", true_stress_pa, positive=False)
    _validate_number("true_strain_ratio", true_strain_ratio, positive=False)
    try:
        strain = math.expm1(true_strain_ratio)
        stress = true_stress_pa / (1 + strain)
        if not math.isfinite(strain) or not math.isfinite(stress):
            raise ValueError("nonfinite engineering point")
        return EngineeringPoint(stress, strain)
    except (OverflowError, ValueError) as exc:
        raise ValueError("engineering point is not representable for these inputs") from exc
