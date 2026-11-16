"""Local lateral soil resistance in SI, from a legacy engineering workbook.

Clay: API RP 2A-WSD, 21st Ed. (2000), clauses 6.8.2 and 6.8.3;
API RP 2A-WSD, Errata and Supplement 3 (2007), replacement clause 6.8.3.
Sand: the 2000 reference, clauses 6.8.6 and 6.8.7.
These clauses are visible in the source's embedded reference images.

Inputs use m, Pa, N/m3 and dimensionless ratios. Clay ultimate resistance is
pressure (Pa); sand ultimate resistance is force per length (N/m).
Multiply clay pressure by diameter to obtain force per length.

Limits: soft-clay and sand local response only; no pile equilibrium solution,
layered-overburden integration, stiff-clay model or cyclic history model.
The local algebraic transition depth assumes constant properties; applying it
to depth-varying strength preserves the source method but does not solve for
the first intersection of two depth-varying resistance profiles.

Input gaps: site strength, effective unit weight, strain, empirical clay J,
sand C1/C2/C3 and subgrade modulus require qualified caller inputs. Sand
regression coefficients have no supplied fit accuracy or derivation and are
required inputs, never embedded constants. No additional standard is claimed
for those regressions or for the generic property interpolation.
"""

from dataclasses import dataclass
from math import atanh, isfinite, tanh


def _check_values(
    *, positive: tuple[float, ...], nonnegative: tuple[float, ...],
    signed: tuple[float, ...] = (),
) -> None:
    """Reject nonfinite values and quantities outside their scalar domains."""
    values = positive + nonnegative + signed
    if any(isinstance(v, bool) or not isinstance(v, (int, float)) for v in values):
        raise ValueError("quantities must be real numbers")
    if not all(isfinite(v) for v in values):
        raise ValueError("quantities must be finite")
    if any(v <= 0 for v in positive) or any(v < 0 for v in nonnegative):
        raise ValueError("quantity outside its positive or nonnegative domain")


@dataclass(frozen=True)
class ClayPoint:
    """Local soft-clay inputs; J and strain are dimensionless and required."""

    depth_m: float
    diameter_m: float
    effective_unit_weight_n_per_m3: float
    undrained_strength_pa: float
    strain_at_half_strength_one: float
    empirical_j_one: float

    def __post_init__(self) -> None:
        _check_values(
            positive=(self.diameter_m, self.undrained_strength_pa,
                      self.strain_at_half_strength_one),
            nonnegative=(self.depth_m, self.effective_unit_weight_n_per_m3,
                         self.empirical_j_one),
        )
        if self.effective_unit_weight_n_per_m3 == self.empirical_j_one == 0:
            raise ValueError("transition depth requires positive unit weight or J")


def clay_characteristic_displacement_m(point: ClayPoint) -> float:
    """yc = 2.5 epsilon_c D; API RP 2A-WSD (2000/2007), clause 6.8.3."""
    return 2.5 * point.strain_at_half_strength_one * point.diameter_m


def clay_transition_depth_m(point: ClayPoint) -> float:
    """XR = 6 D c / (gamma D + J c); 2000 reference, clause 6.8.2."""
    return (6 * point.diameter_m * point.undrained_strength_pa
            / (point.effective_unit_weight_n_per_m3 * point.diameter_m
               + point.empirical_j_one * point.undrained_strength_pa))


def clay_shallow_pressure_pa(point: ClayPoint) -> float:
    """3c + gamma z + J c z/D; API RP 2A-WSD (2000), equation 6.8.2-1."""
    return (3 * point.undrained_strength_pa
            + point.effective_unit_weight_n_per_m3 * point.depth_m
            + point.empirical_j_one * point.undrained_strength_pa
            * point.depth_m / point.diameter_m)


def clay_ultimate_pressure_pa(point: ClayPoint) -> float:
    """Select shallow pressure below XR, otherwise 9c; clause 6.8.2."""
    if point.depth_m >= clay_transition_depth_m(point):
        return 9 * point.undrained_strength_pa
    return clay_shallow_pressure_pa(point)


@dataclass(frozen=True)
class SandPoint:
    """Local sand inputs, including required dimensionless C1/C2/C3."""

    depth_m: float
    diameter_m: float
    effective_unit_weight_n_per_m3: float
    c1_one: float
    c2_one: float
    c3_one: float
    subgrade_modulus_n_per_m3: float

    def __post_init__(self) -> None:
        _check_values(
            positive=(self.diameter_m, self.effective_unit_weight_n_per_m3,
                      self.c1_one, self.c2_one, self.c3_one,
                      self.subgrade_modulus_n_per_m3),
            nonnegative=(self.depth_m,),
        )


def sand_ultimate_resistance_n_per_m(point: SandPoint) -> float:
    """min((C1 z + C2 D) gamma z, C3 D gamma z); clause 6.8.6."""
    shallow = ((point.c1_one * point.depth_m + point.c2_one * point.diameter_m)
               * point.effective_unit_weight_n_per_m3 * point.depth_m)
    deep = (point.c3_one * point.diameter_m
            * point.effective_unit_weight_n_per_m3 * point.depth_m)
    return min(shallow, deep)


def sand_loading_factor_one(point: SandPoint, *, cyclic: bool) -> float:
    """A = 0.9 cyclic; max(3 - 0.8 z/D, 0.9) static; clause 6.8.7."""
    if not isinstance(cyclic, bool):
        raise ValueError("cyclic must be a boolean")
    return 0.9 if cyclic else max(3 - 0.8 * point.depth_m / point.diameter_m, 0.9)


def sand_resistance_n_per_m(point: SandPoint, displacement_m: float, *, cyclic: bool) -> float:
    """A pu tanh(k z y / (A pu)); 2000 reference, equation 6.8.7-1."""
    _check_values(positive=(), nonnegative=(), signed=(displacement_m,))
    limit = sand_loading_factor_one(point, cyclic=cyclic) * sand_ultimate_resistance_n_per_m(point)
    if limit == 0:
        return 0.0
    return limit * tanh(point.subgrade_modulus_n_per_m3 * point.depth_m * displacement_m / limit)


def sand_displacement_m(point: SandPoint, *, resistance_fraction_one: float, cyclic: bool) -> float:
    """Invert 6.8.7-1: y = A pu atanh(f)/(k z), where 0 <= f < 1.

    f is a fraction of A pu. The surface has zero resistance and no unique
    inverse displacement, so depth zero is rejected.
    """
    _check_values(positive=(point.depth_m,), nonnegative=(resistance_fraction_one,))
    if resistance_fraction_one >= 1:
        raise ValueError("resistance fraction must be less than one")
    limit = sand_loading_factor_one(point, cyclic=cyclic) * sand_ultimate_resistance_n_per_m(point)
    return limit / (point.subgrade_modulus_n_per_m3 * point.depth_m) * atanh(resistance_fraction_one)


@dataclass(frozen=True)
class SandCoefficientFit:
    """Caller-supplied cubic fits and qualified angle domain.

    Each tuple is (a0, a1, a2, a3) in units 1, deg^-1, deg^-2, deg^-3,
    respectively. No coefficients from a reference figure are built in.
    A legacy engineering workbook uses cubic approximations of Figure
    6.8.6-1 (2000 reference), but states no clause for the fit procedure.
    """

    c1_coefficients_per_deg_power: tuple[float, float, float, float]
    c2_coefficients_per_deg_power: tuple[float, float, float, float]
    c3_coefficients_per_deg_power: tuple[float, float, float, float]
    minimum_angle_deg: float
    maximum_angle_deg: float

    def __post_init__(self) -> None:
        for coefficients in (self.c1_coefficients_per_deg_power,
                             self.c2_coefficients_per_deg_power,
                             self.c3_coefficients_per_deg_power):
            if not isinstance(coefficients, tuple) or len(coefficients) != 4:
                raise ValueError("each cubic fit requires an immutable four-coefficient tuple")
            _check_values(positive=(), nonnegative=(), signed=coefficients)
        _check_values(positive=(self.maximum_angle_deg,), nonnegative=(self.minimum_angle_deg,))
        if self.maximum_angle_deg <= self.minimum_angle_deg:
            raise ValueError("angle domain must have positive width")


def sand_coefficients_one(friction_angle_deg: float, fit: SandCoefficientFit) -> tuple[float, ...]:
    """Evaluate a0 + a1 phi + a2 phi^2 + a3 phi^3 for each supplied fit.

    The angle is evaluated directly; no rounded grid lookup or angle offset
    is imposed. Fit accuracy and the valid angle domain remain caller duties.
    """
    _check_values(positive=(), nonnegative=(friction_angle_deg,))
    if not fit.minimum_angle_deg <= friction_angle_deg <= fit.maximum_angle_deg:
        raise ValueError("friction angle outside the qualified fit domain")
    return tuple(a3 * friction_angle_deg ** 3 + a2 * friction_angle_deg ** 2
                 + a1 * friction_angle_deg + a0 for a0, a1, a2, a3 in (
                     fit.c1_coefficients_per_deg_power, fit.c2_coefficients_per_deg_power,
                     fit.c3_coefficients_per_deg_power))


def linear_property_at_depth(
    depth_m: float, *, start_depth_m: float, end_depth_m: float,
    start_value_si: float, end_value_si: float, allow_extrapolation: bool,
) -> float:
    """v0 + (z-z0)(v1-v0)/(z1-z0), with endpoint values in matching SI units.

    No standard clause is stated in the source for this generic interpolation.
    Extrapolation requires explicit selection; it does not qualify a soil layer.
    """
    _check_values(positive=(), nonnegative=(depth_m, start_depth_m, end_depth_m),
                  signed=(start_value_si, end_value_si))
    if not isinstance(allow_extrapolation, bool):
        raise ValueError("allow_extrapolation must be a boolean")
    if end_depth_m <= start_depth_m:
        raise ValueError("layer depth interval must have positive width")
    if not allow_extrapolation and not start_depth_m <= depth_m <= end_depth_m:
        raise ValueError("depth outside layer interval")
    return start_value_si + (depth_m - start_depth_m) * (
        (end_value_si - start_value_si) / (end_depth_m - start_depth_m))


def inches_to_metres(length_in: float) -> float:
    """Exact dimensional boundary conversion: 1 international inch = 0.0254 m."""
    _check_values(positive=(), nonnegative=(length_in,))
    return length_in * 0.0254
