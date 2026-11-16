"""Tangential added-mass geometry for a cylindrical buoyancy section.

The algebra follows a legacy in-house calculation. Inputs are SI
lengths: bare pipe outer diameter, radial insulation thickness, outer buoyancy
diameter and individual buoyancy-section length, all in metres. Convert declared
boundary units with the repository's existing converters before constructing
``BuoyancySection``. No units or engineering assumptions are inferred.

Method
------
For coated pipe diameter d = Dpipe + 2*t, outer buoyancy diameter Db and length L:

    N = Db**3 * (1 - d/Db)**2 / 6 = Db * (Db - d)**2 / 6
    Q = (Db**2 + 2*d**2) * L / 4
    Ca = N / Q

N and Q have units m3. Ca and L/Db are dimensionless. Equivalently, with
r = d/Db and ell = L/Db, Ca = (2/3)*(1-r)**2/((1+2*r**2)*ell).
The equality d=Db is the zero-added-volume mathematical limit.

Standards and input gaps
------------------------
The source states ``DNV-C-205`` beside a tangential drag input: clause not stated
in source. That label is retained exactly; its association with the adjacent
added-mass expression, edition and applicable geometry limits are not established.
The drag value has no derivation and is not computed here. Insulation thickness
is an engineering input and has no default. The source uses conflicting labels
for the buoyancy dimension; interpreting it as outer diameter reproduces the
displayed algebra, but requires confirmation for a physical application. The
source does not establish whether the stated section length describes each
discrete element or an aggregate; the caller shall resolve this distinction.

Limits and what is left
-----------------------
This is a geometry calculation, without a design acceptance criterion. It
does not solve catenary equilibrium, derive buoyancy sizing, compute drag or
establish standards compliance. Nominal axial stress and annular steel area are
available from ``installation.pipelay.stress.axial_stress`` and
``drilling_riser.section.cross_sectional_area``.
No combined-stress check is inferred from nominal force divided by steel area.
"""

from dataclasses import dataclass, fields
import math
from numbers import Real


@dataclass(frozen=True)
class BuoyancySection:
    """Required SI geometry; insulation thickness is radial, not diametral."""

    pipe_outer_diameter_m: float
    insulation_thickness_m: float
    buoyancy_outer_diameter_m: float
    buoyancy_length_m: float

    def __post_init__(self) -> None:
        for field in fields(self):
            original = getattr(self, field.name)
            try:
                value = float(original) if isinstance(original, Real) else math.nan
                valid = (
                    not isinstance(original, bool)
                    and math.isfinite(value)
                    and (value != 0 or original == 0)
                )
            except OverflowError:
                valid = False
            if not valid:
                raise ValueError(f"{field.name} must be a finite representable real number")
            if field.name == "insulation_thickness_m":
                if value < 0:
                    raise ValueError(f"{field.name} must be nonnegative")
            elif value <= 0:
                raise ValueError(f"{field.name} must be positive")
            object.__setattr__(self, field.name, float(value))
        coated = self.pipe_outer_diameter_m + 2*self.insulation_thickness_m
        if not math.isfinite(coated) or coated > self.buoyancy_outer_diameter_m:
            raise ValueError("coated pipe diameter must not exceed buoyancy diameter")


@dataclass(frozen=True)
class TangentialProperties:
    """Dimensional terms and dimensionless ratios; no acceptance verdict."""

    length_to_diameter_ratio: float
    numerator_m3: float
    denominator_m3: float
    added_mass_coefficient: float


def buoyancy_tangential_properties(section: BuoyancySection) -> TangentialProperties:
    """Compute N, Q, L/Db and Ca=N/Q using the module's stated geometry.

    Raise ValueError for intermediate overflow or underflow, rather than return
    a nonfinite coefficient or a spurious zero for a positive diameter gap.
    """
    coated_m = section.pipe_outer_diameter_m + 2*section.insulation_thickness_m
    diameter_m = section.buoyancy_outer_diameter_m
    gap_m = diameter_m - coated_m
    try:
        numerator_m3 = diameter_m * gap_m**2 / 6
        denominator_m3 = (diameter_m**2 + 2*coated_m**2) * section.buoyancy_length_m / 4
        length_ratio = section.buoyancy_length_m / diameter_m
        if denominator_m3 <= 0 or (gap_m > 0 and numerator_m3 == 0):
            raise ValueError("geometry terms must be representable in SI units")
        coefficient = numerator_m3 / denominator_m3
    except (OverflowError, ZeroDivisionError) as exc:
        raise ValueError("geometry terms must be representable in SI units") from exc
    values = (length_ratio, numerator_m3, denominator_m3, coefficient)
    if (
        not all(math.isfinite(value) for value in values)
        or length_ratio <= 0
        or (gap_m > 0 and coefficient == 0)
    ):
        raise ValueError("geometry terms must be representable in SI units")
    return TangentialProperties(*values)
