"""Through-wall pipe stress for a stipulated net-pressure reference model.

The source method labels the thick-wall equations ``API 6A/17D``;
clause not stated in source. It also labels comparison blocks ``ASME Pipeline
Codes (B31.4/B31.8)`` and ``API RP 1111``; clause not stated in source for each.
No edition or code qualification is established by these labels.

Inputs are radii (m), net pressure (Pa), and independently calculated mechanical
stress components (Pa). Use existing PipeGeometry/PipeStressAnalyzer for section
properties, force/area and outer-fibre bending stress. For a circular annulus,
torsional outer stress is the existing bending response to half the torque
because polar inertia is twice the bending inertia. Lateral shear input is the
section-average force/area used by the source method; the thin-tube peak at the
neutral axis is about twice this value.
Convert units before constructing inputs through the existing boundary helpers;
this module accepts SI only and supplies no unit-conversion constants.

For inner radius a, outer radius b, evaluation radius r and pressure radius c:
radial = -p*a²*(b²/r²-1)/(b²-a²),
hoop = p*a²*(b²/r²+1)/(b²-a²), axial pressure = p*c²/(b²-a²).
Mechanical bending and torsion scale from their outer-fibre values by r/b.
Axial stress on the two sides is pressure thrust + force/area +/- bending.
Existing calculate_vm_stress combines the three normals and two independent
shears. Normal pair differences exclude shear and are NOT a Tresca criterion.
Arithmetic averages give equal weight to every sampled radius, including ends;
equivalent stress of mean components differs from mean equivalent stress.

Limits / Input gaps
-------------------
Net pressure must be supplied on one datum. Axial-force stress excludes pressure
thrust. Pressure radius may differ from strength radius or be zero to suppress
only pressure thrust. This is the stipulated net-pressure model, without any
claim of equivalence to a general absolute-pressure stress tensor. Absolute
internal/external pressure end loading, end restraint and open/closed-end
conditions require a separate model. The source method treats torsion and lateral
shear as independent shear components. Their actual orientation must be
established for an application.
The arithmetic average is neither integration nor code stress linearization.

Code editions/clauses, material properties, hydrostatic gradient, wall-loss and
cladding treatment, design/joint/temperature factors, stress allowables and
pressure-schedule multipliers are unresolved inputs. No defaults or acceptance
verdicts are supplied. Thin-wall/code-factor comparisons and pressure schedules
remain outside this module; existing burst functions are reused separately.
Elastic stress only: no plasticity, collapse, propagation or fatigue assessment.
"""
from dataclasses import dataclass, fields
import math
import numbers
from typing import Iterable

from .vm_stress import calculate_vm_stress


def _finite(name: str, value: float) -> None:
    if not (isinstance(value, numbers.Real) and not isinstance(value, bool)):
        raise ValueError(f'{name} must be a finite number')
    try:
        valid = math.isfinite(value)
    except OverflowError:
        valid = False
    if not valid:
        raise ValueError(f'{name} must be a finite number')


@dataclass(frozen=True)
class PipeWallStressInput:
    """SI geometry and signed mechanical stresses, excluding pressure thrust.

    ``pressure_radius_m=0`` suppresses axial pressure stress, not hoop or radial.
    Every physical input is required; no material or design factor is inferred.
    """
    outer_radius_m: float
    inner_radius_m: float
    pressure_radius_m: float
    net_pressure_pa: float
    axial_force_stress_pa: float
    bending_outer_stress_pa: float
    torsion_outer_stress_pa: float
    lateral_shear_stress_pa: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _finite(field.name, getattr(self, field.name))
        if not 0 < self.inner_radius_m < self.outer_radius_m:
            raise ValueError('radii must satisfy 0 < inner < outer')
        if not 0 <= self.pressure_radius_m <= self.outer_radius_m:
            raise ValueError('pressure radius must lie between zero and outer radius')


@dataclass(frozen=True)
class PressureFactors:
    """Dimensionless multipliers; radial stress uses the negative multiplier."""
    radius_m: float
    radial_multiplier: float
    hoop_multiplier: float
    axial_multiplier: float


@dataclass(frozen=True)
class WallStress:
    """Signed components at one radius, or their arithmetic component mean.

    Torsional and lateral shear are combined as two independent shear components,
    reproducing the source method's convention. In a circular tube both act as
    tau_theta_z and add algebraically where they coincide; independent combination
    can understate equivalent stress.
    """
    radius_m: float
    radial_pa: float
    hoop_pa: float
    axial_pressure_pa: float
    axial_force_pa: float
    bending_pa: float
    torsional_shear_pa: float
    lateral_shear_pa: float

    def __post_init__(self) -> None:
        for field in fields(self):
            _finite(field.name, getattr(self, field.name))

    @property
    def axial_plus_pa(self) -> float:
        return self.axial_pressure_pa + self.axial_force_pa + self.bending_pa

    @property
    def axial_minus_pa(self) -> float:
        return self.axial_pressure_pa + self.axial_force_pa - self.bending_pa

    @property
    def von_mises_plus_pa(self) -> float:
        return calculate_vm_stress(self.radial_pa, self.hoop_pa, self.axial_plus_pa,
                                   self.torsional_shear_pa, self.lateral_shear_pa)

    @property
    def von_mises_minus_pa(self) -> float:
        return calculate_vm_stress(self.radial_pa, self.hoop_pa, self.axial_minus_pa,
                                   self.torsional_shear_pa, self.lateral_shear_pa)

    @property
    def normal_half_differences_plus_pa(self) -> tuple[float, float, float]:
        """Signed normal-component half-differences; excludes shear."""
        return ((self.radial_pa-self.hoop_pa)/2,
                (self.hoop_pa-self.axial_plus_pa)/2,
                (self.axial_plus_pa-self.radial_pa)/2)

    @property
    def normal_half_differences_minus_pa(self) -> tuple[float, float, float]:
        """Signed normal-component half-differences on the other bending side."""
        return ((self.radial_pa-self.hoop_pa)/2,
                (self.hoop_pa-self.axial_minus_pa)/2,
                (self.axial_minus_pa-self.radial_pa)/2)

    @property
    def normal_differences_plus_pa(self) -> tuple[float, float, float]:
        return tuple(abs(2*x) for x in self.normal_half_differences_plus_pa)

    @property
    def normal_differences_minus_pa(self) -> tuple[float, float, float]:
        return tuple(abs(2*x) for x in self.normal_half_differences_minus_pa)


def pipe_pressure_factors(inp: PipeWallStressInput, fraction: float) -> PressureFactors:
    """Evaluate the pressure multipliers at fraction [0, 1] through the wall.

    API 6A/17D: clause not stated in source. Ratios avoid squared dimensional
    lengths and cancellation in the annular denominator where possible.
    """
    _finite('fraction', fraction)
    if not 0 <= fraction <= 1:
        raise ValueError('fraction must lie between zero and one')
    radius = inp.inner_radius_m + (inp.outer_radius_m-inp.inner_radius_m)*fraction
    q = inp.inner_radius_m/inp.outer_radius_m
    denominator = ((inp.outer_radius_m-inp.inner_radius_m)/inp.outer_radius_m)*(1+q)
    if denominator == 0:
        raise ValueError('annular radii cannot be resolved at this precision')
    inner_ratio = inp.inner_radius_m/radius
    radius_ratio = radius/inp.outer_radius_m
    radial = (inner_ratio**2 * ((inp.outer_radius_m-radius)/inp.outer_radius_m)
              * (1+radius_ratio)/denominator)
    hoop = inner_ratio**2*(1+radius_ratio**2)/denominator
    axial = (inp.pressure_radius_m/inp.outer_radius_m)**2/denominator
    for value in (radial, hoop, axial):
        _finite('pressure multiplier', value)
    return PressureFactors(radius, radial, hoop, axial)


def pipe_wall_stress_at_fraction(inp: PipeWallStressInput, fraction: float) -> WallStress:
    """Combine pressure and mechanical stresses at one through-wall fraction."""
    factors = pipe_pressure_factors(inp, fraction)
    scale = factors.radius_m/inp.outer_radius_m
    return WallStress(
        factors.radius_m,
        -inp.net_pressure_pa*factors.radial_multiplier,
        inp.net_pressure_pa*factors.hoop_multiplier,
        inp.net_pressure_pa*factors.axial_multiplier,
        inp.axial_force_stress_pa,
        inp.bending_outer_stress_pa*scale,
        inp.torsion_outer_stress_pa*scale,
        inp.lateral_shear_stress_pa,
    )


def sample_pipe_wall_stress(inp: PipeWallStressInput, intervals: int) -> tuple[WallStress, ...]:
    """Sample equal radial intervals, including both endpoints; count is explicit."""
    if isinstance(intervals, bool) or not isinstance(intervals, int) or intervals < 1:
        raise ValueError('intervals must be a positive integer')
    return tuple(pipe_wall_stress_at_fraction(inp, i/intervals) for i in range(intervals+1))


def mean_wall_stress(points: Iterable[WallStress]) -> WallStress:
    """Average signed components before combining or taking absolute differences.

    This equal-weight sample average does not establish code membrane stress.
    """
    samples = tuple(points)
    if not samples:
        raise ValueError('at least one stress sample is required')
    return WallStress(**{
        field.name: sum(getattr(point, field.name) for point in samples)/len(samples)
        for field in fields(WallStress)
    })
