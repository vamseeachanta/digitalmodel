# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Public-source mining lift-riser screening, in SI units.

Formula IDs R1 and D1-D9 refer to ``docs/domains/risers/mining-lift-screening.md``.
Inputs describe a fully immersed vertical string, ordered from top to bottom.
These linear reference calculations do not establish strength or operability.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite, pi
from numbers import Integral, Real
from typing import Sequence

import numpy as np
from scipy.linalg import eigh


def _number(
    name: str, value: float, *, minimum: float = 0.0, strict: bool = False
) -> None:
    if isinstance(value, bool) or not isinstance(value, Real) or not isfinite(value):
        raise ValueError(f"{name} must be a finite real number")
    if value < minimum or (strict and value == minimum):
        raise ValueError(f"{name} is outside its permitted range")


def _fraction(name: str, value: float) -> None:
    _number(name, value)
    if value > 1.0:
        raise ValueError(f"{name} must be a fraction between zero and one")


@dataclass(frozen=True)
class SlurryLayer:
    """A piecewise-constant local inventory, with no gas phase (R1 Eq. 6.3).

    The accepted range [0, 1] is algebraic, not a slurry validity range. Callers
    must establish the packing limit for their particle shape and grading and
    remain below it for slurry use (R1, 2016, section 6.2.1, p. 89, after Eq.
    6.3). The c=1 endpoint is mathematical only; packed beds are outside scope.
    """

    length_m: float
    liquid_density_kg_m3: float
    solids_density_kg_m3: float
    solids_volume_fraction: float

    def __post_init__(self) -> None:
        for name in ("length_m", "liquid_density_kg_m3", "solids_density_kg_m3"):
            _number(name, getattr(self, name), strict=True)
        _fraction("solids_volume_fraction", self.solids_volume_fraction)

    @property
    def density_kg_m3(self) -> float:
        """Return volume-weighted density; concentration is local, not delivered."""
        c = self.solids_volume_fraction
        return (1.0 - c) * self.liquid_density_kg_m3 + c * self.solids_density_kg_m3


@dataclass(frozen=True)
class DensityCell:
    """One density cell; coordinates are distance down from the string top."""

    top_m: float
    bottom_m: float
    density_kg_m3: float


def slurry_density_profile(layers: Sequence[SlurryLayer]) -> tuple[DensityCell, ...]:
    """Build a local density snapshot (R1), without interpolation or extrapolation."""
    if not layers:
        raise ValueError("at least one slurry layer is required")
    cells = []
    top = 0.0
    for layer in layers:
        if not isinstance(layer, SlurryLayer):
            raise ValueError("layers must contain SlurryLayer instances")
        bottom = top + layer.length_m
        _number("profile coordinate", bottom, strict=True)
        cells.append(DensityCell(top, bottom, layer.density_kg_m3))
        top = bottom
    return tuple(cells)


@dataclass(frozen=True)
class Tube:
    """Circular tube and contents; axial participation is connection dependent.

    Contents weight always enters D2. ``contents_inertia_fraction`` specifies
    the fraction following axial wall motion in D4; it is an assumption to
    establish separately. A sliding auxiliary adds mass but no axial stiffness.
    """

    outer_diameter_m: float
    wall_thickness_m: float
    material_density_kg_m3: float
    youngs_modulus_pa: float
    contents_density_kg_m3: float
    contents_inertia_fraction: float
    carries_axial_load: bool

    def __post_init__(self) -> None:
        for name in (
            "outer_diameter_m",
            "wall_thickness_m",
            "material_density_kg_m3",
            "youngs_modulus_pa",
        ):
            _number(name, getattr(self, name), strict=True)
        _number("contents_density_kg_m3", self.contents_density_kg_m3)
        _fraction("contents_inertia_fraction", self.contents_inertia_fraction)
        if 2.0 * self.wall_thickness_m >= self.outer_diameter_m:
            raise ValueError("wall thickness must leave a positive bore")
        if not isinstance(self.carries_axial_load, bool):
            raise ValueError("carries_axial_load must be a boolean")

    @property
    def bore_area_m2(self) -> float:
        """Return circular bore area (D1)."""
        return pi * (self.outer_diameter_m - 2.0 * self.wall_thickness_m) ** 2 / 4.0

    @property
    def wall_area_m2(self) -> float:
        """Return annular wall area in its factored form (D1)."""
        return (
            pi * self.wall_thickness_m * (self.outer_diameter_m - self.wall_thickness_m)
        )

    @property
    def displaced_area_m2(self) -> float:
        """Return external displaced volume per unit length (D1)."""
        return pi * self.outer_diameter_m**2 / 4.0

    @property
    def mass_kg_m(self) -> float:
        """Return wall plus complete contents mass per unit length (D2)."""
        return (
            self.wall_area_m2 * self.material_density_kg_m3
            + self.bore_area_m2 * self.contents_density_kg_m3
        )

    @property
    def axial_mass_kg_m(self) -> float:
        """Return wall plus explicitly entrained contents inertia (D4)."""
        return (
            self.wall_area_m2 * self.material_density_kg_m3
            + self.bore_area_m2
            * self.contents_density_kg_m3
            * self.contents_inertia_fraction
        )

    @property
    def axial_stiffness_n(self) -> float:
        """Return EA only when the connection transfers axial load (D1, R2)."""
        return (
            self.youngs_modulus_pa * self.wall_area_m2
            if self.carries_axial_load
            else 0.0
        )


@dataclass(frozen=True)
class BundleSection:
    """Uniform section; tube volumes must not overlap additional displacement.

    Additional mass includes attached hardware and buoyancy material. Additional
    displacement is the displaced volume per length outside the tube envelopes.
    Axial added mass is an explicit inertia input, excluded from static weight.
    """

    length_m: float
    tubes: tuple[Tube, ...]
    external_density_kg_m3: float
    additional_mass_kg_m: float
    additional_displaced_area_m2: float
    axial_added_mass_kg_m: float

    def __post_init__(self) -> None:
        _number("length_m", self.length_m, strict=True)
        for name in (
            "external_density_kg_m3",
            "additional_mass_kg_m",
            "additional_displaced_area_m2",
            "axial_added_mass_kg_m",
        ):
            _number(name, getattr(self, name))
        object.__setattr__(self, "tubes", tuple(self.tubes))
        if not self.tubes or any(not isinstance(t, Tube) for t in self.tubes):
            raise ValueError("at least one Tube is required")
        _number("section axial stiffness", self.axial_stiffness_n, strict=True)
        _number("section axial mass", self.axial_mass_kg_m, strict=True)
        _number("section mass", self.mass_kg_m, strict=True)
        _number("section displacement", self.displaced_area_m2, strict=True)

    @property
    def mass_kg_m(self) -> float:
        """Return complete mass per length for effective weight (D2)."""
        return sum(t.mass_kg_m for t in self.tubes) + self.additional_mass_kg_m

    @property
    def axial_mass_kg_m(self) -> float:
        """Return participating axial inertia per unit length (D4)."""
        return (
            sum(t.axial_mass_kg_m for t in self.tubes)
            + self.additional_mass_kg_m
            + self.axial_added_mass_kg_m
        )

    @property
    def displaced_area_m2(self) -> float:
        """Return summed nonoverlapping displacement (D2)."""
        return (
            sum(t.displaced_area_m2 for t in self.tubes)
            + self.additional_displaced_area_m2
        )

    @property
    def axial_stiffness_n(self) -> float:
        """Return the stiffness of axially connected tubes (D1, R2)."""
        return sum(t.axial_stiffness_n for t in self.tubes)


@dataclass(frozen=True)
class HangingResult:
    """Static effective tensions at boundaries and a linear extension reference."""

    coordinates_m: tuple[float, ...]
    effective_tension_n: tuple[float, ...]
    extension_m: float

    @property
    def has_compression(self) -> bool:
        """Identify any negative effective tension, including internal boundaries."""
        return min(self.effective_tension_n) < 0.0


def free_hanging_screen(
    sections: Sequence[BundleSection], *, gravity_m_s2: float, lower_end_force_n: float
) -> HangingResult:
    """Integrate effective weight upwards from an explicit lower force (D2/D3).

    A positive lower force acts downwards. Zero represents a free lower end.
    Negative tension is returned as a diagnostic, not accepted as a taut string.
    Extension uses effective tension as a pressure-neutral rod reference; it is
    not the actual pressurized wall elongation (see the documented limitations).
    """
    sections = tuple(sections)
    if not sections or any(not isinstance(s, BundleSection) for s in sections):
        raise ValueError("at least one BundleSection is required")
    _number("gravity_m_s2", gravity_m_s2, strict=True)
    if (
        isinstance(lower_end_force_n, bool)
        or not isinstance(lower_end_force_n, Real)
        or not isfinite(lower_end_force_n)
    ):
        raise ValueError("lower_end_force_n must be a finite real number")
    coordinates = [0.0]
    for s in sections:
        coordinates.append(coordinates[-1] + s.length_m)
    tensions = [lower_end_force_n]
    extension = 0.0
    for s in reversed(sections):
        weight = (
            s.mass_kg_m - s.external_density_kg_m3 * s.displaced_area_m2
        ) * gravity_m_s2
        top_force = tensions[-1] + weight * s.length_m
        extension += (
            (top_force + tensions[-1]) * s.length_m / (2.0 * s.axial_stiffness_n)
        )
        tensions.append(top_force)
    if not all(isfinite(v) for v in (*coordinates, *tensions, extension)):
        raise ValueError("static calculation exceeded finite numeric range")
    return HangingResult(tuple(coordinates), tuple(reversed(tensions)), extension)


@dataclass(frozen=True)
class AxialModes:
    """Undamped axial frequencies, periods and max-absolute-normalized shapes."""

    coordinates_m: tuple[float, ...]
    frequencies_hz: tuple[float, ...]
    periods_s: tuple[float, ...]
    mode_shapes: tuple[tuple[float, ...], ...]


def axial_modes(
    sections: Sequence[BundleSection],
    *,
    gravity_m_s2: float,
    lower_end_force_n: float,
    lower_end_mass_kg: float,
    elements_per_section: int,
    mode_count: int,
) -> AxialModes:
    """Solve K phi = omega^2 M phi with consistent rod mass (D4).

    The top displacement is fixed; the bottom has an optional inertial mass and
    no spring. Supply its effective static force separately. Reject compression
    before modal screening. Mesh refinement is the caller's responsibility.

    A positive lower force with zero lower mass represents an externally imposed
    massless force, not a suspended weight. If that force represents a suspended
    assembly, supply its inertial mass separately: omitting it overestimates the
    frequency relative to the same assembly with positive inertia (D6).
    """
    sections = tuple(sections)
    static = free_hanging_screen(
        sections, gravity_m_s2=gravity_m_s2, lower_end_force_n=lower_end_force_n
    )
    if static.has_compression:
        raise ValueError("axial screen requires no effective compression")
    _number("lower_end_mass_kg", lower_end_mass_kg)
    for name, value in (
        ("elements_per_section", elements_per_section),
        ("mode_count", mode_count),
    ):
        if isinstance(value, bool) or not isinstance(value, Integral) or value < 1:
            raise ValueError(f"{name} must be a positive integer")
    element_count = len(sections) * elements_per_section
    if mode_count > element_count:
        raise ValueError("mode_count exceeds the free degrees of freedom")
    stiffness = np.zeros((element_count + 1, element_count + 1))
    mass = np.zeros_like(stiffness)
    coordinates = [0.0]
    node = 0
    for s in sections:
        length = s.length_m / elements_per_section
        k = s.axial_stiffness_n / length * np.array([[1.0, -1.0], [-1.0, 1.0]])
        m = s.axial_mass_kg_m * length / 6.0 * np.array([[2.0, 1.0], [1.0, 2.0]])
        for _ in range(elements_per_section):
            stiffness[node : node + 2, node : node + 2] += k
            mass[node : node + 2, node : node + 2] += m
            coordinates.append(coordinates[-1] + length)
            node += 1
    mass[-1, -1] += lower_end_mass_kg
    values, vectors = eigh(
        stiffness[1:, 1:], mass[1:, 1:], subset_by_index=(0, mode_count - 1)
    )
    if not np.all(np.isfinite(values)) or np.any(values <= 0.0):
        raise ValueError(
            "modal eigenvalues must be finite and positive; check scaling and mesh"
        )
    frequencies = np.sqrt(values) / (2.0 * pi)
    shapes = []
    for column in vectors.T:
        # Normalize at the largest component, also fixing an arbitrary modal sign.
        normalized = column / column[np.argmax(np.abs(column))]
        shapes.append((0.0, *(float(v) for v in normalized)))
    return AxialModes(
        tuple(coordinates),
        tuple(float(f) for f in frequencies),
        tuple(float(1.0 / f) for f in frequencies),
        tuple(shapes),
    )


@dataclass(frozen=True)
class StringWeightInventory:
    """String forces in N, excluding any separately specified lower assembly.

    Dry weight includes attachments; contents weight includes all bore contents.
    Buoyancy is upward-positive. Submerged weight is downward-positive and may
    be negative. Axial added mass is inertial only and is excluded (D2/D8).
    """

    dry_weight_n: float
    contents_weight_n: float
    filled_weight_n: float
    buoyancy_n: float
    submerged_weight_n: float


def string_weight_inventory(
    sections: Sequence[BundleSection], *, gravity_m_s2: float
) -> StringWeightInventory:
    """Resolve D2 into dry, contents, filled and submerged weight (D8).

    Basis: OpenStax University Physics Vol. 1, section 14.4 (R4 in module docs),
    https://openstax.org/books/university-physics-volume-1/pages/14-4-archimedes-principle-and-buoyancy
    and mass inventory. No empirical constants or gravity default are used.
    Top effective tension adds the separate lower effective force to the signed
    submerged inventory; this is not a pressurized wall-tension calculation.
    """
    sections = tuple(sections)
    if not sections or any(not isinstance(s, BundleSection) for s in sections):
        raise ValueError("at least one BundleSection is required")
    _number("gravity_m_s2", gravity_m_s2, strict=True)
    dry = contents = buoyancy = 0.0
    for section in sections:
        length = section.length_m
        dry += (
            length
            * gravity_m_s2
            * (
                sum(t.wall_area_m2 * t.material_density_kg_m3 for t in section.tubes)
                + section.additional_mass_kg_m
            )
        )
        contents += (
            length
            * gravity_m_s2
            * sum(t.bore_area_m2 * t.contents_density_kg_m3 for t in section.tubes)
        )
        buoyancy += (
            length
            * gravity_m_s2
            * section.external_density_kg_m3
            * section.displaced_area_m2
        )
    filled = dry + contents
    submerged = filled - buoyancy
    if not all(isfinite(v) for v in (dry, contents, filled, buoyancy, submerged)):
        raise ValueError("weight inventory exceeded finite numeric range")
    return StringWeightInventory(dry, contents, filled, buoyancy, submerged)


@dataclass(frozen=True)
class WavePeriodScreen:
    """Fundamental axial period and inclusive interval membership, not acceptance."""

    first_axial_period_s: float
    within_wave_period_range: bool


def wave_period_screen(
    *,
    first_axial_frequency_hz: float,
    wave_period_min_s: float,
    wave_period_max_s: float,
) -> WavePeriodScreen:
    """Compare T=1/f with a caller-supplied closed wave-period interval (D9).

    Basis: OpenStax University Physics Vol. 1, section 15.1, equation 15.1 (R5),
    https://openstax.org/books/university-physics-volume-1/pages/15-1-simple-harmonic-motion
    The caller supplies the fundamental frequency, e.g. axial_modes(...).
    frequencies_hz[0], in cycles/s, not rad/s. Equal bounds are permitted.
    No separation margin is invented: false does not establish operability,
    and true does not predict response amplitude. Higher modes are not screened.
    """
    for name, value in (
        ("first_axial_frequency_hz", first_axial_frequency_hz),
        ("wave_period_min_s", wave_period_min_s),
        ("wave_period_max_s", wave_period_max_s),
    ):
        _number(name, value, strict=True)
    if wave_period_min_s > wave_period_max_s:
        raise ValueError("wave period bounds must be ordered minimum to maximum")
    period = 1.0 / first_axial_frequency_hz
    _number("first axial period", period, strict=True)
    return WavePeriodScreen(
        period, bool(wave_period_min_s <= period <= wave_period_max_s)
    )
