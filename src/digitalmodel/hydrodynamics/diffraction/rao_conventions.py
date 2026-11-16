"""Solver-neutral RAO convention calculations from a legacy engineering workbook.

Internal angular quantities use radians. No named engineering standard or clause
is stated in the source; the formulas implement its explicit convention equations.
See ``docs/rao_conventions.md`` for assumptions and input gaps.
"""
from __future__ import annotations

import math
import sys
from dataclasses import dataclass
from numbers import Real

from .diffraction_units import (
    degrees_to_radians,
    radians_to_degrees,
    rad_per_s_to_hz,
    rad_per_s_to_period_s,
)

Three = tuple[float, float, float]
Six = tuple[float, float, float, float, float, float]


def _finite_real(value: float, label: str, *, positive: bool = False) -> float:
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f'{label} must be a finite real number')
    try:
        finite = math.isfinite(value)
    except OverflowError as exc:
        raise ValueError(f'{label} must be representable as a finite real number') from exc
    if not finite:
        raise ValueError(f'{label} must be a finite real number')
    if positive and value <= 0.0:
        raise ValueError(f'{label} must be positive')
    return float(value)


def _finite_tuple(values, length: int, label: str, *, nonnegative: bool = False) -> tuple:
    try:
        result = tuple(_finite_real(x, label) for x in values)
    except TypeError as exc:
        raise ValueError(f'{label} must be a numeric sequence') from exc
    if len(result) != length:
        raise ValueError(f'{label} must have {length} components')
    if nonnegative and any(x < 0.0 for x in result):
        raise ValueError(f'{label} amplitudes must be nonnegative')
    return result


def _boolean(value: bool, label: str) -> None:
    if not isinstance(value, bool):
        raise ValueError(f'{label} must be a boolean')


def _export_degrees(value_rad: float) -> float:
    if abs(value_rad) > sys.float_info.max / (180.0 / math.pi):
        raise ValueError('angle is not representable in degrees')
    return float(radians_to_degrees(value_rad))


@dataclass(frozen=True)
class RAOInput:
    """One normalized sample; translations and rotations each follow x/y/z.

    Phases follow surge/sway/heave/roll/pitch/yaw. Wave normalization is per
    unit wave amplitude, not wave height or wave slope. Rotations use rad/m.
    """

    angular_frequency_rad_s: float
    translation_m_per_m: Three
    rotation_rad_per_m: Three
    phase_rad: Six

    def __post_init__(self) -> None:
        object.__setattr__(self, 'angular_frequency_rad_s', _finite_real(
            self.angular_frequency_rad_s, 'angular_frequency_rad_s', positive=True,
        ))
        for name, length, nonnegative in (
            ('translation_m_per_m', 3, True), ('rotation_rad_per_m', 3, True),
            ('phase_rad', 6, False),
        ):
            object.__setattr__(self, name, _finite_tuple(
                getattr(self, name), length, name, nonnegative=nonnegative,
            ))

    @classmethod
    def from_degrees(
        cls, *, angular_frequency_rad_s: float,
        translation_m_per_m: Three, rotation_deg_per_m: Three, phase_deg: Six,
    ) -> RAOInput:
        """Convert deg/m and deg by pi/180 at the boundary; no source clause."""
        rotation_deg_per_m = _finite_tuple(
            rotation_deg_per_m, 3, 'rotation_deg_per_m', nonnegative=True,
        )
        phase_deg = _finite_tuple(phase_deg, 6, 'phase_deg')
        return cls(
            angular_frequency_rad_s,
            translation_m_per_m,
            tuple(float(degrees_to_radians(x)) for x in rotation_deg_per_m),
            tuple(float(degrees_to_radians(x)) for x in phase_deg),
        )


@dataclass(frozen=True)
class RAOExport:
    """Degree boundary representation; component order is unchanged."""

    frequency_hz: float
    period_s: float
    translation_m_per_m: Three
    rotation_deg_per_m: Three
    phase_deg: Six


def wave_frequency_hz(angular_frequency_rad_s: float) -> float:
    """Compute f = omega / (2*pi), in Hz; source states no standard or clause."""
    omega_rad_s = _finite_real(angular_frequency_rad_s, 'angular_frequency_rad_s', positive=True)
    return _finite_real(float(rad_per_s_to_hz(omega_rad_s)), 'frequency_hz', positive=True)


def wave_period_s(angular_frequency_rad_s: float) -> float:
    """Compute T = 2*pi / omega, in s; source states no standard or clause."""
    omega_rad_s = _finite_real(angular_frequency_rad_s, 'angular_frequency_rad_s', positive=True)
    return _finite_real(float(rad_per_s_to_period_s(omega_rad_s)), 'period_s', positive=True)


def lag_to_lead_phase_rad(phase_lag_rad: float, *, reverse_axis: bool = False) -> float:
    """Compute lead = -lag + pi if reversed, otherwise -lag; do not wrap.

    Source convention: x = a R cos(omega t - lag) becomes
    x = a R cos(omega t + lead). Reversing an axis negates x. The source's
    reflection equation chooses +pi. No standard or clause is stated.
    See ``docs/rao_conventions.md`` under "Related code" for the degree helper.
    """
    phase_lag_rad = _finite_real(phase_lag_rad, 'phase_lag_rad')
    _boolean(reverse_axis, 'reverse_axis')
    return -phase_lag_rad + (math.pi if reverse_axis else 0.0)


def convert_lag_rao_to_lead(
    sample: RAOInput, *, mirrored_about_x: bool = False,
) -> RAOInput:
    """Negate six lag phases; add pi for mirrored sway, roll and yaw only.

    Magnitudes and angular frequency are unchanged. Reflection assumes a
    vessel symmetric about its longitudinal x axis; this is an xz-plane
    reflection, not a coordinate-system rotation. No standard clause is stated.
    """
    _boolean(mirrored_about_x, 'mirrored_about_x')
    phases = tuple(
        lag_to_lead_phase_rad(p, reverse_axis=mirrored_about_x and i in (1, 3, 5))
        for i, p in enumerate(sample.phase_rad)
    )
    return RAOInput(
        sample.angular_frequency_rad_s, sample.translation_m_per_m,
        sample.rotation_rad_per_m, phases,
    )


def export_rao_degrees(sample: RAOInput) -> RAOExport:
    """Convert rad/m and rad by 180/pi; f=omega/(2*pi), T=2*pi/omega.

    Natural component order is preserved; exporters may choose their own column
    order. No standard or clause is stated in the source.
    """
    return RAOExport(
        wave_frequency_hz(sample.angular_frequency_rad_s),
        wave_period_s(sample.angular_frequency_rad_s),
        sample.translation_m_per_m,
        tuple(_export_degrees(x) for x in sample.rotation_rad_per_m),
        tuple(_export_degrees(x) for x in sample.phase_rad),
    )


def opposite_axis_heading_rad(source_heading_rad: float) -> float:
    """Compute heading = source - pi, in (-pi, pi], for source in [0, 2*pi].

    At source zero use +pi, equivalent to the equation's -pi endpoint. Both
    axes measure angles in the same rotational sense. No standard clause is
    stated in the source; this is not a general rotation/heading transformation.
    See ``docs/rao_conventions.md`` under "Related code" for the degree helper.
    """
    source_heading_rad = _finite_real(source_heading_rad, 'source_heading_rad')
    if not 0.0 <= source_heading_rad <= 2.0 * math.pi:
        raise ValueError('source_heading_rad must be finite and in [0, 2*pi]')
    heading_rad = source_heading_rad - math.pi
    return math.pi if heading_rad == -math.pi else heading_rad
