# ABOUTME: CFD-validated analytical model for a prismatic U-tube anti-roll tank.

"""Analytical U-tube anti-roll tank dynamics.

The model is calibrated for conduit form loss and rigid-column inertia.  Its
moment correlation has 4.3 percent RMS error for forcing periods of 20 seconds
and longer.  Below about 16 seconds it degrades as the legs' own longitudinal
(6.25 s) and transverse (2.79 s) sloshing modes begin to participate.

The effective conduit length must be anchored to a measured *natural* period,
never to the amplitude peak of a forced sweep.  The peak is loss-controlled and
moves with roll amplitude, whereas the natural period does not.  Using the peak
for the reference calibration inflates the effective length by 2.76 times.

Conduit loss scales approximately with the inverse square of area, not with
``1 / area``.  The latter follows from a naive derivation but fails in opposite
directions on either side of the calibration area.
"""

from __future__ import annotations

from dataclasses import dataclass
import math


DEFAULT_EFFECTIVE_CONDUIT_LENGTH = 5.2004
DEFAULT_LOSS_COEFFICIENT = 2.3894
DEFAULT_AREA_EXPONENT = 2.138
CALIBRATED_AREA_EXPONENT_RMS_PERCENT = 8.6
LINEAR_AREA_EXPONENT_RMS_PERCENT = 36.5
DEFAULT_GRAVITY = 9.81
DEFAULT_DENSITY = 1025.0


@dataclass(frozen=True)
class UTubeGeometry:
    """Prismatic tank geometry, in metres.

    ``centroid_offset`` is the transverse distance from the vessel centreline
    to either leg centroid.  Each leg has plan area
    ``leg_length * leg_width``.  The bottom conduit has rectangular section
    ``conduit_area`` and joins the leg centroids.
    """

    leg_length: float
    leg_width: float
    fill_depth: float
    centroid_offset: float
    effective_conduit_length: float = DEFAULT_EFFECTIVE_CONDUIT_LENGTH

    def __post_init__(self) -> None:
        for name, value in (
            ("leg_length", self.leg_length),
            ("leg_width", self.leg_width),
            ("fill_depth", self.fill_depth),
            ("centroid_offset", self.centroid_offset),
            ("effective_conduit_length", self.effective_conduit_length),
        ):
            if value <= 0.0:
                raise ValueError(f"{name} must be positive")

    @property
    def leg_area(self) -> float:
        return self.leg_length * self.leg_width


@dataclass(frozen=True)
class TankMoment:
    """Tank moment phasor and transferred-volume response."""

    in_phase: float
    quadrature: float
    q_in_phase: float
    q_quadrature: float

    @property
    def magnitude(self) -> float:
        return math.hypot(self.in_phase, self.quadrature)


@dataclass(frozen=True)
class ConduitOptimum:
    """Result of a bounded conduit-area design search."""

    conduit_area: float
    tank_natural_period: float
    peak_roll_reduction: float


def _positive(name: str, value: float) -> None:
    if value <= 0.0:
        raise ValueError(f"{name} must be positive")


def natural_period(
    *,
    leg_area: float,
    conduit_area: float,
    fill_depth: float,
    effective_conduit_length: float = DEFAULT_EFFECTIVE_CONDUIT_LENGTH,
    gravity: float = DEFAULT_GRAVITY,
) -> float:
    """Return the rigid-column natural period."""

    for name, value in (
        ("leg_area", leg_area),
        ("conduit_area", conduit_area),
        ("fill_depth", fill_depth),
        ("effective_conduit_length", effective_conduit_length),
        ("gravity", gravity),
    ):
        _positive(name, value)
    inertia_length = (
        effective_conduit_length / conduit_area + 2.0 * fill_depth / leg_area
    )
    omega_n = math.sqrt((2.0 * gravity / leg_area) / inertia_length)
    return 2.0 * math.pi / omega_n


def calibrate_effective_conduit_length(
    *,
    measured_period: float,
    leg_area: float,
    conduit_area: float,
    fill_depth: float,
    gravity: float = DEFAULT_GRAVITY,
) -> float:
    """Infer the single conduit-inertia parameter from a natural period."""

    for name, value in (
        ("measured_period", measured_period),
        ("leg_area", leg_area),
        ("conduit_area", conduit_area),
        ("fill_depth", fill_depth),
        ("gravity", gravity),
    ):
        _positive(name, value)
    omega_n = 2.0 * math.pi / measured_period
    inertia_length = (2.0 * gravity / leg_area) / omega_n**2
    effective_length = conduit_area * (inertia_length - 2.0 * fill_depth / leg_area)
    if effective_length <= 0.0:
        raise ValueError("measured period implies a non-positive effective length")
    return effective_length


def equivalent_damping(
    *,
    geometry: UTubeGeometry,
    conduit_area: float,
    forcing_period: float,
    level_difference_amplitude: float,
    loss_coefficient: float = DEFAULT_LOSS_COEFFICIENT,
    area_exponent: float = DEFAULT_AREA_EXPONENT,
    gravity: float = DEFAULT_GRAVITY,
) -> float:
    """Return harmonic-balance equivalent damping for quadratic form loss."""

    for name, value in (
        ("conduit_area", conduit_area),
        ("forcing_period", forcing_period),
        ("loss_coefficient", loss_coefficient),
        ("area_exponent", area_exponent),
        ("gravity", gravity),
    ):
        _positive(name, value)
    if level_difference_amplitude < 0.0:
        raise ValueError("level_difference_amplitude must be non-negative")

    leg_area = geometry.leg_area
    inertia_length = (
        geometry.effective_conduit_length / conduit_area
        + 2.0 * geometry.fill_depth / leg_area
    )
    omega_n = math.sqrt((2.0 * gravity / leg_area) / inertia_length)
    omega = 2.0 * math.pi / forcing_period
    q_amplitude = leg_area * level_difference_amplitude / 2.0
    return (
        (4.0 / (3.0 * math.pi))
        * loss_coefficient
        * omega
        * q_amplitude
        / (2.0 * conduit_area**area_exponent * inertia_length * omega_n)
    )


def _forced_volume_response(
    *,
    geometry: UTubeGeometry,
    conduit_area: float,
    forcing_period: float,
    roll_amplitude: float,
    loss_coefficient: float,
    area_exponent: float,
    gravity: float,
) -> tuple[float, float]:
    """Solve the harmonic-balance transferred-volume response."""

    leg_area = geometry.leg_area
    inertia_length = (
        geometry.effective_conduit_length / conduit_area
        + 2.0 * geometry.fill_depth / leg_area
    )
    omega_n = math.sqrt((2.0 * gravity / leg_area) / inertia_length)
    omega = 2.0 * math.pi / forcing_period
    gravity_forcing = (
        2.0
        * gravity
        * geometry.centroid_offset
        * math.tan(roll_amplitude)
        / inertia_length
    )
    quadrature_forcing = (
        2.0
        * gravity
        * geometry.centroid_offset
        * math.sin(roll_amplitude)
        / inertia_length
    )
    inertia_forcing = (
        2.0
        * (
            geometry.fill_depth
            + geometry.effective_conduit_length
            + conduit_area / (2.0 * geometry.leg_length)
        )
        * geometry.centroid_offset
        * omega**2
        * roll_amplitude
        / inertia_length
    )
    forcing = gravity_forcing + inertia_forcing
    detuning = omega_n**2 - omega**2

    q_amplitude = abs(forcing) / max(abs(detuning), 1.0e-12)
    for _ in range(100):
        level_amplitude = 2.0 * q_amplitude / leg_area
        zeta = equivalent_damping(
            geometry=geometry,
            conduit_area=conduit_area,
            forcing_period=forcing_period,
            level_difference_amplitude=level_amplitude,
            loss_coefficient=loss_coefficient,
            area_exponent=area_exponent,
            gravity=gravity,
        )
        damping = 2.0 * zeta * omega_n * omega
        updated = abs(forcing) / math.hypot(detuning, damping)
        if math.isclose(updated, q_amplitude, rel_tol=1.0e-10, abs_tol=1.0e-12):
            q_amplitude = updated
            break
        q_amplitude = 0.5 * (q_amplitude + updated)

    level_amplitude = 2.0 * q_amplitude / leg_area
    zeta = equivalent_damping(
        geometry=geometry,
        conduit_area=conduit_area,
        forcing_period=forcing_period,
        level_difference_amplitude=level_amplitude,
        loss_coefficient=loss_coefficient,
        area_exponent=area_exponent,
        gravity=gravity,
    )
    damping = 2.0 * zeta * omega_n * omega
    denominator = detuning**2 + damping**2
    return (
        forcing * detuning / denominator,
        -quadrature_forcing * damping / denominator,
    )


def _prismatic_properties(
    geometry: UTubeGeometry, conduit_area: float
) -> tuple[float, float, float, float]:
    """Return volume, vertical centroid, frozen volume inertia, and surface inertia."""

    length = geometry.leg_length
    width = geometry.leg_width
    depth = geometry.fill_depth
    offset = geometry.centroid_offset
    leg_area = geometry.leg_area
    conduit_height = conduit_area / length
    conduit_span = 2.0 * offset

    leg_volume = 2.0 * leg_area * depth
    conduit_volume = conduit_span * conduit_area
    volume = leg_volume + conduit_volume
    leg_first_z = 2.0 * leg_area * depth * (conduit_height + depth / 2.0)
    conduit_first_z = conduit_volume * conduit_height / 2.0
    vertical_centroid = (leg_first_z + conduit_first_z) / volume

    leg_y2 = 2.0 * length * depth * (width**3 / 12.0 + width * offset**2)
    conduit_y2 = conduit_area * conduit_span**3 / 12.0
    leg_z2 = 2.0 * leg_area * ((conduit_height + depth) ** 3 - conduit_height**3) / 3.0
    conduit_z2 = conduit_span * length * conduit_height**3 / 3.0
    frozen_volume_inertia = leg_y2 + conduit_y2 + leg_z2 + conduit_z2
    free_surface_inertia = 2.0 * length * width**3 / 12.0
    return volume, vertical_centroid, frozen_volume_inertia, free_surface_inertia


def tank_moment(
    *,
    geometry: UTubeGeometry,
    conduit_area: float,
    forcing_period: float,
    roll_amplitude: float,
    density: float = DEFAULT_DENSITY,
    gravity: float = DEFAULT_GRAVITY,
    loss_coefficient: float = DEFAULT_LOSS_COEFFICIENT,
    area_exponent: float = DEFAULT_AREA_EXPONENT,
) -> TankMoment:
    """Return the tank-on-vessel moment phasor for sinusoidal roll."""

    for name, value in (
        ("conduit_area", conduit_area),
        ("forcing_period", forcing_period),
        ("density", density),
        ("gravity", gravity),
        ("loss_coefficient", loss_coefficient),
        ("area_exponent", area_exponent),
    ):
        _positive(name, value)
    if roll_amplitude < 0.0:
        raise ValueError("roll_amplitude must be non-negative")

    q_s, q_c = _forced_volume_response(
        geometry=geometry,
        conduit_area=conduit_area,
        forcing_period=forcing_period,
        roll_amplitude=roll_amplitude,
        loss_coefficient=loss_coefficient,
        area_exponent=area_exponent,
        gravity=gravity,
    )
    volume, z_c, frozen_volume_inertia, free_surface_inertia = _prismatic_properties(
        geometry, conduit_area
    )
    omega = 2.0 * math.pi / forcing_period
    in_phase = (
        (density * volume * gravity * z_c + density * gravity * free_surface_inertia)
        * math.sin(roll_amplitude)
        - density * frozen_volume_inertia * omega**2 * roll_amplitude
        + 2.0 * density * gravity * geometry.centroid_offset * q_s
    )
    quadrature = 2.0 * density * gravity * geometry.centroid_offset * q_c
    return TankMoment(in_phase, quadrature, q_s, q_c)


def _quasi_static_moment_per_radian(
    geometry: UTubeGeometry,
    conduit_area: float,
    density: float,
    gravity: float,
) -> float:
    volume, z_c, _, free_surface_inertia = _prismatic_properties(geometry, conduit_area)
    fixed = density * gravity * (volume * z_c + free_surface_inertia)
    redistribution = (
        density * gravity * geometry.leg_area * geometry.centroid_offset**2 * 2.0
    )
    return fixed + redistribution


def coupled_roll_response(
    *,
    forcing_period: float,
    vessel_roll_period: float,
    hull_damping_ratio: float,
    tank_authority: float,
    geometry: UTubeGeometry,
    conduit_area: float,
    tank_enabled: bool = True,
    density: float = DEFAULT_DENSITY,
    gravity: float = DEFAULT_GRAVITY,
    loss_coefficient: float = DEFAULT_LOSS_COEFFICIENT,
    area_exponent: float = DEFAULT_AREA_EXPONENT,
) -> complex:
    """Return roll per unit wave moment for the normalized coupled system."""

    for name, value in (
        ("forcing_period", forcing_period),
        ("vessel_roll_period", vessel_roll_period),
        ("conduit_area", conduit_area),
        ("density", density),
        ("gravity", gravity),
    ):
        _positive(name, value)
    if hull_damping_ratio < 0.0:
        raise ValueError("hull_damping_ratio must be non-negative")
    if tank_authority < 0.0:
        raise ValueError("tank_authority must be non-negative")

    omega = 2.0 * math.pi / forcing_period
    vessel_omega = 2.0 * math.pi / vessel_roll_period
    vessel_inertia = 1.0 / vessel_omega**2
    hull_damping = 2.0 * hull_damping_ratio / vessel_omega
    bare_denominator = 1.0 - omega**2 * vessel_inertia + 1j * omega * hull_damping
    if not tank_enabled or tank_authority == 0.0:
        return 1.0 / bare_denominator

    # Set the forcing level by a five-degree bare-roll reference.  The returned
    # value remains theta/M_wave; the finite reference is needed only because a
    # quadratic-loss system has no amplitude-independent transfer function.
    wave_moment = max(2.0 * hull_damping_ratio, 1.0e-6) * math.radians(5.0)
    response = wave_moment / bare_denominator
    static_moment = _quasi_static_moment_per_radian(
        geometry, conduit_area, density, gravity
    )
    scale = tank_authority / static_moment
    for _ in range(100):
        amplitude = abs(response)
        moment = tank_moment(
            geometry=geometry,
            conduit_area=conduit_area,
            forcing_period=forcing_period,
            roll_amplitude=amplitude,
            density=density,
            gravity=gravity,
            loss_coefficient=loss_coefficient,
            area_exponent=area_exponent,
        )
        impedance = (
            scale
            * complex(moment.in_phase, moment.quadrature)
            / max(amplitude, 1.0e-15)
        )
        updated = wave_moment / (bare_denominator - impedance)
        if abs(updated - response) <= 1.0e-9 * max(1.0, abs(updated)):
            return updated / wave_moment
        response = 0.5 * (response + updated)
    return response / wave_moment


def optimize_conduit_area(
    *,
    vessel_roll_period: float,
    hull_damping_ratio: float,
    tank_authority: float,
    geometry: UTubeGeometry,
    area_bounds: tuple[float, float],
    samples: int = 121,
) -> ConduitOptimum:
    """Optimize conduit area over a bounded deterministic grid search."""

    lower, upper = area_bounds
    if lower <= 0.0 or upper <= lower:
        raise ValueError("area_bounds must be positive and increasing")
    if samples < 2:
        raise ValueError("samples must be at least two")

    forcing_periods = [
        0.7 * vessel_roll_period + index * (0.6 * vessel_roll_period / 80.0)
        for index in range(81)
    ]
    bare_peak = max(
        abs(
            coupled_roll_response(
                forcing_period=period,
                vessel_roll_period=vessel_roll_period,
                hull_damping_ratio=hull_damping_ratio,
                tank_authority=tank_authority,
                geometry=geometry,
                conduit_area=lower,
                tank_enabled=False,
            )
        )
        for period in forcing_periods
    )

    best_area = lower
    best_peak = math.inf
    for index in range(samples):
        area = lower + index * (upper - lower) / (samples - 1)
        coupled_peak = max(
            abs(
                coupled_roll_response(
                    forcing_period=period,
                    vessel_roll_period=vessel_roll_period,
                    hull_damping_ratio=hull_damping_ratio,
                    tank_authority=tank_authority,
                    geometry=geometry,
                    conduit_area=area,
                )
            )
            for period in forcing_periods
        )
        if coupled_peak < best_peak:
            best_area = area
            best_peak = coupled_peak

    period = natural_period(
        leg_area=geometry.leg_area,
        conduit_area=best_area,
        fill_depth=geometry.fill_depth,
        effective_conduit_length=geometry.effective_conduit_length,
    )
    return ConduitOptimum(best_area, period, 1.0 - best_peak / bare_peak)


__all__ = [
    "CALIBRATED_AREA_EXPONENT_RMS_PERCENT",
    "ConduitOptimum",
    "DEFAULT_AREA_EXPONENT",
    "DEFAULT_EFFECTIVE_CONDUIT_LENGTH",
    "DEFAULT_LOSS_COEFFICIENT",
    "LINEAR_AREA_EXPONENT_RMS_PERCENT",
    "TankMoment",
    "UTubeGeometry",
    "calibrate_effective_conduit_length",
    "coupled_roll_response",
    "equivalent_damping",
    "natural_period",
    "optimize_conduit_area",
    "tank_moment",
]
