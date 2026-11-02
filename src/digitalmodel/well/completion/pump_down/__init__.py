"""Steady pump-down conveyance core; no workflow, transient or envelope layer.

All physical inputs accept quantities from digitalmodel.units; bare scalars
are SI except explicitly named inch/bpm field conveniences. The rate-position
table is an idealized vertical-to-horizontal screen, not a toe qualification.
"""

from digitalmodel.units import Q_

from ._validation import finite, scalar
from .force_balance import cable_stretch, resisting_force, tension_profile
from .geometry import BypassGeometry, api_drift, bypass, runnable
from .kinematics import solve_line_speed
from .restriction import restriction

__all__ = ["BypassGeometry", "api_drift", "bypass", "runnable", "rate_position_table"]

# Public basis references accompany table results. None is a field calibration.
PUBLIC_BASIS = (
    {
        "method": "continuity and minor loss",
        "source": "DOE-HDBK-1012/3-92, Fluid Flow",
        "url": "https://www.energy.gov/ehss/articles/doe-hdbk-10123-92",
    },
    {
        "method": "capstan",
        "source": "Euler-Eytelwein; Harvard Natural Sciences, Friction",
        "url": "https://sciencedemonstrations.fas.harvard.edu/presentations/friction",
    },
    {
        "method": "restriction effective area and contact coefficients",
        "source": "task model assumptions; K, mu and e require field calibration",
    },
)


def rate_position_table(
    *,
    rates,
    lateral_positions,
    geometry: BypassGeometry,
    density,
    buoyed_bha_weight,
    buoyed_cable_weight,
    vertical_length,
    cable_axial_stiffness,
    weak_point,
    mu=0.30,
    eccentricity=0.12,
    k=2.0,
) -> list[dict]:
    """Return one quantity-valued row per rate and horizontal cable position.

    Idealization: vertical cable of the stated ACTUAL length, a massless
    right-angle heel, then a straight 90-degree lateral. No surface brake;
    cable-head tension equals mu*(1+e)*wc*L, heel tension is zero, and vertical
    tension comes from buoyed cable weight. The 90-degree capstan therefore
    acts on zero heel tension; actual build-section effects require a full
    tension_profile with a specified bottom boundary. This does not model
    curved sections using only TVD or substitute TVD for measured cable length.

    Resistance is the task's horizontal lumped balance (force_balance.py).
    Moving dp/Fh use the SOLVED speed; static force is a separate capacity.
    Weak-point margin is a conservative cable-wide comparison to MAXIMUM
    profile tension, not a claim about actual weak-point-location loading.
    Check the cable certificate separately. The stalled row's tension/stretch
    are the incipient-payout reference profile, not a static-friction solution.
    Geometry clearance must be checked separately with runnable(). Coefficients
    are representative task assumptions, not certified values (PUBLIC_BASIS).
    """
    vertical = scalar(vertical_length, "m", "vertical measured cable length")
    wc = scalar(buoyed_cable_weight, "N/m", "buoyed cable weight")
    scalar(cable_axial_stiffness, "N", "cable axial stiffness", positive=True)
    weak = Q_(scalar(weak_point, "N", "weak-point limit", positive=True), "N")
    friction = finite(mu, "friction coefficient") * (
        1 + finite(eccentricity, "eccentricity")
    )
    positions = [scalar(p, "m", "lateral position") for p in lateral_positions]
    rates = list(rates)
    if not rates or not positions:
        raise ValueError("rates and lateral positions must not be empty")
    rows = []
    for rate in rates:
        for position in positions:
            load = resisting_force(
                buoyed_bha_weight,
                buoyed_cable_weight,
                Q_(position, "m"),
                90,
                mu,
                eccentricity,
            )
            speed = solve_line_speed(rate, geometry, density, load, k)
            hydraulic = restriction(rate, speed.line_speed, geometry, density, k)
            static = restriction(rate, Q_(0, "m/s"), geometry, density, k)
            cable_head = Q_(friction * wc * position, "N")
            # Separate straight surveys meet at a zero-tension, massless heel.
            profiles = []
            for length, inclination, boundary in [
                (vertical, 0, Q_(0, "N")),
                (position, 90, cable_head),
            ]:
                if length > 0:
                    stations = [
                        dict(
                            measured_depth_m=md,
                            inclination_deg=inclination,
                            azimuth_deg=0,
                        )
                        for md in (0, length)
                    ]
                    profiles.append(
                        tension_profile(
                            stations, boundary, buoyed_cable_weight, mu, eccentricity
                        )
                    )
            surface = Q_(wc * vertical, "N")
            maximum = max(
                [surface, cable_head]
                + [p.tension for profile in profiles for p in profile]
            )
            stretch = sum(
                (cable_stretch(profile, cable_axial_stiffness) for profile in profiles),
                Q_(0, "m"),
            )
            rows.append(
                {
                    "rate": Q_(scalar(rate, "m**3/s", "rate"), "m**3/s"),
                    "lateral_position": Q_(position, "m"),
                    "beta": geometry.beta,
                    "bypass_velocity": hydraulic.bypass_velocity,
                    "pressure_drop": hydraulic.pressure_drop,
                    "driving_force": hydraulic.driving_force,
                    "static_driving_force": static.driving_force,
                    "resisting_force": load,
                    "line_speed": speed.line_speed,
                    "slip_fraction": speed.slip_fraction,
                    "stall": speed.stall,
                    "surface_tension": surface,
                    "cable_head_tension": cable_head,
                    "max_tension": maximum,
                    "weak_point_margin": weak - maximum,
                    "stretch": stretch,
                    "citations": PUBLIC_BASIS,
                    "tension_basis": "incipient payout; zero heel tension; idealized vertical/lateral",
                }
            )
    return rows
