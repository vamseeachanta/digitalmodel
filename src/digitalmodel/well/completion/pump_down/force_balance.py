"""Quasistatic gravity, contact friction, capstan tension and cable stretch.

The lumped task balance follows axial/normal resolution of buoyed weight and
Coulomb friction (OpenStax University Physics vol. 1, sections 6.2 and 14.4).
Euler-Eytelwein T_high/T_low=exp(mu*turn) supplies curvature friction; a public
demonstration/derivation is Harvard Natural Sciences, 'Friction'.
Station tangents and doglegs reuse well.wellpath's minimum-curvature survey.
Stretch is integral T(s)/(EA) ds (OpenStax vol. 1, section 12.3, tensile strain).
mu=0.30, eccentricity multiplier e=0.12 and steel-equivalent density 65.4 ppg
are representative task assumptions, not certified cable or well properties.
"""

import math
from dataclasses import dataclass

from digitalmodel.units import Q_
from digitalmodel.well.wellpath.workflow import _minimum_curvature_survey, _stations

from ._validation import finite, scalar


def buoyancy_factor(density, material_density=Q_(65.4, "ppg")) -> float:
    """Return 1-rho/rho_material (Archimedes; OpenStax vol. 1, section 14.4).

    Representative material default 65.4 ppg is task-specified steel-equivalent
    density. Composite cable requires the certificate's submerged weight.
    """
    rho = scalar(density, "kg/m**3", "fluid density")
    material = scalar(material_density, "kg/m**3", "material density", positive=True)
    return 1 - rho / material


def resisting_force(
    buoyed_bha_weight,
    buoyed_cable_weight,
    lateral_length,
    inclination_deg,
    mu=0.30,
    eccentricity=0.12,
):
    """Return mu*(1+e)*(Wbha*sin(theta)+wc*L)-(Wbha+wc*L)*cos(theta) [N].

    Task lumped-section model; force resolution/Coulomb basis cited above.
    theta is degrees FROM VERTICAL. Wbha is force, wc force/length (not mass).
    Cable normal weight term assumes a lateral; use tension_profile for actual
    station geometry. A negative result explicitly denotes gravity assistance.
    """
    bha = scalar(buoyed_bha_weight, "N", "buoyed BHA weight")
    cable = scalar(buoyed_cable_weight, "N/m", "buoyed cable weight")
    length = scalar(lateral_length, "m", "lateral length")
    theta = math.radians(finite(inclination_deg, "inclination", maximum=180))
    friction = finite(mu, "friction coefficient") * (
        1 + finite(eccentricity, "eccentricity")
    )
    return Q_(
        friction * (bha * math.sin(theta) + cable * length)
        - (bha + cable * length) * math.cos(theta),
        "N",
    )


def critical_inclination(mu: float) -> float:
    """Return atan(1/mu) [degrees], gravity/friction crossover for one body.

    Derived from mu*W*sin(theta)=W*cos(theta), Coulomb basis above. If an
    eccentricity correction is desired, pass the effective mu*(1+e).
    """
    friction = finite(mu, "friction coefficient")
    return math.degrees(math.atan2(1, friction))


def capstan_factor(mu: float, turn_deg: float) -> float:
    """Return exp(mu*turn_rad), the Euler-Eytelwein high/low tension ratio.

    Harvard Natural Sciences 'Friction', Euler-Eytelwein equation; cumulative
    turn includes azimuth changes and is positive regardless of bend direction.
    """
    friction = finite(mu, "friction coefficient")
    turn = finite(turn_deg, "turn angle")
    return math.exp(friction * math.radians(turn))


@dataclass(frozen=True)
class TensionStation:
    """Depth [m], tension [N], integral T ds [N*m] to the next deeper point."""

    measured_depth: Q_
    tension: Q_
    tension_integral: Q_


def _tangent(inc, azi):
    return (math.sin(inc) * math.cos(azi), math.sin(inc) * math.sin(azi), math.cos(inc))


def _segment_integral(tension, source, exponent_rate, length):
    """Exact update and integral for dT/dx=source+exponent_rate*T.

    Piecewise-constant midpoint quadrature on a minimum-curvature arc; the
    analytical exponential avoids replacing the profile by surface tension.
    """
    z = exponent_rate * length
    if abs(z) < 1e-4:
        phi1 = 1 + z / 2 + z * z / 6 + z**3 / 24 + z**4 / 120
        phi2 = 0.5 + z / 6 + z * z / 24 + z**3 / 120 + z**4 / 720
    else:
        phi1 = math.expm1(z) / z
        phi2 = (math.expm1(z) - z) / (z * z)
    upper = tension * math.exp(z) + source * length * phi1
    integral = tension * length * phi1 + source * length * length * phi2
    return upper, integral


def tension_profile(
    stations,
    bottom_tension,
    buoyed_cable_weight,
    mu=0.30,
    eccentricity=0.0,
    direction="payout",
    *,
    max_step=Q_(20, "m"),
    max_turn_deg=1.0,
) -> list[TensionStation]:
    """Integrate a taut cable from known bottom tension to the surface.

    Input survey is well.wellpath's list of measured_depth_m, inclination_deg
    and azimuth_deg mappings, starting at MD=0. Returned points run surface
    to bottom. On upward integration coordinate x:

        dT/dx = w*cos(theta) + sign*mu_eff*w*sin(theta)
                + sign*mu_eff*curvature*T

    sign is +1 for pickup and -1 for payout; Euler-Eytelwein and Coulomb basis
    are cited above. Curvature-normal and gravity-normal contact magnitudes
    are added as a scalar screening approximation, not a vector-contact model.
    The requested bottom boundary must be consistent with a taut cable;
    negative tension is rejected as slack, never reported as compression.
    Integration uses midpoint spherical tangents on each minimum-curvature
    arc with analytical exponential updates. 20 m / 1 degree are numerical
    step controls, not physical coefficients; callers can refine them.
    """
    if direction not in ("payout", "pickup"):
        raise ValueError("direction must be payout or pickup")
    tension = scalar(bottom_tension, "N", "bottom tension")
    weight = scalar(buoyed_cable_weight, "N/m", "buoyed cable weight")
    friction = finite(mu, "friction coefficient") * (
        1 + finite(eccentricity, "eccentricity")
    )
    step = scalar(max_step, "m", "integration step", positive=True)
    turn_limit = scalar(max_turn_deg, "dimensionless", "turn step", positive=True)
    checked = _stations({"stations": stations})
    for station in checked:
        for name, value in station.items():
            if not math.isfinite(value):
                raise ValueError(f"survey {name} must be finite")
    if checked[0]["measured_depth_m"] != 0:
        raise ValueError("surface survey must start at measured_depth_m=0")
    survey = _minimum_curvature_survey(checked, 30.0)
    points = [TensionStation(Q_(survey[-1]["md"], "m"), Q_(tension, "N"), Q_(0, "N*m"))]
    sign = 1 if direction == "pickup" else -1
    for upper, lower in reversed(list(zip(survey, survey[1:]))):
        length = lower["md"] - upper["md"]
        a = _tangent(math.radians(upper["inc"]), math.radians(upper["azi"]))
        b = _tangent(math.radians(lower["inc"]), math.radians(lower["azi"]))
        # wellpath supplies DLS from the full 3D dogleg, not just delta-inclination.
        turn = math.radians(lower["dls"] * length / 30.0)
        if turn >= math.pi - 1e-8:
            raise ValueError("antiparallel survey tangents do not define a unique arc")
        count = max(
            1, math.ceil(length / step), math.ceil(math.degrees(turn) / turn_limit)
        )
        dx = length / count
        for index in range(count):
            fraction = 1 - (index + 0.5) / count
            if turn < 1e-10:
                tangent = a
            else:
                wa = math.sin((1 - fraction) * turn) / math.sin(turn)
                wb = math.sin(fraction * turn) / math.sin(turn)
                tangent = tuple(wa * av + wb * bv for av, bv in zip(a, b))
            cosine = max(-1.0, min(1.0, tangent[2]))
            sine = math.sqrt(max(0.0, 1 - cosine * cosine))
            source = weight * (cosine + sign * friction * sine)
            tension, integral = _segment_integral(
                tension, source, sign * friction * turn / length, dx
            )
            if tension < -1e-7:
                raise ValueError(
                    "boundary condition implies slack cable; compression is unsupported"
                )
            tension = max(0.0, tension)
            md = upper["md"] if index == count - 1 else lower["md"] - (index + 1) * dx
            points.append(
                TensionStation(
                    Q_(md, "m"), Q_(tension, "N"), Q_(max(0.0, integral), "N*m")
                )
            )
    return list(reversed(points))


def cable_stretch(profile: list[TensionStation], axial_stiffness):
    """Return integral T ds/(EA) [m] from tension_profile's interval integrals.

    OpenStax University Physics vol. 1, section 12.3, Young's modulus and
    tensile strain. EA is the certificate's effective axial stiffness (force),
    not E alone and not nominal cable area times an assumed steel modulus.
    """
    stiffness = scalar(axial_stiffness, "N", "cable axial stiffness", positive=True)
    if not profile:
        raise ValueError("tension profile must not be empty")
    integral = sum(
        scalar(point.tension_integral, "N*m", "tension integral") for point in profile
    )
    return Q_(integral / stiffness, "m")
