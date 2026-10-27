# ABOUTME: Gyradius (radius of gyration) calculator for floating platforms
# ABOUTME: 6-DOF mass distribution per SNAME PNA and DNV standards
"""
Gyradius calculator for floating platforms.

Computes radius of gyration (k = sqrt(I/m)) for 6-DOF motions:
  surge, sway, heave, roll, pitch, yaw

Supports FPSO, semi-submersible, spar, TLP, and barge platforms.

References:
    SNAME, Principles of Naval Architecture, Ch. 3
    DNV-RP-H103: Modelling and analysis of marine operations
"""

import math
from collections.abc import Iterable
from dataclasses import dataclass
from fractions import Fraction
from typing import Union


@dataclass
class GyradiusResult:
    """All 6-DOF gyradius values for a floating platform."""
    surge: float       # m
    sway: float        # m
    heave: float       # m
    roll: float        # m (kxx)
    pitch: float       # m (kyy)
    yaw: float         # m (kzz)
    mass_kg: float
    displacement_t: float  # displacement in tonnes

    @property
    def kxx(self) -> float:
        """Roll gyradius alias."""
        return self.roll

    @property
    def kyy(self) -> float:
        """Pitch gyradius alias."""
        return self.pitch

    @property
    def kzz(self) -> float:
        """Yaw gyradius alias."""
        return self.yaw

    @property
    def ixx(self) -> float:
        """Roll moment of inertia (kg·m²)."""
        return self.mass_kg * self.roll ** 2

    @property
    def iyy(self) -> float:
        """Pitch moment of inertia (kg·m²)."""
        return self.mass_kg * self.pitch ** 2

    @property
    def izz(self) -> float:
        """Yaw moment of inertia (kg·m²)."""
        return self.mass_kg * self.yaw ** 2


# Typical gyradius ratios from SNAME PNA and field data.
# kx = ratio * beam, ky = ratio * length, kz = ratio * beam
GYRADIUS_RATIOS = {
    "fpso": {
        "kx_beam": 0.40,     # kxx ~ 0.35-0.40 * B
        "ky_length": 0.25,   # kyy ~ 0.25 * L
        "kz_beam": 0.25,     # kzz ~ 0.25 * B
    },
    "semisubmersible": {
        "kx_beam": 0.40,
        "ky_length": 0.30,   # larger — semi has wide column spacing
        "kz_beam": 0.30,
    },
    "spar": {
        "kx_beam": 0.35,
        "ky_length": 0.35,   # spar is cylindrical, L ~ B
        "kz_beam": 0.35,
    },
    "tlp": {
        "kx_beam": 0.35,
        "ky_length": 0.22,
        "kz_beam": 0.25,
    },
    "barge": {
        "kx_beam": 0.35,
        "ky_length": 0.27,
        "kz_beam": 0.27,
    },
}


def _estimate_dimensions(platform_type: str, displacement_t: float):
    """Estimate platform dimensions from displacement using power-law scaling."""
    dwt = displacement_t

    if platform_type == "fpso":
        length = 50.0 * (dwt ** 0.30)
        beam = length / 5.5
    elif platform_type == "semisubmersible":
        length = 25.0 * (dwt ** 0.28)
        beam = length / 1.5
    elif platform_type == "spar":
        beam = 3.0 * (dwt ** 0.30)
        length = beam  # cylindrical
    elif platform_type == "tlp":
        length = 30.0 * (dwt ** 0.33)
        beam = length / 1.4
    elif platform_type == "barge":
        length = 35.0 * (dwt ** 0.30)
        beam = length / 4.5
    else:
        raise ValueError(f"Unknown platform type: {platform_type}")

    draft = beam / 3.5
    return length, beam, draft


def gyradius_for_platform_type(
    platform_type: str,
    displacement_t: float,
) -> GyradiusResult:
    """Estimate 6-DOF gyradius for a given platform type.

    Args:
        platform_type: 'fpso', 'semisubmersible', 'spar', 'tlp', 'barge'
        displacement_t: displacement in tonnes

    Returns:
        GyradiusResult with kx, ky, kz in meters.

    Raises:
        ValueError: unknown type or non-positive displacement.
    """
    platform_type = platform_type.lower()
    if platform_type not in GYRADIUS_RATIOS:
        raise ValueError(
            f"Unknown platform type '{platform_type}'. "
            f"Valid: {sorted(GYRADIUS_RATIOS.keys())}"
        )
    if displacement_t <= 0:
        raise ValueError("displacement_t must be positive")

    length, beam, draft = _estimate_dimensions(platform_type, displacement_t)
    ratios = GYRADIUS_RATIOS[platform_type]

    roll = beam * ratios["kx_beam"]
    pitch = length * ratios["ky_length"]
    yaw = beam * ratios["kz_beam"]

    return GyradiusResult(
        surge=length * 0.25,
        sway=beam * 0.25,
        heave=draft * 0.25,
        roll=roll,
        pitch=pitch,
        yaw=yaw,
        mass_kg=displacement_t * 1000.0,
        displacement_t=displacement_t,
    )


def gyradius_from_inertia(
    mass_kg: float,
    ixx: float,
    iyy: float,
    izz: float,
) -> GyradiusResult:
    """Compute gyradius from mass moments of inertia.

    k = sqrt(I/m)

    Args:
        mass_kg: total mass in kg
        ixx: roll inertia (kg·m²)
        iyy: pitch inertia (kg·m²)
        izz: yaw inertia (kg·m²)
    """
    if mass_kg <= 0:
        raise ValueError("mass_kg must be positive")
    if ixx < 0 or iyy < 0 or izz < 0:
        raise ValueError("inertias must be non-negative")

    return GyradiusResult(
        surge=math.sqrt(ixx / mass_kg) * 0.9,
        sway=math.sqrt(iyy / mass_kg) * 0.9,
        heave=math.sqrt(izz / mass_kg) * 0.5,
        roll=math.sqrt(ixx / mass_kg),
        pitch=math.sqrt(iyy / mass_kg),
        yaw=math.sqrt(izz / mass_kg),
        mass_kg=mass_kg,
        displacement_t=mass_kg / 1000.0,
    )


@dataclass(frozen=True)
class PointMassGyradiusResult:
    """Centroidal rotational properties along the supplied coordinate axes.

    Mass is in kg, the centroid in m, inertias in kg·m², and radii in m.
    No translational factors or principal-axis transformation are applied.
    Radius properties raise ValueError if the radius exceeds finite float range.
    """

    mass_kg: float
    centroid_m: tuple[float, float, float]
    ixx: float
    iyy: float
    izz: float

    @property
    def roll(self) -> float:
        """Return sqrt(Ixx/M), the radius about the centroidal x axis."""
        return _point_mass_radius(self.ixx, self.mass_kg)

    @property
    def pitch(self) -> float:
        """Return sqrt(Iyy/M), the radius about the centroidal y axis."""
        return _point_mass_radius(self.iyy, self.mass_kg)

    @property
    def yaw(self) -> float:
        """Return sqrt(Izz/M), the radius about the centroidal z axis."""
        return _point_mass_radius(self.izz, self.mass_kg)


def _point_mass_radius(inertia: float, mass: float) -> float:
    """Evaluate sqrt(I/M) without forming an out-of-range intermediate ratio."""
    radius = math.sqrt(inertia) / math.sqrt(mass)
    if not math.isfinite(radius):
        raise ValueError("radius must be finite")
    return radius


def gyradius_from_point_masses(
    masses_kg: Iterable[float],
    coordinates_m: Iterable[tuple[float, float, float]],
) -> PointMassGyradiusResult:
    """Compute rotational gyradii from a synthetic point-mass distribution.

    The mass centroid is c = sum(m*p)/M, where M = sum(m). For offsets
    (dx, dy, dz) from c, Ixx = sum(m*(dy² + dz²)); Iyy and Izz follow by
    cycling the axes. Each radius follows directly from k = sqrt(I/M).
    These point-mass definitions introduce no empirical coefficients.

    Basis: OpenStax, University Physics Volume 1 (first edition, 2016),
    Section 9.6, Center of Mass; Section 10.5, Calculating Moments of Inertia.

    Args:
        masses_kg: Finite nonnegative masses in kg, one per point.
        coordinates_m: Finite (x, y, z) coordinates in m; x is longitudinal,
            y transverse, and z vertical. Iterables are consumed once.

    Returns:
        Total mass, centroid, diagonal centroidal inertias, and roll/pitch/yaw
        radii about axes parallel to the supplied axes through that centroid.

    Raises:
        ValueError: Empty or mismatched inputs, malformed coordinates, negative
            or nonfinite masses, nonfinite coordinates, nonpositive total mass,
            a total or inertia outside finite float range, or a nonzero inertia
            that rounds to zero. Exact intermediate balances are converted to
            floats only after the centroid and inertias have been calculated.

    Zero-mass points are validated but contribute nothing. Intrinsic inertia
    of each component, products of inertia, added mass, and translational
    properties are outside this point-mass model.
    """
    try:
        masses = tuple(float(mass) for mass in masses_kg)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError("masses must be finite and non-negative") from exc
    if any(not math.isfinite(mass) or mass < 0 for mass in masses):
        raise ValueError("masses must be finite and non-negative")

    try:
        points = tuple(tuple(float(value) for value in p) for p in coordinates_m)
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError("coordinates must be finite numbers") from exc
    if len(masses) != len(points):
        raise ValueError("masses and coordinates must have the same length")
    if not masses:
        raise ValueError("at least one point mass is required")
    if any(len(point) != 3 for point in points):
        raise ValueError("each point must have three coordinates")
    if any(not math.isfinite(value) for point in points for value in point):
        raise ValueError("coordinates must be finite")

    try:
        total_mass = math.fsum(masses)
    except OverflowError as exc:
        raise ValueError("total mass must be finite") from exc
    if total_mass <= 0:
        raise ValueError("total mass must be positive")

    # Exact float-input balances preserve small residuals in any input order.
    active = [
        (Fraction(mass), tuple(Fraction(value) for value in point))
        for mass, point in zip(masses, points) if mass > 0
    ]
    exact_mass = sum(mass for mass, _ in active)
    exact_centroid = tuple(
        sum(mass * point[axis] for mass, point in active) / exact_mass
        for axis in range(3)
    )
    try:
        centroid = tuple(float(value) for value in exact_centroid)
        centered = [
            (mass, tuple(value - center
                         for value, center in zip(point, exact_centroid)))
            for mass, point in active
        ]
        # I = sum(m*r²), using the two coordinates perpendicular to each axis.
        exact_inertias = tuple(
            sum(
                mass * (point[a] ** 2 + point[b] ** 2)
                for mass, point in centered
            )
            for a, b in ((1, 2), (0, 2), (0, 1))
        )
        inertias = tuple(float(value) for value in exact_inertias)
    except (OverflowError, ValueError) as exc:
        raise ValueError("centroid offsets and inertias must be finite") from exc
    if not all(math.isfinite(value) for value in (*centroid, *inertias)):
        raise ValueError("centroid offsets and inertias must be finite")
    if any(exact and rounded == 0 for exact, rounded in zip(exact_inertias, inertias)):
        raise ValueError("nonzero inertias must be representable as nonzero floats")

    return PointMassGyradiusResult(total_mass, centroid, *inertias)


def estimate_inertia_from_gyradius(
    mass_kg: float,
    kx: float,
    ky: float,
    kz: float,
) -> dict:
    """I = m * k²."""
    if mass_kg <= 0:
        raise ValueError("mass_kg must be positive")
    if kx < 0 or ky < 0 or kz < 0:
        raise ValueError("gyradius values must be non-negative")
    return {
        "ixx": mass_kg * kx ** 2,
        "iyy": mass_kg * ky ** 2,
        "izz": mass_kg * kz ** 2,
    }


# Vessel-type → platform-type mapping for gyradius estimation
_VESSEL_TO_PLATFORM: dict[str, str] = {
    "fpso": "fpso",
    "semi_submersible": "semisubmersible",
    "semisubmersible": "semisubmersible",
    "crane_vessel": "semisubmersible",
    "spar": "spar",
    "tlp": "tlp",
    "tension_leg_platform": "tlp",
    "drillship": "fpso",
    "pipelay_vessel": "barge",
    "anchor_handler": "fpso",
    "platform_supply_vessel": "fpso",
    "jack_up": "barge",
}


def gyradius_for_fleet_vessel(hull_id: str) -> GyradiusResult:
    """Compute 6-DOF gyradius for a registered fleet vessel.

    Looks up the vessel in the ship registry, maps its vessel_type to a
    platform type, and delegates to ``gyradius_for_platform_type``.

    Args:
        hull_id: Vessel name as registered (e.g. "THIALF", "SLEIPNIR").

    Returns:
        GyradiusResult with kx, ky, kz in meters.

    Raises:
        KeyError: vessel not found in registry.
        ValueError: vessel lacks displacement or mappable type.
    """
    from digitalmodel.naval_architecture.ship_data import get_ship

    ship = get_ship(hull_id)
    if ship is None:
        raise KeyError(f"Vessel '{hull_id}' not found in ship registry")

    # Determine displacement in tonnes
    disp_lt = ship.get("displacement_lt")
    if disp_lt is None or float(disp_lt) <= 0:
        # Fall back to block-coefficient estimate
        loa = ship.get("loa_ft")
        beam = ship.get("beam_ft")
        draft = ship.get("draft_ft")
        if loa and beam and draft:
            vol_ft3 = 0.65 * float(loa) * float(beam) * float(draft)
            disp_lt = (64.0 * vol_ft3) / 2240.0
        else:
            raise ValueError(
                f"Vessel '{hull_id}' lacks displacement and sufficient "
                "dimensions for estimation"
            )

    # Convert LT to tonnes
    disp_tonnes = float(disp_lt) * 1.01605

    # Map vessel type to platform type
    vtype = ship.get("vessel_type", "")
    vsubtype = ship.get("vessel_subtype", "")
    platform_type = None
    for candidate in (vsubtype, vtype):
        if candidate:
            key = str(candidate).lower().strip()
            if key in _VESSEL_TO_PLATFORM:
                platform_type = _VESSEL_TO_PLATFORM[key]
                break

    if platform_type is None:
        # Default to barge for unknown types
        platform_type = "barge"

    return gyradius_for_platform_type(platform_type, disp_tonnes)
