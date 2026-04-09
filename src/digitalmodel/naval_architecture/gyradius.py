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
from dataclasses import dataclass
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
