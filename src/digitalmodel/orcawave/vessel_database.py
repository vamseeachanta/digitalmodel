"""
ABOUTME: Common vessel database for OrcaWave preliminary studies.
Provides typical vessel parameters (dimensions, displacement, GM) and
representative RAO data for FPSO, semi-submersible, drillship, barge,
and LNGC vessel types.  Includes parametric hull generation for
quick feasibility checks.
"""

from typing import Dict, List, Optional, Tuple

import numpy as np
from pydantic import BaseModel, Field


# ---------------------------------------------------------------------------
# Pydantic models
# ---------------------------------------------------------------------------

class VesselParameters(BaseModel):
    """Physical parameters for a vessel."""

    name: str = Field(..., description="Vessel identifier")
    vessel_type: str = Field(
        ...,
        description="Type: FPSO, SemiSubmersible, Drillship, Barge, LNGC",
    )
    length_m: float = Field(..., description="Length overall (m)", gt=0)
    beam_m: float = Field(..., description="Beam/breadth (m)", gt=0)
    draft_m: float = Field(..., description="Operating draft (m)", gt=0)
    depth_m: float = Field(..., description="Depth (m)", gt=0)
    displacement_te: float = Field(..., description="Displacement (tonnes)", gt=0)
    gm_t_m: float = Field(..., description="Transverse metacentric height (m)")
    gm_l_m: float = Field(..., description="Longitudinal metacentric height (m)")
    waterplane_area_m2: float = Field(default=0.0, description="Waterplane area (m^2)")
    block_coefficient: float = Field(
        default=0.8,
        description="Block coefficient Cb",
        ge=0.3,
        le=1.0,
    )
    natural_periods: Dict[str, float] = Field(
        default_factory=dict,
        description="Natural periods {heave_s, roll_s, pitch_s}",
    )


class VesselRAOSet(BaseModel):
    """Representative RAO data for a vessel type."""

    vessel_type: str
    periods: List[float] = Field(..., description="Wave periods (s)")
    headings_deg: List[float] = Field(
        default=[0, 45, 90, 135, 180],
        description="Wave headings (degrees)",
    )
    heave_rao: List[List[float]] = Field(
        ...,
        description="Heave RAO amplitudes [n_headings x n_periods] (m/m)",
    )
    roll_rao: List[List[float]] = Field(
        ...,
        description="Roll RAO amplitudes [n_headings x n_periods] (deg/m)",
    )
    pitch_rao: List[List[float]] = Field(
        ...,
        description="Pitch RAO amplitudes [n_headings x n_periods] (deg/m)",
    )
    surge_rao: List[List[float]] = Field(
        default_factory=list,
        description="Surge RAO amplitudes [n_headings x n_periods] (m/m)",
    )


class ParametricHull(BaseModel):
    """Parametric hull definition for preliminary mesh generation."""

    vessel_type: str
    length_m: float
    beam_m: float
    draft_m: float
    bilge_radius_m: float = Field(default=0.0)
    bow_shape: str = Field(default="pointed", description="'pointed' or 'blunt'")
    n_waterline_sections: int = Field(default=20)
    waterline_offsets: List[List[float]] = Field(
        default_factory=list,
        description="Half-breadth offsets [[x, y], ...] at waterline",
    )


# ---------------------------------------------------------------------------
# Vessel database
# ---------------------------------------------------------------------------

_VESSEL_DB: Dict[str, VesselParameters] = {
    "FPSO_Generic_250m": VesselParameters(
        name="FPSO_Generic_250m",
        vessel_type="FPSO",
        length_m=250.0,
        beam_m=46.0,
        draft_m=16.0,
        depth_m=28.0,
        displacement_te=180000.0,
        gm_t_m=5.0,
        gm_l_m=300.0,
        waterplane_area_m2=9200.0,
        block_coefficient=0.85,
        natural_periods={"heave_s": 10.5, "roll_s": 14.0, "pitch_s": 11.0},
    ),
    "SemiSub_Generic": VesselParameters(
        name="SemiSub_Generic",
        vessel_type="SemiSubmersible",
        length_m=110.0,
        beam_m=80.0,
        draft_m=22.0,
        depth_m=45.0,
        displacement_te=52000.0,
        gm_t_m=3.5,
        gm_l_m=3.5,
        waterplane_area_m2=2400.0,
        block_coefficient=0.40,
        natural_periods={"heave_s": 22.0, "roll_s": 50.0, "pitch_s": 50.0},
    ),
    "Drillship_Generic": VesselParameters(
        name="Drillship_Generic",
        vessel_type="Drillship",
        length_m=230.0,
        beam_m=42.0,
        draft_m=12.0,
        depth_m=21.0,
        displacement_te=70000.0,
        gm_t_m=4.0,
        gm_l_m=250.0,
        waterplane_area_m2=7700.0,
        block_coefficient=0.78,
        natural_periods={"heave_s": 9.0, "roll_s": 16.0, "pitch_s": 9.5},
    ),
    "Barge_Generic_100m": VesselParameters(
        name="Barge_Generic_100m",
        vessel_type="Barge",
        length_m=100.0,
        beam_m=30.0,
        draft_m=5.0,
        depth_m=7.0,
        displacement_te=14000.0,
        gm_t_m=8.0,
        gm_l_m=150.0,
        waterplane_area_m2=2800.0,
        block_coefficient=0.90,
        natural_periods={"heave_s": 6.0, "roll_s": 8.0, "pitch_s": 6.5},
    ),
    "LNGC_Generic_280m": VesselParameters(
        name="LNGC_Generic_280m",
        vessel_type="LNGC",
        length_m=280.0,
        beam_m=46.0,
        draft_m=11.5,
        depth_m=26.0,
        displacement_te=100000.0,
        gm_t_m=4.5,
        gm_l_m=350.0,
        waterplane_area_m2=10500.0,
        block_coefficient=0.76,
        natural_periods={"heave_s": 9.5, "roll_s": 18.0, "pitch_s": 10.0},
    ),
}


def list_vessels() -> List[str]:
    """List all available vessel names in the database.

    Returns:
        List of vessel name strings.
    """
    return list(_VESSEL_DB.keys())


def get_vessel(name: str) -> VesselParameters:
    """Retrieve vessel parameters by name.

    Args:
        name: Vessel name (case-sensitive).

    Returns:
        VesselParameters.

    Raises:
        KeyError: If vessel not found.
    """
    if name not in _VESSEL_DB:
        available = ", ".join(_VESSEL_DB.keys())
        raise KeyError(f"Vessel '{name}' not found. Available: {available}")
    return _VESSEL_DB[name]


def get_vessels_by_type(vessel_type: str) -> List[VesselParameters]:
    """Get all vessels of a specific type.

    Args:
        vessel_type: One of FPSO, SemiSubmersible, Drillship, Barge, LNGC.

    Returns:
        List of matching VesselParameters.
    """
    return [v for v in _VESSEL_DB.values() if v.vessel_type == vessel_type]


# ---------------------------------------------------------------------------
# Representative RAO generation
# ---------------------------------------------------------------------------

def _generate_ship_rao(
    periods: np.ndarray,
    natural_period: float,
    max_rao: float,
    damping_ratio: float = 0.05,
) -> np.ndarray:
    """Generate a simplified single-DOF RAO using a 1-DOF transfer function.

    |RAO| = max_rao / sqrt((1 - r^2)^2 + (2*zeta*r)^2)
    where r = T_n / T (frequency ratio).

    Args:
        periods: Wave periods (s).
        natural_period: Natural period (s).
        max_rao: Maximum RAO amplitude at resonance.
        damping_ratio: Damping ratio zeta.

    Returns:
        RAO amplitude array.
    """
    r = natural_period / np.maximum(periods, 0.1)
    rao = max_rao / np.sqrt((1.0 - r**2) ** 2 + (2.0 * damping_ratio * r) ** 2)
    # Cap at max_rao * 1/(2*zeta)
    rao = np.minimum(rao, max_rao / (2.0 * damping_ratio))
    return rao


def get_representative_raos(vessel_type: str) -> VesselRAOSet:
    """Generate representative RAO data for a vessel type.

    RAOs are generated using simplified single-DOF transfer functions
    with realistic natural periods and damping.  These are suitable
    for preliminary screening only.

    Args:
        vessel_type: One of FPSO, SemiSubmersible, Drillship, Barge, LNGC.

    Returns:
        VesselRAOSet with heave, roll, and pitch RAOs.
    """
    # Get first vessel of this type for parameters
    vessels = get_vessels_by_type(vessel_type)
    if not vessels:
        raise ValueError(f"No vessels of type '{vessel_type}' in database")
    v = vessels[0]
    nat = v.natural_periods

    periods = np.array([3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18, 20, 25], dtype=float)
    headings = [0.0, 45.0, 90.0, 135.0, 180.0]

    t_heave = nat.get("heave_s", 10.0)
    t_roll = nat.get("roll_s", 14.0)
    t_pitch = nat.get("pitch_s", 10.0)

    # Heading-dependent scaling factors
    # Heave: similar at all headings (hydrostatic-dominated)
    # Roll: maximum at beam seas (90 deg), minimal at head/following
    # Pitch: maximum at head/following (0, 180), minimal at beam
    heading_scale_heave = [0.9, 0.95, 1.0, 0.95, 0.9]
    heading_scale_roll = [0.1, 0.5, 1.0, 0.5, 0.1]
    heading_scale_pitch = [1.0, 0.7, 0.1, 0.7, 1.0]
    heading_scale_surge = [1.0, 0.7, 0.05, 0.7, 1.0]

    # Damping ratios vary by vessel type
    damping = {
        "FPSO": {"heave": 0.08, "roll": 0.05, "pitch": 0.08},
        "SemiSubmersible": {"heave": 0.12, "roll": 0.03, "pitch": 0.12},
        "Drillship": {"heave": 0.08, "roll": 0.04, "pitch": 0.08},
        "Barge": {"heave": 0.10, "roll": 0.06, "pitch": 0.10},
        "LNGC": {"heave": 0.08, "roll": 0.04, "pitch": 0.08},
    }.get(vessel_type, {"heave": 0.08, "roll": 0.05, "pitch": 0.08})

    heave_raos = []
    roll_raos = []
    pitch_raos = []
    surge_raos = []

    for hi, heading in enumerate(headings):
        h_rao = _generate_ship_rao(periods, t_heave, 1.0, damping["heave"])
        h_rao *= heading_scale_heave[hi]
        heave_raos.append(h_rao.tolist())

        r_rao = _generate_ship_rao(periods, t_roll, 5.0, damping["roll"])
        r_rao *= heading_scale_roll[hi]
        roll_raos.append(r_rao.tolist())

        p_rao = _generate_ship_rao(periods, t_pitch, 3.0, damping["pitch"])
        p_rao *= heading_scale_pitch[hi]
        pitch_raos.append(p_rao.tolist())

        s_rao = _generate_ship_rao(periods, t_heave * 1.2, 0.8, damping["heave"])
        s_rao *= heading_scale_surge[hi]
        surge_raos.append(s_rao.tolist())

    return VesselRAOSet(
        vessel_type=vessel_type,
        periods=periods.tolist(),
        headings_deg=headings,
        heave_rao=heave_raos,
        roll_rao=roll_raos,
        pitch_rao=pitch_raos,
        surge_rao=surge_raos,
    )


# ---------------------------------------------------------------------------
# Parametric hull generation
# ---------------------------------------------------------------------------

def generate_parametric_hull(
    vessel_type: str,
    length_m: float,
    beam_m: float,
    draft_m: float,
    n_sections: int = 20,
) -> ParametricHull:
    """Generate a parametric hull waterline for preliminary panel mesh creation.

    The waterline shape is determined by vessel type:
    - Ship-type (FPSO, Drillship, LNGC): pointed bow, rounded stern
    - Barge: blunt bow and stern
    - SemiSubmersible: twin pontoons (rectangular)

    Args:
        vessel_type: Vessel type string.
        length_m: Length (m).
        beam_m: Beam (m).
        draft_m: Draft (m).

    Returns:
        ParametricHull with waterline offsets.
    """
    half_beam = beam_m / 2.0
    x = np.linspace(0, length_m, n_sections)

    if vessel_type in ("FPSO", "Drillship", "LNGC"):
        # Ship-shape: sinusoidal waterline with pointed bow
        fwd_frac = x / length_m
        # Bow taper (last 20% of x i.e. forward end)
        bow_start = 0.8
        # Stern taper (first 10%)
        stern_end = 0.1
        y = np.ones_like(x) * half_beam
        # Stern taper
        stern_mask = fwd_frac < stern_end
        y[stern_mask] = half_beam * (fwd_frac[stern_mask] / stern_end) ** 0.5
        # Bow taper
        bow_mask = fwd_frac > bow_start
        y[bow_mask] = half_beam * ((1.0 - fwd_frac[bow_mask]) / (1.0 - bow_start)) ** 0.7
        bow_shape = "pointed"
    elif vessel_type == "Barge":
        # Rectangular waterline with small corner radii
        y = np.ones_like(x) * half_beam
        # Small taper at ends (5%)
        end_frac = 0.05
        stern_mask = x < length_m * end_frac
        y[stern_mask] = half_beam * (x[stern_mask] / (length_m * end_frac)) ** 0.3
        bow_mask = x > length_m * (1 - end_frac)
        y[bow_mask] = half_beam * ((length_m - x[bow_mask]) / (length_m * end_frac)) ** 0.3
        bow_shape = "blunt"
    elif vessel_type == "SemiSubmersible":
        # Twin pontoons: each pontoon width ~ beam/4
        pontoon_width = beam_m / 4.0
        y = np.ones_like(x) * pontoon_width / 2.0
        # Simple rectangular pontoon
        bow_shape = "blunt"
    else:
        y = np.ones_like(x) * half_beam
        bow_shape = "pointed"

    offsets = [[float(xi), float(yi)] for xi, yi in zip(x, y)]

    bilge_r = 0.0
    if vessel_type in ("FPSO", "Drillship", "LNGC"):
        bilge_r = min(draft_m * 0.3, beam_m * 0.05)
    elif vessel_type == "Barge":
        bilge_r = min(draft_m * 0.1, 0.5)

    return ParametricHull(
        vessel_type=vessel_type,
        length_m=length_m,
        beam_m=beam_m,
        draft_m=draft_m,
        bilge_radius_m=bilge_r,
        bow_shape=bow_shape,
        n_waterline_sections=n_sections,
        waterline_offsets=offsets,
    )
