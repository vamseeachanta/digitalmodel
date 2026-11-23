"""Vessel silhouette polygon registry for polar_force_overlay (digitalmodel#616).

Provides hand-tuned silhouette polygons for common vessel classes. Polygons are
expressed in body-fixed coordinates (x = longitudinal, +x = bow direction;
y = transverse, +y = starboard). The polar renderer transforms these into the
appropriate angular position based on the chart's `direction`/`rotation` settings.

Per plan r2 n3 + TDD #17: this module declares NO @dataclass. The only @dataclass
in the package is `VesselSilhouetteSpec` in types.py.

Per plan §Risks Open m5: silhouette polygons are classified "conventional, not
standards-derived" — they emit no Citation. Each silhouette is a stylized hull
shape for visual reference only, not engineering geometry.
"""
from __future__ import annotations

from typing import Optional


# Normalized silhouettes — points in (x, y) with x_max ≈ +1 (bow), x_min ≈ -1 (stern),
# y_max ≈ +0.5 (starboard side), y_min ≈ -0.5 (port side). These get scaled at
# render time to (length_bp_m, beam_m).

_TANKER_NORM = [
    # Stern to bow on starboard side, then back along port. Closed polygon.
    (-1.00,  0.00),   # stern centerline (square stern)
    (-1.00, +0.40),
    (-0.80, +0.50),
    (-0.20, +0.50),   # parallel midbody starts
    (+0.20, +0.50),   # parallel midbody ends
    (+0.70, +0.45),   # bow shoulder taper
    (+0.95, +0.20),   # bow taper
    (+1.00,  0.00),   # bow tip
    (+0.95, -0.20),
    (+0.70, -0.45),
    (+0.20, -0.50),
    (-0.20, -0.50),
    (-0.80, -0.50),
    (-1.00, -0.40),
    (-1.00,  0.00),   # close polygon
]

_GAS_CARRIER_NORM = [
    # Similar tanker proportions but with a slightly more bulbous bow
    # (small visual differentiation from tanker for the explorer use case).
    (-1.00,  0.00),
    (-1.00, +0.40),
    (-0.80, +0.50),
    (+0.30, +0.50),
    (+0.75, +0.42),
    (+1.00,  0.00),
    (+0.75, -0.42),
    (+0.30, -0.50),
    (-0.80, -0.50),
    (-1.00, -0.40),
    (-1.00,  0.00),
]

_GENERIC_NORM = [
    # Diamond/leaf shape — neutral when vessel kind is unknown.
    (-1.00,  0.00),
    (-0.30, +0.45),
    (+0.30, +0.45),
    (+1.00,  0.00),
    (+0.30, -0.45),
    (-0.30, -0.45),
    (-1.00,  0.00),
]


_KIND_REGISTRY: dict[str, list[tuple[float, float]]] = {
    "tanker": _TANKER_NORM,
    "gas_carrier": _GAS_CARRIER_NORM,
    "generic": _GENERIC_NORM,
}


def get_polygon(
    silhouette_kind: str,
    length_bp_m: float,
    beam_m: float,
    custom_path: Optional[list[tuple[float, float]]] = None,
) -> list[tuple[float, float]]:
    """Return a silhouette polygon in body-fixed (x, y) coordinates.

    Args:
        silhouette_kind: "tanker" | "gas_carrier" | "generic" | "custom_path"
        length_bp_m: vessel length between perpendiculars in meters (silhouette x-scale)
        beam_m: vessel beam in meters (silhouette y-scale)
        custom_path: required when silhouette_kind="custom_path"; returned verbatim

    Returns:
        List of (x, y) tuples in vessel-body coordinates. Polygon is closed
        (first point == last point).

    Raises:
        ValueError: silhouette_kind unknown OR custom_path missing for custom kind.
    """
    if silhouette_kind == "custom_path":
        if not custom_path:
            raise ValueError("silhouette_kind='custom_path' requires non-empty custom_path")
        return list(custom_path)

    if silhouette_kind not in _KIND_REGISTRY:
        raise ValueError(
            f"unknown silhouette_kind={silhouette_kind!r}; "
            f"known: {sorted(_KIND_REGISTRY.keys())} + 'custom_path'"
        )

    norm = _KIND_REGISTRY[silhouette_kind]
    half_length = length_bp_m / 2.0
    return [(x * half_length, y * beam_m) for (x, y) in norm]
