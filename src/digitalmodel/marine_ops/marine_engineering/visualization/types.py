"""Types for the polar_force_overlay module (digitalmodel#616).

Per plan r1 M2 decision: `VesselSilhouetteSpec` is the ONLY @dataclass declared
in this visualization package. It composes a `HullProfile` (reused from
hydrodynamics.hull_library.profile_schema) rather than duplicating LOA/beam/draft.

TDD #17 enforces "dataclass-count exactly 1" via AST static analysis across all
.py files in this package.
"""
from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Optional

from digitalmodel.hydrodynamics.hull_library.profile_schema import HullProfile


class FrameConvention(Enum):
    """Coordinate-frame convention for the input data's `theta_deg` column.

    See `_convention.py` for the OCIMF MEG3/MEG4 citation that backs the
    `INCIDENCE_HEADING_BODY_FIXED` default.
    """

    INCIDENCE_HEADING_BODY_FIXED = "incidence_heading_body_fixed"
    """Per OCIMF MEG3 §A1 / MEG4 §A2: θ is wind/current flow direction (stern→bow at 0°);
    positive anti-clockwise from above; +Y_body = port. Default."""

    FORCE_DIRECTION_INERTIAL = "force_direction_inertial"
    """θ is the force-vector direction in the inertial frame (caller-supplied,
    no OCIMF-style transform applied)."""


class ForceArrowKind(Enum):
    """Which force-arrow component(s) to render on the silhouette."""

    LATERAL_ONLY = "lateral_only"            # Y-component only
    LONGITUDINAL_ONLY = "longitudinal_only"  # X-component only
    RESULTANT_2D = "resultant_2d"            # vector sum in XY
    NONE = "none"                            # no arrows; data-trace only


class RadialAxisMode(Enum):
    """How the radial axis encodes the coefficient/force value."""

    MAGNITUDE = "magnitude"  # r = |C|; sign by line style (today's build script behavior)
    SIGNED = "signed"        # r = C directly; positive/negative halves split visually


@dataclass(frozen=True)
class VesselSilhouetteSpec:
    """Silhouette-rendering spec that composes a HullProfile.

    Does NOT duplicate LOA/beam/draft fields — those live on the composed HullProfile.
    `silhouette_kind` and `custom_path` are the only rendering-specific fields.

    NOTE: HullProfile uses `length_bp` (Length Between Perpendiculars), NOT LOA.
    For visual silhouette rendering at polar-chart scale the LBP-vs-LOA distinction
    is small (typically LBP ≈ 0.95-0.98 × LOA for tankers).
    """

    hull_profile: HullProfile
    silhouette_kind: str = "tanker"  # tanker | gas_carrier | generic | custom_path
    custom_path: Optional[list[tuple[float, float]]] = None
    opacity: float = 0.20  # transparent overlay; default ≤ 0.30 per acceptance criterion #8

    def __post_init__(self) -> None:
        if self.silhouette_kind == "custom_path" and not self.custom_path:
            raise ValueError(
                "silhouette_kind='custom_path' requires non-empty custom_path"
            )
        if not (0.0 < self.opacity <= 0.30):
            raise ValueError(
                f"opacity must be in (0.0, 0.30]; got {self.opacity}"
            )
