#!/usr/bin/env python3
"""
ABOUTME: The named pressure-tap data model shared by the OpenFOAM tap rendering
and post-processing modules (dm#661). A tap is a point probe, a point snapped
onto a named wall patch, or a whole named wall patch, together with the pressure
fields sampled there. Kept separate from rendering and analysis so both can
depend on the model without depending on each other.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional, Tuple

# Fields written for a multiphase (interFoam) sloshing tap: dynamic pressure p
# and the buoyant/hydrostatic-reduced pressure p_rgh.
DEFAULT_MULTIPHASE_FIELDS: Tuple[str, ...] = ("p", "p_rgh")


def _fmt(value: float) -> str:
    """Format a float for an OpenFOAM dict (6 significant figures)."""
    return "{:.6g}".format(value)


# ============================================================================
# PressureTap
# ============================================================================


@dataclass
class PressureTap:
    """A single named wall pressure tap.

    A tap is one of three kinds, inferred from which of ``location`` / ``patch``
    are set:

    - point tap (``location`` only) -> emitted as a ``probes`` entry that
      interpolates the field to the point;
    - patch-point tap (``location`` **and** ``patch``) -> emitted as a
      ``patchProbes`` entry that snaps the point onto the named wall patch (so
      the sample sits exactly on the boundary face rather than half a cell off);
    - surface tap (``patch`` only) -> emitted as a ``surfaceFieldValue`` entry
      that reduces the field over the whole named wall patch.

    Attributes:
        name: Unique, human-readable tap name (e.g. ``"tank_top_centreline"``).
        location: ``(x, y, z)`` sample point in metres, or ``None`` for a
            whole-patch surface tap.
        patch: Named wall patch/surface, or ``None`` for a free point tap.
        fields: Fields to sample (default ``("p",)``; use ``("p", "p_rgh")``
            for a multiphase VOF case).
        operation: Reduction for a surface tap (``surfaceFieldValue``
            ``operation``), e.g. ``"areaAverage"`` or ``"max"``. Ignored for
            point / patch-point taps.
    """

    name: str
    location: Optional[Tuple[float, float, float]] = None
    patch: Optional[str] = None
    fields: Tuple[str, ...] = ("p",)
    operation: str = "areaAverage"

    def __post_init__(self) -> None:
        if not self.name or not self.name.strip():
            raise ValueError("PressureTap.name must be a non-empty string.")
        if self.location is None and self.patch is None:
            raise ValueError(
                f"Tap {self.name!r}: provide a location, a patch, or both."
            )
        if self.location is not None and len(tuple(self.location)) != 3:
            raise ValueError(
                f"Tap {self.name!r}: location must be an (x, y, z) triple."
            )
        if not self.fields:
            raise ValueError(f"Tap {self.name!r}: at least one field required.")

    @property
    def kind(self) -> str:
        """One of ``'point'``, ``'patch_point'`` or ``'surface'``."""
        if self.patch is None:
            return "point"
        if self.location is None:
            return "surface"
        return "patch_point"
