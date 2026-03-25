"""
ABOUTME: Parametric hull form definition — Phase 1 of WRK-043.

Defines ParametricRange (linspace generator) and HullParametricSpace
(cartesian product of dimension ranges → scaled HullProfile variants).

Used to enumerate families of hull geometries for batch RAO generation
without requiring a diffraction solver at definition time.
"""

from __future__ import annotations

import itertools
from typing import Any, Iterator

import numpy as np
from pydantic import BaseModel, Field, model_validator


# ---------------------------------------------------------------------------
# ParametricRange
# ---------------------------------------------------------------------------


class ParametricRange(BaseModel):
    """A one-dimensional parametric range using np.linspace spacing.

    Generates ``steps`` evenly-spaced values from ``min`` to ``max``
    inclusive. Equivalent to ``np.linspace(min, max, steps)``.

    Example::

        pr = ParametricRange(min=10.0, max=20.0, steps=3)
        list(pr.values())  # [10.0, 15.0, 20.0]
    """

    min: float = Field(..., description="Minimum value of the range")
    max: float = Field(..., description="Maximum value of the range")
    steps: int = Field(..., ge=1, description="Number of evenly-spaced steps")

    @model_validator(mode="after")
    def _validate_range(self) -> "ParametricRange":
        if self.max < self.min:
            raise ValueError(
                f"max ({self.max}) must not be less than min ({self.min})"
            )
        return self

    def values(self) -> list[float]:
        """Return the list of evenly-spaced values in this range."""
        return list(np.linspace(self.min, self.max, self.steps))

    def __repr__(self) -> str:
        return (
            f"ParametricRange(min={self.min}, max={self.max}, "
            f"steps={self.steps})"
        )


# ---------------------------------------------------------------------------
# HullParametricSpace
# ---------------------------------------------------------------------------


class HullParametricSpace(BaseModel):
    """Cartesian product of parametric ranges for hull dimension scaling.

    Each key in ``ranges`` names a scaling factor dimension (e.g.
    ``"length_scale"``, ``"beam_scale"``, ``"draft_scale"``).  The
    ``combinations()`` method yields all cartesian products.

    ``generate_profiles()`` applies each combination to the base hull
    from the catalog, returning ``(variation_id, HullProfile)`` pairs.

    Supported scale keys
    --------------------
    - ``length_scale`` — multiplies ``length_bp`` and all station
      ``x_position`` values
    - ``beam_scale``   — multiplies ``beam`` and all half-breadth ``y``
      offsets in every station
    - ``draft_scale``  — multiplies ``draft`` and all vertical ``z``
      offsets in every station
    - ``depth_scale``  — multiplies ``depth``

    Any unrecognised key in *ranges* or *fixed_params* is stored in the
    combination dict but does not modify the hull geometry.
    """

    base_hull_id: str = Field(
        ..., description="ID of the base hull in the catalog"
    )
    ranges: dict[str, ParametricRange] = Field(
        default_factory=dict,
        description="Parametric range per dimension name",
    )
    fixed_params: dict[str, float] = Field(
        default_factory=dict,
        description="Fixed parameter values applied to every combination",
    )

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def combinations(self) -> Iterator[dict[str, float]]:
        """Yield all parameter combinations as dicts.

        Each dict merges the cartesian product of range values with the
        fixed parameters (fixed params override range values if there is
        a key collision).

        Yields nothing but a single dict with only fixed params when
        ``ranges`` is empty.
        """
        if not self.ranges:
            yield dict(self.fixed_params)
            return

        keys = list(self.ranges.keys())
        value_lists = [self.ranges[k].values() for k in keys]

        for combo_values in itertools.product(*value_lists):
            combo: dict[str, float] = dict(zip(keys, combo_values))
            combo.update(self.fixed_params)
            yield combo

    def generate_profiles(
        self, catalog: Any
    ) -> Iterator[tuple[str, Any]]:
        """Yield (variation_id, HullProfile) pairs for each combination.

        Parameters
        ----------
        catalog:
            A ``HullCatalog`` instance.  The base hull is fetched from
            it via ``catalog.get_hull(self.base_hull_id)``.

        Raises
        ------
        KeyError
            If ``self.base_hull_id`` is not registered in the catalog.
        """
        # Fetch base hull — raises KeyError if not found
        entry = catalog.get_hull(self.base_hull_id)
        base_profile = entry.profile

        for idx, combo in enumerate(self.combinations()):
            variation_id = _make_variation_id(
                self.base_hull_id, idx, combo
            )
            scaled_profile = _scale_profile(base_profile, combo,
                                            variation_id)
            yield variation_id, scaled_profile


# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------


def _make_variation_id(base_id: str, idx: int,
                       combo: dict[str, float]) -> str:
    """Build a deterministic variation ID from base hull + combo values."""
    parts = [f"{k}={v:.4g}" for k, v in sorted(combo.items())]
    suffix = "_".join(parts) if parts else f"var{idx:04d}"
    return f"{base_id}__{suffix}"


def _scale_profile(base: Any, combo: dict[str, float],
                   new_name: str) -> Any:
    """Return a new HullProfile scaled by the factors in *combo*.

    Only the recognised scale keys are applied to hull geometry:
    - ``length_scale``: scale ``length_bp`` and station x-positions
    - ``beam_scale``:   scale ``beam`` and station half-breadth y values
    - ``draft_scale``:  scale ``draft`` and station z offsets
    - ``depth_scale``:  scale ``depth``

    All other keys in *combo* are ignored for geometry purposes.
    """
    from digitalmodel.hydrodynamics.hull_library.profile_schema import (
        HullProfile, HullStation,
    )

    length_scale = combo.get("length_scale", 1.0)
    beam_scale = combo.get("beam_scale", 1.0)
    draft_scale = combo.get("draft_scale", 1.0)
    depth_scale = combo.get("depth_scale", draft_scale)

    new_length_bp = base.length_bp * length_scale
    new_beam = base.beam * beam_scale
    new_draft = base.draft * draft_scale
    new_depth = base.depth * depth_scale

    scaled_stations = []
    for station in base.stations:
        new_x = station.x_position * length_scale
        new_offsets = [
            (z * draft_scale, y * beam_scale)
            for z, y in station.waterline_offsets
        ]
        scaled_stations.append(
            HullStation(
                x_position=new_x,
                waterline_offsets=new_offsets,
            )
        )

    # Copy optional fields, scaling appropriately
    deck_profile = None
    if base.deck_profile is not None:
        deck_profile = [
            (x * length_scale, y * beam_scale)
            for x, y in base.deck_profile
        ]

    keel_profile = None
    if base.keel_profile is not None:
        keel_profile = [
            (x * length_scale, z * draft_scale)
            for x, z in base.keel_profile
        ]

    # Displacement scales with volume ≈ L * B * T (rough proportionality)
    displacement = None
    if base.displacement is not None:
        displacement = (
            base.displacement * length_scale * beam_scale * draft_scale
        )

    return HullProfile(
        name=new_name,
        hull_type=base.hull_type,
        stations=scaled_stations,
        length_bp=new_length_bp,
        beam=new_beam,
        draft=new_draft,
        depth=new_depth,
        source=base.source,
        deck_profile=deck_profile,
        keel_profile=keel_profile,
        block_coefficient=base.block_coefficient,
        displacement=displacement,
    )


# ---------------------------------------------------------------------------
# Public API
# ---------------------------------------------------------------------------

__all__ = [
    "ParametricRange",
    "HullParametricSpace",
]
