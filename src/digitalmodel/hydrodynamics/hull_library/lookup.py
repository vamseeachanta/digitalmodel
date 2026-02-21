"""
ABOUTME: Nearest-neighbour hull form lookup by target vessel dimensions.
ABOUTME: Returns closest matching hull with similarity score and scaling factors.
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Any, Optional, Union

from .catalog import HullCatalog, HullCatalogEntry
from .panel_catalog import PanelCatalog, PanelCatalogEntry


# ---------------------------------------------------------------------------
# Built-in fallback hull set
# ---------------------------------------------------------------------------

_BUILTIN_HULLS: list[dict] = [
    {
        "hull_id": "FST-100",
        "loa_m": 100.0,
        "beam_m": 18.0,
        "draft_m": 5.5,
        "displacement_t": 3000,
    },
    {
        "hull_id": "FST-150",
        "loa_m": 150.0,
        "beam_m": 25.0,
        "draft_m": 7.0,
        "displacement_t": 8000,
    },
    {
        "hull_id": "LNGC-250",
        "loa_m": 250.0,
        "beam_m": 43.0,
        "draft_m": 11.5,
        "displacement_t": 90000,
    },
    {
        "hull_id": "LNGC-300",
        "loa_m": 300.0,
        "beam_m": 50.0,
        "draft_m": 13.0,
        "displacement_t": 130000,
    },
    {
        "hull_id": "FPSO-260",
        "loa_m": 260.0,
        "beam_m": 46.0,
        "draft_m": 14.0,
        "displacement_t": 110000,
    },
    {
        "hull_id": "FPSO-320",
        "loa_m": 320.0,
        "beam_m": 60.0,
        "draft_m": 18.0,
        "displacement_t": 220000,
    },
    {
        "hull_id": "BOX-50",
        "loa_m": 50.0,
        "beam_m": 12.0,
        "draft_m": 2.0,
        "displacement_t": 300,
    },
    {
        "hull_id": "SEMI-100",
        "loa_m": 100.0,
        "beam_m": 80.0,
        "draft_m": 22.0,
        "displacement_t": 50000,
    },
]

# Normalisation reference values (order-of-magnitude typical for the fleet)
_NORM_LOA_M = 200.0
_NORM_BEAM_M = 40.0
_NORM_DRAFT_M = 12.0


# ---------------------------------------------------------------------------
# Data classes
# ---------------------------------------------------------------------------


@dataclass
class HullLookupTarget:
    """Target vessel dimensions for hull form lookup."""

    loa_m: float
    beam_m: float
    draft_m: float
    displacement_t: Optional[float] = None


@dataclass
class HullMatch:
    """Result of a nearest-neighbour hull form lookup."""

    hull_id: str
    similarity_score: float
    scaling_factors: dict
    matched_entry: Any
    source: str


# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------


def _normalised_distance(
    target: HullLookupTarget,
    loa_m: float,
    beam_m: float,
    draft_m: float,
) -> float:
    """Compute normalised L2 distance between target and a candidate hull.

    Each dimension is normalised by a reference value to make the
    three axes comparable before computing the Euclidean distance.
    """
    d_loa = (target.loa_m - loa_m) / _NORM_LOA_M
    d_beam = (target.beam_m - beam_m) / _NORM_BEAM_M
    d_draft = (target.draft_m - draft_m) / _NORM_DRAFT_M
    return math.sqrt(d_loa**2 + d_beam**2 + d_draft**2)


def _distance_to_score(distance: float) -> float:
    """Convert a non-negative L2 distance to a similarity score in [0, 1].

    Uses a decaying exponential so that:
    - distance = 0.0  → score = 1.0 (exact match)
    - distance → inf  → score → 0.0
    """
    return math.exp(-distance)


def _scaling_factors(
    target: HullLookupTarget,
    loa_m: float,
    beam_m: float,
    draft_m: float,
) -> dict:
    """Return per-dimension scale factors to resize the candidate to the target."""
    return {
        "loa": target.loa_m / loa_m if loa_m > 0 else 1.0,
        "beam": target.beam_m / beam_m if beam_m > 0 else 1.0,
        "draft": target.draft_m / draft_m if draft_m > 0 else 1.0,
    }


def _validate_target(target: HullLookupTarget) -> None:
    """Raise ValueError for missing or non-positive required dimensions."""
    if target.loa_m is None or target.loa_m <= 0:
        raise ValueError("loa_m must be a positive number")
    if target.beam_m is None or target.beam_m <= 0:
        raise ValueError("beam_m must be a positive number")
    if target.draft_m is None or target.draft_m <= 0:
        raise ValueError("draft_m must be a positive number")


# ---------------------------------------------------------------------------
# Lookup engine
# ---------------------------------------------------------------------------


class HullLookup:
    """Nearest-neighbour hull form lookup.

    Uses normalised L2 distance across [loa, beam, draft] dimensions to
    rank candidate hulls. Falls back to the built-in hull set when the
    supplied catalog contains no entries with valid dimensions.

    Args:
        catalog: Optional ``HullCatalog`` instance whose registered profiles
                 are used as candidates. When *None* or empty, the built-in
                 fallback set is used instead.
    """

    def __init__(
        self,
        catalog: Optional[Union[HullCatalog, PanelCatalog]] = None,
    ) -> None:
        self._catalog = catalog
        self._candidates: list[dict] = self._build_candidates()

    # ------------------------------------------------------------------
    # Public API
    # ------------------------------------------------------------------

    def find_closest(
        self, target: HullLookupTarget, n: int = 3
    ) -> list[HullMatch]:
        """Return the top-*n* closest hull forms, ordered by similarity.

        Args:
            target: Target vessel dimensions.
            n: Number of results to return.

        Returns:
            List of ``HullMatch`` objects, best match first.

        Raises:
            ValueError: If required dimensions (loa_m, beam_m, draft_m)
                        are missing or non-positive.
        """
        _validate_target(target)
        scored = self._score_all(target)
        scored.sort(key=lambda x: x.similarity_score, reverse=True)
        return scored[:n]

    def get_hull_form(self, target: HullLookupTarget) -> HullMatch:
        """Return the single best-matching hull form.

        Args:
            target: Target vessel dimensions.

        Returns:
            Best ``HullMatch``.

        Raises:
            ValueError: If required dimensions are missing or non-positive.
        """
        matches = self.find_closest(target, n=1)
        return matches[0]

    # ------------------------------------------------------------------
    # Private helpers
    # ------------------------------------------------------------------

    def _build_candidates(self) -> list[dict]:
        """Extract candidate records from the catalog or fall back to built-ins."""
        candidates: list[dict] = []

        if self._catalog is not None:
            if isinstance(self._catalog, HullCatalog):
                candidates = self._candidates_from_hull_catalog(self._catalog)
            elif isinstance(self._catalog, PanelCatalog):
                candidates = self._candidates_from_panel_catalog(self._catalog)

        if not candidates:
            candidates = [
                {
                    "hull_id": h["hull_id"],
                    "loa_m": h["loa_m"],
                    "beam_m": h["beam_m"],
                    "draft_m": h["draft_m"],
                    "entry": h,
                    "source": "builtin",
                }
                for h in _BUILTIN_HULLS
            ]

        return candidates

    @staticmethod
    def _candidates_from_hull_catalog(
        catalog: HullCatalog,
    ) -> list[dict]:
        """Build candidate list from a ``HullCatalog``."""
        result = []
        for hull_id in catalog.list_hulls():
            entry: HullCatalogEntry = catalog.get_hull(hull_id)
            p = entry.profile
            if p.length_bp > 0 and p.beam > 0 and p.draft > 0:
                result.append(
                    {
                        "hull_id": hull_id,
                        "loa_m": p.length_bp,
                        "beam_m": p.beam,
                        "draft_m": p.draft,
                        "entry": entry,
                        "source": "catalog",
                    }
                )
        return result

    @staticmethod
    def _candidates_from_panel_catalog(
        catalog: PanelCatalog,
    ) -> list[dict]:
        """Build candidate list from a ``PanelCatalog``."""
        result = []
        for entry in catalog.entries:
            loa = entry.length_m
            beam = entry.beam_m
            draft = entry.draft_m
            if loa and beam and draft and loa > 0 and beam > 0 and draft > 0:
                result.append(
                    {
                        "hull_id": entry.hull_id,
                        "loa_m": loa,
                        "beam_m": beam,
                        "draft_m": draft,
                        "entry": entry,
                        "source": "panel_catalog",
                    }
                )
        return result

    def _score_all(self, target: HullLookupTarget) -> list[HullMatch]:
        """Score every candidate and return unsorted list of ``HullMatch``."""
        results = []
        for cand in self._candidates:
            dist = _normalised_distance(
                target, cand["loa_m"], cand["beam_m"], cand["draft_m"]
            )
            score = _distance_to_score(dist)
            factors = _scaling_factors(
                target, cand["loa_m"], cand["beam_m"], cand["draft_m"]
            )
            results.append(
                HullMatch(
                    hull_id=cand["hull_id"],
                    similarity_score=score,
                    scaling_factors=factors,
                    matched_entry=cand["entry"],
                    source=cand["source"],
                )
            )
        return results


# ---------------------------------------------------------------------------
# Convenience function
# ---------------------------------------------------------------------------


def get_hull_form(target_dimensions: dict) -> HullMatch:
    """Convenience function for one-shot hull form lookup.

    Args:
        target_dimensions: Dict with keys ``loa_m``, ``beam_m``, ``draft_m``
                           and optionally ``displacement_t``.

    Returns:
        Best-matching ``HullMatch``.

    Raises:
        ValueError: If required keys are missing or dimensions are invalid.

    Example::

        match = get_hull_form({"loa_m": 250, "beam_m": 43, "draft_m": 11.5})
        print(match.hull_id)           # "LNGC-250"
        print(match.similarity_score)  # close to 1.0
    """
    required = ("loa_m", "beam_m", "draft_m")
    missing = [k for k in required if k not in target_dimensions]
    if missing:
        raise ValueError(
            f"target_dimensions is missing required keys: {missing}"
        )

    target = HullLookupTarget(
        loa_m=target_dimensions["loa_m"],
        beam_m=target_dimensions["beam_m"],
        draft_m=target_dimensions["draft_m"],
        displacement_t=target_dimensions.get("displacement_t"),
    )
    lookup = HullLookup()
    return lookup.get_hull_form(target)


__all__ = [
    "HullLookupTarget",
    "HullMatch",
    "HullLookup",
    "get_hull_form",
]
