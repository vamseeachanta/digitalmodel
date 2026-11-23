"""FFS assessment router — API 579-1 Part 4 (GML) / Part 5 (LML) / Part 6 (pitting).

Classification logic follows the applicability criteria of API 579-1/ASME FFS-1
2021 Edition:

* General Metal Loss (GML, Part 4): degradation extends over a sufficiently
  large area that the assessment length criterion L_a governs.  The degraded
  fraction of the grid exceeds the GML_FRACTION_THRESHOLD.

* Local Metal Loss (LML, Part 5): degradation is confined to a localised
  region whose longitudinal extent is small relative to the shell diameter.

* Pitting (Part 6): many small, mutually separated sharp minima standing
  against an essentially sound background — as opposed to contiguous loss.
  Detected by a simple, documented heuristic (all conditions must hold, so
  grids that previously classified GML/LML keep their classification):

    1. at least ``_PITTING_MIN_PIT_COUNT`` distinct pits (connected
       components of readings below the pit threshold),
    2. the pitted cells cover at most ``_PITTING_MAX_PITTED_FRACTION`` of
       the grid (sharp minima, not area loss),
    3. the largest single pit spans at most
       ``_PITTING_MAX_COMPONENT_FRACTION`` of the grid (no contiguous LTA
       hiding among the pits), and
    4. the background (non-pitted) median thickness is at least
       ``_PITTING_MIN_BACKGROUND_FRACTION`` of nominal (clear contrast).

  Pitting routes to
  :mod:`digitalmodel.asset_integrity.assessment.pitting` (conservative
  Part 6 tracer: characterization + Level 1 screen + Level 2 equivalent-LTA
  bounding).

The router computes the fraction of grid cells that have degraded below
(nominal_wt - DEGRADED_THRESHOLD_FRACTION * nominal_wt) to separate uniform
from localised loss patterns.  Engineers may override via ``force_type``
('GML', 'LML' or 'PITTING').
"""

from __future__ import annotations

import pandas as pd

# Fraction of nominal WT below which a cell is considered "degraded"
_DEGRADED_FRACTION_OF_NOMINAL = 0.10  # >10 % loss counts as degraded cell

# If more than this fraction of all cells are degraded, classify as GML
_GML_FRACTION_THRESHOLD = 0.25

# --- Pitting-morphology heuristic (all must hold; see module docstring) ----
_PITTING_MIN_PIT_COUNT = 5
_PITTING_MAX_PITTED_FRACTION = 0.10
_PITTING_MAX_COMPONENT_FRACTION = 0.02
_PITTING_MIN_BACKGROUND_FRACTION = 0.95

_VALID_FORCE_TYPES = {"GML", "LML", "PITTING"}


class FFSRouter:
    """Classify a wall-thickness grid as GML, LML or PITTING per API 579."""

    @staticmethod
    def classify(
        grid_df: pd.DataFrame,
        nominal_od_in: float,
        nominal_wt_in: float,
        *,
        force_type: str | None = None,
    ) -> dict:
        """Classify the damage morphology and return routing metadata.

        Args:
            grid_df: Normalised wall-thickness grid (inches), rows × cols.
            nominal_od_in: Nominal pipe outside diameter (inches).
            nominal_wt_in: Nominal pipe wall thickness (inches).
            force_type: User override — ``'GML'``, ``'LML'`` or ``'PITTING'``.
                When provided the classification heuristic is bypassed and the
                user-specified type is returned.  Any other value raises
                :class:`ValueError`.

        Returns:
            dict with keys:
                assessment_type (str): ``'GML'``, ``'LML'`` or ``'PITTING'``
                degraded_fraction (float): fraction of cells below threshold
                nominal_od_in (float): echoed input
                nominal_wt_in (float): echoed input
                auto_classified (bool): True when heuristic was used
                pitting (dict): heuristic metrics when auto-classified —
                    detected flag, pit_count, pitted_cell_fraction,
                    largest_pit_fraction, background_median_ratio

        Raises:
            ValueError: If force_type is not ``'GML'``, ``'LML'``,
                ``'PITTING'``, or None.
        """
        if force_type is not None:
            _normalised = force_type.upper().strip()
            if _normalised not in _VALID_FORCE_TYPES:
                raise ValueError(
                    f"force_type '{force_type}' is not valid.  "
                    f"Choose from: {sorted(_VALID_FORCE_TYPES)} or None."
                )
            degraded_fraction = FFSRouter._compute_degraded_fraction(
                grid_df, nominal_wt_in
            )
            return {
                "assessment_type": _normalised,
                "degraded_fraction": degraded_fraction,
                "nominal_od_in": nominal_od_in,
                "nominal_wt_in": nominal_wt_in,
                "auto_classified": False,
            }

        degraded_fraction = FFSRouter._compute_degraded_fraction(
            grid_df, nominal_wt_in
        )

        pitting_metrics = FFSRouter._pitting_morphology(grid_df, nominal_wt_in)
        if pitting_metrics["detected"]:
            assessment_type = "PITTING"
        else:
            assessment_type = (
                "GML" if degraded_fraction >= _GML_FRACTION_THRESHOLD else "LML"
            )

        return {
            "assessment_type": assessment_type,
            "degraded_fraction": degraded_fraction,
            "nominal_od_in": nominal_od_in,
            "nominal_wt_in": nominal_wt_in,
            "auto_classified": True,
            "pitting": pitting_metrics,
        }

    @staticmethod
    def _pitting_morphology(
        grid_df: pd.DataFrame, nominal_wt_in: float
    ) -> dict:
        """Evaluate the documented pitting-morphology heuristic.

        Returns a small metrics dict with a ``detected`` flag.  Conservative
        by design: ALL four conditions must hold, so contiguous-loss grids
        (GML/LML) never flip to PITTING.
        """
        # Local import: pitting depends on level2_engine; keep router import light.
        from .pitting import characterize_pit_field

        char = characterize_pit_field(grid_df, nominal_wt_in)
        total = max(char.total_valid_cells, 1)
        largest_fraction = char.largest_pit_cells / total
        background_ratio = (
            char.background_median_in / nominal_wt_in if nominal_wt_in > 0 else 0.0
        )

        detected = (
            char.pit_count >= _PITTING_MIN_PIT_COUNT
            and 0.0 < char.pitted_cell_fraction <= _PITTING_MAX_PITTED_FRACTION
            and largest_fraction <= _PITTING_MAX_COMPONENT_FRACTION
            and background_ratio >= _PITTING_MIN_BACKGROUND_FRACTION
        )

        return {
            "detected": bool(detected),
            "pit_count": char.pit_count,
            "pitted_cell_fraction": char.pitted_cell_fraction,
            "largest_pit_fraction": largest_fraction,
            "background_median_ratio": background_ratio,
        }

    @staticmethod
    def _compute_degraded_fraction(
        grid_df: pd.DataFrame, nominal_wt_in: float
    ) -> float:
        """Return the fraction of cells that have lost more than threshold WT."""
        threshold = nominal_wt_in * (1.0 - _DEGRADED_FRACTION_OF_NOMINAL)
        total_cells = grid_df.count().sum()
        if total_cells == 0:
            return 0.0
        degraded_cells = (grid_df < threshold).sum().sum()
        return float(degraded_cells) / float(total_cells)
