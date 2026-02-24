"""FFS assessment router — API 579-1 Part 4 (GML) vs Part 5 (LML).

Classification logic follows the applicability criteria of API 579-1/ASME FFS-1
2021 Edition:

* General Metal Loss (GML, Part 4): degradation extends over a sufficiently
  large area that the assessment length criterion L_a governs.  The degraded
  fraction of the grid exceeds the GML_FRACTION_THRESHOLD.

* Local Metal Loss (LML, Part 5): degradation is confined to a localised
  region whose longitudinal extent is small relative to the shell diameter.

The router computes the fraction of grid cells that have degraded below
(nominal_wt - DEGRADED_THRESHOLD_FRACTION * nominal_wt) to separate uniform
from localised loss patterns.  Engineers may override via ``force_type``.

Part 6 (pitting) is out of scope for Phase 1 — the router will raise if pitting
morphology detection logic is invoked.
"""

from __future__ import annotations

import pandas as pd

# Fraction of nominal WT below which a cell is considered "degraded"
_DEGRADED_FRACTION_OF_NOMINAL = 0.10  # >10 % loss counts as degraded cell

# If more than this fraction of all cells are degraded, classify as GML
_GML_FRACTION_THRESHOLD = 0.25

_VALID_FORCE_TYPES = {"GML", "LML"}


class FFSRouter:
    """Classify a wall-thickness grid as GML or LML per API 579 Part 4/5."""

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
            force_type: User override — ``'GML'`` or ``'LML'``.  When
                provided the classification heuristic is bypassed and the
                user-specified type is returned.  Any other value raises
                :class:`ValueError`.

        Returns:
            dict with keys:
                assessment_type (str): ``'GML'`` or ``'LML'``
                degraded_fraction (float): fraction of cells below threshold
                nominal_od_in (float): echoed input
                nominal_wt_in (float): echoed input
                auto_classified (bool): True when heuristic was used

        Raises:
            ValueError: If force_type is not ``'GML'``, ``'LML'``, or None.
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
        assessment_type = (
            "GML" if degraded_fraction >= _GML_FRACTION_THRESHOLD else "LML"
        )

        return {
            "assessment_type": assessment_type,
            "degraded_fraction": degraded_fraction,
            "nominal_od_in": nominal_od_in,
            "nominal_wt_in": nominal_wt_in,
            "auto_classified": True,
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
