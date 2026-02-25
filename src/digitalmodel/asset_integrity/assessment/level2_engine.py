"""FFS Level 2 detailed assessment — RSF and Folias-factor calculations.

Implements API 579-1/ASME FFS-1 Level 2 acceptance criterion:

  RSF >= RSFa  →  ACCEPT
  RSF <  RSFa  →  FAIL_LEVEL_2

Two assessment paths:

GML (General Metal Loss, Part 4):
  RSF = t_am / t_c
  where t_am = area-averaged measured thickness over the assessment region,
        t_c  = future thickness (= t_min for conservatism at current FCA = 0).
  Simplified per API 579 Part 4 Level 2 Section 4.4.2.

LML (Local Metal Loss, Part 5):
  RSF = t_mm / (t_c * (1 - (1 - Rt) / Mt))     [equivalent to Part 5 eq.]
  where Rt  = (t_mm) / t_c                       (thickness ratio)
        Mt  = Folias (bulging) factor (Eq. 4.20 / Table 4.4)
        L_a = longitudinal flaw length (derived from flaw bounding box)

The Folias factor M_t for a cylindrical shell (API 579 Eq. 4.20):
  For lambda <= 0.354:   M_t = 1.0
  For lambda in (0.354, 20]:
      M_t = sqrt(1 + 0.48 * lambda^2)   [Table 4.4 simplified column]
  For lambda > 20:        M_t = 1.0 / Rt  (very long flaw → Rt floor = 0.9 RSF)

  lambda = 1.285 * L_a / sqrt(D * t_c)
  where D = nominal inside diameter = OD - 2*t_c (approximately OD - 2*t_min)

RSFa default = 0.9 per API 579 Annex 2B.

References:
  API 579-1/ASME FFS-1 2021 Edition, Part 4 §4.4, Part 5 §5.4
"""

from __future__ import annotations

import math

import numpy as np
import pandas as pd

_SUPPORTED_TYPES = {"GML", "LML"}

# Folias factor breakpoints per API 579 Table 4.4 simplified cylindrical column
_LAMBDA_LOWER = 0.354
_LAMBDA_UPPER = 20.0


class Level2Engine:
    """Perform a Level 2 FFS assessment against RSFa.

    Args:
        assessment_type: ``'GML'`` or ``'LML'``.
        nominal_od_in: Nominal outside diameter (inches).
        nominal_wt_in: Nominal wall thickness (inches).
        t_min_in: Code-required minimum wall thickness (inches).
        rsf_a: Allowable remaining strength factor (default 0.9).

    Raises:
        ValueError: If assessment_type is not ``'GML'`` or ``'LML'``.
    """

    def __init__(
        self,
        assessment_type: str,
        nominal_od_in: float,
        nominal_wt_in: float,
        t_min_in: float,
        rsf_a: float = 0.9,
    ) -> None:
        normalised = assessment_type.strip().upper()
        if normalised not in _SUPPORTED_TYPES:
            raise ValueError(
                f"assessment_type '{assessment_type}' is not supported.  "
                f"Choose from: {sorted(_SUPPORTED_TYPES)}"
            )
        self._type = normalised
        self._od = nominal_od_in
        self._nominal_wt = nominal_wt_in
        self._t_min = t_min_in
        self._rsf_a = rsf_a

    # ------------------------------------------------------------------
    # Public interface
    # ------------------------------------------------------------------

    def evaluate(self, grid_df: pd.DataFrame) -> dict:
        """Run the Level 2 assessment on the parsed wall-thickness grid.

        Args:
            grid_df: Normalised wall-thickness DataFrame (inches).

        Returns:
            dict with keys:
                verdict (str): ``'ACCEPT'`` or ``'FAIL_LEVEL_2'``
                rsf (float): Computed remaining strength factor
                rsf_a (float): Allowable RSF threshold
                t_am_in (float): Area-averaged thickness (inches)
                t_mm_in (float): Minimum measured thickness (inches)
                assessment_type (str): ``'GML'`` or ``'LML'``
                folias_factor (float): M_t (always present; 1.0 for pure GML)
        """
        if self._type == "GML":
            return self._evaluate_gml(grid_df)
        return self._evaluate_lml(grid_df)

    # ------------------------------------------------------------------
    # GML assessment (Part 4 Level 2)
    # ------------------------------------------------------------------

    def _evaluate_gml(self, grid_df: pd.DataFrame) -> dict:
        """Part 4 Level 2: area-averaged RSF for general metal loss.

        RSF = t_am / t_c  (t_c = t_min for current assessment)
        """
        t_am = float(grid_df.mean(skipna=True).mean())
        t_mm = float(grid_df.min(skipna=True).min())
        t_c = self._t_min  # conservative: use t_min as the reference

        rsf = t_am / t_c if t_c > 0 else float("inf")
        rsf = min(rsf, 1.0)  # RSF cannot exceed 1.0

        verdict = "ACCEPT" if rsf >= self._rsf_a else "FAIL_LEVEL_2"

        return {
            "verdict": verdict,
            "rsf": rsf,
            "rsf_a": self._rsf_a,
            "t_am_in": t_am,
            "t_mm_in": t_mm,
            "assessment_type": "GML",
            "folias_factor": 1.0,  # not used for GML area-averaging
        }

    # ------------------------------------------------------------------
    # LML assessment (Part 5 Level 2)
    # ------------------------------------------------------------------

    def _evaluate_lml(self, grid_df: pd.DataFrame) -> dict:
        """Part 5 Level 2: localised RSF with Folias factor.

        1. Identify the flaw bounding box (cells below threshold).
        2. Compute flaw longitudinal length L_a from bounding box rows.
        3. Compute Folias factor M_t.
        4. Compute RSF = Rt / (1 - (1 - Rt) / M_t).
        """
        t_mm = float(grid_df.min(skipna=True).min())
        t_am = float(grid_df.mean(skipna=True).mean())

        # t_c: future thickness outside the flaw = t_min (conservative)
        t_c = self._t_min

        # Flaw characterisation: bounding box of cells below threshold
        threshold = self._nominal_wt * 0.9
        flaw_mask = grid_df < threshold

        # Longitudinal flaw extent in grid rows (integer count)
        flaw_rows = flaw_mask.any(axis=1)
        flaw_row_count = int(flaw_rows.sum())

        # Row spacing: derive from index if numeric; default to 1.0 (unit index)
        row_spacing = self._infer_spacing(grid_df.index)
        l_a = max(flaw_row_count * row_spacing, row_spacing)

        # Folias factor
        nominal_id = self._od - 2.0 * t_c
        mt = self._folias_factor(l_a, nominal_id, t_c)

        # Thickness ratio Rt = t_mm / t_c
        rt = t_mm / t_c if t_c > 0 else 1.0
        rt = min(rt, 1.0)  # cap at 1.0

        # RSF per API 579 Part 5 Level 2 Eq. (5.13)
        denominator = 1.0 - (1.0 - rt) / mt
        if denominator <= 0:
            rsf = 0.0
        else:
            rsf = rt / denominator
        rsf = min(rsf, 1.0)

        verdict = "ACCEPT" if rsf >= self._rsf_a else "FAIL_LEVEL_2"

        return {
            "verdict": verdict,
            "rsf": rsf,
            "rsf_a": self._rsf_a,
            "t_am_in": t_am,
            "t_mm_in": t_mm,
            "assessment_type": "LML",
            "folias_factor": mt,
            "flaw_length_in": l_a,
            "rt": rt,
        }

    # ------------------------------------------------------------------
    # Static helpers
    # ------------------------------------------------------------------

    @staticmethod
    def _folias_factor(l_a: float, nominal_id: float, t_c: float) -> float:
        """Compute the Folias bulging factor M_t per API 579 Table 4.4.

        lambda = 1.285 * L_a / sqrt(D * t_c)

        For lambda <= 0.354:          M_t = 1.0
        For lambda in (0.354, 20]:    M_t = sqrt(1 + 0.48 * lambda**2)
        For lambda > 20:              M_t → large (cap at formula limit)

        Args:
            l_a: Flaw longitudinal extent (inches).
            nominal_id: Nominal inside diameter (inches).
            t_c: Reference thickness (inches).

        Returns:
            Folias factor M_t (always >= 1.0).
        """
        if nominal_id <= 0 or t_c <= 0:
            return 1.0

        lam = 1.285 * l_a / math.sqrt(nominal_id * t_c)

        if lam <= _LAMBDA_LOWER:
            mt = 1.0
        elif lam <= _LAMBDA_UPPER:
            mt = math.sqrt(1.0 + 0.48 * lam ** 2)
        else:
            # Long flaw: use upper-bound approximation
            mt = math.sqrt(1.0 + 0.48 * _LAMBDA_UPPER ** 2)

        return max(mt, 1.0)

    @staticmethod
    def _infer_spacing(index: pd.Index) -> float:
        """Infer the row spacing from a numeric index, defaulting to 1.0."""
        try:
            idx_vals = [float(v) for v in index]
        except (TypeError, ValueError):
            return 1.0
        if len(idx_vals) < 2:
            return 1.0
        gaps = [idx_vals[i + 1] - idx_vals[i] for i in range(len(idx_vals) - 1)]
        positive_gaps = [g for g in gaps if g > 0]
        if not positive_gaps:
            return 1.0
        return min(positive_gaps)
