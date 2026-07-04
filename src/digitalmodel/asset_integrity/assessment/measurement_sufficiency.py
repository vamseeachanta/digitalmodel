# ABOUTME: Field measurement-sufficiency engine for FFS — tells an inspector
# ABOUTME: whether thickness data is adequate (sufficient / take more / escalate).
"""Measurement-sufficiency engine (FFS Phase 1).

The FFS assessment modules return a *verdict* (ACCEPT / RE_RATE / ...), but a
field inspector also needs to know whether the wall-thickness measurements they
took are **adequate to trust that verdict** — and, if not, exactly what else to
capture *before leaving the site*. This module answers that question so that
re-measurement does not become a new work order / re-mobilisation.

It consumes the outputs of the existing Phase-1 pipeline
(:class:`~...assessment.grid_parser.GridParser`,
:class:`~...assessment.ffs_router.FFSRouter`,
:class:`~...assessment.level1_screener.Level1Screener`,
:class:`~...assessment.level2_engine.Level2Engine`) and returns one of three
field actions:

* ``SUFFICIENT`` — data is adequate and Level 1 passes with margin; the
  inspector can demobilise.
* ``TAKE_MORE`` — capture specific additional readings now (with counts and
  locations) before leaving.
* ``ESCALATE`` — data is adequate but the result needs a higher-level (office
  Level 2 / Level 3) assessment; do not re-measure in the field, hand to
  engineering.

This screening **does not replace** a detailed Level 2/3 assessment; it guides
the field decision.

Thresholds are engineering defaults informed by API 579-1/ASME FFS-1 Part 4
(point-thickness averaging is justified when the coefficient of variation is
small) and common UT practice; they are configurable, not code mandates.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Optional

import numpy as np
import pandas as pd

# RSF below which the component is in repair/replace territory — more field
# readings will not change the need for an engineering (Level 3) assessment.
_SEVERE_RSF_FLOOR = 0.50


@dataclass
class SufficiencyAction:
    """A concrete recommended field action."""

    action: str          # e.g. add_readings | densify_readings | extend_coverage
                         #      | remeasure_min_cell | escalate_level_2 | escalate_level_3
    location: str        # where to act (human-readable)
    rationale: str       # why
    count: Optional[int] = None   # recommended number of additional readings


@dataclass
class SufficiencyResult:
    """Outcome of a measurement-sufficiency evaluation."""

    status: str          # SUFFICIENT | TAKE_MORE | ESCALATE
    confidence: str      # high | medium | low
    reasons: list = field(default_factory=list)
    actions: list = field(default_factory=list)  # list[SufficiencyAction]
    metrics: dict = field(default_factory=dict)


class MeasurementSufficiency:
    """Assess whether a thickness-measurement grid is adequate for an FFS verdict.

    Parameters
    ----------
    min_readings:
        Minimum number of valid thickness readings before area-averaging /
        screening is considered statistically supported (Part 4 favours a
        reasonable sample; 15 is a common practice default).
    max_cov_gml:
        Maximum coefficient of variation (std/mean) for which uniform
        (general metal loss) area-averaging is trustworthy. Above this the
        loss is non-uniform and a local thin spot may be masked.
    ut_tolerance_in:
        Ultrasonic thickness measurement tolerance (inches). If the pass
        margin is smaller than this, the verdict is within measurement error.
    min_lml_profile_cells:
        Minimum number of grid cells that must resolve a local flaw for the
        river-bottom (critical thickness) profile to be considered sampled.
    """

    def __init__(
        self,
        *,
        min_readings: int = 15,
        max_cov_gml: float = 0.10,
        ut_tolerance_in: float = 0.020,
        min_lml_profile_cells: int = 5,
    ) -> None:
        self.min_readings = min_readings
        self.max_cov_gml = max_cov_gml
        self.ut_tolerance_in = ut_tolerance_in
        self.min_lml_profile_cells = min_lml_profile_cells

    # ------------------------------------------------------------------
    def evaluate(
        self,
        grid_df: pd.DataFrame,
        *,
        nominal_wt_in: float,
        assessment_type: str,
        level1_result: dict,
        level2_result: dict,
    ) -> SufficiencyResult:
        """Return a :class:`SufficiencyResult` for the given grid and results.

        Args:
            grid_df: Normalised wall-thickness grid (inches), rows x cols.
            nominal_wt_in: Nominal wall thickness (inches).
            assessment_type: ``'GML'`` or ``'LML'`` (from FFSRouter).
            level1_result: dict from ``Level1Screener.evaluate`` (needs
                ``verdict``, ``t_min_in``, ``margin_in``).
            level2_result: dict from ``Level2Engine.evaluate`` (needs ``rsf``,
                ``rsf_a``, ``t_mm_in``).
        """
        atype = assessment_type.upper().strip()
        metrics = self._metrics(grid_df, nominal_wt_in, level1_result, level2_result)

        rsf = float(level2_result["rsf"])
        rsf_a = float(level2_result["rsf_a"])
        l1_pass = level1_result.get("verdict") == "ACCEPT"

        reasons: list = []
        actions: list = []

        # --- Precedence 1: severe loss → escalate (data won't change need) ---
        if rsf < _SEVERE_RSF_FLOOR:
            reasons.append(
                f"RSF={rsf:.3f} is below the {_SEVERE_RSF_FLOOR:.2f} floor — the "
                "component is in repair/replace territory; a Level 3 / engineering "
                "assessment is required regardless of additional field readings."
            )
            actions.append(SufficiencyAction(
                action="escalate_level_3",
                location="office / engineering review",
                rationale="RSF below safe re-rating floor",
            ))
            # Still useful to map the full extent for repair scoping.
            if metrics["min_on_edge"] and metrics["min_degraded"]:
                actions.append(SufficiencyAction(
                    action="extend_coverage",
                    location=f"beyond grid edge near min cell {metrics['min_location']}",
                    rationale="bound the full extent of loss for repair scoping",
                ))
            return SufficiencyResult(
                status="ESCALATE", confidence="high",
                reasons=reasons, actions=actions, metrics=metrics,
            )

        # --- Precedence 2: data-adequacy gaps → take more readings now ---
        if metrics["n_readings"] < self.min_readings:
            reasons.append(
                f"Only {metrics['n_readings']} valid readings; at least "
                f"{self.min_readings} are recommended before averaging/screening "
                "is statistically supported."
            )
            actions.append(SufficiencyAction(
                action="add_readings",
                count=self.min_readings - metrics["n_readings"],
                location="distributed across the component / CML grid",
                rationale="insufficient reading count",
            ))

        if atype == "GML" and metrics["cov"] is not None and metrics["cov"] > self.max_cov_gml:
            reasons.append(
                f"Thickness scatter COV={metrics['cov']:.2f} exceeds {self.max_cov_gml:.2f}; "
                "loss is non-uniform, so general (area-averaged) assessment may mask a "
                "local thin spot."
            )
            actions.append(SufficiencyAction(
                action="densify_readings",
                location=f"around the minimum-thickness cell {metrics['min_location']}",
                rationale="high scatter — possible localised metal loss",
            ))

        if atype == "LML" and metrics["profile_cells"] < self.min_lml_profile_cells:
            reasons.append(
                f"Local flaw is resolved by only {metrics['profile_cells']} grid cells; "
                "the critical (river-bottom) thickness profile is under-sampled."
            )
            actions.append(SufficiencyAction(
                action="densify_readings",
                count=self.min_lml_profile_cells - metrics["profile_cells"],
                location="across and immediately around the flaw",
                rationale="under-resolved local thickness profile",
            ))

        if metrics["min_on_edge"] and metrics["min_degraded"]:
            reasons.append(
                "The minimum thickness lies on the grid boundary; the thinned region "
                "may continue beyond the measured area."
            )
            actions.append(SufficiencyAction(
                action="extend_coverage",
                location=f"beyond the grid edge near min cell {metrics['min_location']}",
                rationale="boundary minimum — coverage may be incomplete",
            ))

        if (metrics["t_margin_in"] is not None
                and 0.0 <= metrics["t_margin_in"] < self.ut_tolerance_in):
            reasons.append(
                f"Minimum thickness exceeds t_min by only {metrics['t_margin_in']:.3f} in, "
                f"within the UT tolerance of {self.ut_tolerance_in:.3f} in — the pass is "
                "within measurement error."
            )
            actions.append(SufficiencyAction(
                action="remeasure_min_cell",
                count=3,
                location=f"at/around the minimum-thickness cell {metrics['min_location']}",
                rationale="verdict is sensitive to measurement error",
            ))

        if actions:
            confidence = "low" if metrics["n_readings"] < self.min_readings else "medium"
            return SufficiencyResult(
                status="TAKE_MORE", confidence=confidence,
                reasons=reasons, actions=actions, metrics=metrics,
            )

        # --- Precedence 3: data adequate → sufficient or escalate by verdict ---
        if l1_pass and rsf >= rsf_a:
            comfortable = (metrics["t_margin_in"] is None
                           or metrics["t_margin_in"] >= 2.0 * self.ut_tolerance_in)
            reasons.append(
                "Grid coverage and scatter are adequate and Level 1 passes with margin "
                "— the field measurements are sufficient to support the verdict."
            )
            return SufficiencyResult(
                status="SUFFICIENT",
                confidence="high" if comfortable else "medium",
                reasons=reasons, actions=actions, metrics=metrics,
            )

        # Data adequate but Level 1 fails or Level 2 below RSFa → office escalation.
        if not l1_pass and rsf >= rsf_a:
            reasons.append(
                "Level 1 fails (t_mm < t_min) but Level 2 RSF passes — a formal "
                "Level 2 re-rating in the office is warranted; field data is adequate."
            )
            actions.append(SufficiencyAction(
                action="escalate_level_2",
                location="office Level 2 re-rating",
                rationale="Level 1 screening fail with passing Level 2 RSF",
            ))
        else:  # rsf in [floor, rsf_a)
            reasons.append(
                f"RSF={rsf:.3f} is between the {_SEVERE_RSF_FLOOR:.2f} floor and "
                f"RSFa={rsf_a:.2f} — re-rating / Level 2 evaluation in the office; "
                "field data is adequate."
            )
            actions.append(SufficiencyAction(
                action="escalate_level_2",
                location="office Level 2 re-rating",
                rationale="RSF below RSFa but above the replace floor",
            ))
        return SufficiencyResult(
            status="ESCALATE", confidence="high",
            reasons=reasons, actions=actions, metrics=metrics,
        )

    # ------------------------------------------------------------------
    def _metrics(
        self,
        grid_df: pd.DataFrame,
        nominal_wt_in: float,
        level1_result: dict,
        level2_result: dict,
    ) -> dict:
        """Compute grid-adequacy and margin metrics."""
        arr = grid_df.to_numpy(dtype=float)
        n = int(np.count_nonzero(~np.isnan(arr)))
        mean = float(np.nanmean(arr)) if n else 0.0
        std = float(np.nanstd(arr)) if n else 0.0  # population std (ddof=0)
        cov = (std / mean) if (n > 1 and mean > 0) else (0.0 if n else None)

        # Cells that participate in a local flaw (>10% loss from nominal).
        # NaN comparisons evaluate False, so NaN cells are not counted.
        degraded_threshold = nominal_wt_in * 0.90
        profile_cells = int((arr < degraded_threshold).sum()) if n else 0

        # Location of the minimum thickness and whether it sits on a boundary.
        if n:
            r, c = np.unravel_index(np.nanargmin(arr), arr.shape)
            min_thickness = float(arr[r, c])
            on_edge = bool(
                r == 0 or r == arr.shape[0] - 1
                or c == 0 or c == arr.shape[1] - 1
            )
            min_location = (int(r), int(c))   # (row, col) positional indices
            min_degraded = bool(min_thickness < degraded_threshold)
        else:
            min_thickness = None
            on_edge = False
            min_location = None
            min_degraded = False

        # Pass margin in thickness terms (Level 1).
        t_margin_in = level1_result.get("margin_in")
        if t_margin_in is not None:
            t_margin_in = float(t_margin_in)

        return {
            "n_readings": n,
            "cov": cov,
            "min_thickness_in": min_thickness,
            "min_location": min_location,
            "min_on_edge": on_edge,
            "min_degraded": min_degraded,
            "profile_cells": profile_cells,
            "t_margin_in": t_margin_in,
            "rsf": float(level2_result.get("rsf", 0.0)),
            "rsf_a": float(level2_result.get("rsf_a", 0.0)),
        }
