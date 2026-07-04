# ABOUTME: API 579-1 Part 6 pitting assessment — pit-field characterization,
# ABOUTME: conservative Level 1 screening, Level 2 via equivalent-LTA bounding.
"""Pitting assessment per the METHODOLOGY of API 579-1/ASME FFS-1 Part 6.

Honesty statement (assessment basis)
------------------------------------
API 579-1 Part 6 Level 1 works by comparing the observed pit field to the
standard's coupled-pit damage charts, and Level 2 computes an RSF from the
pit-couple grid.  Those charts / tabulated coefficients are NOT transcribed
here (no verified copy is available to this implementation, and fabricating
table values is prohibited).  Instead this module implements the documented,
conservative bounding practice that Part 6 itself sanctions for widespread
pitting:

1. **Pit-field characterization** — pure data reduction from a UT C-scan grid:
   pits are connected components of readings below a threshold fraction of
   nominal WT (default 0.90, i.e. > 10 % loss) that stand against a sounder
   background (contrast is inherent to the component labelling: every cell
   bordering a pit reads at/above the threshold).  Reported: pit count, max
   and average pit depth ratio w/t, pit-couple centre-to-centre spacing
   statistics, and pitted-region extent.  Derivation-anchored — no standard
   values involved.

2. **Level 1 screening (conservative closed form)** — consistent with the
   Part 6 Level 1 philosophy (average remaining thickness in the pitted
   region must satisfy the code minimum, and no single pit may approach
   through-wall):

   * average criterion:  t_avg_pit - FCA >= t_min
   * deep-pit criterion: R_t = (t_min_pit - FCA) / t_nom >= R_t_min
     (default R_t_min = 0.20 — a configurable conservative screening
     parameter chosen consistent with the remaining-thickness-ratio
     applicability limits used across API 579-1 metal-loss parts; it is NOT
     a transcription of a Part 6 chart value).

3. **Level 2 via equivalent LTA** — the pit field is bounded by an
   equivalent local thin area: effective uniform depth = average pit depth
   over the pitted cells, longitudinal extent = pitted-region length.  The
   equivalent LTA is then assessed with the EXISTING validated Part 5 Level 2
   engine (:class:`~digitalmodel.asset_integrity.assessment.level2_engine.Level2Engine`,
   Folias/RSF).  Treating a pit field as an equivalent region of local metal
   loss is the documented conservative practice for widespread pitting
   (API 579-1 Part 6 permits assessing pitting as an equivalent LTA per
   Part 5); the result carries an ``assessment_basis`` note stating exactly
   this.

What a full Part 6 implementation would add later: the standard pit-couple
charts (Level 1) and the pit-couple RSF summation (Level 2), which can credit
ligament interaction between pit pairs and is therefore typically less
conservative than the equivalent-LTA bound used here.

Units: inches throughout (grids are normalised by
:class:`~digitalmodel.asset_integrity.assessment.grid_parser.GridParser`).

References (methodology only):
  API 579-1/ASME FFS-1 2021 Edition, Part 6 (pitting corrosion) — assessment
  philosophy and the Part-5-equivalent-LTA bounding option; Part 5 §5.4
  (Level 2 RSF / Folias factor, implemented in ``level2_engine``).
"""

from __future__ import annotations

import math
from dataclasses import dataclass, field
from typing import Optional

import numpy as np
import pandas as pd
from scipy import ndimage

from .level2_engine import Level2Engine

# --- Characterization defaults --------------------------------------------
# Readings below this fraction of nominal WT are pit candidates (>10 % loss).
DEFAULT_PIT_THRESHOLD_FRACTION = 0.90

_ASSESSMENT_BASIS = (
    "Pit field bounded by an equivalent local thin area (effective depth = "
    "average pit depth over the pitted cells, longitudinal extent = "
    "pitted-region length) and assessed with the validated API 579-1 Part 5 "
    "Level 2 LML engine (Folias/RSF). This is the documented conservative "
    "bounding practice API 579-1 Part 6 permits for widespread pitting; the "
    "standard's coupled-pit charts are NOT transcribed here."
)


@dataclass
class PitFieldCharacterization:
    """Pure data reduction of a UT grid's pit morphology (inches)."""

    nominal_wt_in: float
    pit_threshold_in: float        # absolute threshold used (inches)
    pit_count: int                 # connected components below threshold
    pitted_cell_count: int
    total_valid_cells: int
    pitted_cell_fraction: float
    max_pit_depth_in: float        # w_max = t_nom - min reading in any pit
    max_pit_depth_ratio: float     # w_max / t_nom
    avg_pit_depth_in: float        # mean (t_nom - reading) over pitted cells
    avg_pit_depth_ratio: float
    t_min_pit_in: float            # deepest remaining ligament (min reading)
    t_avg_pit_in: float            # mean reading over pitted cells
    background_median_in: float    # median reading of non-pitted cells
    largest_pit_cells: int         # size (cells) of the largest component
    pit_spacing_mean_in: Optional[float]   # nearest-neighbour centre spacing
    pit_spacing_min_in: Optional[float]
    pitted_region_row_count: int   # axial rows spanned by the pit field
    pitted_region_length_in: float # axial extent (rows x row spacing)
    pitted_region_col_count: int   # circumferential columns spanned
    row_spacing_in: float          # inferred from the grid index
    pit_sizes_cells: list = field(default_factory=list)

    def to_dict(self) -> dict:
        """JSON-serialisable summary."""
        d = {k: getattr(self, k) for k in (
            "nominal_wt_in", "pit_threshold_in", "pit_count",
            "pitted_cell_count", "total_valid_cells", "pitted_cell_fraction",
            "max_pit_depth_in", "max_pit_depth_ratio", "avg_pit_depth_in",
            "avg_pit_depth_ratio", "t_min_pit_in", "t_avg_pit_in",
            "background_median_in", "largest_pit_cells",
            "pit_spacing_mean_in", "pit_spacing_min_in",
            "pitted_region_row_count", "pitted_region_length_in",
            "pitted_region_col_count", "row_spacing_in",
        )}
        return d


def characterize_pit_field(
    grid_df: pd.DataFrame,
    nominal_wt_in: float,
    *,
    pit_threshold_fraction: float = DEFAULT_PIT_THRESHOLD_FRACTION,
) -> PitFieldCharacterization:
    """Characterize the pit field in a normalised wall-thickness grid.

    Pits are 4-connected components of cells reading below
    ``pit_threshold_fraction * nominal_wt_in``.  Because components are cut at
    the threshold, every cell bordering a pit reads at or above the threshold
    — i.e. detected pits are genuine local minima with neighbourhood contrast
    by construction.

    Args:
        grid_df: Normalised wall-thickness grid (inches), rows = axial.
        nominal_wt_in: Nominal wall thickness (inches).
        pit_threshold_fraction: Fraction of nominal WT below which a reading
            is a pit candidate (default 0.90).

    Returns:
        :class:`PitFieldCharacterization` (all lengths in inches; spacing in
        grid units scaled by the inferred row spacing).
    """
    values = grid_df.to_numpy(dtype=float)
    valid = np.isfinite(values)
    n_valid = int(valid.sum())
    threshold = pit_threshold_fraction * nominal_wt_in
    row_spacing = Level2Engine._infer_spacing(grid_df.index)

    pit_mask = valid & (values < threshold)
    labels, n_pits = ndimage.label(pit_mask)  # 4-connectivity default

    if n_pits == 0 or n_valid == 0:
        return PitFieldCharacterization(
            nominal_wt_in=nominal_wt_in,
            pit_threshold_in=threshold,
            pit_count=0,
            pitted_cell_count=0,
            total_valid_cells=n_valid,
            pitted_cell_fraction=0.0,
            max_pit_depth_in=0.0,
            max_pit_depth_ratio=0.0,
            avg_pit_depth_in=0.0,
            avg_pit_depth_ratio=0.0,
            t_min_pit_in=float(nominal_wt_in),
            t_avg_pit_in=float(nominal_wt_in),
            background_median_in=(
                float(np.median(values[valid])) if n_valid else float(nominal_wt_in)
            ),
            largest_pit_cells=0,
            pit_spacing_mean_in=None,
            pit_spacing_min_in=None,
            pitted_region_row_count=0,
            pitted_region_length_in=0.0,
            pitted_region_col_count=0,
            row_spacing_in=row_spacing,
            pit_sizes_cells=[],
        )

    pitted_values = values[pit_mask]
    sizes = np.bincount(labels.ravel())[1:]  # per-component cell counts

    # Pit-couple spacing: nearest-neighbour distance between pit centroids,
    # in grid-cell units scaled by the row spacing (columns treated at the
    # same pitch — a documented simplification of the physical arc length).
    centroids = np.asarray(
        ndimage.center_of_mass(pit_mask, labels, range(1, n_pits + 1)),
        dtype=float,
    )
    spacing_mean: Optional[float] = None
    spacing_min: Optional[float] = None
    if n_pits >= 2:
        deltas = centroids[:, None, :] - centroids[None, :, :]
        dist = np.sqrt((deltas ** 2).sum(axis=2))
        np.fill_diagonal(dist, np.inf)
        nearest = dist.min(axis=1) * row_spacing
        spacing_mean = float(nearest.mean())
        spacing_min = float(nearest.min())

    pit_rows = np.where(pit_mask.any(axis=1))[0]
    pit_cols = np.where(pit_mask.any(axis=0))[0]
    row_count = int(pit_rows[-1] - pit_rows[0] + 1)
    col_count = int(pit_cols[-1] - pit_cols[0] + 1)

    t_min_pit = float(pitted_values.min())
    t_avg_pit = float(pitted_values.mean())
    background = values[valid & ~pit_mask]

    return PitFieldCharacterization(
        nominal_wt_in=nominal_wt_in,
        pit_threshold_in=threshold,
        pit_count=int(n_pits),
        pitted_cell_count=int(pit_mask.sum()),
        total_valid_cells=n_valid,
        pitted_cell_fraction=float(pit_mask.sum()) / float(n_valid),
        max_pit_depth_in=float(nominal_wt_in - t_min_pit),
        max_pit_depth_ratio=float(nominal_wt_in - t_min_pit) / nominal_wt_in,
        avg_pit_depth_in=float(nominal_wt_in - t_avg_pit),
        avg_pit_depth_ratio=float(nominal_wt_in - t_avg_pit) / nominal_wt_in,
        t_min_pit_in=t_min_pit,
        t_avg_pit_in=t_avg_pit,
        background_median_in=(
            float(np.median(background)) if background.size else t_avg_pit
        ),
        largest_pit_cells=int(sizes.max()),
        pit_spacing_mean_in=spacing_mean,
        pit_spacing_min_in=spacing_min,
        pitted_region_row_count=row_count,
        pitted_region_length_in=float(row_count) * row_spacing,
        pitted_region_col_count=col_count,
        row_spacing_in=row_spacing,
        pit_sizes_cells=[int(s) for s in sizes],
    )


# --- Level 1 ---------------------------------------------------------------

# Conservative deep-pit screening floor: minimum remaining thickness ratio at
# the deepest pit, R_t = (t_min_pit - FCA) / t_nom.  Configurable screening
# parameter chosen consistent with the remaining-thickness-ratio applicability
# limits used across the API 579-1 metal-loss parts — NOT a Part 6 chart value.
DEFAULT_RT_MIN = 0.20


def screen_pitting_level1(
    characterization: PitFieldCharacterization,
    *,
    t_min_in: float,
    fca_in: float = 0.0,
    rt_min: float = DEFAULT_RT_MIN,
) -> dict:
    """Conservative closed-form Level 1 pitting screen.

    Consistent with the API 579-1 Part 6 Level 1 philosophy (widespread
    pitting screened on average remaining thickness, with a separate limit on
    the deepest single pit) WITHOUT using the standard's pit charts:

    * average criterion:  t_avg_pit - FCA >= t_min
    * deep-pit criterion: R_t = (t_min_pit - FCA) / t_nom >= rt_min

    Args:
        characterization: output of :func:`characterize_pit_field`.
        t_min_in: code-required minimum wall thickness (inches).
        fca_in: future corrosion allowance (inches).
        rt_min: deep-pit remaining-thickness-ratio floor (default 0.20,
            conservative screening parameter — see module docstring).

    Returns:
        dict with ``verdict`` ('ACCEPT'/'FAIL_LEVEL_1'), both criteria, and
        the numbers behind them.
    """
    c = characterization
    if c.pit_count == 0:
        return {
            "verdict": "ACCEPT",
            "average_criterion_pass": True,
            "deep_pit_criterion_pass": True,
            "t_avg_pit_in": c.t_avg_pit_in,
            "t_min_in": t_min_in,
            "fca_in": fca_in,
            "rt_deepest_pit": 1.0,
            "rt_min": rt_min,
            "basis": "No readings below the pit threshold — no pitting to screen.",
        }

    avg_ok = (c.t_avg_pit_in - fca_in) >= t_min_in
    rt_deepest = (c.t_min_pit_in - fca_in) / c.nominal_wt_in
    deep_ok = rt_deepest >= rt_min

    return {
        "verdict": "ACCEPT" if (avg_ok and deep_ok) else "FAIL_LEVEL_1",
        "average_criterion_pass": bool(avg_ok),
        "deep_pit_criterion_pass": bool(deep_ok),
        "t_avg_pit_in": c.t_avg_pit_in,
        "t_min_in": t_min_in,
        "fca_in": fca_in,
        "rt_deepest_pit": float(rt_deepest),
        "rt_min": rt_min,
        "basis": (
            "Conservative closed-form screen (average pitted-region thickness "
            "vs t_min; deepest-pit remaining-thickness ratio vs floor) "
            "consistent with API 579-1 Part 6 Level 1 philosophy; standard "
            "pit charts not transcribed."
        ),
    }


# --- Level 2 (equivalent LTA) ----------------------------------------------

def assess_pitting_level2_equivalent_lta(
    characterization: PitFieldCharacterization,
    *,
    nominal_od_in: float,
    nominal_wt_in: float,
    t_min_in: float,
    rsf_a: float = 0.9,
) -> dict:
    """Level 2 pitting assessment by bounding the pit field as an LTA.

    The pit field is replaced by an equivalent local thin area of uniform
    thickness ``t_avg_pit`` (average reading over the pitted cells) spanning
    the pitted region's axial extent, and assessed with the existing validated
    Part 5 Level 2 engine (Folias factor / RSF).

    Returns:
        The Level 2 result dict (same keys as
        :meth:`Level2Engine.evaluate` for LML) with
        ``assessment_type='PITTING'``, an ``assessment_basis`` note, and an
        ``equivalent_lta`` sub-dict describing the bounding region.
    """
    c = characterization
    if c.pit_count == 0:
        return {
            "verdict": "ACCEPT",
            "rsf": 1.0,
            "rsf_a": rsf_a,
            "t_am_in": c.t_avg_pit_in,
            "t_mm_in": c.t_min_pit_in,
            "assessment_type": "PITTING",
            "folias_factor": 1.0,
            "flaw_length_in": 0.0,
            "rt": 1.0,
            "assessment_basis": (
                "No readings below the pit threshold — no equivalent LTA "
                "required; RSF = 1.0."
            ),
            "equivalent_lta": None,
        }

    n_rows = max(int(c.pitted_region_row_count), 1)
    # Equivalent LTA grid: uniform effective thickness over the pitted-region
    # axial extent; index carries the physical row spacing so the LML engine
    # recovers the correct flaw length.
    eq_grid = pd.DataFrame(
        np.full((n_rows, 1), c.t_avg_pit_in),
        index=np.arange(n_rows, dtype=float) * c.row_spacing_in,
    )

    engine = Level2Engine(
        assessment_type="LML",
        nominal_od_in=nominal_od_in,
        nominal_wt_in=nominal_wt_in,
        t_min_in=t_min_in,
        rsf_a=rsf_a,
    )
    result = engine.evaluate(eq_grid)

    result["assessment_type"] = "PITTING"
    result["assessment_basis"] = _ASSESSMENT_BASIS
    result["equivalent_lta"] = {
        "effective_thickness_in": c.t_avg_pit_in,
        "effective_depth_in": c.avg_pit_depth_in,
        "axial_extent_in": c.pitted_region_length_in,
        "axial_rows": n_rows,
        "row_spacing_in": c.row_spacing_in,
        "source_pit_count": c.pit_count,
    }
    return result


# --- One-call convenience ---------------------------------------------------

def assess_pitting(
    grid_df: pd.DataFrame,
    *,
    nominal_od_in: float,
    nominal_wt_in: float,
    t_min_in: float,
    rsf_a: float = 0.9,
    fca_in: float = 0.0,
    pit_threshold_fraction: float = DEFAULT_PIT_THRESHOLD_FRACTION,
    rt_min: float = DEFAULT_RT_MIN,
) -> dict:
    """Characterize + Level 1 screen + Level 2 equivalent-LTA in one call.

    Returns:
        dict with keys ``characterization`` (:class:`PitFieldCharacterization`),
        ``level1`` (dict), ``level2`` (dict incl. ``assessment_basis``).
    """
    char = characterize_pit_field(
        grid_df, nominal_wt_in, pit_threshold_fraction=pit_threshold_fraction
    )
    l1 = screen_pitting_level1(char, t_min_in=t_min_in, fca_in=fca_in, rt_min=rt_min)
    l2 = assess_pitting_level2_equivalent_lta(
        char,
        nominal_od_in=nominal_od_in,
        nominal_wt_in=nominal_wt_in,
        t_min_in=t_min_in,
        rsf_a=rsf_a,
    )
    return {"characterization": char, "level1": l1, "level2": l2}
