"""Multi-vessel comparison and screening (Phase 2).

Evaluates N vessels × M structures to produce an operability matrix
and ranking table for vessel selection.

References:
    DNV-ST-N001 (2021) -- Marine Operations and Marine Warranty
"""
from __future__ import annotations

import numpy as np
from dataclasses import dataclass
from .models import Vessel, Structure, InstallationCriteria
from .operability import compute_operability, weather_window_operability


@dataclass
class VesselScreeningResult:
    """Results of multi-vessel screening.

    Attributes
    ----------
    vessels : list[str]
        Vessel names in order.
    structures : list[str]
        Structure names in order.
    operability_matrix_pct : np.ndarray
        Shape (n_vessels, n_structures). Percentage operability.
    hs_limit_matrix_m : np.ndarray
        Shape (n_vessels, n_structures). Governing Hs limit.
    governing_criteria : list[list[str]]
        Which criterion governs for each (vessel, structure) pair.
    rankings : list[dict]
        Vessels ranked by average operability across all structures.
    """

    vessels: list[str]
    structures: list[str]
    operability_matrix_pct: np.ndarray
    hs_limit_matrix_m: np.ndarray
    governing_criteria: list[list[str]]
    rankings: list[dict]


def screen_vessels(
    vessels: list[Vessel],
    structures: list[Structure],
    criteria: InstallationCriteria,
    tp_range_s: np.ndarray,
    heading_deg: float = 0.0,
    scatter_hs: np.ndarray | None = None,
    scatter_tp: np.ndarray | None = None,
    scatter_counts: np.ndarray | None = None,
) -> VesselScreeningResult:
    """Screen multiple vessels against multiple structures.

    Parameters
    ----------
    vessels : list[Vessel]
        Candidate installation vessels with RAOs.
    structures : list[Structure]
        Structures to be installed.
    criteria : InstallationCriteria
        Operational limits.
    tp_range_s : np.ndarray
        Peak periods for operability evaluation [s].
    heading_deg : float
        Wave heading [deg] relative to vessel.
    scatter_hs, scatter_tp, scatter_counts : np.ndarray, optional
        Wave scatter diagram. If provided, computes operability %.
        scatter_counts shape: (n_hs, n_tp).

    Returns
    -------
    VesselScreeningResult
        Complete comparison results with rankings.
    """
    n_v = len(vessels)
    n_s = len(structures)

    op_matrix = np.zeros((n_v, n_s))
    hs_matrix = np.zeros((n_v, n_s))
    gov_criteria: list[list[str]] = []

    has_scatter = scatter_counts is not None and scatter_hs is not None and scatter_tp is not None

    for i, vessel in enumerate(vessels):
        gov_row: list[str] = []
        for j, structure in enumerate(structures):
            result = compute_operability(
                vessel, structure, criteria, tp_range_s, heading_deg
            )
            hs_matrix[i, j] = result.hs_limit_m
            gov_row.append(result.governing_criterion)

            if has_scatter:
                op_matrix[i, j] = weather_window_operability(
                    result, scatter_hs, scatter_tp, scatter_counts
                )
        gov_criteria.append(gov_row)

    # Rank vessels by average operability (or Hs limit if no scatter)
    if has_scatter:
        avg_metric = np.mean(op_matrix, axis=1)
        metric_name = "avg_operability_pct"
    else:
        avg_metric = np.mean(hs_matrix, axis=1)
        metric_name = "avg_hs_limit_m"

    rank_order = np.argsort(-avg_metric)  # Descending
    rankings = [
        {
            "rank": rank + 1,
            "vessel": vessels[idx].name,
            metric_name: float(avg_metric[idx]),
        }
        for rank, idx in enumerate(rank_order)
    ]

    return VesselScreeningResult(
        vessels=[v.name for v in vessels],
        structures=[s.name for s in structures],
        operability_matrix_pct=op_matrix,
        hs_limit_matrix_m=hs_matrix,
        governing_criteria=gov_criteria,
        rankings=rankings,
    )
