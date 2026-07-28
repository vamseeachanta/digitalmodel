# ABOUTME: Surface-card metrics (PPRL, MPRL, area, PRHP) and the load datum sanity check.
# ABOUTME: A minimum load above the rod string's air weight is flagged, never passed silently.
"""Metrics read directly off a surface dynamometer card.

Also home to :func:`load_datum_check`, which catches a class of bad data that
otherwise propagates silently through an entire analysis: a card whose
minimum polished-rod load exceeds the weight of the rod string hanging in air.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import List, Optional, Sequence

import numpy as np

from .constants import FT_LB_PER_MIN_PER_HP


@dataclass
class CardMetrics:
    """Metrics derived from a closed surface card."""

    peak_load_lb: float               # PPRL
    peak_load_position_in: float
    minimum_load_lb: float            # MPRL
    minimum_load_position_in: float
    load_range_lb: float
    stroke_in: float
    card_area_in_lb: float            # work per stroke
    card_area_ft_lb: float
    polished_rod_hp: float
    warnings: List[str] = field(default_factory=list)


def card_area(position_in: Sequence[float], load_lb: Sequence[float]) -> float:
    """Enclosed area of a closed card by the shoelace formula, inch-pounds.

    This is the net work done at the polished rod per stroke. The absolute
    value is taken so the result does not depend on traversal direction.
    """
    x = np.asarray(position_in, dtype=float)
    y = np.asarray(load_lb, dtype=float)
    if len(x) != len(y):
        raise ValueError(
            f"position ({len(x)}) and load ({len(y)}) must have equal length"
        )
    if len(x) < 3:
        raise ValueError(f"need at least 3 points to enclose an area; got {len(x)}")
    # Close the polygon if the caller did not.
    if x[0] != x[-1] or y[0] != y[-1]:
        x = np.append(x, x[0])
        y = np.append(y, y[0])
    return float(abs(np.sum(x[:-1] * y[1:] - x[1:] * y[:-1])) / 2.0)


def polished_rod_horsepower(
    card_area_in_lb: float, strokes_per_minute: float
) -> float:
    """Polished-rod power, horsepower.

    Work per stroke in foot-pounds times strokes per minute, over 33,000.
    """
    return (card_area_in_lb / 12.0) * strokes_per_minute / FT_LB_PER_MIN_PER_HP


def load_datum_check(
    minimum_load_lb: float,
    rod_weight_in_air_lb: float,
    buoyant_rod_weight_lb: Optional[float] = None,
) -> List[str]:
    """Flag a physically impossible minimum polished-rod load.

    On the downstroke the polished rod carries at most the buoyed rod weight,
    and friction acts *upward* — resisting the descending rod — which lowers
    the reading further. Friction therefore widens a card; it cannot lift the
    whole card. So an MPRL above the rod string's weight in air has no
    mechanical explanation and points at the data or the string record:

    1. A load-cell zero or scale offset (the common case).
    2. A rod string heavier than recorded — extra taper, sinker bars, or a
       length that does not match the paperwork.

    Returns a list of warnings, empty when the card is self-consistent.
    """
    warnings: List[str] = []
    if minimum_load_lb > rod_weight_in_air_lb:
        excess = minimum_load_lb - rod_weight_in_air_lb
        warnings.append(
            f"MPRL ({minimum_load_lb:,.0f} lb) exceeds the rod string's weight "
            f"in air ({rod_weight_in_air_lb:,.0f} lb) by {excess:,.0f} lb. "
            "Friction cannot produce this — it acts upward on the downstroke "
            "and lowers polished-rod load. Candidate causes: (1) load-cell "
            "zero/scale offset; (2) rod string heavier than recorded. Treat "
            "absolute loads as unreliable until resolved; load *differences* "
            "remain usable."
        )
    elif (
        buoyant_rod_weight_lb is not None
        and minimum_load_lb > buoyant_rod_weight_lb
    ):
        excess = minimum_load_lb - buoyant_rod_weight_lb
        warnings.append(
            f"MPRL ({minimum_load_lb:,.0f} lb) exceeds the buoyed rod weight "
            f"({buoyant_rod_weight_lb:,.0f} lb) by {excess:,.0f} lb. Possible "
            "with heavy downhole friction or a partially supported string, "
            "but worth confirming the load-cell calibration."
        )
    return warnings


def analyse_card(
    position_in: Sequence[float],
    load_lb: Sequence[float],
    strokes_per_minute: float,
    rod_weight_in_air_lb: Optional[float] = None,
    buoyant_rod_weight_lb: Optional[float] = None,
) -> CardMetrics:
    """Compute all surface-card metrics and run the datum check.

    Args:
        position_in: Polished-rod position samples, inches.
        load_lb: Polished-rod load samples, pounds.
        strokes_per_minute: Pumping speed.
        rod_weight_in_air_lb: Enables the load datum check when supplied.
        buoyant_rod_weight_lb: Enables the softer buoyed-weight check.
    """
    x = np.asarray(position_in, dtype=float)
    y = np.asarray(load_lb, dtype=float)
    if len(x) != len(y):
        raise ValueError(
            f"position ({len(x)}) and load ({len(y)}) must have equal length"
        )
    if len(x) == 0:
        raise ValueError("card is empty")

    peak_idx = int(np.argmax(y))
    min_idx = int(np.argmin(y))
    area = card_area(x, y)

    warnings: List[str] = []
    if rod_weight_in_air_lb is not None:
        warnings.extend(
            load_datum_check(
                float(y[min_idx]), rod_weight_in_air_lb, buoyant_rod_weight_lb
            )
        )

    return CardMetrics(
        peak_load_lb=float(y[peak_idx]),
        peak_load_position_in=float(x[peak_idx]),
        minimum_load_lb=float(y[min_idx]),
        minimum_load_position_in=float(x[min_idx]),
        load_range_lb=float(y.max() - y.min()),
        stroke_in=float(x.max() - x.min()),
        card_area_in_lb=area,
        card_area_ft_lb=area / 12.0,
        polished_rod_hp=polished_rod_horsepower(area, strokes_per_minute),
        warnings=warnings,
    )
