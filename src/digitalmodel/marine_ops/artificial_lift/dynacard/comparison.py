# ABOUTME: Phase-invariant comparison metrics for closed dynamometer cards.
# ABOUTME: Traversal direction and sample origin are canonicalized before scoring.
"""Compare dynamometer cards without assigning meaning to storage phase."""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np

from .models import CardData


@dataclass(frozen=True)
class CardComparison:
    """Percentage errors for an actual card against a reference card."""

    load_nrmse_pct: float
    position_nrmse_pct: float
    peak_load_error_pct: float
    minimum_load_error_pct: float
    stroke_error_pct: float
    enclosed_area_error_pct: float


def _card_arrays(card: CardData, name: str) -> tuple[np.ndarray, np.ndarray]:
    position = np.asarray(card.position, dtype=np.float64)
    load = np.asarray(card.load, dtype=np.float64)
    if position.ndim != 1 or load.ndim != 1:
        raise ValueError(f"{name} card position and load must be one-dimensional")
    if len(position) != len(load):
        raise ValueError(
            f"{name} card position ({len(position)}) and load ({len(load)}) "
            "must have equal length"
        )
    if len(position) < 3:
        raise ValueError(f"{name} card needs at least 3 points; got {len(position)}")
    if not np.all(np.isfinite(position)) or not np.all(np.isfinite(load)):
        raise ValueError(f"{name} card contains non-finite values")
    return position, load


def _signed_area(position: np.ndarray, load: np.ndarray) -> float:
    return 0.5 * float(
        np.sum(position * np.roll(load, -1) - np.roll(position, -1) * load)
    )


def canonicalize_card_direction(
    position: np.ndarray,
    load: np.ndarray,
    *,
    clockwise: bool = False,
) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Return card arrays and source indices in one signed-area direction."""
    source_indices = np.arange(len(position))
    area = _signed_area(position, load)
    reverse = area > 0.0 if clockwise else area < 0.0
    if reverse:
        return position[::-1], load[::-1], source_indices[::-1]
    return position, load, source_indices


def _canonical_direction(
    position: np.ndarray, load: np.ndarray
) -> tuple[np.ndarray, np.ndarray]:
    position, load, _ = canonicalize_card_direction(position, load)
    return position, load


def _positive_reference_scale(value: float, metric: str) -> float:
    if value <= np.finfo(np.float64).eps:
        raise ValueError(f"reference card has zero {metric}; percentage is undefined")
    return value


def _relative_error_pct(actual: float, reference: float, metric: str) -> float:
    scale = _positive_reference_scale(abs(reference), metric)
    return 100.0 * abs(actual - reference) / scale


def compare_cards(actual: CardData, reference: CardData) -> CardComparison:
    """Compare two equal-sample closed cards independent of direction and phase.

    Both loops are oriented by the sign of their signed shoelace area. The
    actual card is then circularly shifted to minimise the sum of squared
    position and load errors after each axis is normalised by the corresponding
    reference range.
    """
    actual_position, actual_load = _card_arrays(actual, "actual")
    reference_position, reference_load = _card_arrays(reference, "reference")
    if len(actual_position) != len(reference_position):
        raise ValueError(
            f"actual ({len(actual_position)}) and reference "
            f"({len(reference_position)}) cards must have equal sample counts"
        )

    actual_position, actual_load = _canonical_direction(
        actual_position, actual_load
    )
    reference_position, reference_load = _canonical_direction(
        reference_position, reference_load
    )
    position_range = _positive_reference_scale(
        float(np.ptp(reference_position)), "stroke"
    )
    load_range = _positive_reference_scale(
        float(np.ptp(reference_load)), "load range"
    )

    best_score = np.inf
    aligned_position = actual_position
    aligned_load = actual_load
    for shift in range(len(actual_position)):
        shifted_position = np.roll(actual_position, shift)
        shifted_load = np.roll(actual_load, shift)
        position_error = (shifted_position - reference_position) / position_range
        load_error = (shifted_load - reference_load) / load_range
        score = float(np.mean(position_error**2 + load_error**2))
        if score < best_score:
            best_score = score
            aligned_position = shifted_position
            aligned_load = shifted_load

    load_nrmse_pct = (
        100.0
        * float(np.sqrt(np.mean((aligned_load - reference_load) ** 2)))
        / load_range
    )
    position_nrmse_pct = (
        100.0
        * float(np.sqrt(np.mean((aligned_position - reference_position) ** 2)))
        / position_range
    )
    actual_area = abs(_signed_area(aligned_position, aligned_load))
    reference_area = abs(_signed_area(reference_position, reference_load))

    return CardComparison(
        load_nrmse_pct=load_nrmse_pct,
        position_nrmse_pct=position_nrmse_pct,
        peak_load_error_pct=_relative_error_pct(
            float(np.max(actual_load)),
            float(np.max(reference_load)),
            "peak load",
        ),
        minimum_load_error_pct=_relative_error_pct(
            float(np.min(actual_load)),
            float(np.min(reference_load)),
            "minimum load",
        ),
        stroke_error_pct=_relative_error_pct(
            float(np.ptp(actual_position)),
            position_range,
            "stroke",
        ),
        enclosed_area_error_pct=_relative_error_pct(
            actual_area,
            reference_area,
            "enclosed area",
        ),
    )
