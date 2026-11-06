# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Supplied-data air-gap arithmetic, without environmental or code criteria.

Method: docs/domains/asset-integrity/air-gap-method.md. Clearance is
deck underside minus downward subsidence minus still water minus crest height;
margin subtracts the supplied minimum. Zero is the algebraic comparison boundary,
not a selected engineering criterion. Annual labels identify samples only.
"""

from __future__ import annotations

from dataclasses import dataclass
from math import isfinite
from typing import Sequence


@dataclass(frozen=True)
class Elevation:
    """Elevation in metres, positive upward from an explicit datum label."""

    value_m: float
    datum: str


@dataclass(frozen=True)
class AirGapBasis:
    """Caller-supplied elevations, crest above still water and minimum, in metres."""

    deck_underside: Elevation
    still_water: Elevation
    crest_height_m: float
    required_clearance_m: float


@dataclass(frozen=True)
class AirGapResult:
    """Arithmetic comparison with the supplied minimum, at the stated datum."""

    datum: str
    clearance_m: float
    margin_m: float
    meets_required_clearance: bool


@dataclass(frozen=True)
class SubsidenceSample:
    """An integer annual date label paired with supplied downward movement in metres."""

    date_label: int
    subsidence_m: float


@dataclass(frozen=True)
class SampledAirGapResult:
    """A supplied date label and its arithmetic result."""

    date_label: int
    air_gap: AirGapResult


@dataclass(frozen=True)
class SampledAirGap:
    """Ordered results and first sampled breach label; None means no sampled breach."""

    samples: tuple[SampledAirGapResult, ...]
    first_sampled_breach: int | None


def _finite_number(value: float, name: str, *, nonnegative: bool = False) -> None:
    """Reject missing, nonnumeric, nonfinite and disallowed negative quantities."""
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError(f"{name} requires a finite number")
    try:
        finite = isfinite(value)
    except OverflowError as exc:
        raise ValueError(f"{name} requires a finite representable number") from exc
    if not finite or (nonnegative and value < 0):
        raise ValueError(f"{name} requires a finite valid magnitude")


def calculate_air_gap(basis: AirGapBasis, subsidence_m: float) -> AirGapResult:
    """Subtract supplied quantities and compare clearance >= supplied minimum.

    Args:
        basis: Deck and still-water elevations sharing exactly the same nonblank
            datum label, crest height above that still water and required clearance.
        subsidence_m: Nonnegative supplied downward deck movement in metres.

    Returns:
        Clearance and margin in metres; equality meets the supplied minimum.

    Raises:
        ValueError: Datum labels conflict or are missing, a magnitude is invalid,
            or derived arithmetic is nonfinite.
    """
    for elevation in (basis.deck_underside, basis.still_water):
        if not isinstance(elevation.datum, str) or not elevation.datum.strip():
            raise ValueError("An explicit nonblank datum label is required")
        _finite_number(elevation.value_m, "elevation")
    if basis.deck_underside.datum != basis.still_water.datum:
        raise ValueError("Deck and still-water datum labels must match exactly")
    _finite_number(subsidence_m, "subsidence_m", nonnegative=True)
    _finite_number(basis.crest_height_m, "crest_height_m", nonnegative=True)
    _finite_number(basis.required_clearance_m, "required_clearance_m", nonnegative=True)
    clearance = (
        basis.deck_underside.value_m - subsidence_m
        - basis.still_water.value_m - basis.crest_height_m
    )
    margin = clearance - basis.required_clearance_m
    _finite_number(clearance, "clearance")
    _finite_number(margin, "margin")
    return AirGapResult(
        datum=basis.deck_underside.datum,
        clearance_m=clearance,
        margin_m=margin,
        meets_required_clearance=clearance >= basis.required_clearance_m,
    )


def evaluate_sampled_air_gap(
    basis: AirGapBasis, samples: Sequence[SubsidenceSample],
) -> SampledAirGap:
    """Evaluate supplied samples in order without interpolation or subsidence fitting.

    Args:
        basis: Fixed caller-supplied basis for all samples.
        samples: Nonempty sequence of paired integer annual labels and subsidence.
            Labels must strictly increase. Values need not increase.

    Returns:
        Ordered results and the first supplied label below the minimum, if any.

    Raises:
        ValueError: Samples are empty, labels are invalid, duplicated or unordered,
            or any sample fails the scalar input contract.
    """
    if not samples:
        raise ValueError("At least one subsidence sample is required")
    rows = []
    previous = None
    first_breach = None
    for sample in samples:
        label = sample.date_label
        if isinstance(label, bool) or not isinstance(label, int):
            raise ValueError("Each annual date label must be an integer")
        if previous is not None and label <= previous:
            raise ValueError("Date labels must strictly increase without duplicates")
        result = calculate_air_gap(basis, sample.subsidence_m)
        rows.append(SampledAirGapResult(label, result))
        if first_breach is None and not result.meets_required_clearance:
            first_breach = label
        previous = label
    return SampledAirGap(tuple(rows), first_breach)
