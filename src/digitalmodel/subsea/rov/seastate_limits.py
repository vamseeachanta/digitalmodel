"""ROV LARS sea-state operability — launch/recover operability window.

A work-class ROV (API 17H) is deployed from a host vessel through its Launch And
Recovery System (LARS). The single dominant constraint on whether an intervention
dive can start (or must be aborted) is the sea state at the splash zone: in the
wave-affected upper water column the LARS / Tether Management System experiences
snatch loads and the ROV experiences excessive excursion. Operators therefore
publish a *limiting significant wave height* ``Hs_limit`` for launch and recovery
(IMCA R 004 / IMCA R 015 LARS operability practice; consistent with the workability
concept used throughout DNV-RP / API 17H intervention planning).

This module provides the deterministic, public-standard core of that check. It does
not model spectral vessel motion (deferred to ``hydrodynamics/seakeeping`` and an
RAO-based response solver); it works on the bounding parameter the standards and
operability tables are written in: significant wave height.

Single sea-state operability (deterministic screening)
------------------------------------------------------
A LARS operation is operable when the on-site significant wave height stays at or
below the rated limit::

    operable  <=>  Hs <= Hs_limit                                          (1)

The utilisation and the remaining weather margin are::

    utilisation = Hs / Hs_limit                                            (2)
    margin      = Hs_limit - Hs                                            (3)

An optional *weather-forecast allowance* ``alpha`` (the "alpha factor" from the
weather-restricted-operation framework, DNV-ST-N001 / IMCA practice) de-rates the
operational limit to account for forecast uncertainty::

    Hs_oplim = alpha * Hs_limit          (0 < alpha <= 1, default 1.0)      (4)

so the operable test (1) is applied against ``Hs_oplim``.

Scatter-diagram operability (weather-window percentage)
-------------------------------------------------------
Given a wave scatter diagram (each cell a significant wave height with an
occurrence probability), the operability percentage is the probability-weighted
fraction of sea states at or below the operational limit::

    operability_pct = 100 * sum(p_i for Hs_i <= Hs_oplim) / sum(p_i)       (5)

This is the same accept/reject-then-weight reduction used by
``hydrodynamics.seakeeping.operability_analysis`` but expressed directly on Hs so
it is solver-free and deterministic for planning screens. All wave heights are in
metres; ``alpha`` is dimensionless.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Iterable, Mapping, Sequence


@dataclass(frozen=True)
class SeaStateResult:
    """Outcome of a single-sea-state LARS operability check.

    Attributes
    ----------
    hs:
        On-site significant wave height [m].
    hs_oplim:
        Operational limit after the forecast allowance, ``alpha * hs_limit`` [m].
    utilisation:
        ``hs / hs_oplim`` (<= 1.0 means operable).
    margin:
        ``hs_oplim - hs`` [m] (positive = weather margin remaining).
    operable:
        ``True`` when ``hs <= hs_oplim``.
    """

    hs: float
    hs_oplim: float
    utilisation: float
    margin: float
    operable: bool


def seastate_operability(
    hs: float,
    hs_limit: float,
    alpha: float = 1.0,
) -> SeaStateResult:
    """Check ROV LARS operability for a single sea state (API 17H / IMCA R 004).

    Parameters
    ----------
    hs:
        On-site significant wave height [m], must be >= 0.
    hs_limit:
        Rated limiting significant wave height for the LARS launch/recovery [m],
        must be > 0.
    alpha:
        Weather-forecast allowance / alpha factor (0 < alpha <= 1, default 1.0).
        The operational limit is ``alpha * hs_limit`` (eq. 4).

    Returns
    -------
    SeaStateResult

    Raises
    ------
    ValueError
        On negative ``hs``, non-positive ``hs_limit``, or ``alpha`` outside (0, 1].
    """
    if hs < 0.0:
        raise ValueError(f"hs must be >= 0, got {hs!r}")
    if hs_limit <= 0.0:
        raise ValueError(f"hs_limit must be > 0, got {hs_limit!r}")
    if not (0.0 < alpha <= 1.0):
        raise ValueError(f"alpha must be in (0, 1], got {alpha!r}")

    hs_oplim = alpha * hs_limit                  # eq. (4)
    utilisation = hs / hs_oplim                   # eq. (2)
    margin = hs_oplim - hs                        # eq. (3)
    operable = hs <= hs_oplim                     # eq. (1)

    return SeaStateResult(
        hs=hs,
        hs_oplim=hs_oplim,
        utilisation=utilisation,
        margin=margin,
        operable=operable,
    )


@dataclass(frozen=True)
class OperabilityResult:
    """Probability-weighted LARS operability over a wave scatter diagram.

    Attributes
    ----------
    operability_pct:
        Percentage [0-100] of probability-weighted sea states that are operable.
    hs_oplim:
        Operational limit ``alpha * hs_limit`` [m] used for the screen.
    n_operable:
        Number of scatter cells that are operable.
    n_total:
        Number of scatter cells considered.
    """

    operability_pct: float
    hs_oplim: float
    n_operable: int
    n_total: int


def scatter_operability(
    scatter_diagram: Sequence[Mapping[str, float]],
    hs_limit: float,
    alpha: float = 1.0,
) -> OperabilityResult:
    """Probability-weighted LARS operability percentage over a scatter diagram.

    Parameters
    ----------
    scatter_diagram:
        Sequence of cells, each a mapping with keys ``"hs"`` (significant wave
        height [m], >= 0) and ``"probability"`` (occurrence weight, >= 0). Weights
        need not sum to 1; the result is normalised by their sum (eq. 5).
    hs_limit:
        Rated limiting significant wave height [m], must be > 0.
    alpha:
        Weather-forecast allowance (0 < alpha <= 1, default 1.0).

    Returns
    -------
    OperabilityResult

    Raises
    ------
    ValueError
        On non-positive ``hs_limit``, ``alpha`` outside (0, 1], an empty diagram,
        a negative ``hs`` or ``probability``, or a zero total probability.
    """
    if hs_limit <= 0.0:
        raise ValueError(f"hs_limit must be > 0, got {hs_limit!r}")
    if not (0.0 < alpha <= 1.0):
        raise ValueError(f"alpha must be in (0, 1], got {alpha!r}")
    if not scatter_diagram:
        raise ValueError("scatter_diagram must be non-empty")

    hs_oplim = alpha * hs_limit
    operable_weight = 0.0
    total_weight = 0.0
    n_operable = 0
    n_total = 0

    for cell in scatter_diagram:
        hs = cell["hs"]
        prob = cell["probability"]
        if hs < 0.0:
            raise ValueError(f"scatter hs must be >= 0, got {hs!r}")
        if prob < 0.0:
            raise ValueError(f"scatter probability must be >= 0, got {prob!r}")
        n_total += 1
        total_weight += prob
        if hs <= hs_oplim:
            operable_weight += prob
            n_operable += 1

    if total_weight <= 0.0:
        raise ValueError("total scatter probability must be > 0")

    operability_pct = 100.0 * operable_weight / total_weight   # eq. (5)

    return OperabilityResult(
        operability_pct=operability_pct,
        hs_oplim=hs_oplim,
        n_operable=n_operable,
        n_total=n_total,
    )
