"""Engineering-feedback rollup (digitalmodel #1360).

Rolls a sequence of go/no-go states (measured or forecast) into an operability
summary — the number that feeds field-development economics. A proxy for full
weather-downtime statistics (deferred).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Sequence

from digitalmodel.marine_ops.installation.go_no_go import DecisionState


@dataclass
class OperabilitySummary:
    operable_fraction: float
    downtime_fraction: float
    waiting_on_weather_fraction: float
    longest_operable_run: int  # samples
    n: int


def operability_summary(
    states: Sequence[DecisionState], *, marginal_operable: bool = False
) -> OperabilitySummary:
    """Summarise operability over a state sequence.

    ``operable`` = ``DecisionState.GO`` (or GO + MARGINAL when
    ``marginal_operable``). ``waiting_on_weather_fraction`` = the not-operable
    fraction (a downtime proxy). ``longest_operable_run`` counts the longest
    consecutive operable stretch, in samples.
    """
    n = len(states)
    if n == 0:
        raise ValueError("operability_summary needs a non-empty state sequence")

    def _operable(s: DecisionState) -> bool:
        return s is DecisionState.GO or (marginal_operable and s is DecisionState.MARGINAL)

    flags = [_operable(s) for s in states]
    operable_fraction = sum(flags) / n

    longest = cur = 0
    for f in flags:
        cur = cur + 1 if f else 0
        longest = max(longest, cur)

    return OperabilitySummary(
        operable_fraction=operable_fraction,
        downtime_fraction=1.0 - operable_fraction,
        waiting_on_weather_fraction=1.0 - operable_fraction,
        longest_operable_run=longest,
        n=n,
    )
