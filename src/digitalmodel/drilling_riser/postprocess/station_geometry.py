"""Shared section-length validity contract for riser coupling geometry."""

import math
from collections.abc import Mapping, Sequence, Set
from numbers import Real


def section_lengths(sections_m: Sequence[float]) -> list[float]:
    """Return positive finite lengths with a finite, strictly increasing arc axis."""
    if isinstance(sections_m, (str, bytes, Mapping, Set)):
        raise ValueError("sections_m must be an ordered iterable of numeric lengths")
    try:
        lengths = []
        for index, value in enumerate(sections_m):
            if isinstance(value, bool) or not isinstance(value, Real):
                raise ValueError(f"sections_m[{index}] must be a real numeric length")
            lengths.append(float(value))
    except (TypeError, ValueError, OverflowError) as exc:
        raise ValueError("sections_m must contain numeric section lengths") from exc
    if not lengths:
        raise ValueError("sections_m must contain at least one section")
    arc = 0.0
    for index, length in enumerate(lengths):
        if not math.isfinite(length) or length <= 0.0:
            raise ValueError(f"sections_m[{index}] must be finite and positive")
        end = arc + length
        if not math.isfinite(end) or end <= arc:
            raise ValueError("sections_m must form a finite, strictly increasing arc axis")
        arc = end
    return lengths
