# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit length conversion for synthetic specification inputs."""

import math


def length_in_metres(value: float, unit: str) -> float:
    """Convert a nonnegative length in metres or international feet to metres.

    No unit is inferred. Survey feet and unrecognised spellings require caller
    resolution. This helper does not alter a parsed specification bundle.
    """
    # Zero is the mathematical lower bound for a length, not a design limit.
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("length must be a finite nonnegative number")
    try:
        numeric = float(value)
    except OverflowError as exc:
        raise ValueError("length must be a finite nonnegative number") from exc
    if not math.isfinite(numeric) or numeric < 0:
        raise ValueError("length must be a finite nonnegative number")
    if unit == "m":
        return numeric
    if unit == "ft":
        # NIST SP 811 (2008), Appendix B.8, foot: exactly 0.3048 m.
        # https://www.nist.gov/pml/special-publication-811/nist-guide-si-appendix-b-conversion-factors/nist-guide-si-appendix-b8
        return numeric * 0.3048
    raise ValueError("length unit must be explicitly 'm' or 'ft'")
