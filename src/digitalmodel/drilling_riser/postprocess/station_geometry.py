"""Shared section-length validity contract for riser coupling geometry."""

import math
from collections.abc import Mapping, Sequence, Set
from numbers import Real


def finite_parameter(value: float, name: str, *, nonnegative: bool = False) -> float:
    """Validate a real coordinate, or a nonnegative distance supplied by a caller."""
    if isinstance(value, bool) or not isinstance(value, Real):
        raise ValueError(f"{name} must be a finite real number")
    try:
        result = float(value)
    except (ValueError, OverflowError) as exc:
        raise ValueError(f"{name} must be a finite real number") from exc
    if not math.isfinite(result) or (nonnegative and result < 0.0):
        raise ValueError(f"{name} must be finite" + (" and nonnegative" if nonnegative else ""))
    return result


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


def joint_counts(sections_m: Sequence[float], joint_length_m: float, tol_m: float) -> list[int]:
    """Count coupling positions per section without expanding any joint sequence.

    OpenStax, College Algebra 2e (2021), section 9.2, arithmetic sequences:
    positions are section_start + k * joint_length_m. A short section contributes
    its endpoint; a multi-joint section contributes its rounded joint count,
    including its endpoint. The length residual must satisfy the caller's tol_m.
    No engineering tolerance or resource limit is inferred from this arithmetic.
    """
    lengths = section_lengths(sections_m)
    joint = finite_parameter(joint_length_m, "joint_length_m", nonnegative=True)
    if joint == 0.0:
        raise ValueError("joint_length_m must be positive")
    tol = finite_parameter(tol_m, "tol_m", nonnegative=True)
    counts = []
    for i, length in enumerate(lengths):
        count = 1  # one section endpoint, per the sequence definition above
        if length - joint > tol:
            ratio = length / joint
            if not math.isfinite(ratio):
                raise ValueError(f"section {i}: joint count is not finite")
            count = round(ratio)
            if abs(ratio - count) * joint > tol:
                raise ValueError(f"section {i} ({length} m) is longer than a joint and not a whole number of "
                                 f"{joint} m joints")
        counts.append(count)
    return counts
