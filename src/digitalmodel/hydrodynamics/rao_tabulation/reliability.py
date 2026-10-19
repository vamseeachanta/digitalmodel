"""Annotate raw diffraction RAO rows without changing or removing results.

A dataset-specific frequency policy is required; no universal cutoff is assumed.
Source for the motivating policy: issue 1548 (2026), Recommendation section,
https://github.com/vamseeachanta/digitalmodel/issues/1548.

This is a frequency-range screen, not a physical-validity test or solver repair.
Use on native samples before publication. Interpolation and operability consumers
must propagate defect flags from every contributing sample themselves.
"""

from __future__ import annotations

import argparse
import csv
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Mapping


@dataclass(frozen=True)
class ScreeningPolicy:
    """Explicit dataset-specific lower frequency bound, in rad/s, and its source."""

    minimum_frequency_rad_s: float
    basis: str

    def __post_init__(self) -> None:
        _positive_frequency(self.minimum_frequency_rad_s)
        if not isinstance(self.basis, str) or not self.basis.strip():
            raise ValueError("screening policy basis must be a non-empty citation")


def _positive_frequency(value: Any) -> float:
    """Reject invalid angular frequencies, including booleans and non-finite data."""
    try:
        number = float(value)
    except (ValueError, TypeError, OverflowError) as exc:
        raise ValueError("frequency must be positive finite rad/s") from exc
    if isinstance(value, bool) or not math.isfinite(number) or number <= 0:
        raise ValueError("frequency must be positive finite rad/s")
    return number


def _join_labels(previous: Any, current: str) -> str:
    labels = [part.strip() for part in str(previous or "").split(";") if part.strip()]
    if current not in labels:
        labels.append(current)
    return ";".join(labels)


def annotate_rows(
    rows: Iterable[Mapping[str, Any]],
    *,
    policy: ScreeningPolicy | None = None,
    frequency_column: str = "frequency_rad_s",
) -> list[dict[str, Any]]:
    """Return copies with reliability flags and screening provenance.

    A frequency strictly below the supplied bound is ``low_freq_artifact``;
    equality is inside ``screening_range``. This follows the comparison in issue
    1548 (2026), Recommendation, and does not assert physical validity above it.
    Missing policy is ``NOT_EVALUATED``. Existing reliability labels and bases
    are retained, so an independent defect is never cleared by this screen.
    Amplitudes, phases, headings, ordering and row count are preserved.
    """
    result = []
    for index, row in enumerate(rows):
        if frequency_column not in row:
            raise ValueError(
                f"row {index}: missing frequency column {frequency_column}"
            )
        frequency = _positive_frequency(row[frequency_column])
        status = "NOT_EVALUATED"
        basis = ""
        cutoff: float | str = ""
        if policy is not None:
            cutoff = _positive_frequency(policy.minimum_frequency_rad_s)
            basis = policy.basis
            status = "low_freq_artifact" if frequency < cutoff else "screening_range"
        annotated = dict(row)
        # Keep independent findings and provenance rather than replacing them.
        previous = row.get("reliability", "")
        if status == "screening_range" and previous:
            reliability = str(previous)
        else:
            reliability = _join_labels(previous, status)
        annotated.update(
            reliability=reliability,
            reliability_basis=_join_labels(row.get("reliability_basis", ""), basis)
            if basis
            else row.get("reliability_basis", ""),
            frequency_screening_status=status,
            frequency_screening_basis=basis,
            minimum_screening_frequency_rad_s=cutoff,
        )
        result.append(annotated)
    return result


def main(argv: list[str] | None = None) -> int:
    """Write a new annotated CSV; refuse to replace an existing file."""
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    parser.add_argument("--frequency-column", default="frequency_rad_s")
    parser.add_argument("--minimum-frequency-rad-s", type=float)
    parser.add_argument("--basis")
    args = parser.parse_args(argv)
    if (args.minimum_frequency_rad_s is None) != (args.basis is None):
        parser.error("--minimum-frequency-rad-s and --basis must be supplied together")
    policy = None
    if args.minimum_frequency_rad_s is not None:
        policy = ScreeningPolicy(args.minimum_frequency_rad_s, args.basis)
    with args.input.open(newline="", encoding="utf-8") as stream:
        reader = csv.DictReader(stream)
        fields = reader.fieldnames
        if (
            not fields
            or any(not field.strip() for field in fields)
            or len(fields) != len(set(fields))
        ):
            raise ValueError("CSV must have non-empty unique column names")
        rows = list(reader)
    if not rows or any(None in row or None in row.values() for row in rows):
        raise ValueError("CSV must contain complete rows matching the header")
    annotated = annotate_rows(
        rows, policy=policy, frequency_column=args.frequency_column
    )
    columns = list(annotated[0])
    # Exclusive creation also protects aliases/symlinks to the input file.
    with args.output.open("x", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=columns)
        writer.writeheader()
        writer.writerows(annotated)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
