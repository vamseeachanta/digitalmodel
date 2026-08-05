"""Prove an existing processor* decomposition matches the run resuming into it.

MPI ``resume: true`` reuses whatever ``processor*`` directories a previous
attempt left behind. Before this module the entire precondition was
``any(case_dir.glob("processor*"))`` -- a boolean that never counted the
directories, never ordered them, and never compared anything against the rank
count the run was about to request, so a 4-rank run would happily restart from
a 1-rank decomposition.

``verify_resumable_decomposition`` refuses instead, and refuses *before*
``system/controlDict`` is touched, so a rejected resume leaves the case
byte-identical and the operator can act on the message.

Every value the gate compares is a named input (``workers``) or on-disk state
(a directory listing, ``numberOfSubdomains``, a time directory name). There is
no threshold and no tolerance: the cross-rank time comparison is equality.

Time directory names are validated by
:func:`digitalmodel.solvers.openfoam.artifact_index.is_numeric_time_name`,
re-exported here rather than reimplemented, so the repo keeps exactly one
OpenFOAM time-name parser.
"""

from __future__ import annotations

from decimal import Decimal
from pathlib import Path
import re

from digitalmodel.solvers.openfoam.artifact_index import is_numeric_time_name

__all__ = [
    "DecompositionMismatch",
    "is_numeric_time_name",
    "verify_resumable_decomposition",
]

_NUMBER_OF_SUBDOMAINS = re.compile(r"\bnumberOfSubdomains\s+(\d+)\s*;")


class DecompositionMismatch(RuntimeError):
    """A resume was refused: the decomposition on disk does not match the run."""


def _refuse(detail: str) -> None:
    raise DecompositionMismatch(f"resume refused: {detail}")


def verify_resumable_decomposition(case_dir: Path, workers: int) -> str:
    """Return the common latest time, or raise :class:`DecompositionMismatch`.

    Checks, in order, so that the cheapest and most likely mismatch is
    reported first:

    1. the ``processor*`` directory names are exactly
       ``processor0 .. processor{workers-1}`` -- contiguous, no gaps, no extras;
    2. ``system/decomposeParDict`` exists and its ``numberOfSubdomains``
       equals ``workers``;
    3. every rank holds at least one numeric time directory, and every rank's
       maximum agrees with rank 0's.

    Ordering matters: a rank-count mismatch explains a missing rank directory,
    so reporting the rank set first names the actual fault rather than a
    symptom of it.
    """
    ranks = _verify_rank_set(case_dir, workers)
    _verify_subdomain_count(case_dir, workers)
    return _verify_common_latest_time(case_dir, ranks)


def _verify_rank_set(case_dir: Path, workers: int) -> list[str]:
    required = [f"processor{index}" for index in range(workers)]
    observed = sorted(
        entry.name for entry in case_dir.iterdir()
        if entry.is_dir() and entry.name.startswith("processor")
    )
    missing = [name for name in required if name not in set(observed)]
    unexpected = [name for name in observed if name not in set(required)]
    if missing or unexpected:
        detail = f"observed {len(observed)}, required {workers}"
        if missing:
            detail += "; missing " + ", ".join(missing)
        if unexpected:
            detail += "; unexpected " + ", ".join(unexpected)
        _refuse(
            f"processor directory set does not match workers {workers}: {detail}"
        )
    return required


def _verify_subdomain_count(case_dir: Path, workers: int) -> None:
    path = case_dir / "system" / "decomposeParDict"
    if not path.is_file():
        _refuse(
            "system/decomposeParDict is missing, so the decomposition cannot "
            f"be shown to match workers {workers}"
        )
    found = _NUMBER_OF_SUBDOMAINS.search(path.read_text(errors="replace"))
    if found is None:
        _refuse(
            "system/decomposeParDict declares no numberOfSubdomains, so the "
            f"decomposition cannot be shown to match workers {workers}"
        )
    subdomains = int(found.group(1))
    if subdomains != workers:
        _refuse(
            f"system/decomposeParDict declares numberOfSubdomains {subdomains}, "
            f"required {workers}"
        )


def _verify_common_latest_time(case_dir: Path, ranks: list[str]) -> str:
    latest = {name: _latest_time(case_dir / name) for name in ranks}
    reference = ranks[0]
    newest = latest[reference]
    for name in ranks[1:]:
        if Decimal(latest[name]) != Decimal(newest):
            _refuse(
                f"ranks disagree on the latest time: {reference} is at "
                f"{newest} but {name} is at {latest[name]}"
            )
    return newest


def _latest_time(rank_dir: Path) -> str:
    times = [
        entry.name for entry in rank_dir.iterdir()
        if entry.is_dir() and is_numeric_time_name(entry.name)
    ]
    if not times:
        _refuse(
            f"{rank_dir.name} holds no numeric time directory to restart from"
        )
    return max(times, key=Decimal)
