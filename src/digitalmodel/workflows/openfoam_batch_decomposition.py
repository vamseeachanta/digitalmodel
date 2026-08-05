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

**Symlinked directories are never followed; a symlinked dictionary is.** The
asymmetry is deliberate.

``Path.is_dir()`` follows symlinks, so a rank symlinked to another rank -- or
to an unrelated case -- would otherwise satisfy every check and let a 1-rank
decomposition pass as an N-rank one. Four names pointing at one directory is
one rank of data, and four ranks writing through them would corrupt it, so
directory membership here is decided by ``lstat``. That matches the traversal
discipline ``artifact_index`` already applies (``O_NOFOLLOW``,
``follow_symlinks=False``).

``system/decomposeParDict`` is read *through* a symlink on purpose. Sharing a
dictionary between cases by symlink is ordinary OpenFOAM practice, and the
solver will read the same target this gate does -- so following it is what
makes the check agree with the run it is validating. The rule is not "never
follow a symlink"; it is "never let a symlink misrepresent the shape of the
decomposition".

Refusal leaves ``system/controlDict`` byte-identical. It does not leave the
whole case untouched: the caller records a failed checkpoint afterwards, which
is the caller's business and not a mutation of the decomposition.
"""

from __future__ import annotations

from decimal import Decimal
from pathlib import Path
import re
import stat
from typing import NoReturn

from digitalmodel.solvers.openfoam.artifact_index import is_numeric_time_name

__all__ = [
    "DecompositionMismatch",
    "is_numeric_time_name",
    "verify_resumable_decomposition",
]

# [0-9] rather than \d: Python's \d matches Unicode decimal digits, so
# "numberOfSubdomains ٤;" would parse as 4 from a file OpenFOAM cannot
# read. Matches artifact_index._TIME_NAME, which spells it the same way.
_NUMBER_OF_SUBDOMAINS = re.compile(r"\bnumberOfSubdomains\s+([0-9]+)\s*;")
# OpenFOAM dictionaries take C++ comments. A stale "// numberOfSubdomains 4;"
# above the live entry would otherwise be read as the live value, which
# accepts a decomposition of the wrong size -- the exact hazard this module
# exists to prevent.
_COMMENT = re.compile(r"//[^\n]*|/\*.*?\*/", re.DOTALL)


class DecompositionMismatch(RuntimeError):
    """A resume was refused: the decomposition on disk does not match the run."""


def _refuse(detail: str) -> NoReturn:
    raise DecompositionMismatch(f"resume refused: {detail}")


def _is_real_dir(entry: Path) -> bool:
    """Whether ``entry`` is a directory, following no symlink to decide it."""
    return stat.S_ISDIR(entry.lstat().st_mode)


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

    ``workers`` is rejected below 1 before anything else, because checks 1 and
    2 are both satisfied *vacuously* at zero -- an empty required set matches
    an empty observed set -- and check 3 would then index rank 0 of an empty
    list. ``resolve_workers`` already rejects that on the router path; this
    gate is directly callable and refuses in its own right.
    """
    if workers < 1:
        _refuse(
            f"workers must be >= 1 to reuse a decomposition, got {workers}"
        )
    ranks = _verify_rank_set(case_dir, workers)
    _verify_subdomain_count(case_dir, workers)
    return _verify_common_latest_time(case_dir, ranks)


def _verify_rank_set(case_dir: Path, workers: int) -> list[str]:
    if not case_dir.is_dir():
        _refuse(
            "the case directory does not exist, so there is no decomposition "
            "to reuse"
        )
    required = [f"processor{index}" for index in range(workers)]
    # Sorted, so which offending entry a refusal names is decided by the name
    # and not by iterdir order, i.e. not by the filesystem.
    observed = [
        entry.name
        for entry in sorted(case_dir.iterdir(), key=lambda path: path.name)
        if entry.name.startswith("processor") and _rank_dir_is_usable(entry)
    ]
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


def _rank_dir_is_usable(entry: Path) -> bool:
    if entry.is_symlink():
        _refuse(
            f"{entry.name} is a symlink, not a real decomposition directory"
        )
    return _is_real_dir(entry)


def _verify_subdomain_count(case_dir: Path, workers: int) -> None:
    path = case_dir / "system" / "decomposeParDict"
    if not path.is_file():
        _refuse(
            "system/decomposeParDict is missing, so the decomposition cannot "
            f"be shown to match workers {workers}"
        )
    text = _COMMENT.sub(" ", path.read_text(errors="replace"))
    declared = [int(value) for value in _NUMBER_OF_SUBDOMAINS.findall(text)]
    if not declared:
        _refuse(
            "system/decomposeParDict declares no numberOfSubdomains, so the "
            f"decomposition cannot be shown to match workers {workers}"
        )
    distinct = sorted(set(declared))
    if len(distinct) > 1:
        # Which declaration OpenFOAM honours is not verified here and is not
        # guessed. An ambiguous dictionary is not evidence of anything.
        _refuse(
            "system/decomposeParDict declares numberOfSubdomains more than "
            "once, with conflicting values "
            + ", ".join(str(value) for value in distinct)
        )
    subdomains = distinct[0]
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
    times = []
    for entry in sorted(rank_dir.iterdir(), key=lambda path: path.name):
        if not is_numeric_time_name(entry.name):
            continue
        if entry.is_symlink():
            _refuse(
                f"{rank_dir.name}/{entry.name} is a symlink, not a real time "
                "directory"
            )
        if _is_real_dir(entry):
            times.append(entry.name)
    if not times:
        _refuse(
            f"{rank_dir.name} holds no numeric time directory to restart from"
        )
    _verify_unambiguous_spellings(rank_dir, times)
    return max(times, key=Decimal)


def _verify_unambiguous_spellings(rank_dir: Path, times: list[str]) -> None:
    """Refuse a rank holding one instant under two names, e.g. 0.5 and 0.50.

    ``max`` would break that tie by ``iterdir`` order, i.e. by filesystem.
    ``artifact_index._time_roots`` treats the same condition as fatal; this
    gate agrees rather than guessing which one OpenFOAM would restart from.
    """
    by_value: dict[Decimal, set[str]] = {}
    for name in times:
        by_value.setdefault(Decimal(name), set()).add(name)
    for names in by_value.values():
        if len(names) > 1:
            _refuse(
                f"{rank_dir.name} holds duplicate numeric time spellings: "
                + ", ".join(sorted(names))
            )
