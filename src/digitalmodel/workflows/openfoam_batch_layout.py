"""Legacy filesystem helpers for OpenFOAM batch cases."""

from __future__ import annotations

import re
import shutil
from pathlib import Path

DECOMPOSE_PAR_DICT = """\
FoamFile {{ version 2.0; format ascii; class dictionary; object decomposeParDict; }}

numberOfSubdomains {n};
method scotch;
"""


def write_decompose_par_dict(case_dir: Path, workers: int) -> None:
    """Pin the legacy scotch decomposition to the requested rank count."""
    (case_dir / "system" / "decomposeParDict").write_text(
        DECOMPOSE_PAR_DICT.format(n=workers)
    )


def prune_processor_dirs(case_dir: Path) -> None:
    """Remove reconstructed processor directories."""
    if not case_dir.is_dir():
        return
    for proc_dir in case_dir.glob("processor*"):
        shutil.rmtree(proc_dir, ignore_errors=True)


def has_processor_dirs(case_dir: Path) -> bool:
    """Report whether a resumable decomposition exists."""
    return case_dir.is_dir() and any(case_dir.glob("processor*"))


def clean_case_dir(case_dir: Path) -> None:
    """Remove a stale case tree before a fresh rebuild."""
    if case_dir.is_dir():
        shutil.rmtree(case_dir, ignore_errors=True)


def set_start_from_latest_time(case_dir: Path) -> None:
    """Patch controlDict so a resumed solve starts at its latest time."""
    control = case_dir / "system" / "controlDict"
    if not control.is_file():
        raise RuntimeError(
            f"resume requested but no system/controlDict in {case_dir}"
        )
    text = control.read_text()
    patched = re.sub(r"startFrom\s+\w+\s*;", "startFrom       latestTime;", text)
    if patched == text and "startFrom" not in text:
        patched = text + "\nstartFrom       latestTime;\n"
    control.write_text(patched)
