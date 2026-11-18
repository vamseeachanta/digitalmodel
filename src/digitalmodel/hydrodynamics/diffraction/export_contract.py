# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Export-path metadata without filesystem checks or export production."""

from dataclasses import dataclass
from pathlib import Path
from typing import Literal


@dataclass(frozen=True)
class ExportDisposition:
    """Snapshot of a recorded path, without asserting a usable export exists."""

    state: Literal["NOT_PRODUCED", "PATH_RECORDED_UNVERIFIED"]
    path: Path | None
    reason: str


def describe_exports(
    xlsx_path: Path | None, report_path: Path | None
) -> dict[str, ExportDisposition]:
    """Describe both current fields independently, without discovering paths."""
    return {
        name: ExportDisposition(
            state="NOT_PRODUCED" if path is None else "PATH_RECORDED_UNVERIFIED",
            path=path,
            reason=(
                "No export path is recorded; automatic export production is not integrated."
                if path is None
                else "File existence and contents are unverified."
            ),
        )
        for name, path in (("xlsx", xlsx_path), ("report", report_path))
    }


def format_export(disposition: ExportDisposition) -> str:
    """Format the path, if recorded, alongside its disposition and reason."""
    label = disposition.state
    if disposition.path is not None:
        label = f"{disposition.path} [{label}]"
    return f"{label} ({disposition.reason})"
