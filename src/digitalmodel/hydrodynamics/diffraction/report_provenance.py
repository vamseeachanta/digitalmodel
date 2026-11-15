"""Portable source stamps for diffraction report entry points (#1019)."""

from hashlib import file_digest
from pathlib import Path
from typing import Iterable

from digitalmodel.reporting import Provenance
from digitalmodel.reporting._metadata import package_code_edition


def result_provenance(paths: Iterable[str | Path], *, units_system: str) -> Provenance:
    """Declare actual result files, hashing only files that exist.

    Comparison or extraction alone does not establish pipeline validation;
    these reports are screening views. Identifiers retain the supplied paths
    so declared reads match them and equal basenames remain distinguishable.
    """
    provenance = Provenance(
        code_edition=package_code_edition(),
        units_system=units_system,
        validated_path="screening",
    )
    for item in paths:
        path = Path(item)
        digest = None
        if path.is_file():
            with path.open("rb") as stream:
                digest = "sha256:" + file_digest(stream, "sha256").hexdigest()
        provenance.add("file", str(item), digest=digest)
    return provenance
