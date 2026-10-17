"""Boundary-field entries for the patches snappyHexMesh will create.

Separate from the surface plumbing because it is a different concern: that
module decides which surfaces the mesher meets, this one makes sure the
initial fields agree with the patches those surfaces produce.

The two disagreeing is not a loud failure. It aborts at setFields -- AFTER the
mesh is built -- so it costs a full meshing run to discover:

    FOAM FATAL IO ERROR: Cannot find patchField entry for rudder
"""
from __future__ import annotations

import re
from pathlib import Path
from typing import Optional, Sequence, Tuple

from .hull_case_regions import SurfaceRegion

def _block(text: str, name: str) -> Optional[Tuple[int, int, str]]:
    """Locate a ``name { ... }`` entry by brace matching. None if absent."""
    m = re.search(rf"\n(\s*){re.escape(name)}\s*\n\s*\{{", text)
    if m is None:
        return None
    i = text.index("{", m.start())
    depth = 0
    for j in range(i, len(text)):
        if text[j] == "{":
            depth += 1
        elif text[j] == "}":
            depth -= 1
            if depth == 0:
                return m.start() + 1, j + 1, m.group(1)
    return None


def add_appendage_patchfields(
    case: Path, regions: Sequence[SurfaceRegion], hull_patch: str = "hull"
) -> int:
    """Give every appendage patch the hull's OWN patchField, in every field.

    snappyHexMesh creates one patch per surface region, and OpenFOAM refuses
    any field whose boundaryField lacks an entry for a patch that exists:

        FOAM FATAL IO ERROR: Cannot find patchField entry for rudder

    It fails at setFields -- after the mesh is built, so the failure costs a
    full meshing run. That is what happened on the first three-region case.

    The appendages are walls on the same body as the hull, so the correct
    entry is the hull's, and it is COPIED per file rather than restated here.
    Restating it would put the boundary condition in two places and let a
    change to the hull's leave the appendages silently on the old one -- which
    is the same shape as the Aref defect this module already guards.
    """
    names = [r.name for r in regions if r.name != hull_patch]
    if not names:
        return 0
    written = 0
    for field in sorted((case / "0.orig").glob("*")):
        if not field.is_file():
            continue
        text = field.read_text()
        found = _block(text, hull_patch)
        if found is None:
            continue
        start, end, indent = found
        body = text[start:end]
        additions = "".join(
            "\n" + body.replace(f"{indent}{hull_patch}", f"{indent}{name}", 1)
            for name in names
            if _block(text, name) is None
        )
        if additions:
            field.write_text(text[:end] + additions + text[end:])
            written += 1
    return written
