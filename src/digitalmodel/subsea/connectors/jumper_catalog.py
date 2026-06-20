"""Subsea jumper type registry (API 17R).

Lightweight catalogue mapping jumper families to their typical analysis
attributes so a workflow can pick the right load cases:

* ``rigid``    — welded/forged steel M- or U-shape; thermal-expansion driven.
* ``flexible`` — bonded/unbonded flexible pipe; MBR governed, fatigue for
  dynamic service.
* ``vertical`` — vertical (gooseneck) connection; combined pressure + bending.

Each entry flags which checks in this module are normally governing.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum


class JumperType(str, Enum):
    RIGID = "rigid"
    FLEXIBLE = "flexible"
    VERTICAL = "vertical"


@dataclass(frozen=True)
class JumperSpec:
    """Catalogue entry for a jumper family."""

    jumper_type: JumperType
    description: str
    governing_checks: tuple = field(default_factory=tuple)
    dynamic: bool = False  # True -> fatigue assessment required


JUMPER_CATALOG: dict[JumperType, JumperSpec] = {
    JumperType.RIGID: JumperSpec(
        jumper_type=JumperType.RIGID,
        description="Rigid forged/welded steel M- or U-jumper (tree-to-manifold).",
        governing_checks=("thermal_expansion", "connector_body", "seal_preload"),
        dynamic=False,
    ),
    JumperType.FLEXIBLE: JumperSpec(
        jumper_type=JumperType.FLEXIBLE,
        description="Flexible-pipe jumper; MBR + fatigue governed (API 17J/17B).",
        governing_checks=("bending_mbr", "fatigue"),
        dynamic=True,
    ),
    JumperType.VERTICAL: JumperSpec(
        jumper_type=JumperType.VERTICAL,
        description="Vertical/gooseneck jumper; combined pressure + bending.",
        governing_checks=("connector_body", "bending_mbr", "seal_preload"),
        dynamic=True,
    ),
}


def get_jumper_spec(jumper_type: JumperType) -> JumperSpec:
    """Return the catalogue entry for a jumper type."""
    return JUMPER_CATALOG[jumper_type]
