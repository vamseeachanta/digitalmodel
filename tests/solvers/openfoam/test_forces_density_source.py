"""The `forces` function object must use the VOF density FIELD, not rhoInf.

Measured on the KCS validation case (digitalmodel#1173, 2026-08-19):

    hull patch total area      9.4216 m2
    submerged                  4.8601 m2   (51.6%)
    ABOVE the waterline        4.5615 m2   (48.4%)

With `rho rhoInf; rhoInf 998.8;` the dry topsides are integrated at WATER
density and contributed 62.3% of the reported viscous force:

    viscous_x   rhoInf 998.8 -> 77.711 N        rho rho -> 29.389 N
    Cv          +141.5% vs ITTC-57              -8.9%
    Ct          +113.8% vs experiment           -5.7%

Pressure barely moved (-0.2%), because topside pressure is near-atmospheric
and largely cancels fore-aft. That asymmetry is the signature: a defect
hitting only the friction integral.

The correct pattern was ALREADY in this repo, with a comment explaining it,
in maccamy_fuchs.py -- it just never reached the ship-resistance template.
These tests exist so it cannot drift back.
"""
from __future__ import annotations

import re
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[3]
SHIP = (ROOT / "src/digitalmodel/solvers/openfoam/templates"
        / "ship_resistance/system/controlDict")


@pytest.fixture(scope="module")
def ship_controldict() -> str:
    assert SHIP.is_file(), f"template not found at {SHIP}"
    return SHIP.read_text()


def _forces_block(text: str) -> str:
    """The forces sub-dict only, comments stripped.

    Comments are stripped because this file DOCUMENTS the defect verbatim --
    a naive substring search would match the explanation and pass while the
    configuration did the wrong thing.
    """
    m = re.search(r"forces\s*\{(.*?)\n    \}", text, re.S)
    assert m, "no forces block in the ship_resistance controlDict"
    return "\n".join(ln for ln in m.group(1).splitlines()
                     if not ln.strip().startswith("//"))


def test_ship_resistance_forces_uses_the_density_field(ship_controldict):
    body = _forces_block(ship_controldict)
    assert re.search(r"^\s*rho\s+rho\s*;", body, re.M), (
        "forces must use `rho rho` (the interFoam VOF density field)")


def test_ship_resistance_forces_does_not_use_rhoinf(ship_controldict):
    """A constant rhoInf integrates dry topsides at water density."""
    body = _forces_block(ship_controldict)
    assert not re.search(r"^\s*rho\s+rhoInf\s*;", body, re.M), (
        "`rho rhoInf` over-counts the above-water hull surface")
    assert not re.search(r"^\s*rhoInf\s", body, re.M), (
        "a stray rhoInf entry means the density source is ambiguous")


def test_the_defect_explanation_survives_in_the_template(ship_controldict):
    """The reasoning must stay with the setting.

    A bare `rho rho` looks arbitrary and invites a future editor to 'restore'
    rhoInf for consistency with other cases.
    """
    assert "rhoInf" in ship_controldict, "the warning comment was removed"
    assert re.search(r"above[- ]water", ship_controldict, re.I)


def test_maccamy_fuchs_still_uses_the_density_field():
    """The sibling case this pattern was copied FROM.

    It was right before ship_resistance was; if it regresses, the reference
    implementation is gone.
    """
    src = (ROOT / "src/digitalmodel/solvers/openfoam/validation"
           / "maccamy_fuchs.py").read_text()
    m = re.search(r"_FORCES_FO\s*=\s*\"\"\"(.*?)\"\"\"", src, re.S)
    assert m, "_FORCES_FO not found in maccamy_fuchs.py"
    assert re.search(r"^\s*rho\s+rho\s*;", m.group(1), re.M)


@pytest.mark.parametrize("template", sorted(
    (ROOT / "src/digitalmodel/solvers/openfoam/templates").glob(
        "*/system/controlDict")))
def test_no_vof_template_integrates_forces_at_constant_density(template):
    """Sweep every solver template, not just the one that was wrong.

    A case is only exempt if it has no free surface -- with no air phase
    there is no dry surface to over-count.
    """
    text = template.read_text()
    if "forces" not in text:
        pytest.skip(f"{template.parent.parent.name} configures no forces")
    body = _forces_block(text)
    if not re.search(r"alpha|interFoam|VOF", text, re.I) and \
       not (template.parent.parent / "0.orig" / "alpha.water").exists():
        pytest.skip(f"{template.parent.parent.name} is single-phase")
    assert not re.search(r"^\s*rho\s+rhoInf\s*;", body, re.M), (
        f"{template.parent.parent.name} integrates forces at constant density "
        "in a VOF case")
