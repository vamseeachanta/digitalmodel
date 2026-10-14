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
TEMPLATES_DIR = ROOT / "src/digitalmodel/solvers/openfoam/templates"
SHIP = TEMPLATES_DIR / "ship_resistance/system/controlDict"

#: Every solver template in the tree. Enumerated once so the multiphase gate
#: and its single-phase mirror below cannot drift apart in coverage.
TEMPLATES = sorted(TEMPLATES_DIR.glob("*/system/controlDict"))


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


def _drop_comments(text: str) -> str:
    """Both comment forms. See ``_forces_block``: these templates DOCUMENT the
    defect they guard against, so any search over raw text can match the
    explanation and pass while the configuration does the wrong thing."""
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.S)
    return "\n".join(ln for ln in text.splitlines()
                     if not ln.strip().startswith("//"))


def phase_evidence(case: Path) -> dict:
    """How many phases this case transports, read from the case itself.

    A POSITIVE structural test, never a filename exemption. "hull_double_body
    sounds single-phase" is a promise that every future template will be named
    honestly, and the defect this file exists to prevent is precisely a case
    whose configuration does not match what it looks like.

    Two independent facts, from two different files:

    * ``constant/transportProperties`` declares a ``phases (...)`` list. A VOF
      transport model needs one entry per phase and cannot run without it.
    * ``0.orig/`` carries an ``alpha.*`` phase-fraction field for the solver to
      transport. With one phase there is nothing to transport and no such
      field exists.

    They must AGREE. A case where they disagree is NOT classified single-phase
    and is not skipped: it is reported, because "skip" is the outcome a
    half-converted case would otherwise be handed.
    """
    transport = case / "constant" / "transportProperties"
    declared: list[str] = []
    if transport.is_file():
        m = re.search(r"^\s*phases\s*\((.*?)\)\s*;",
                      _drop_comments(transport.read_text()), re.M | re.S)
        declared = m.group(1).split() if m else []
    alphas = sorted(p.name for p in (case / "0.orig").glob("alpha.*"))
    return {
        "case": case.name,
        "phase_list": declared,
        "alpha_fields": alphas,
        "multiphase": bool(declared) or bool(alphas),
        "consistent": bool(declared) == bool(alphas),
    }


@pytest.mark.parametrize("template", TEMPLATES)
def test_no_vof_template_integrates_forces_at_constant_density(template):
    """Sweep every solver template, not just the one that was wrong.

    A case is only exempt if it has no free surface -- with no air phase
    there is no dry surface to over-count.
    """
    text = template.read_text()
    if "forces" not in text:
        pytest.skip(f"{template.parent.parent.name} configures no forces")
    evidence = phase_evidence(template.parent.parent)
    assert evidence["consistent"], (
        f"{evidence['case']} declares phases {evidence['phase_list']} but "
        f"carries alpha fields {evidence['alpha_fields']}: the two disagree, "
        f"so the density source cannot be judged either way")
    if not evidence["multiphase"]:
        pytest.skip(f"{evidence['case']} is single-phase: {evidence}")
    body = _forces_block(text)
    assert not re.search(r"^\s*rho\s+rhoInf\s*;", body, re.M), (
        f"{template.parent.parent.name} integrates forces at constant density "
        "in a VOF case")


@pytest.mark.parametrize("template", TEMPLATES)
def test_single_phase_template_names_the_constant_density_explicitly(template):
    """The mirror image, and it is not decoration.

    A skip records that a case was not judged. This asserts what the SAME
    structural evidence requires of the other branch: a single-phase
    incompressible case has no density field in the registry, so `rho rho`
    would abort at run time and `rho rhoInf` with an explicit constant is the
    only correct setting. Without this, the inversion is enforced in one
    direction and merely tolerated in the other -- and a single-phase template
    that quietly copied `rho rho` from its VOF sibling would sail through as a
    skip.
    """
    text = template.read_text()
    if "forces" not in text:
        pytest.skip(f"{template.parent.parent.name} configures no forces")
    evidence = phase_evidence(template.parent.parent)
    if evidence["multiphase"]:
        pytest.skip(f"{evidence['case']} transports {evidence['phase_list']}")
    body = _forces_block(text)
    assert re.search(r"^\s*rho\s+rhoInf\s*;", body, re.M), (
        f"{evidence['case']} is single-phase and must name the constant "
        "density: there is no rho field for `rho rho` to resolve")
    assert re.search(r"^\s*rhoInf\s+\S+\s*;", body, re.M), (
        f"{evidence['case']} says `rho rhoInf` without supplying rhoInf")
    assert not re.search(r"^\s*rho\s+rho\s*;", body, re.M), (
        f"{evidence['case']} names a VOF density field it does not have")


@pytest.mark.parametrize("template", TEMPLATES)
def test_per_patch_forces_take_the_density_source_from_the_case(template):
    """The per-patch objects hardcoded the VOF field.

    Right for a two-phase case, fatal for a single-phase one: simpleFoam has
    no rho field and aborts with "Could not find rho:rho in database" one
    iteration into a solve that had already meshed. The MAIN blocks were
    correct throughout -- only these were not, so the case looked right
    everywhere a reviewer would think to check.

    Asserted as agreement between the main and per-patch blocks, so neither
    can drift from the other regardless of which is right for the case.
    """
    text = _drop_comments(template.read_text())
    main = re.search(r"^\s{4}forces\s*$.*?^\s{4}\}", text, re.M | re.S)
    if main is None:
        pytest.skip(f"{template.parent.parent.name} configures no forces")
    main_rho = re.search(r"rho\s+(\w+)\s*;", main.group(0))
    assert main_rho, "the union block must name a density source"
    for blk in re.findall(r"^\s{4}forces_\w+\s*$.*?^\s{4}\}", text, re.M | re.S):
        per_rho = re.search(r"rho\s+(\w+)\s*;", blk)
        assert per_rho, "a per-patch block must name a density source"
        assert per_rho.group(1) == main_rho.group(1), (
            f"{template.parent.parent.name}: per-patch uses "
            f"{per_rho.group(1)!r}, union uses {main_rho.group(1)!r}")
