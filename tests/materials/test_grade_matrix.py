# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic material property and adapter contracts; no reference strengths."""
import math

import pytest

from digitalmodel.materials import (
    GRADES,
    MaterialGrade,
    all_grades,
    by_standard,
    get,
    legacy_smts_psi_dict,
    legacy_smys_psi_dict,
    smts_mpa,
    PSI_PER_MPA,
)


@pytest.mark.parametrize("index,name", list(enumerate([
    "A25", "A", "B", "X42", "X46", "X52", "X56", "X60", "X65", "X70",
    "X80", "X90", "X100", "X120",
])))
def test_synthetic_line_pipe_values(index, name):
    grade = get(name)
    assert grade.smys_mpa == 301 + index * 2
    assert grade.E_mpa == 198001 + index
    assert grade.rho_kg_m3 == 7701 + index
    assert grade.source.kind == "synthetic"
    if index < 10:
        assert smts_mpa(name, psl="PSL1") == 581 + index * 2
    else:
        with pytest.raises(ValueError, match="PSL1"):
            smts_mpa(name, psl="PSL1")
    if index < 2:
        with pytest.raises(ValueError, match="PSL2"):
            smts_mpa(name)
    else:
        assert smts_mpa(name) == 601 + index * 2


@pytest.mark.parametrize("name,index", [
    ("Grade A", 14), ("Grade E", 17), ("AH32", 18), ("AH36", 22),
    ("EH36", 24), ("FH36", 25), ("EH40", 28),
])
def test_synthetic_hull_values(name, index):
    grade = get(name)
    assert grade.smys_mpa == 301 + index * 2
    assert grade.smts_mpa == 601 + index * 2
    assert grade.charpy_test_temperature_degc == -13
    assert grade.source.kind == "synthetic"


def test_hull_toughness_letter_does_not_change_strength():
    # Identity never synthesizes a strength; each synthetic row is independent.
    for level in (32, 36, 40):
        assert len({get(f"{t}H{level}").smys_mpa for t in "ADEF"}) == 4


@pytest.mark.parametrize("name,index", [("S275", 30), ("S355", 31), ("S420", 32)])
def test_synthetic_structural_values(name, index):
    grade = get(name)
    assert grade.smys_mpa == 301 + index * 2
    assert grade.smts_mpa == 601 + index * 2
    assert grade.E_mpa == 198001 + index


# --- Back-compat: adapters reproduce the live legacy FFS dicts exactly ---------
def test_legacy_smys_psi_matches_corroded_pipe():
    from digitalmodel.asset_integrity.corroded_pipe import SMYS_PSI

    assert legacy_smys_psi_dict() == SMYS_PSI


def test_legacy_smts_psi_matches_dnv_rp_f101():
    from digitalmodel.asset_integrity.dnv_rp_f101 import SMTS_PSI

    assert legacy_smts_psi_dict() == SMTS_PSI


# --- Lookup behavior ----------------------------------------------------------
def test_iso_designation_alias_resolves():
    assert get("L360").name == "X52"
    assert get("l485").name == "X70"


def test_api_a_and_hull_grade_a_do_not_collide():
    assert get("A").standard == "API 5L"          # Distinct synthetic identities.
    assert get("Grade A").standard == "IACS UR W11"  # Distinct synthetic identities.
    assert get("A").smys_mpa != get("Grade A").smys_mpa


def test_get_is_case_insensitive():
    assert get("x52") is get("X52")
    assert get("ah36") is get("AH36")


def test_get_unknown_raises():
    with pytest.raises(KeyError):
        get("ZZ999")


# --- psi conversions ----------------------------------------------------------
def test_psi_properties():
    g = get("X52")
    assert math.isclose(g.smys_psi, 311.0 * PSI_PER_MPA, rel_tol=1e-9)
    assert math.isclose(g.smts_psi, 611.0 * PSI_PER_MPA, rel_tol=1e-9)


# --- Registry integrity -------------------------------------------------------
def test_registry_completeness():
    # 14 API 5L + 4 hull NS + 12 hull HS + 3 EN + 5 A312 + 2 A790 = 40 grades.
    assert len(GRADES) == 40
    assert len(by_standard("API 5L")) == 14
    assert len(by_standard("IACS")) == 16
    assert len(by_standard("EN 10025")) == 3
    assert len(by_standard("ASTM A312")) == 5
    assert len(by_standard("ASTM A790")) == 2
    assert all(g.material_class == "ferritic"
               for g in all_grades() if g.standard not in ("ASTM A312", "ASTM A790"))
    assert all(isinstance(g, MaterialGrade) for g in all_grades())


def test_every_record_has_a_source():
    assert all(g.source for g in all_grades())
