# ABOUTME: Golden tests for the canonical material-grade matrix — published
# ABOUTME: reference strengths, legacy back-compat adapters, and lookup behavior.
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


# --- Golden: API 5L / ISO 3183 (SMYS = ISO L-grade; SMTS = PSL2 min) ----------
@pytest.mark.parametrize(
    "name, smys, smts_psl2, smts_psl1",
    [
        ("A25", 175.0, 310.0, 310.0),
        ("X42", 290.0, 415.0, 414.0),
        ("X52", 360.0, 460.0, 455.0),   # PSL1 455 vs PSL2 460 -> legacy split
        ("X65", 450.0, 535.0, 530.0),
        ("X70", 485.0, 570.0, 565.0),
        ("X80", 555.0, 625.0, 620.0),
        ("X90", 625.0, 695.0, None),    # PSL2 only
        ("X100", 690.0, 760.0, None),
        ("X120", 830.0, 915.0, None),
    ],
)
def test_api_5l_published_values(name, smys, smts_psl2, smts_psl1):
    g = get(name)
    assert g.standard == "API 5L"
    assert g.smys_mpa == smys
    assert g.smts_mpa == smts_psl2
    assert g.smts_psl1_mpa == smts_psl1
    assert g.E_mpa == 207_000.0
    # PSL helper returns PSL1 value where it differs, else PSL2.
    if smts_psl1 is not None:
        assert smts_mpa(name, psl="PSL1") == smts_psl1
    assert smts_mpa(name, psl="PSL2") == smts_psl2


# --- Golden: IACS UR W11 hull steel -------------------------------------------
@pytest.mark.parametrize(
    "name, smys, smts, tough",
    [
        ("Grade A", 235.0, 400.0, "A"),
        ("Grade E", 235.0, 400.0, "E"),
        ("AH32", 315.0, 440.0, "A"),
        ("AH36", 355.0, 490.0, "A"),
        ("EH36", 355.0, 490.0, "E"),
        ("FH36", 355.0, 490.0, "F"),
        ("EH40", 390.0, 510.0, "E"),
    ],
)
def test_iacs_w11_published_values(name, smys, smts, tough):
    g = get(name)
    assert g.standard == "IACS UR W11"
    assert g.smys_mpa == smys
    assert g.smts_mpa == smts
    assert g.toughness_grade == tough
    assert g.E_mpa == 206_000.0


def test_hull_toughness_letter_does_not_change_strength():
    # The leading letter is the Charpy test-temperature class only.
    for level in (32, 36, 40):
        strengths = {
            (get(f"{t}H{level}").smys_mpa, get(f"{t}H{level}").smts_mpa)
            for t in ("A", "D", "E", "F")
        }
        assert len(strengths) == 1


# --- Golden: EN 10025-2 structural --------------------------------------------
@pytest.mark.parametrize(
    "name, smys, smts",
    [("S275", 275.0, 430.0), ("S355", 355.0, 510.0), ("S420", 420.0, 520.0)],
)
def test_en_10025_published_values(name, smys, smts):
    g = get(name)
    assert g.standard == "EN 10025-2"
    assert g.smys_mpa == smys
    assert g.smts_mpa == smts
    assert g.E_mpa == 210_000.0


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
    assert get("A").standard == "API 5L"          # API 5L Grade A (210 MPa)
    assert get("Grade A").standard == "IACS UR W11"  # hull NS A (235 MPa)
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
    assert math.isclose(g.smys_psi, 360.0 * PSI_PER_MPA, rel_tol=1e-9)
    assert math.isclose(g.smts_psi, 460.0 * PSI_PER_MPA, rel_tol=1e-9)


# --- Registry integrity -------------------------------------------------------
def test_registry_completeness():
    # 14 API 5L + 4 hull NS + 12 hull HS + 3 EN = 33 grades.
    assert len(GRADES) == 33
    assert len(by_standard("API 5L")) == 14
    assert len(by_standard("IACS")) == 16
    assert len(by_standard("EN 10025")) == 3
    assert all(isinstance(g, MaterialGrade) for g in all_grades())


def test_every_record_has_a_source():
    assert all(g.source for g in all_grades())
