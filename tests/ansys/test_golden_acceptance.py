"""Tamper regressions and synthetic supported-load checks for issue 2094."""
import copy
import json
import shutil

import pytest

from tests.ansys import test_example_goldens as goldens


@pytest.fixture
def copied_case(tmp_path, monkeypatch):
    source = goldens.EXAMPLES / "pressure-vessel"
    target = tmp_path / "pressure-vessel"
    shutil.copytree(source, target)
    monkeypatch.setattr(goldens, "EXAMPLES", tmp_path)
    return target


@pytest.mark.parametrize("damage", ["two_csv", "duplicate", "nan", "inf", "json_nan",
                                     "parent_deck", "windows_deck", "absolute_deck"])
def test_loader_refuses_ambiguous_or_nonfinite_evidence(copied_case, damage):
    csv = copied_case / "golden/pv_result.csv"
    path = copied_case / "golden/PROVENANCE.json"
    provenance = json.loads(path.read_text())
    if damage == "two_csv":
        shutil.copyfile(csv, csv.with_name("second_result.csv"))
    elif damage == "duplicate":
        csv.write_text(csv.read_text() + ", MAX_SEQV_MPA ,134.7185")
    elif damage in {"nan", "inf"}:
        csv.write_text(csv.read_text().replace("134.7185", damage))
    elif damage == "json_nan":
        provenance["acceptance"]["unity_check"] = float("nan")
    else:
        alternate = copied_case.parent / "outside.inp"
        shutil.copyfile(copied_case / "pv.inp", alternate)
        provenance["input"]["deck"] = {
            "parent_deck": "../outside.inp", "windows_deck": "..\\outside.inp",
            "absolute_deck": str(alternate.resolve()),
        }[damage]
    path.write_text(json.dumps(provenance))
    with pytest.raises((AssertionError, ValueError)):
        goldens._golden("pressure-vessel")


@pytest.mark.parametrize("damage", ["relax_pv", "relax_force", "status", "yield_flag",
                                     "unity_value", "allowable", "peak_percent",
                                     "observed_stress", "expected_stress", "deviation",
                                     "reaction_record", "peak_node", "d2_boundary"])
def test_artifact_cannot_relax_or_contradict_acceptance(monkeypatch, damage):
    digest, provenance = copy.deepcopy(goldens._golden("pressure-vessel"))
    checks = damage_acceptance(digest, provenance, damage)
    monkeypatch.setattr(goldens, "_golden", lambda case: (digest, provenance))
    with pytest.raises(AssertionError):
        for check in checks:
            check("pressure-vessel")


def damage_acceptance(digest, provenance, damage):
    checks = [goldens.test_golden_matches_recorded_status,
              goldens.test_golden_respects_linear_elastic_validity]
    if damage == "relax_pv":
        digest["max_seqv_mpa"] *= 1.1
        provenance["comparator"]["tolerance_pct"] = 50
        checks = [lambda case: goldens.test_pressure_vessel_golden_matches_closed_form()]
    elif damage == "relax_force":
        digest["reaction_fy_n"] = 20
        provenance["comparator"]["tolerance_n"] = 100
        checks = [goldens.test_golden_reactions_balance]
    elif damage == "status":
        digest["uc"] = 1.1
        provenance["acceptance"]["expected_status"] = "exceeds_allowable"
    elif damage == "yield_flag":
        digest["max_seqv_mpa"] = 270
        provenance["acceptance"]["linear_elastic_limit_exceeded"] = True
        checks = [goldens.test_golden_respects_linear_elastic_validity]
    elif damage == "d2_boundary":
        digest["uc"] = 1
    elif damage == "unity_value":
        provenance["acceptance"]["unity_check"] = 0.2
    elif damage == "allowable":
        digest["allowable_mpa"] = provenance["acceptance"]["allowable_mpa"] = 999
    elif damage == "peak_percent":
        provenance["acceptance"]["peak_vs_yield_pct"] = 3
    elif damage in {"observed_stress", "expected_stress", "deviation"}:
        key = {"observed_stress": "observed_von_mises_mpa",
               "expected_stress": "expected_von_mises_mpa", "deviation": "deviation_pct"}[damage]
        provenance["comparator"][key] = 0
        checks = [lambda case: goldens.test_pressure_vessel_golden_matches_closed_form()]
    elif damage == "reaction_record":
        provenance["equilibrium"]["observed_reaction_fy_n"] = 0.5
        checks = [goldens.test_golden_reactions_balance]
    else:
        provenance["peak_location"]["node"] = 99
    return checks


def supported_digest():
    return {"applied_fx_n": 300, "applied_fy_n": 400,
            "reaction_fx_n": -300, "reaction_fy_n": -400, "force_residual_n": 0}


def test_supported_load_accepts_nonzero_balancing_reactions():
    from tests.ansys.golden_acceptance import validate_equilibrium
    validate_equilibrium("padeye", supported_digest(), {})


@pytest.mark.parametrize("damage", ["wrong_sign", "no_load", "residual_lie", "missing", "nan"])
def test_supported_load_recomputes_balance(damage):
    from tests.ansys.golden_acceptance import validate_equilibrium
    digest = supported_digest()
    if damage == "wrong_sign":
        digest["reaction_fx_n"] = 300
    elif damage == "no_load":
        digest["applied_fx_n"] = digest["applied_fy_n"] = 0
    elif damage == "residual_lie":
        digest["reaction_fx_n"] += 0.1  # Within equilibrium tolerance, false residual.
    elif damage == "missing":
        del digest["applied_fy_n"]
    else:
        digest["reaction_fx_n"] = float("nan")
    with pytest.raises(AssertionError):
        validate_equilibrium("padeye", digest, {})
