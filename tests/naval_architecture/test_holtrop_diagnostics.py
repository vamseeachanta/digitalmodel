# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Fixture qualification and component accounting; no empirical accuracy claim."""

from copy import deepcopy
from pathlib import Path

import pytest
import yaml

from digitalmodel.naval_architecture import holtrop_diagnostics as diagnostics

FIXTURE = (
    Path(__file__).resolve().parents[1]
    / "fixtures/test_vectors/naval_architecture/holtrop_mennen.yaml"
)


@pytest.fixture
def fixture():
    return yaml.safe_load(FIXTURE.read_text())


def test_every_enabled_row_and_output_is_reported(fixture):
    """All legacy outputs need a disposition, including the form-factor band."""
    report = diagnostics.assess_fixture(fixture)
    enabled = {row["id"]: row for row in fixture["test_cases"] if row["use_as_test"]}
    assert {row["id"] for row in report} == set(enabled)
    assert len(report) == len(enabled)
    for row in report:
        assert set(row["checks"]) == set(enabled[row["id"]]["outputs"])
        for check in row["checks"].values():
            assert check["status"] == "NOT_EVALUATED"
            assert check["reason"]
            assert check["required_sources"]
            assert check["reference"] is not None


def test_matching_number_cannot_qualify_an_unsourced_reference(fixture):
    """A coincidental match is not evidence of a published worked example."""
    row = fixture["test_cases"][0]
    row["outputs"]["ct_approx"] = diagnostics.diagnose(row)["ct"]
    check = diagnostics.assess_fixture(fixture)[0]["checks"]["ct_approx"]
    assert check["status"] == "NOT_EVALUATED"
    assert check["computed"] == check["reference"]


@pytest.mark.parametrize("key", ["ct_approx", "ct_tolerance_pct", "froude_number"])
def test_unsourced_targets_do_not_change_the_calculation(fixture, key):
    original = diagnostics.diagnose(fixture["test_cases"][0])
    fixture["test_cases"][0]["outputs"][key] *= 2  # Synthetic mutation, not a limit.
    changed = diagnostics.diagnose(fixture["test_cases"][0])
    assert changed["ct"] == original["ct"]
    assert (
        diagnostics.assess_fixture(fixture)[0]["checks"][key]["status"]
        == "NOT_EVALUATED"
    )


@pytest.mark.parametrize(
    "mutation, message",
    [
        ("duplicate", "duplicate"),
        ("unknown_output", "unsupported"),
        ("missing_flag", "use_as_test"),
        ("string_flag", "use_as_test"),
        ("empty_outputs", "outputs"),
        ("orphan_tolerance", "ct_approx"),
        ("missing_sources", "sources"),
        ("claimed_qualification", "unverified"),
    ],
)
def test_malformed_fixture_is_rejected(fixture, mutation, message):
    if mutation == "duplicate":
        fixture["test_cases"].append(deepcopy(fixture["test_cases"][0]))
    elif mutation == "unknown_output":
        fixture["test_cases"][0]["outputs"]["unknown"] = 0
    elif mutation == "missing_flag":
        del fixture["test_cases"][0]["use_as_test"]
    elif mutation == "string_flag":
        fixture["test_cases"][0]["use_as_test"] = "true"
    elif mutation == "empty_outputs":
        fixture["test_cases"][0]["outputs"] = {}
    elif mutation == "orphan_tolerance":
        del fixture["test_cases"][0]["outputs"]["ct_approx"]
    elif mutation == "missing_sources":
        fixture["qualification"]["required_sources"] = []
    else:
        fixture["qualification"]["reference_status"] = "verified"
    with pytest.raises(ValueError, match=message):
        diagnostics.assess_fixture(fixture)


@pytest.mark.parametrize(
    "disabled_id", ["series60_cb060", "tanker_cb080", "form_factor_validation"]
)
def test_disabled_rows_are_explicit_and_order_does_not_select_cases(
    fixture, disabled_id
):
    disabled = next(case for case in fixture["test_cases"] if case["id"] == disabled_id)
    disabled["use_as_test"] = False
    disabled["inputs"] = {}  # A disabled row must not invoke the engine.
    report = diagnostics.assess_fixture(fixture)
    assert {row["id"] for row in report} == {row["id"] for row in fixture["test_cases"]}
    assert len(report) == len(fixture["test_cases"])
    record = next(row for row in report if row["id"] == disabled_id)
    assert record == {
        "id": disabled_id,
        "status": "DISABLED",
        "reason": "use_as_test is false",
        "checks": {},
    }
    fixture["test_cases"].reverse()
    assert diagnostics.assess_fixture(fixture) == list(reversed(report))


def test_component_accounting_includes_every_optional_term(fixture, monkeypatch):
    """Independent arithmetic on synthetic component stubs (SI).

    S=2, rho=10, V=3 -> qS=90; RF=5, (1+k)=2, RW=7,
    RAPP=11, RB=13, RTR=17, CA=1/10 -> RA=9 and RT=67.
    Hence CT=67/90 and PE=67*3=201. These are test inputs,
    not empirical coefficients or published hull performance.
    """
    hm = diagnostics.hm
    monkeypatch.setattr(diagnostics.hc, "RHO_SW", 10)
    for name, value in {
        "wetted_surface_holtrop": 2,
        "form_factor_k1": 2,
        "frictional_resistance": 5,
        "wave_resistance": 7,
        "appendage_resistance": 11,
        "bulbous_bow_resistance": 13,
        "transom_resistance": 17,
        "correlation_allowance": 0.1,
        "total_resistance": 67,
    }.items():
        monkeypatch.setattr(hm, name, lambda *args, _value=value, **kwargs: _value)
    row = diagnostics.diagnose(fixture["test_cases"][0], speed=3)
    assert row["rapp_n"] == 11
    assert row["rb_n"] == 13
    assert row["rtr_n"] == 17
    assert row["ra_n"] == 9
    assert row["component_sum_n"] == 67
    assert row["rt_n"] == 67
    assert row["ct"] == pytest.approx(67 / 90)
    assert row["effective_power_w"] == 201


@pytest.mark.parametrize("speed", [None, 3.0, 10.0])
def test_real_component_sum_and_common_froude(fixture, speed):
    """Accounting identities, not a numerical accuracy acceptance limit."""
    for case in fixture["test_cases"]:
        if "speed_ms" not in case["inputs"]:
            continue
        row = diagnostics.diagnose(case, speed=speed)
        assert row["rt_n"] == pytest.approx(row["component_sum_n"], rel=1e-12)
        assert row["ct"] == pytest.approx(
            row["viscous_ct"] + row["wave_ct"] + row["ca"]
        )


@pytest.mark.parametrize("reverse", [False, True])
def test_common_froude_uses_different_speeds_for_different_lengths(fixture, reverse):
    """Fn=V/sqrt(gL): set Fn=1/5 as a synthetic comparison point.

    Rearranging the definition gives V=sqrt(gL)/5. No expectation about
    cross-hull CT ordering or empirical separation is imposed.
    """
    if reverse:
        fixture["test_cases"].reverse()
    rows = [
        diagnostics.diagnose(
            case, speed=(diagnostics.hc.G * case["inputs"]["lwl"]) ** 0.5 / 5
        )
        for case in fixture["test_cases"]
        if "speed_ms" in case["inputs"]
    ]
    assert all(row["froude_number"] == pytest.approx(1 / 5) for row in rows)
    shorter = min(rows, key=lambda row: row["lwl_m"])
    longer = max(rows, key=lambda row: row["lwl_m"])
    assert longer["speed_ms"] > shorter["speed_ms"]
