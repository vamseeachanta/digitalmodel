"""#1280 suites: stack-up assembly engine — model/aggregates, the API RP 16Q
minimum-top-tension chain, and rig-capability checks.

Public/deterministic layer only (the in-context golden lives in
test_assembly_golden.py). Fixture = the already-vendored wed component CSV
(sanctioned public vocabulary); adapter registries are always passed
explicitly (never the module-global).
"""
from __future__ import annotations

import math
from pathlib import Path

import pytest

from digitalmodel.drilling_riser import assembly
from digitalmodel.drilling_riser.adapter import (
    KIPS_TO_KN,
    register_riser_components,
)
from digitalmodel.drilling_riser.assembly import (
    RigCapabilityCheck,
    RiserStackupModel,
    StackupItem,
    check_rig_capability,
    minimum_top_tension_16q,
    tensioner_system_factor,
)

FIXTURE = Path(__file__).resolve().parent / "fixtures" / "drilling_riser_components.csv"


@pytest.fixture()
def registry() -> dict:
    reg: dict = {}
    added, _skipped = register_riser_components(FIXTURE, registry=reg)
    assert added >= 30
    return reg


# -- model + aggregates ----------------------------------------------------------


def test_from_component_counts_aggregates(registry):
    model = RiserStackupModel.from_component_counts(
        {"RJ-21-75-BARE": 10, "RJ-21-75-BUOY": 5}, registry
    )
    # Hand-computed from the vendored wed vocabulary:
    # bare 3.8 kips net submerged, buoyed -4.2 kips net; air 22.5 kips each.
    expected_subm = (10 * 3.8 + 5 * -4.2) * KIPS_TO_KN
    expected_dry = (15 * 22.5) * KIPS_TO_KN
    assert model.total_submerged_weight_kn() == pytest.approx(expected_subm)
    assert model.total_dry_weight_kn() == pytest.approx(expected_dry)
    # 75 ft joints -> 22.86 m each
    assert model.total_length_m() == pytest.approx(15 * 75.0 * 0.3048)


def test_unknown_component_escalates(registry):
    with pytest.raises(KeyError):
        RiserStackupModel.from_component_counts({"NO-SUCH-ID": 1}, registry)


def test_dry_weight_fail_closed():
    model = RiserStackupModel.from_records(
        [{"component_id": "x", "submerged_weight_kn": 1.0}]
    )
    with pytest.raises(ValueError, match="weight_air_kn"):
        model.total_dry_weight_kn()


def test_split_weights_fail_closed_on_buoyed_without_uplift():
    # A buoyed item (negative net submerged weight) WITHOUT uplift data must
    # raise — degrading to net weight is NON-conservative for the 16Q chain.
    model = RiserStackupModel.from_records(
        [
            {"component_id": "bare", "submerged_weight_kn": 100.0},
            {"component_id": "buoyed", "submerged_weight_kn": -10.0},
        ]
    )
    with pytest.raises(ValueError, match="buoyed"):
        model.gross_submerged_weight_and_uplift()


def test_split_weights_when_uplift_present():
    model = RiserStackupModel.from_records(
        [
            {"component_id": "bare", "submerged_weight_kn": 100.0},
            {
                "component_id": "buoyed",
                "submerged_weight_kn": -10.0,
                "buoyancy_uplift_kn": 60.0,
            },
        ]
    )
    gross, uplift = model.gross_submerged_weight_and_uplift()
    # gross = net + uplift for the buoyed item: -10 + 60 = 50 gross
    assert gross == pytest.approx(150.0)
    assert uplift == pytest.approx(60.0)
    # identity: net == gross - uplift
    assert model.total_submerged_weight_kn() == pytest.approx(gross - uplift)


def test_length_rule_height_fallback_and_overrides(registry):
    model = RiserStackupModel.from_component_counts(
        {"RJ-21-75-BARE": 1, "BOP-SUB-18.75-15K": 1}, registry
    )
    # BOP has height_m only -> fallback engages (no raise).
    assert model.total_length_m() > 22.86
    # A component with neither (flex joint) requires an explicit override.
    fj_model = RiserStackupModel.from_component_counts(
        {"RJ-21-75-BARE": 1, "FJ-UPPER-21": 1}, registry
    )
    with pytest.raises(ValueError, match="FJ-UPPER-21"):
        fj_model.total_length_m()
    assert fj_model.total_length_m(
        length_overrides={"FJ-UPPER-21": 0.9}
    ) == pytest.approx(75.0 * 0.3048 + 0.9)


# -- 16Q chain --------------------------------------------------------------------


def test_tensioner_system_factor_identity_edges():
    assert tensioner_system_factor(16, 0, 1.0) == pytest.approx(1.0)
    assert tensioner_system_factor(4, 1, 1.0) == pytest.approx(4.0 / 3.0)
    assert tensioner_system_factor(16, 2, 0.95) == pytest.approx(
        16.0 / (0.95 * 14.0)
    )


@pytest.mark.parametrize(
    "n_units,n_fail,efficiency",
    [(0, 0, 1.0), (4, 4, 1.0), (4, 5, 1.0), (4, 1, 0.0), (4, 1, 1.5)],
)
def test_tensioner_system_factor_validation(n_units, n_fail, efficiency):
    with pytest.raises(ValueError):
        tensioner_system_factor(n_units, n_fail, efficiency)


def test_minimum_top_tension_16q_composition():
    # Synthetic tuple, deliberately disjoint from any documented calculation.
    t = minimum_top_tension_16q(
        1000.0, n_units=8, n_fail=1, efficiency=0.9, fleet_angle_factor=1.02
    )
    assert t == pytest.approx(1000.0 * (8.0 / (0.9 * 7.0)) * 1.02)


def test_minimum_top_tension_requires_kwargs():
    with pytest.raises(TypeError):
        minimum_top_tension_16q(1000.0, 8, 1, 0.9, 1.02)  # positional forbidden


# -- rig capability ---------------------------------------------------------------


def _rig_rows():
    from digitalmodel.riser_database import RiserDatabase

    db = RiserDatabase.load()
    return db.rig("drillship-6g-16x3600k-dat"), db.rig("drillship-16w-wireline")


def test_check_rig_capability_pass_and_exceeds():
    dat, _ = _rig_rows()
    # 16 x 3600 kips direct-acting: far above a modest demand -> PASS
    ok = check_rig_capability(10_000.0, dat, n_fail=1)
    assert isinstance(ok, RigCapabilityCheck)
    assert ok.verdict == "PASS"
    assert ok.utilization is not None and 0 < ok.utilization < 1
    # absurd demand -> EXCEEDS
    assert check_rig_capability(1e9, dat, n_fail=1).verdict == "EXCEEDS"


def test_check_rig_capability_insufficient_data():
    _, wireline = _rig_rows()
    # wireline row carries n_tension_wires but no capacity -> INSUFFICIENT_DATA
    assert check_rig_capability(10_000.0, wireline, n_fail=2).verdict == (
        "INSUFFICIENT_DATA"
    )


def test_unit_count_selection_rule():
    dat, wireline = _rig_rows()
    assert assembly._unit_count(dat) == 16  # direct-acting -> n_tensioners
    assert assembly._unit_count(wireline) == 16  # wireline -> n_tension_wires
