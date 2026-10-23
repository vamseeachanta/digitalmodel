"""Standalone register contracts from issue #2334 (2026), follow-on scope.

All lives and campaign settings below are synthetic test inputs, not design
limits. Expected counts are independent hand calculations documented below.
"""

import pandas as pd
import pytest

from digitalmodel.asset_integrity.riser_joint_ffs import (
    RiserJointFFSWorkflow,
    fleet_rollup,
)


@pytest.fixture
def register():
    """Hand basis: min(10, 4) = 4 for A; B = 12, C = 0, D = 6 years."""
    return pd.DataFrame(
        {
            "component": ["Main"] * 5,
            "joint_id": ["A", "A", "B", "C", "D"],
            "min_life_years": [10.0, 4.0, 12.0, 0.0, 6.0],
        }
    )


@pytest.mark.parametrize("joint_id", ["A", "unselected"])
@pytest.mark.parametrize(
    "life",
    [float("nan"), float("inf"), -float("inf"), -1.0, None, pd.NA, "bad", ""],
    ids=["nan", "infinity", "negative-infinity", "negative", "none", "na", "text", "empty"],
)
def test_every_contributing_life_is_validated(register, joint_id, life):
    """Issue #2334: invalid scans must fail even beside a valid scan of A."""
    register = pd.DataFrame(
        [
            *register.to_dict("records"),
            {"component": "Main", "joint_id": joint_id, "min_life_years": life},
        ]
    )
    with pytest.raises(ValueError, match="finite nonnegative life for every row"):
        fleet_rollup(register)


@pytest.mark.parametrize("joint_id", [None, pd.NA, float("nan"), "", " \t"])
def test_every_contributing_joint_id_is_required(register, joint_id):
    """Issue #2334: grouping must not drop a scan whose joint is unidentified."""
    register.loc[0, "joint_id"] = joint_id
    with pytest.raises(ValueError, match="joint IDs"):
        fleet_rollup(register)


@pytest.mark.parametrize("numeric_text", [False, True])
def test_valid_totals_and_boundaries_are_preserved(register, numeric_text):
    """Hand calculation using the fixture's governing lives [4, 12, 0, 6].

    User inputs: duration 2 years, margin 2, required high-fatigue inventory 3.
    Horizons 2, 4, 6 give FIT counts 3, 3, 2 and REPAIR counts 1, 1, 2.
    High-fatigue horizons 4, 8, 12 give counts 3, 1, 1 and shortfalls 0, 2, 2.
    Equality counts as fit. Zero life is valid and counts as repair throughout.
    """
    if numeric_text:
        register["min_life_years"] = register["min_life_years"].astype(str)
    original = register.copy(deep=True)
    result = fleet_rollup(
        register,
        campaign_years=2.0,
        n_campaigns=3,
        high_fatigue_margin=2.0,
        min_high_fatigue_joints=3,
    )
    assert result == {
        "component": "Main",
        "n_joints": 4,
        "campaign_years": 2.0,
        "min_high_fatigue_joints": 3,
        "campaigns": [
            {
                "campaign": 1,
                "horizon_years": 2.0,
                "fit_joints": 3,
                "high_fatigue_qualified": 3,
                "repair_joints": 1,
                "high_fatigue_shortfall": 0,
            },
            {
                "campaign": 2,
                "horizon_years": 4.0,
                "fit_joints": 3,
                "high_fatigue_qualified": 1,
                "repair_joints": 1,
                "high_fatigue_shortfall": 2,
            },
            {
                "campaign": 3,
                "horizon_years": 6.0,
                "fit_joints": 2,
                "high_fatigue_qualified": 1,
                "repair_joints": 2,
                "high_fatigue_shortfall": 2,
            },
        ],
    }
    pd.testing.assert_frame_equal(register, original)


@pytest.mark.parametrize("component", ["Main", "Auxiliary"])
def test_only_selected_component_contributes(register, component):
    """The same four-joint hand basis applies; invalid other rows contribute none."""
    register["component"] = component
    other = pd.DataFrame(
        {"component": ["Other"], "joint_id": [None], "min_life_years": ["bad"]}
    )
    result = fleet_rollup(pd.concat([other, register]), component=component)
    assert result["component"] == component
    assert result["n_joints"] == 4


def test_no_selected_rows_fails(register):
    with pytest.raises(ValueError, match="no rows for component"):
        fleet_rollup(register, component="absent")


@pytest.mark.parametrize(
    "field,value",
    [
        ("min_life_years", float("nan")),
        ("min_life_years", float("inf")),
        ("min_life_years", -1.0),
        ("joint_id", None),
    ],
    ids=["nan", "infinity", "negative", "missing-id"],
)
def test_standalone_router_rejects_invalid_csv(tmp_path, register, field, value):
    """Issue #2334: standalone CSV mode propagates failure before publishing counts."""
    register.loc[0, field] = value
    path = tmp_path / "register.csv"
    register.to_csv(path, index=False)
    block = {"mode": "rollup", "rollup": {"register_csv": str(path)}}
    cfg = {"basename": "riser_joint_ffs", "riser_joint_ffs": block}
    with pytest.raises(ValueError, match="register requires"):
        RiserJointFFSWorkflow().router(cfg)
    assert cfg["riser_joint_ffs"] is block


def test_standalone_router_preserves_valid_totals(tmp_path, register):
    """Hand basis [4, 12, 0, 6] at horizon 6 gives FIT 2 and REPAIR 2.

    Supplied margin 2 gives high-fatigue horizon 12: one qualified joint.
    Supplied inventory requirement 3 therefore gives a shortfall of 2.
    """
    path = tmp_path / "register.csv"
    register.to_csv(path, index=False)
    cfg = {
        "basename": "riser_joint_ffs",
        "riser_joint_ffs": {
            "mode": "rollup",
            "rollup": {
                "register_csv": str(path),
                "campaign_years": 6.0,
                "n_campaigns": 1,
                "high_fatigue_margin": 2.0,
                "min_high_fatigue_joints": 3,
            },
        },
    }
    result = RiserJointFFSWorkflow().router(cfg)["riser_joint_ffs"]
    assert result["n_joints"] == 4
    assert result["campaigns"] == [
        {
            "campaign": 1,
            "horizon_years": 6.0,
            "fit_joints": 2,
            "high_fatigue_qualified": 1,
            "repair_joints": 2,
            "high_fatigue_shortfall": 2,
        }
    ]
