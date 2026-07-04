"""Criteria loader/validation tests (digitalmodel #1359)."""

import pytest

from digitalmodel.motion_forecast.criteria import Criterion, load_criteria


def test_defaults_load_and_are_valid():
    crits = load_criteria()
    assert {"crane_lift", "gangway_transfer", "helideck_landing"} <= set(crits)
    crane = crits["crane_lift"]
    assert crane.governing == "crane_tip_vertical_velocity"
    assert 0.0 < crane.caution <= crane.limit
    assert crane.warning_factor == pytest.approx(crane.caution / crane.limit)
    assert len(crane.poi_offset) == 3


def test_load_from_path(tmp_path):
    p = tmp_path / "c.yml"
    p.write_text(
        "criteria:\n"
        "  lift:\n"
        "    label: L\n"
        "    governing: crane_tip_vertical_velocity\n"
        "    caution: 0.2\n"
        "    limit: 0.4\n"
        "    unit: m/s\n"
        "    poi_offset: [10, 0, 0]\n"
    )
    crits = load_criteria(str(p))
    assert crits["lift"].limit == 0.4
    assert crits["lift"].alpha == 1.0  # default


def _write(tmp_path, body):
    p = tmp_path / "bad.yml"
    p.write_text(body)
    return str(p)


def test_rejects_caution_above_limit(tmp_path):
    body = ("criteria:\n  x:\n    label: X\n    governing: crane_tip_vertical_velocity\n"
            "    caution: 0.6\n    limit: 0.4\n    unit: m/s\n    poi_offset: [0,0,0]\n")
    with pytest.raises(ValueError, match="caution"):
        load_criteria(_write(tmp_path, body))


def test_rejects_unknown_governing(tmp_path):
    body = ("criteria:\n  x:\n    label: X\n    governing: bogus\n"
            "    caution: 0.2\n    limit: 0.4\n    unit: m/s\n    poi_offset: [0,0,0]\n")
    with pytest.raises(ValueError, match="unknown governing"):
        load_criteria(_write(tmp_path, body))


def test_rejects_empty(tmp_path):
    with pytest.raises(ValueError, match="non-empty"):
        load_criteria(_write(tmp_path, "criteria: {}\n"))


def test_rejects_nonpositive_caution(tmp_path):
    body = ("criteria:\n  x:\n    label: X\n    governing: crane_tip_vertical_velocity\n"
            "    caution: 0.0\n    limit: 0.4\n    unit: m/s\n    poi_offset: [0,0,0]\n")
    with pytest.raises(ValueError, match="caution"):
        load_criteria(_write(tmp_path, body))


def test_rejects_missing_key(tmp_path):
    body = ("criteria:\n  x:\n    label: X\n    governing: crane_tip_vertical_velocity\n"
            "    caution: 0.2\n    unit: m/s\n    poi_offset: [0,0,0]\n")  # no limit
    with pytest.raises(ValueError, match="malformed"):
        load_criteria(_write(tmp_path, body))
