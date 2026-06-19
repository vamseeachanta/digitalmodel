"""Tests for the crane Go/No-Go demo wired to vessel-database crane curves."""

from __future__ import annotations

import importlib.util
import sys
from pathlib import Path

# Load the demo module by path (it lives under examples/, not the package).
_DEMO = (
    Path(__file__).resolve().parents[3]
    / "examples"
    / "demos"
    / "installation"
    / "go_no_go_crane_curves.py"
)
_spec = importlib.util.spec_from_file_location("go_no_go_crane_curves", _DEMO)
demo = importlib.util.module_from_spec(_spec)
sys.modules[_spec.name] = demo  # so @dataclass can resolve cls.__module__
_spec.loader.exec_module(demo)


def test_demo_file_exists():
    assert _DEMO.is_file()


def test_light_lift_go_for_capable_vessels():
    # The fleet now spans 80+ real vessels incl. small crane barges, so a fixed
    # light lift is not GO for *every* vessel — but any vessel with ample
    # capacity (SWL >= 2x lift, well within the 0.70 static + dynamic limits)
    # must be GO.
    lift = 50.0
    verdicts = demo.screen_fleet(lift_te=lift, radius_m=20.0)
    assert verdicts, "no vessels screened"
    ample = [v for v in verdicts if v.swl_at_radius_te >= 2 * lift]
    assert ample, "expected some vessels with ample capacity for a light lift"
    assert all(v.decision == "GO" for v in ample)


def test_absurd_lift_all_no_go():
    verdicts = demo.screen_fleet(lift_te=40000.0, radius_m=30.0)
    assert all(v.decision == "NO_GO" for v in verdicts)


def test_verdicts_sorted_go_first():
    verdicts = demo.screen_fleet(lift_te=5000.0, radius_m=35.0)
    order = {"GO": 0, "MARGINAL": 1, "NO_GO": 2}
    keys = [order[v.decision] for v in verdicts]
    assert keys == sorted(keys), "verdicts not ordered GO -> NO_GO"


def test_radius_curve_is_applied_when_published():
    """A vessel with a published radius curve gives different SWL at different radii."""
    near = {v.vessel: v for v in demo.screen_fleet(lift_te=1000.0, radius_m=25.0)}
    far = {v.vessel: v for v in demo.screen_fleet(lift_te=1000.0, radius_m=45.0)}
    curved = [name for name, v in near.items() if not v.headline_only]
    assert curved, "expected at least one vessel with a published radius curve"
    name = curved[0]
    assert (
        far[name].swl_at_radius_te <= near[name].swl_at_radius_te
    ), f"{name}: SWL should not increase with radius"


def test_decisions_are_valid_states():
    for v in demo.screen_fleet(lift_te=2500.0, radius_m=40.0):
        assert v.decision in {"GO", "MARGINAL", "NO_GO"}
        assert v.swl_at_radius_te > 0
