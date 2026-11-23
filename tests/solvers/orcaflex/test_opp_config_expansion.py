"""Unit tests for offline expansion of high-level OrcaFlex post-process flags.

Issue #885: the ``orcaflex-strength-post`` template supplies only high-level
``summary.flag`` / ``RangeGraph.flag`` + ``parameters.VarNames.Line`` but the
post-processors consume concrete ``summary_settings.groups`` /
``RangeGraph`` (list) blocks that nothing derived. These tests verify the
expansion fills those blocks, using a mock OrcFxAPI model (no license needed).
"""

import copy

import pytest

from digitalmodel.solvers.orcaflex.opp_config_expansion import (
    expand_high_level_postprocess,
    get_object_names_from_model,
)
from digitalmodel.solvers.orcaflex.opp import OrcaFlexPostProcess


# --------------------------------------------------------------------------- #
# Mock OrcFxAPI model (no license)
# --------------------------------------------------------------------------- #
class _FakeType:
    def __init__(self, name):
        self.name = name

    def __int__(self):
        return hash(self.name) % 1000


class _FakeObject:
    def __init__(self, name, type_name):
        self.name = name
        self.type = _FakeType(type_name)


class _FakeModel:
    """Stand-in for an OrcFxAPI model exposing a few objects."""

    def __init__(self, objects):
        self.objects = objects


def _strength_post_cfg():
    """The high-level template form (mirrors orcaflex-strength-post/input.yml)."""
    return {
        "meta": {"basename": "orcaflex_post_process"},
        "orcaflex": {
            "postprocess": {
                "summary": {"flag": True},
                "RangeGraph": {"flag": True},
                "time_series": {"flag": False},
                "visualization": {"flag": False},
            }
        },
        "parameters": {
            "VarNames": {
                "Line": ["Effective tension", "Wall tension", "Bend moment"],
            }
        },
    }


# --------------------------------------------------------------------------- #
# Pure expansion
# --------------------------------------------------------------------------- #
def test_high_level_flags_expand_to_per_line_summary_settings():
    cfg = _strength_post_cfg()
    line_names = ["Line1", "Line2"]

    # Before: nothing derives the concrete blocks (the reproduced bug).
    assert "summary_settings" not in cfg
    assert "RangeGraph" not in cfg

    expand_high_level_postprocess(cfg, line_names, object_type="Line")

    # summary_settings: one group per variable, one Column per Line.
    groups = cfg["summary_settings"]["groups"]
    assert [g["Label"] for g in groups] == [
        "Effective tension",
        "Wall tension",
        "Bend moment",
    ]
    for group in groups:
        assert [c["ObjectName"] for c in group["Columns"]] == line_names
        for col in group["Columns"]:
            assert col["Command"] == "Range Graph"
            assert col["Variable"] == group["Label"]
            assert col["Statistic_Type"] == "Max"
            assert col["Label"] in line_names


def test_high_level_flags_expand_to_per_line_range_graph():
    cfg = _strength_post_cfg()
    line_names = ["Line1", "Line2"]

    expand_high_level_postprocess(cfg, line_names, object_type="Line")

    rg = cfg["RangeGraph"]
    assert isinstance(rg, list)
    # one entry per Line x variable = 2 lines * 3 vars
    assert len(rg) == 6
    pairs = [(e["ObjectName"], e["Variable"]) for e in rg]
    assert ("Line1", "Effective tension") in pairs
    assert ("Line2", "Bend moment") in pairs
    for entry in rg:
        assert entry["AdditionalData"] == ["Max"]
        assert entry["SimulationPeriod"] == "WholeSimulation"


def test_explicit_blocks_are_not_overwritten_backward_compatible():
    """A detailed input with explicit blocks must be left untouched."""
    cfg = _strength_post_cfg()
    cfg["summary_settings"] = {
        "groups": [{"Label": "custom", "Columns": [{"ObjectName": "Line99"}]}]
    }
    cfg["RangeGraph"] = [{"ObjectName": "Line99", "Variable": "Tension"}]
    before = copy.deepcopy(cfg)

    expand_high_level_postprocess(cfg, ["Line1", "Line2"], object_type="Line")

    assert cfg["summary_settings"] == before["summary_settings"]
    assert cfg["RangeGraph"] == before["RangeGraph"]


def test_flags_off_leaves_cfg_unchanged():
    cfg = _strength_post_cfg()
    cfg["orcaflex"]["postprocess"]["summary"]["flag"] = False
    cfg["orcaflex"]["postprocess"]["RangeGraph"]["flag"] = False

    expand_high_level_postprocess(cfg, ["Line1", "Line2"], object_type="Line")

    assert "summary_settings" not in cfg
    assert "RangeGraph" not in cfg


# --------------------------------------------------------------------------- #
# Mock-model wiring through OrcaFlexPostProcess
# --------------------------------------------------------------------------- #
def test_get_object_names_from_model_filters_lines():
    from digitalmodel.solvers.orcaflex.orcaflex_objects import OrcaFlexObjects

    model = _FakeModel(
        [
            _FakeObject("Line1", "Line"),
            _FakeObject("Line2", "Line"),
            _FakeObject("Vessel1", "Vessel"),
            _FakeObject("Environment", "Environment"),
        ]
    )
    names = get_object_names_from_model(model, OrcaFlexObjects(), "Line")
    assert names == ["Line1", "Line2"]


def test_post_process_expansion_via_mock_model_object_names():
    """End-to-end (offline): high-level cfg + mock Line names -> concrete config."""
    cfg = _strength_post_cfg()
    opp = OrcaFlexPostProcess()

    # Drive expansion with names that would come from OrcFxAPI in a licensed run.
    opp.get_cfg_with_expanded_postprocess(cfg, object_names=["LineA", "LineB"])

    assert cfg["summary_settings"]["groups"][0]["Label"] == "Effective tension"
    assert [c["ObjectName"] for c in cfg["summary_settings"]["groups"][0]["Columns"]] == [
        "LineA",
        "LineB",
    ]
    assert len(cfg["RangeGraph"]) == 6


def test_no_line_names_offline_leaves_cfg_unchanged():
    """No discoverable Lines (offline / no license) -> cfg unchanged, no crash."""
    cfg = _strength_post_cfg()
    opp = OrcaFlexPostProcess()

    opp.get_cfg_with_expanded_postprocess(cfg, object_names=[])

    assert "summary_settings" not in cfg
    assert "RangeGraph" not in cfg
