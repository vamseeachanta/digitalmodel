# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic intake contracts for an unevaluated tank handoff."""

import builtins
from copy import deepcopy
from dataclasses import asdict
import importlib.util
import io
import json
import os
from pathlib import Path
import shutil
import socket
import subprocess
import urllib.request

import pytest

from digitalmodel.asset_integrity.assessment.level3_escalation import Level3Handoff
from digitalmodel.asset_integrity import tank_settlement_handoff as adapter
from digitalmodel.asset_integrity.tank_settlement_workflow import assess


@pytest.fixture
def block():
    """Return synthetic measurements in the existing screening input shape."""
    return {
        "component_id": "SYNTHETIC-TANK",
        "tank": {"diameter_m": 10, "height_m": 12, "fill_height_m": 8,
                 "course_thicknesses_mm": [8, 6], "material_grade": "unspecified"},
        "shell_survey": {"angles_deg": [0, 90, 180, 270],
                         "elevations_mm": [10, 4, 2, 8]},
        "edge": {"radial_extent_m": 2, "shell_elevation_mm": -12,
                 "reference_elevation_mm": -2},
        "bottom_profile": {"distances_m": [0, 1, 3], "elevations_mm": [0, -4, 1]},
    }


def test_unevaluated_handoff_retains_diagnostics(block):
    handoff = adapter.prepare_tank_handoff(block)
    assert isinstance(handoff, Level3Handoff)
    assert handoff.triggered is True
    assert handoff.triggers and handoff.recommended_analysis and handoff.notes
    package = handoff.data_package
    assert package["original_inputs"] == block
    assert package["screening"] == assess(block)
    assert package["screening"]["decision"]["verdict"] == "ESCALATE"
    for key in ("margin", "margin_allowable", "rsf", "rsf_a", "remaining_life_yr"):
        assert package["screening"]["decision"][key] is None
    checks = package["proposed_checks"]
    assert set(checks) == {"strain_limit", "buckling", "plastic_collapse", "weld_strain"}
    for check in checks.values():
        assert check["status"] == "NOT_EVALUATED"
        assert check["value"] is None and check["margin"] is None
        assert check["required_evidence"]
    assert json.loads(json.dumps(asdict(handoff), allow_nan=False)) == asdict(handoff)
    text = " ".join(handoff.open_questions + [handoff.notes]).lower()
    for gap in ("datum", "angle", "units", "course", "roof", "annular", "material",
                "criteria", "crosswalk", "published", "imposed displacement"):
        assert gap in text


@pytest.mark.parametrize("missing", [(), ("edge",), ("bottom_profile",),
                                     ("edge", "bottom_profile")])
def test_optional_surveys_remain_missing(block, missing):
    for key in missing:
        del block[key]
    package = adapter.prepare_tank_handoff(block).data_package
    assert package["original_inputs"] == block
    for key, name, output in (("edge", "edge", "edge_survey"),
                              ("bottom_profile", "bottom", "bottom_profile")):
        if key in missing:
            assert key not in package["original_inputs"]
            assert package["screening"][output] is None
            assert package["screening"]["geometry"][name] is None
            flags = package["screening"]["decision"]["applicability"]["flags"]
            assert f"MISSING_{name.upper()}_SURVEY" in flags


@pytest.mark.parametrize("mutation", ["caller", "adapter", "screening"])
def test_nested_snapshots_are_independent(block, monkeypatch, mutation):
    original = deepcopy(block)
    screening = assess(block)
    expected_screening = deepcopy(screening)
    monkeypatch.setattr(adapter, "assess", lambda value: screening)
    handoff = adapter.prepare_tank_handoff(block)
    retained = handoff.data_package
    if mutation == "caller":
        block["tank"]["course_thicknesses_mm"][0] = 99
        block["shell_survey"]["elevations_mm"][0] = 99
        block["bottom_profile"]["elevations_mm"][0] = 99
        assert retained["original_inputs"] == original
        assert retained["screening"] == expected_screening
    elif mutation == "adapter":
        retained["original_inputs"]["tank"]["course_thicknesses_mm"][0] = 99
        retained["screening"]["shell_survey"]["elevations_mm"][0] = 99
        retained["screening"]["sources"][0]["document"] = "changed"
        assert block == original
        assert screening == expected_screening
    else:
        screening["checks"]["edge"]["status"] = "changed"
        assert retained["screening"] == expected_screening


@pytest.mark.parametrize("invalid", [None, [], {}, {"tank": {}}])
def test_malformed_mapping_matches_screening(invalid):
    with pytest.raises(ValueError) as expected:
        assess(invalid)
    with pytest.raises(ValueError) as actual:
        adapter.prepare_tank_handoff(invalid)
    assert str(actual.value) == str(expected.value)


@pytest.mark.parametrize("section,key,value", [
    ("shell_survey", "elevations_mm", [0, 1, 2]),
    ("shell_survey", "angles_deg", [0, 90, 90, 270]),
    ("shell_survey", "elevations_mm", [0, float("nan"), 2, 3]),
    ("shell_survey", "angles_deg", [0, 90, float("inf"), 270]),
    ("tank", "height_m", None),
    ("edge", "shell_elevation_mm", float("-inf")),
    ("bottom_profile", "elevations_mm", [0, float("nan"), 1]),
])
def test_invalid_measurements_match_screening(block, section, key, value):
    block[section][key] = value
    with pytest.raises(ValueError) as expected:
        assess(block)
    with pytest.raises(ValueError) as actual:
        adapter.prepare_tank_handoff(block)
    assert str(actual.value) == str(expected.value)


def test_call_performs_no_external_operations(block, monkeypatch):
    def denied(*args, **kwargs):
        raise AssertionError("External operation attempted")

    real_import = builtins.__import__

    def guarded_import(name, *args, **kwargs):
        assert not any(word in name.lower() for word in ("solver", "calculix", "ccx"))
        return real_import(name, *args, **kwargs)

    with monkeypatch.context() as guard:
        for obj, names in (
            (builtins, ("open",)), (io, ("open",)),
            (Path, ("open", "write_text", "write_bytes", "mkdir", "exists")),
            (os, ("open", "mkdir", "makedirs", "system", "popen", "stat", "access")),
            (subprocess, ("Popen", "run", "call", "check_call", "check_output")),
            (socket, ("socket", "create_connection", "getaddrinfo")),
            (urllib.request, ("urlopen",)), (shutil, ("which",)),
            (importlib.util, ("find_spec",)), (importlib, ("import_module",)),
        ):
            for name in names:
                guard.setattr(obj, name, denied)
        guard.setattr(builtins, "__import__", guarded_import)
        handoff = adapter.prepare_tank_handoff(block)
    assert handoff.triggered is True
