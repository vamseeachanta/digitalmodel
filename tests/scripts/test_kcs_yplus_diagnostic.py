"""The y+ wall-resolution diagnostic in the #1173 scorer.

The property under test is not "y+ parses". It is that an ABSENT or
UNREADABLE diagnostic can never be mistaken for a clean one, and that the
diagnostic never acquires gating power. Both are failure modes this repo has
been bitten by: a missing check reads greener than a failing check, and a
threshold chosen after seeing the answer is not a threshold.
"""
from __future__ import annotations

import importlib.util
from pathlib import Path

import pytest

SCRIPT = (Path(__file__).resolve().parents[2]
          / "scripts" / "cfd" / "generate-kcs-verification.py")


@pytest.fixture(scope="module")
def mod():
    assert SCRIPT.is_file(), f"scorer not found at {SCRIPT}"
    spec = importlib.util.spec_from_file_location("kcs_verification", SCRIPT)
    m = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(m)
    return m


SAMPLE = """Time = 25000
yPlus yPlus write:
    writing field yPlus
    patch hull y+ : min = 0.104438, max = 353.482, average = 45.1912
    patch deck y+ : min = 0.0, max = 12.5, average = 3.1
End
"""


def test_absent_log_reports_unavailable_not_clean(mod, tmp_path):
    r = mod.yplus_summary(tmp_path)
    assert r["available"] is False
    assert "not present" in r["reason"]
    assert "hull" not in r, "an absent diagnostic must not report a hull result"


def test_present_but_unparseable_log_is_also_unavailable(mod, tmp_path):
    """rc=0 with no parseable summary is a format mismatch, not a pass."""
    (tmp_path / "log.yPlus").write_text("exited 0 with nothing to parse\n")
    r = mod.yplus_summary(tmp_path)
    assert r["available"] is False
    assert "no patch summary parsed" in r["reason"]


def test_parses_every_patch_and_the_hull_values(mod, tmp_path):
    (tmp_path / "log.yPlus").write_text(SAMPLE)
    r = mod.yplus_summary(tmp_path)
    assert r["available"] is True
    assert set(r["patches"]) == {"hull", "deck"}
    assert r["hull"]["min"] == pytest.approx(0.104438)
    assert r["hull"]["max"] == pytest.approx(353.482)
    assert r["hull"]["average"] == pytest.approx(45.1912)


def test_writing_field_line_is_not_mistaken_for_a_summary(mod, tmp_path):
    """`writing field yPlus` sits adjacent to the real lines in every log."""
    (tmp_path / "log.yPlus").write_text(SAMPLE)
    r = mod.yplus_summary(tmp_path)
    assert "yPlus" not in r["patches"]
    assert "field" not in r["patches"]


def test_missing_named_patch_is_reported_not_silently_dropped(mod, tmp_path):
    (tmp_path / "log.yPlus").write_text(SAMPLE)
    r = mod.yplus_summary(tmp_path, patch="rudder")
    assert r["available"] is True
    assert "hull" not in r
    assert "rudder" in r["reason"]


@pytest.mark.parametrize(
    "avg,expected",
    [(0.4, "viscous sublayer"), (5.0, "viscous sublayer"),
     (12.0, "buffer layer"), (29.9, "buffer layer"),
     (30.0, "log layer"), (250.0, "log layer")],
)
def test_band_boundaries(mod, avg, expected):
    assert mod._yplus_band(avg).startswith(expected)


def test_buffer_span_is_detected_even_when_the_average_looks_fine(mod, tmp_path):
    """The mean can sit in the log layer while part of the hull does not.

    This is the whole reason the span is reported alongside the average.
    """
    (tmp_path / "log.yPlus").write_text(
        "    patch hull y+ : min = 0.104438, max = 353.482, average = 45.1912\n")
    r = mod.yplus_summary(tmp_path)
    assert r["hull"]["band_at_average"].startswith("log layer")
    assert r["hull"]["spans_buffer_layer"] is True


def test_wholly_resolved_hull_does_not_claim_a_buffer_span(mod, tmp_path):
    (tmp_path / "log.yPlus").write_text(
        "    patch hull y+ : min = 0.2, max = 1.9, average = 0.9\n")
    r = mod.yplus_summary(tmp_path)
    assert r["hull"]["spans_buffer_layer"] is False


def test_diagnostic_declares_that_it_does_not_gate(mod, tmp_path):
    (tmp_path / "log.yPlus").write_text(SAMPLE)
    assert mod.yplus_summary(tmp_path)["gates"] is False


def test_yplus_appears_in_no_criterion(mod):
    """The guard against this diagnostic quietly becoming a gate.

    If a future change routes y+ into the criteria, this fails — which is the
    point. It was added after the answer was visible and must stay evidence.
    """
    source = SCRIPT.read_text()
    marker = source.index("def build_html")
    criteria_region = source[:marker]
    for token in ("V1", "V2a", "V2b", "V3"):
        for line in criteria_region.splitlines():
            if token in line and "yplus" in line.lower():
                pytest.fail(f"y+ reached a criterion definition: {line.strip()}")


def test_html_renders_both_the_present_and_absent_cases(mod):
    r_present = {"available": True, "gates": False,
                 "patches": {"hull": {}},
                 "hull": {"min": 0.1, "average": 45.2, "max": 353.5,
                          "band_at_average": "log layer (wall-function regime)",
                          "spans_buffer_layer": True}}
    r_absent = {"available": False, "reason": "log.yPlus not present"}
    manifest = _minimal_manifest(r_present, r_absent)
    html = mod.build_html(manifest)
    assert "gates nothing" in html
    assert "not collected" in html
    assert "45.2" in html


def _minimal_manifest(prod, comp) -> dict:
    """Only the keys build_html reads, so the test does not need a solved run."""
    return {
        "generated_utc": "2026-08-14T00:00:00+00:00",
        "wall_resolution": {"production": prod, "companion": comp,
                            "gates": False, "note": "diagnostic"},
        "mesh_quality": {"production": {"passed": True}},
        "criteria": {
            "V1": {"quantity": "Ct", "computed": 3.8e-3, "reference": 3.56e-3,
                   "relative_error": 0.0699, "tolerance": 0.03, "passed": False},
            "V2a": {"quantity": "Cp", "computed": 3.9e-4, "reference": 7.28e-4,
                    "relative_error": -0.4675, "tolerance_low": -0.15,
                    "tolerance_high": 0.06, "passed": False},
            "V2b": {"quantity": "Cv", "computed": 3.4e-3,
                    "reference": 2.832045e-3, "relative_error": 0.2120,
                    "tolerance": 0.05, "passed": False},
        },
        "identity_check": {"holds": True},
        "detection_floor": {"ct_fraction": 0.0091},
        "measurement": {
            "ct": 3.8e-3, "cp": 3.9e-4, "cv": 3.4e-3,
            "force_total_N": 86.59, "averaging_window": 2000,
            "window_first_iteration": 8000, "window_last_iteration": 10000,
            "iterative_scatter_ct": 1.2e-5, "mesh_cells": 1539965,
        },
        "normalisation": {
            "reference_area_m2": 9.4379, "generated_surface_area_m2": 9.5609,
            "generated_vs_reference": 0.013, "note": "n", "diagnosis": "d",
            "bias_direction": "b",
        },
        "provenance": {
            "reference": {"ct": 3.56e-3}, "body_condition": "fixed even keel",
            "appendages": "without rudder", "wetted_surface_m2": 9.4379,
            "reynolds": 1.4e7, "froude": 0.26, "hull": "KCS",
            "geometry_source": "workshop grid", "model_scale": "1:31.6",
            "nu": 1.1418e-6, "velocity_used_for_nu": 2.1962, "density": 998.8,
            "iterations": 25000, "ranks": 8, "declared_deviations": {},
        },
    }
