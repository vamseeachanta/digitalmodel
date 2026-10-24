"""Validation contract: atlas-spec.md Decisions 2–4 and issue 2118.

All numbers are dimensionless synthetic inputs. Independent hand calculation:
f(x) = x on knots (1, 1), (3, 3); at x=2, weight=(2-1)/(3-1)=1/2,
f=1*(1-1/2)+3*(1/2)=2. With measured e=0 the band is [2, 2].
Zero denotes a measured residual, never an assumption about missing evidence.
"""

import math
from pathlib import Path

import pandas as pd
import pytest
import yaml

from digitalmodel.parametric.atlas import Atlas, Axis
from digitalmodel.parametric.catalog import _summarize
from digitalmodel.parametric.query import (
    _handle_annual_damage,
    _handle_capacity_demand,
    _handle_fatigue_bins,
    _handle_utilisation,
    _handle_value,
    router,
)


def _atlas(validation):
    return Atlas(
        basename="viv_analysis", atlas_id="identity-example", physics="linear",
        response="response", axes=[Axis("x", grid=[1.0, 3.0])],
        grid=pd.DataFrame({"x": [1.0, 3.0], "response": [1.0, 3.0]}),
        validation=validation,
    )


@pytest.mark.parametrize("validation", [
    {}, {"max_rel_error": None}, {"max_rel_error": math.nan},
    {"max_rel_error": math.inf}, {"max_rel_error": -math.inf},
    {"max_rel_error": "0"}, {"max_rel_error": False},
    {"max_rel_error": -0.1},
    {"status": "unavailable", "max_rel_error": 0.0},
])
def test_unavailable_validation_has_no_error_or_confidence(validation):
    atlas = _atlas(validation)
    assert not atlas.validation_available
    assert math.isnan(atlas.max_rel_error)
    assert math.isnan(atlas.local_error({"x": 2.0}))
    result = _handle_value(atlas, {"x": 2.0})
    assert result["action"] == "escalate"
    assert result["reason"] == "validation unavailable"
    assert result["value"] is None
    assert "confidence" not in result
    assert _summarize(atlas)["max_err"] == "unavailable"


def test_measured_zero_retains_zero_width_band():
    atlas = _atlas({"max_rel_error": 0.0})
    assert atlas.validation_available
    assert atlas.max_rel_error == 0.0
    result = _handle_value(atlas, {"x": 2.0})
    assert result["value"] == 2.0
    assert result["confidence"]["band"] == [2.0, 2.0]


@pytest.mark.parametrize("handler", [
    _handle_value, _handle_utilisation, _handle_capacity_demand,
    _handle_fatigue_bins, _handle_annual_damage,
])
def test_every_confidence_handler_escalates_before_consuming_inputs(handler):
    result = handler(_atlas({}), {})
    assert "validation unavailable" in result["reason"]
    assert "confidence" not in result


def test_local_map_cannot_substitute_for_missing_global_validation():
    atlas = _atlas({"local_error_map": {"": {"x": [0.0]}}})
    assert math.isnan(atlas.local_error({"x": 2.0}))


@pytest.mark.parametrize("validation", [{}, {"max_rel_error": 0.0}])
def test_save_load_preserves_validation_state(tmp_path, validation):
    atlas = _atlas(validation)
    out = atlas.save(tmp_path)
    manifest = yaml.safe_load((out / "manifest.yaml").read_text())
    loaded = Atlas.load(tmp_path, atlas.basename)
    assert loaded.validation_available == atlas.validation_available
    if atlas.validation_available:
        assert manifest["validation"]["max_rel_error"] == 0.0
        assert loaded.max_rel_error == 0.0
    else:
        assert manifest["validation"]["max_rel_error"] is None
        assert manifest["validation"]["status"] == "unavailable"
        assert math.isnan(loaded.max_rel_error)
    result = router({"parametric_query": {
        "atlas": atlas.basename, "atlas_root": str(tmp_path),
        "point": {"x": 2.0}, "output_dir": str(tmp_path / "out"),
    }})["parametric_query"]["result"]
    assert result["in_range"] is True


def test_operability_screen_escalates_without_validation(monkeypatch):
    from digitalmodel.drilling_riser import operability_screening as screening

    monkeypatch.setattr(screening, "_atlas", lambda root: _atlas({}))
    result = screening.screen_operability("synthetic", 2.0, 2.0)
    assert result.light == screening.ESCALATE
    assert result.governing_utilisation is None
    assert "validation unavailable" in result.reason


def test_rao_without_validation_escalates_with_an_invalid_period():
    """A zero period cannot produce a lookup or a confidence band."""
    from digitalmodel.parametric.query import _handle_rao

    assert "validation unavailable" in _handle_rao(_atlas({}), {"period_s": 0.0})["reason"]


@pytest.mark.parametrize("basename", [
    "diffraction_library", "orcaflex_library",
    "drilling_riser_envelope", "drilling_riser_drift_off",
])
def test_committed_library_manifest_matches_validation_state(basename):
    root = Path(__file__).resolve().parents[2] / "atlases"
    atlas = Atlas.load(root, basename)
    manifest = yaml.safe_load(
        (root / basename / atlas.atlas_id / "manifest.yaml").read_text()
    )
    assert not atlas.validation_available
    assert manifest["validation"]["max_rel_error"] is None
    assert manifest["validation"]["status"] == "unavailable"


@pytest.mark.parametrize("handler", [
    _handle_value, _handle_utilisation, _handle_capacity_demand,
    _handle_fatigue_bins, _handle_annual_damage,
])
@pytest.mark.parametrize("x, in_range", [(2.0, True), (4.0, False)])
def test_unavailable_validation_preserves_range_and_all_reasons(handler, x, in_range):
    point = {"x": x, "tension_range_bins": [{"n_cycles": 1.0}]}
    result = handler(_atlas({}), point)
    assert result["in_range"] is in_range
    assert result["value"] is None
    assert result["action"] == "escalate"
    assert "validation unavailable" in result["reason"]
    assert ("outside" in result["reason"]) is (not in_range)


@pytest.mark.parametrize("offset, in_range", [(2.0, True), (4.0, False)])
def test_operability_reports_validation_and_range(monkeypatch, offset, in_range):
    from digitalmodel.drilling_riser import operability_screening as screening

    atlas = _atlas({})
    atlas.axes[0].name = "offset_pct"
    atlas.grid = atlas.grid.rename(columns={"x": "offset_pct"})
    monkeypatch.setattr(screening, "_atlas", lambda root: atlas)
    result = screening.screen_operability("synthetic", offset, 1.0)
    assert result.in_range is in_range
    assert result.light == screening.ESCALATE
    assert "validation unavailable" in result.reason
    assert ("outside" in result.reason) is (not in_range)


def test_rao_period_conversion_preserves_geometric_range_without_validation():
    from digitalmodel.parametric.query import _handle_rao

    atlas = _atlas({})
    atlas.axes[0].name = "frequency_rad_s"
    atlas.grid = atlas.grid.rename(columns={"x": "frequency_rad_s"})
    result = _handle_rao(atlas, {"period_s": math.pi})
    assert result["in_range"] is True
    assert result["reason"] == "validation unavailable"
    assert result["value"] is None


def test_straddling_confidence_band_preserves_geometric_range():
    result = _handle_utilisation(_atlas({"max_rel_error": 0.1}), {"x": 1.0})
    assert result["in_range"] is True
    assert result["action"] == "escalate"
    assert result["value"] is None
    assert "straddles" in result["reason"]


@pytest.mark.parametrize("x", [2.0, 4.0])
def test_router_range_policy_is_independent_of_validation(tmp_path, x):
    atlas = _atlas({})
    atlas.save(tmp_path)
    cfg = {"parametric_query": {
        "atlas": atlas.basename, "atlas_root": str(tmp_path), "point": {"x": x},
        "policy": {"on_out_of_range": "error"}, "output_dir": str(tmp_path / "out"),
    }}
    if x == 4.0:
        with pytest.raises(ValueError, match="validation unavailable; x=4 outside"):
            router(cfg)
    else:
        result = router(cfg)["parametric_query"]["result"]
        assert result["in_range"] is True
        assert result["action"] == "escalate"
        assert result["value"] is None


def test_unvalidated_fatigue_checks_coverage_of_every_bin():
    result = _handle_fatigue_bins(_atlas({}), {
        "tension_range_bins": [{"x": 2.0}, {"x": 4.0}],
    })
    assert result["in_range"] is False
    assert result["reason"] == "validation unavailable; x=4 outside [1, 3]"
