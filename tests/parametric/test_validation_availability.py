"""Validation contract: atlas-spec.md Decisions 2–4 and issue 2118.

All numbers are dimensionless synthetic inputs. Independent hand calculation:
f(x) = x on knots (1, 1), (3, 3); at x=2, weight=(2-1)/(3-1)=1/2,
f=1*(1-1/2)+3*(1/2)=2. With measured e=0 the band is [2, 2].
Zero denotes a measured residual, never an assumption about missing evidence.
"""

import math

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
    assert result["reason"] == "validation unavailable"
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
    assert result["in_range"] == atlas.validation_available


def test_operability_screen_escalates_without_validation(monkeypatch):
    from digitalmodel.drilling_riser import operability_screening as screening

    monkeypatch.setattr(screening, "_atlas", lambda root: _atlas({}))
    result = screening.screen_operability("synthetic", 2.0, 2.0)
    assert result.light == screening.ESCALATE
    assert result.governing_utilisation is None
    assert result.reason == "validation unavailable"


def test_rao_without_validation_escalates_before_period_conversion():
    """A zero period is deliberately invalid; no conversion is needed to escalate."""
    from digitalmodel.parametric.query import _handle_rao

    assert _handle_rao(_atlas({}), {"period_s": 0.0})["reason"] == "validation unavailable"


@pytest.mark.parametrize("basename", [
    "diffraction_library", "orcaflex_library", "drilling_riser_envelope",
    "drilling_riser_drift_off",
])
def test_committed_library_manifest_matches_validation_artifact(basename):
    from digitalmodel.parametric.query import DEFAULT_ATLAS_ROOT

    atlas = Atlas.load(DEFAULT_ATLAS_ROOT, basename)
    assert not atlas.validation_available
    manifest_path = DEFAULT_ATLAS_ROOT / basename / atlas.atlas_id / "manifest.yaml"
    summary = yaml.safe_load(manifest_path.read_text())["validation"]
    assert summary["max_rel_error"] is None
    assert summary["status"] == "unavailable"
