"""Negative cases from #2746; all numerical fixtures are synthetic inputs.

Expected values use elementary interpolation and capacity/demand arithmetic,
written out below, rather than the response implementations under test.
"""

import math
import warnings
from pathlib import Path
from types import SimpleNamespace

import pandas as pd
import pytest
import yaml

from digitalmodel.parametric import build, generate, query, refresh
from digitalmodel.parametric.atlas import Atlas, Axis, Prediction
from digitalmodel.parametric.library import build_library_atlas


def _atlas():
    """Constant synthetic capacity: interpolation of [100, 100] is 100 kN."""
    return Atlas(
        "pile_capacity", "synthetic", "linear", "capacity_kN",
        [Axis("x", grid=[0.0, 1.0])],
        pd.DataFrame({"x": [0.0, 1.0], "capacity_kN": [100.0, 100.0]}),
        validation={"passes": True, "max_rel_error": 0.0},
    )


def _generate(monkeypatch, fn, axes=None):
    monkeypatch.setitem(generate.RESPONSE_FUNCS, "synthetic", fn)
    return generate.generate_atlas(
        "synthetic", "linear", "value", axes or [Axis("x", grid=[0.0, 1.0])],
        tolerance=0.1,  # Synthetic gate limit, supplied by the test.
    )


@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("where", [0.0, 0.5])
def test_nonfinite_grid_or_holdout_fails(monkeypatch, bad, where):
    atlas = _generate(monkeypatch, lambda p: bad if p["x"] == where else 1.0)
    assert atlas.validation["passes"] is False
    assert atlas.validation["failures"]


def test_out_of_range_holdout_fails(monkeypatch):
    monkeypatch.setattr(Atlas, "predict", lambda *args: Prediction(1.0, False, "test"))
    atlas = _generate(monkeypatch, lambda p: 1.0)
    assert atlas.validation["passes"] is False


def test_zero_actual_is_not_zero_error(monkeypatch):
    """Endpoints are 1, midpoint is 0: absolute residual is 1, not 0."""
    atlas = _generate(monkeypatch, lambda p: 0.0 if p["x"] == 0.5 else 1.0)
    assert atlas.validation["passes"] is False


def test_off_median_curvature_fails(monkeypatch):
    """f=1+y^4*x^2. At x=.5,y=1, linear=1.5, actual=1.25: error=20%."""
    atlas = _generate(monkeypatch, lambda p: 1 + p["y"]**4 * p["x"]**2,
                      [Axis("x", grid=[0.0, 1.0]), Axis("y", grid=[0.0, 0.5, 1.0])])
    assert atlas.validation["passes"] is False
    assert atlas.validation["local_error_map"][""]["x"][0] >= 0.2 - 1e-12


@pytest.mark.parametrize("bins", [[], [{"n_cycles": 0}], [{"n_cycles": -1}],
                                  [{"n_cycles": math.nan}]])
def test_invalid_fatigue_bins_escalate(bins):
    result = query._handle_fatigue_bins(_atlas(), {"tension_range_bins": bins})
    assert result["in_range"] is False
    assert result["action"] == "escalate"


def test_capacity_requires_factor():
    with pytest.raises(ValueError, match="factor_of_safety"):
        query._handle_capacity_demand(_atlas(), {"x": 0.5, "demand_kN": 20.0})


def test_capacity_records_factor():
    """20 kN * factor 2 / capacity 100 kN = utilisation 0.4."""
    result = query._handle_capacity_demand(
        _atlas(), {"x": 0.5, "demand_kN": 20.0, "factor_of_safety": 2.0})
    assert result["factor_of_safety"] == 2.0
    assert result["value"] == pytest.approx(0.4, abs=1e-9)


def test_missing_validation_is_unknown(tmp_path):
    atlas = _atlas()
    atlas.validation = {}
    assert atlas.max_rel_error is None
    result = query._handle_value(atlas, {"x": 0.5})
    assert result["confidence"]["band"] is None
    assert result["confidence"]["basis"] == "not established"
    out = atlas.save(tmp_path)
    assert yaml.safe_load((out / "manifest.yaml").read_text())["validation"]["max_rel_error"] is None


def test_failed_build_does_not_install(monkeypatch, tmp_path):
    atlas = _atlas()
    atlas.save(tmp_path)
    pointer = tmp_path / atlas.basename / "default.txt"
    old = pointer.read_bytes()
    atlas.atlas_id = "failed"
    atlas.validation["passes"] = False
    monkeypatch.setattr(build, "_row", lambda _: {
        "basename": atlas.basename, "input": "unused",
        "parametric": {"physics": "linear", "response": "capacity_kN", "axes": []}})
    monkeypatch.setattr(build, "generate_atlas_adaptive", lambda **kwargs: atlas)
    monkeypatch.setattr(refresh, "content_fingerprint", lambda *args: "synthetic")
    build.build_atlas_from_registry("synthetic", atlas_root=tmp_path)
    assert pointer.read_bytes() == old
    assert not (pointer.parent / "failed").exists()


@pytest.mark.parametrize("key", ["unknown", "environment"])
def test_unknown_point_key_rejected(monkeypatch, tmp_path, key):
    atlas = _atlas()
    monkeypatch.setattr(Atlas, "load", lambda *args: atlas)
    with pytest.raises(ValueError, match="unknown point"):
        query.router({"parametric_query": {"atlas": "pile_capacity",
                     "point": {"x": 0.5, key: "ignored"}, "output_dir": str(tmp_path)}})


def test_unknown_policy_key_rejected():
    with pytest.raises(ValueError, match="unknown policy"):
        query.router({"parametric_query": {"atlas": "pile_capacity",
                     "policy": {"confidnce": "required"}}})


@pytest.mark.parametrize("legacy_policy", [False, True])
def test_shipped_query_defaults_serve_validated_capacity(monkeypatch, tmp_path, legacy_policy):
    """The engine merges this base policy into every atlas-query input."""
    defaults = Path(query.__file__).parents[1] / "base_configs/modules/parametric_query/parametric_query.yml"
    cfg = yaml.safe_load(defaults.read_text())
    cfg["parametric_query"].update(
        atlas="pile_capacity", output_dir=str(tmp_path),
        point={"x": 0.5, "demand_kN": 20.0, "factor_of_safety": 2.0},
    )
    if legacy_policy:
        cfg["parametric_query"]["policy"]["confidence"] = "required"
    monkeypatch.setattr(Atlas, "load", lambda *args: _atlas())
    monkeypatch.setattr(query, "_staleness", lambda _: None)
    with warnings.catch_warnings(record=True) as caught:
        warnings.simplefilter("always")
        result = query.router(cfg)["parametric_query"]["result"]
    assert len(caught) == int(legacy_policy)
    if legacy_policy:
        assert caught[0].category is DeprecationWarning
        assert "policy.confidence" in str(caught[0].message)
        assert "remove" in str(caught[0].message)
        assert caught[0].filename == __file__
    assert result["value"] == pytest.approx(0.4)
    assert result["confidence"]["band"] == pytest.approx([0.4, 0.4])
    assert result["screening_status"] == "pass"


@pytest.mark.parametrize("confidence", ["optional", "ignore", None])
def test_deprecated_confidence_cannot_disable_validation(confidence):
    with pytest.raises(ValueError, match="confidence"):
        query.router({"parametric_query": {"atlas": "pile_capacity",
                     "policy": {"confidence": confidence}}})


def test_provenance_includes_response_kwargs():
    atlas = _atlas()
    atlas.provenance["response_kwargs"] = {"basis": "synthetic"}
    assert query._provenance(atlas)["response_kwargs"] == {"basis": "synthetic"}


@pytest.mark.parametrize("legacy_policy", [False, True])
def test_failed_validation_query_escalates(monkeypatch, tmp_path, legacy_policy):
    atlas = _atlas()
    atlas.validation["passes"] = False
    monkeypatch.setattr(Atlas, "load", lambda *args: atlas)
    monkeypatch.setattr(query, "_staleness", lambda _: None)
    result = query.router({"parametric_query": {"atlas": "pile_capacity",
                          "policy": {"confidence": "required"} if legacy_policy else {},
                          "output_dir": str(tmp_path)}})["parametric_query"]["result"]
    assert result["action"] == "escalate"
    assert "validation" in result["reason"]


@pytest.mark.parametrize("failure", ["missing", "recompute", "library"])
def test_staleness_failure_is_closed(monkeypatch, failure):
    atlas = _atlas()
    def unavailable(*args):
        raise RuntimeError("unavailable")
    if failure == "recompute":
        atlas.provenance.update(workflow_id="synthetic", content_fingerprint="old")
        monkeypatch.setattr(refresh, "content_fingerprint", unavailable)
    elif failure == "library":
        atlas.provenance["kind"] = "library"
        monkeypatch.setattr(refresh, "library_drift", unavailable)
    assert query._staleness(atlas)


@pytest.mark.parametrize("xs", [[0.0, 0.0], [0.0, 0.75]])
def test_library_rejects_wrong_cartesian_membership(xs):
    with pytest.raises(ValueError, match="grid"):
        build_library_atlas(
            basename="synthetic", key_axis="case", key_values=["a"],
            continuous_axes=[Axis("x", grid=[0.0, 1.0])], response="value",
            records=[{"case": "a", "x": x, "value": 1.0} for x in xs],
            solver={}, coverage_note="Synthetic fixture")


def test_load_rejects_duplicate_missing_row(tmp_path):
    atlas = _atlas()
    out = atlas.save(tmp_path)
    atlas.grid.loc[1, "x"] = 0.0
    atlas.grid.to_parquet(out / "grid.parquet", index=False)
    with pytest.raises(ValueError, match="grid"):
        Atlas.load(tmp_path, atlas.basename)


def test_zero_sliding_capacity_is_infinite(monkeypatch):
    from digitalmodel.geotechnical import mudmat
    monkeypatch.setattr(mudmat, "mudmat_bearing_capacity", lambda **kwargs: SimpleNamespace(
        vertical_capacity_kn=100.0, sliding_capacity_kn=0.0))
    assert math.isinf(generate._mudmat_bearing_utilisation(
        {"vertical_load_kN": 1.0, "undrained_shear_strength_kpa": 1.0}))

