"""Fail-closed regressions for audit sections 3--13."""
import json
import math
from types import SimpleNamespace

import pandas as pd
import pytest
import yaml

from digitalmodel.parametric import build, generate, query, refresh
from digitalmodel.parametric.atlas import Atlas, Axis, Prediction
from digitalmodel.parametric.library import build_library_atlas


def simple_atlas():
    return Atlas("pile_capacity", "test", "linear", "capacity", [Axis("x", grid=[1, 2])],
                 pd.DataFrame({"x": [1, 2], "capacity": [100, 100]}),
                 validation={"max_rel_error": 0.01, "threshold": 0.1, "passes": True},
                 provenance={"workflow_id": "test", "content_fingerprint": "test",
                             "response_kwargs": {"environment": "seawater_cp"}})


@pytest.mark.parametrize("physics", ["linear", "log_log"])
def test_refinement_selects_curved_coordinate(physics):
    from digitalmodel.parametric.refinement import refinement_point

    atlas = simple_atlas()
    atlas.physics = physics
    atlas.axes = [Axis("x", grid=[0, 2]), Axis("y", grid=[0, 2])]
    response = lambda p: p["x"] + p["y"] ** 2
    if physics == "log_log":
        response = lambda p: 10 ** (p["x"] + p["y"] ** 2)
    assert refinement_point(atlas, {"x": 0.5, "y": 0.5}, response) == {"x": 0, "y": 1}
    assert refinement_point(atlas, {"x": 0, "y": 2}, response) == {"x": 0, "y": 2}


@pytest.mark.parametrize("actual", [math.inf, math.nan, 0.0])
def test_failed_holdouts_are_strict_json(actual):
    from digitalmodel.parametric.validation import validate_atlas

    atlas = simple_atlas()
    validate_atlas(atlas, lambda point: actual, 0.1)
    assert not atlas.validation["passes"]
    json.dumps(atlas.validation, allow_nan=False)


def test_cli_reports_unknown_error(monkeypatch, capsys):
    import importlib.util
    from pathlib import Path

    path = Path(__file__).parents[2] / "scripts/parametric/build_atlas.py"
    spec = importlib.util.spec_from_file_location("atlas_cli", path)
    cli = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(cli)
    atlas = simple_atlas()
    atlas.validation.update(max_rel_error=None, passes=False, n_holdout=0)
    monkeypatch.setattr(cli, "build_atlas_from_registry", lambda wid: atlas)
    with pytest.raises(SystemExit, match="validation FAILED"):
        cli.main("test")
    assert "not established" in capsys.readouterr().out


def test_refresh_rejects_contradictory_pass_flag(tmp_path, monkeypatch):
    atlas = simple_atlas()
    atlas.validation["max_rel_error"] = 0.2
    root = tmp_path / "atlases"
    atlas.save(root)
    before = {p.relative_to(root): p.read_bytes() for p in root.rglob("*") if p.is_file()}
    monkeypatch.setattr(refresh, "DEFAULT_ATLAS_ROOT", root)
    monkeypatch.setattr(build, "build_atlas_from_registry", lambda *a, **kw: atlas)
    assert not refresh._rebuild_atlas("test")
    assert before == {p.relative_to(root): p.read_bytes() for p in root.rglob("*") if p.is_file()}


@pytest.mark.parametrize("bad", [math.nan, math.inf, -math.inf])
@pytest.mark.parametrize("at_grid", [True, False])
def test_nonfinite_response_fails_gate(monkeypatch, bad, at_grid):
    def response(point):
        return bad if point["x"] == (1 if at_grid else 1.5) else 1
    monkeypatch.setitem(generate.RESPONSE_FUNCS, "audit", response)
    atlas = generate.generate_atlas("audit", "linear", "value", [Axis("x", grid=[1, 2])])
    assert atlas.validation["passes"] is False
    assert atlas.validation["failures"]


def test_out_of_range_holdout_fails_gate(monkeypatch):
    monkeypatch.setitem(generate.RESPONSE_FUNCS, "audit", lambda p: 1)
    monkeypatch.setattr(Atlas, "predict", lambda self, p: Prediction(1, False, "outside"))
    atlas = generate.generate_atlas("audit", "linear", "value", [Axis("x", grid=[1, 2])])
    assert atlas.validation["passes"] is False


def test_finite_linear_atlas_remains_exact(monkeypatch):
    monkeypatch.setitem(generate.RESPONSE_FUNCS, "audit", lambda p: p["x"] * 2)
    atlas = generate.generate_atlas("audit", "linear", "value", [Axis("x", grid=[1, 2])])
    assert atlas.validation["passes"] is True
    assert atlas.predict({"x": 1.5}).value == 3


@pytest.mark.parametrize("bins", [[], [{"n_cycles": 0}], [{"n_cycles": -1}], [{"n_cycles": math.nan}]])
def test_missing_or_invalid_fatigue_bins_escalate(bins):
    result = query._handle_fatigue_bins(simple_atlas(), {
        "x": 1, "tension_range_bins": bins, "design_life_years": 20, "dff": 3})
    assert result["in_range"] is False
    assert result["action"] == "escalate"


def test_capacity_requires_factor():
    result = query._handle_capacity_demand(simple_atlas(), {"x": 1, "demand_kN": 20})
    assert result["action"] == "escalate"


def test_capacity_records_factor_and_hand_number():
    result = query._handle_capacity_demand(simple_atlas(), {
        "x": 1, "demand_kN": 20, "factor_of_safety": 2})
    assert result["factor_of_safety"] == 2
    assert result["value"] == pytest.approx(0.4, abs=1e-9)


def test_missing_validation_is_unknown(tmp_path):
    atlas = simple_atlas()
    atlas.validation = {}
    atlas.provenance["kind"] = "library"
    out = atlas.save(tmp_path)
    assert yaml.safe_load((out / "manifest.yaml").read_text())["validation"]["max_rel_error"] is None
    result = query._handle_value(atlas, {"x": 1})
    assert result["confidence"]["band"] is None
    assert result["confidence"]["basis"] == "not established"


def test_failed_build_preserves_default(tmp_path, monkeypatch):
    atlas = simple_atlas()
    atlas.save(tmp_path)
    before = (tmp_path / atlas.basename / "default.txt").read_bytes()
    atlas.atlas_id = "rejected"
    atlas.validation["passes"] = False
    monkeypatch.setattr(build, "_row", lambda _: {
        "basename": atlas.basename, "input": "input.yml",
        "parametric": {"physics": "linear", "response": "capacity", "axes": []}})
    monkeypatch.setattr(build, "generate_atlas_adaptive", lambda **kw: atlas)
    monkeypatch.setattr(refresh, "content_fingerprint", lambda *a: "test")
    result = build.build_atlas_from_registry("test", atlas_root=tmp_path)
    assert result.validation["passes"] is False
    assert (tmp_path / atlas.basename / "default.txt").read_bytes() == before
    assert not (tmp_path / atlas.basename / "rejected").exists()


def route(atlas, tmp_path, monkeypatch, **settings):
    monkeypatch.setattr(Atlas, "load", lambda *a: atlas)
    return query.router({"parametric_query": {
        "atlas": atlas.basename, "point": {"x": 1, "demand_kN": 20, "factor_of_safety": 2},
        "output_dir": str(tmp_path), **settings}})["parametric_query"]["result"]


def test_failed_validation_cannot_be_served(tmp_path, monkeypatch):
    atlas = simple_atlas()
    atlas.validation["passes"] = False
    monkeypatch.setattr(refresh, "content_fingerprint", lambda *a: "test")
    assert route(atlas, tmp_path, monkeypatch)["action"] == "escalate"


def test_off_median_curvature_is_measured(monkeypatch):
    monkeypatch.setitem(generate.RESPONSE_FUNCS, "audit", lambda p: 1 + p["y"]**8 * p["x"]**2)
    atlas = generate.generate_atlas("audit", "linear", "value", [
        Axis("x", grid=[0, 1]), Axis("y", grid=[0, 0.5, 1])], tolerance=0.1)
    assert atlas.validation["passes"] is False
    assert atlas.validation["local_error_map"][""]["x"][0] >= 0.2 - 1e-12
    assert generate._holdout_points(atlas.axes) == generate._holdout_points(atlas.axes)


@pytest.mark.parametrize("settings", [{"point": {"x": 1, "environment": "free_corrosion"}},
                                      {"policy": {"confidence": "required"}}])
def test_unknown_keys_rejected(tmp_path, monkeypatch, settings):
    monkeypatch.setattr(refresh, "content_fingerprint", lambda *a: "test")
    with pytest.raises(ValueError, match="unknown"):
        route(simple_atlas(), tmp_path, monkeypatch, **settings)


def test_response_basis_in_provenance():
    assert query._provenance(simple_atlas())["response_kwargs"] == {"environment": "seawater_cp"}


@pytest.mark.parametrize("records", [
    [{"key": "a", "f": 1, "v": 1}, {"key": "a", "f": 1, "v": 2}],
    [{"key": "a", "f": 1, "v": 1}, {"key": "a", "f": 3, "v": 2}],
])
def test_library_rejects_wrong_grid(records):
    with pytest.raises(ValueError, match="grid"):
        build_library_atlas(basename="audit", key_axis="key", key_values=["a"],
                            continuous_axes=[Axis("f", grid=[1, 2])], records=records,
                            response="v", solver={}, coverage_note="synthetic")


def test_load_rejects_duplicate_grid(tmp_path):
    atlas = simple_atlas()
    out = atlas.save(tmp_path)
    atlas.grid.loc[1, "x"] = 1
    atlas.grid.to_parquet(out / "grid.parquet", index=False)
    with pytest.raises(ValueError, match="grid"):
        Atlas.load(tmp_path, atlas.basename)


def test_zero_sliding_capacity_is_infinite(monkeypatch):
    from digitalmodel.geotechnical import mudmat
    monkeypatch.setattr(mudmat, "mudmat_bearing_capacity", lambda **kw: SimpleNamespace(
        vertical_capacity_kn=10000, sliding_capacity_kn=0))
    assert generate._mudmat_bearing_utilisation({
        "vertical_load_kN": 100, "undrained_shear_strength_kpa": 12}) == math.inf


@pytest.mark.parametrize("missing", [True, False])
def test_unverifiable_fingerprint_escalates(tmp_path, monkeypatch, missing):
    atlas = simple_atlas()
    if missing:
        del atlas.provenance["content_fingerprint"]
    def broken(*args):
        raise RuntimeError("unavailable basis")
    monkeypatch.setattr(refresh, "content_fingerprint", broken)
    result = route(atlas, tmp_path, monkeypatch)
    assert result["action"] == "escalate"
    assert "unverified" in result["reason"]


def test_local_error_does_not_hide_programming_errors(monkeypatch):
    atlas = simple_atlas()
    atlas.axes[0].derived = {"fn": "broken", "inputs": ["raw"]}
    atlas.validation["local_error_map"] = {"": {"x": [0.01]}}
    def broken(*args):
        raise RuntimeError("bug")
    monkeypatch.setattr(Axis, "derived_value", broken)
    with pytest.raises(RuntimeError, match="bug"):
        atlas.local_error({"raw": 1})


def test_operability_interval_maps_match_grid():
    from digitalmodel.drilling_riser.operability_atlas import build_atlas
    atlas = build_atlas()
    for per_axis in atlas.validation["local_error_map"].values():
        for axis in atlas.continuous_axes:
            assert len(per_axis[axis.name]) == len(axis.grid) - 1


def test_claimed_pass_above_threshold_escalates(tmp_path, monkeypatch):
    atlas = simple_atlas()
    atlas.validation["max_rel_error"] = 0.2
    monkeypatch.setattr(refresh, "content_fingerprint", lambda *a: "test")
    assert route(atlas, tmp_path, monkeypatch)["action"] == "escalate"
