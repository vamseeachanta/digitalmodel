"""Synthetic DXF acceptance and fail-closed geometry regressions."""

import json
import subprocess
import sys
from pathlib import Path

import numpy as np
import pytest
import yaml

pytest.importorskip("ezdxf")
import ezdxf

from digitalmodel.hydrodynamics.hull_library.line_generator.dxf_lines_reader import (
    DxfLinesConfig,
    DxfLinesError,
    hull_line_definition_to_profile,
    read_dxf_body_plan,
    read_dxf_body_plan_with_report,
    write_body_plan_dxf,
)
from digitalmodel.hydrodynamics.hull_library.parametric_form import (
    MonohullFormParameters,
    generate_profile,
)
from digitalmodel.visualization.design_tools.hull_hydrostatics import HullHydrostatics

DIMENSIONS = {"length_bp": 100, "beam": 20, "draft": 10, "depth": 12}


def form(**kwargs):
    return generate_profile(MonohullFormParameters(**DIMENSIONS, **kwargs))


@pytest.fixture
def wigley():
    return form(cb=4 / 9, wigley=True, bilge_radius_fraction=0)


def config(profile=None, **kwargs):
    values = {"body_plan_layers": ["BODY_PLAN"], "n_waterlines": 201}
    if profile:
        values["station_x"] = [s.x_position for s in profile.stations]
    return DxfLinesConfig(**(values | kwargs))


def metadata(profile):
    return profile.model_dump(mode="json", include={"name", "hull_type", *DIMENSIONS})


def round_trip(profile, path):
    write_body_plan_dxf(profile, path)
    defn, report = read_dxf_body_plan_with_report(path, config(profile))
    result = hull_line_definition_to_profile(defn, **metadata(profile))
    assert len(result.stations) == len(profile.stations)
    errors = []
    for original, recovered in zip(profile.stations, result.stations):
        assert recovered.x_position == pytest.approx(original.x_position)
        old, new = np.array(original.waterline_offsets), np.array(
            recovered.waterline_offsets
        )
        errors.extend(abs(np.interp(old[:, 0], new[:, 0], new[:, 1]) - old[:, 1]))
    offset_error = max(errors) / (profile.beam / 2) * 100
    source_volume = HullHydrostatics(profile).compute_displaced_volume()
    volume_error = (
        abs(HullHydrostatics(result).compute_displaced_volume() / source_volume - 1)
        * 100
    )
    assert offset_error < 0.5
    assert volume_error < 0.5
    assert report.stations_found == len(profile.stations)
    assert all(s["method"] == "station_x" for s in report.station_assignments)
    metrics = {"form": profile.name, "offset_error_pct": offset_error}
    print(json.dumps(metrics | {"volume_error_pct": volume_error}))
    return result


def test_wigley_round_trip(wigley, tmp_path):
    round_trip(wigley, tmp_path / "wigley.dxf")


def test_transom_round_trip(tmp_path):
    source = form(cb=0.72, transom_fraction=0.9, lcb_fraction=-0.10)
    source.name = "synthetic_transom"
    round_trip(source, tmp_path / "transom.dxf")


def test_box_round_trip(box_profile, tmp_path):
    result = round_trip(box_profile, tmp_path / "box.dxf")
    assert HullHydrostatics(result).compute_displaced_volume() == pytest.approx(20000)


def mixed_drawing(path, twin=False):
    doc = ezdxf.new()
    doc.units = 4
    msp = doc.modelspace()
    attrs = {"layer": "BODY_PLAN"}
    if twin:
        t = np.linspace(0, 1, 4097)
        y = 1000 * (1 - t) ** 2 + 4000 * t * (1 - t) + 1400 * t * t
        msp.add_lwpolyline(list(zip(y, 2000 * t)), dxfattribs=attrs)
    else:
        msp.add_open_spline(
            [(1000, 0, 0), (2000, 1000, 0), (1400, 2000, 0)], degree=2, dxfattribs=attrs
        )
    msp.add_polyline2d([(2000, 0), (2200, 1000), (2400, 2000)], dxfattribs=attrs)
    if twin:
        t = np.linspace(-np.pi / 2, 0, 4097)
        msp.add_lwpolyline(
            list(zip(3000 + 1000 * np.cos(t), 1000 + 1000 * np.sin(t)))
            + [(4000, 2000)],
            dxfattribs=attrs,
        )
    else:
        msp.add_arc((3000, 1000), 1000, 270, 360, dxfattribs=attrs)
        msp.add_line((4000, 1000), (4000, 2000), dxfattribs=attrs)
    msp.add_lwpolyline([(5000, 0), (5200, 2000)], dxfattribs=attrs)
    msp.add_circle((0, 0), 2, dxfattribs={"layer": "DISTRACTOR"})
    msp.add_circle((0, 0), 3, dxfattribs=attrs)
    doc.saveas(path)
    return path


def test_mixed_entities(tmp_path):
    cfg = config(station_x=[0, 10, 20, 30], tolerance=0.01)
    mixed, report = read_dxf_body_plan_with_report(
        mixed_drawing(tmp_path / "mixed.dxf"), cfg
    )
    twin = read_dxf_body_plan(mixed_drawing(tmp_path / "twin.dxf", True), cfg)
    for a, b in zip(mixed.stations, twin.stations):
        assert np.allclose(
            a.offsets, b.offsets, atol=report.flattening_tolerance_m, rtol=0
        )
    assert report.entities_seen == 7
    assert report.entities_used == 5
    assert report.entities_skipped == 2
    assert report.skipped_by_type == {"CIRCLE": 2}
    assert report.skipped_by_layer == {"DISTRACTOR": 1, "BODY_PLAN": 1}
    assert report.fragments_joined == 1
    assert report.stations_found == 4
    print(report.model_dump_json())


def simple_drawing(path, curves=None, labels=True, units=6):
    doc = ezdxf.new()
    doc.units = units
    msp = doc.modelspace()
    curves = curves or [[(1, 0), (2, 2)], [(3, 0), (4, 2)]]
    handles = []
    for i, points in enumerate(curves):
        entity = msp.add_lwpolyline(points, dxfattribs={"layer": "BODY_PLAN"})
        handles.append(entity.dxf.handle)
        if labels:
            msp.add_text(
                str(i * 10), dxfattribs={"layer": "STATIONS", "insert": points[-1]}
            )
    doc.saveas(path)
    return path, handles


def test_labels_and_mtext(tmp_path):
    path, _ = simple_drawing(tmp_path / "labels.dxf")
    doc = ezdxf.readfile(path)
    label = list(doc.modelspace().query("TEXT"))[1]
    position = label.dxf.insert
    doc.modelspace().delete_entity(label)
    doc.modelspace().add_mtext(
        "{\\H2x;10}", dxfattribs={"layer": "STATIONS", "insert": position}
    )
    doc.saveas(path)
    defn, report = read_dxf_body_plan_with_report(path, config())
    assert [s.x for s in defn.stations] == [0, 10]
    assert all(s["method"] == "label" for s in report.station_assignments)
    assert report.entities_used == 4


def test_missing_labels_names_curve(tmp_path):
    path, handles = simple_drawing(tmp_path / "missing.dxf", labels=False)
    with pytest.raises(DxfLinesError) as caught:
        read_dxf_body_plan(path, config())
    assert handles[0] in str(caught.value)
    assert "BODY_PLAN" in str(caught.value)
    assert caught.value.report.entities_seen == 2


def test_unknown_units_and_override(tmp_path):
    path, _ = simple_drawing(tmp_path / "units.dxf", units=0)
    with pytest.raises(DxfLinesError, match="INSUNITS"):
        read_dxf_body_plan(path, config())
    assert len(read_dxf_body_plan(path, config(units="m")).stations) == 2


@pytest.mark.parametrize("units", ["mm", "m", "ft", "in"])
def test_export_units(box_profile, tmp_path, units):
    path = tmp_path / "units.dxf"
    write_body_plan_dxf(box_profile, path, units=units)
    result = read_dxf_body_plan(path, config(box_profile))
    assert result.stations[0].offsets[-1] == pytest.approx((10, 10))
    assert next(iter(ezdxf.readfile(path).modelspace().query("TEXT"))).dxf.text == "0"


def test_scale_origin_and_port(tmp_path):
    path, _ = simple_drawing(
        tmp_path / "port.dxf", labels=False, curves=[[(9, 3), (8, 5)], [(7, 3), (6, 5)]]
    )
    cfg = config(
        station_x=[0, 20], scale=2, centreline_y=10, baseline_z=3, mirror_side="port"
    )
    result = read_dxf_body_plan(path, cfg)
    assert result.stations[0].offsets[-1] == pytest.approx((4, 4))
    cfg.mirror_side = "starboard"
    with pytest.raises(DxfLinesError, match="side"):
        read_dxf_body_plan(path, cfg)


@pytest.mark.parametrize(
    "curves,match",
    [
        ([[(1, 0), (2, 2), (3, 1)], [(4, 0), (5, 2)]], "monotone"),
        ([[(1, 0), (2, 0), (3, 2)], [(4, 0), (5, 2)]], "multiple"),
    ],
)
def test_invalid_curves(tmp_path, curves, match):
    path, handles = simple_drawing(
        tmp_path / "invalid.dxf", curves=curves, labels=False
    )
    with pytest.raises(DxfLinesError, match=match) as caught:
        read_dxf_body_plan(path, config(station_x=[0, 10]))
    assert handles[0] in str(caught.value)


def test_ambiguous_labels_and_empty_layer(box_profile, tmp_path):
    path = tmp_path / "box.dxf"
    write_body_plan_dxf(box_profile, path)
    with pytest.raises(DxfLinesError, match="ambiguous"):
        read_dxf_body_plan(path, config())
    with pytest.raises(DxfLinesError, match="EMPTY"):
        read_dxf_body_plan(path, config(body_plan_layers=["EMPTY"]))


@pytest.mark.parametrize("xs", [[0, 0], [10, 0], [0]])
def test_bad_station_order(tmp_path, xs):
    path, _ = simple_drawing(tmp_path / "order.dxf", labels=False)
    with pytest.raises(DxfLinesError, match="station_x"):
        read_dxf_body_plan(path, config(station_x=xs))


@pytest.mark.parametrize(
    "values",
    [
        {"scale": 0},
        {"tolerance": -1},
        {"scale": float("nan")},
        {"n_waterlines": 1},
        {"body_plan_layers": []},
    ],
)
def test_config_validation(values):
    with pytest.raises(ValueError):
        config(**values)


def cli_run(tmp_path, profile, labels=True):
    path = tmp_path / "input.dxf"
    write_body_plan_dxf(profile, path)
    cfg = {
        "reader": config(profile if labels else None).model_dump(),
        "profile": metadata(profile),
    }
    (tmp_path / "config.yaml").write_text(yaml.safe_dump(cfg))
    script = (
        Path(__file__).resolve().parents[4] / "scripts/hull_library/dxf_to_profile.py"
    )
    return subprocess.run(
        [
            sys.executable,
            str(script),
            str(path),
            str(tmp_path / "config.yaml"),
            "--output-dir",
            str(tmp_path / "output"),
        ],
        check=False,
        capture_output=True,
        text=True,
    )


def test_cli_smoke(wigley, tmp_path):
    result = cli_run(tmp_path, wigley)
    assert result.returncode == 0, result.stderr
    for name in (
        "profile.yaml",
        "read_report.json",
        "sections.svg",
        "hydrostatics.json",
    ):
        assert (tmp_path / "output" / name).is_file()
    summary = json.loads((tmp_path / "output/hydrostatics.json").read_text())
    assert summary["displaced_volume"] > 0


def test_cli_signature(wigley, tmp_path):
    pytest.importorskip("hullprod")
    assert cli_run(tmp_path, wigley).returncode == 0
    signature = json.loads((tmp_path / "output/hullprod_signature.json").read_text())
    assert signature["lref"] == 100


def test_cli_failure_report(box_profile, tmp_path):
    result = cli_run(tmp_path, box_profile, labels=False)
    assert result.returncode == 1
    report = json.loads((tmp_path / "output/read_report.json").read_text())
    assert report["errors"]
    assert not (tmp_path / "output/profile.yaml").exists()


def test_bulge_arc_and_branch(tmp_path):
    path = tmp_path / "bulge.dxf"
    doc = ezdxf.new()
    doc.units = 6
    msp = doc.modelspace()
    attrs = {"layer": "BODY_PLAN"}
    msp.add_lwpolyline(
        [(1, 0, np.tan(np.pi / 8)), (2, 1, 0), (2, 2, 0)],
        format="xyb",
        dxfattribs=attrs,
    )
    msp.add_lwpolyline([(3, 0), (4, 2)], dxfattribs=attrs)
    doc.saveas(path)
    result = read_dxf_body_plan(path, config(station_x=[0, 10]))
    assert result.stations[0].offsets[50][1] == pytest.approx(
        1 + np.sqrt(0.75), abs=0.001
    )
    msp.add_line((4, 2), (5, 3), dxfattribs=attrs)
    msp.add_line((4, 2), (6, 3), dxfattribs=attrs)
    doc.saveas(path)
    with pytest.raises(DxfLinesError, match="Ambiguous fragment"):
        read_dxf_body_plan(path, config(station_x=[0, 10]))


def test_missing_dependency_is_actionable(monkeypatch):
    from digitalmodel.hydrodynamics.hull_library.line_generator import dxf_entities

    def unavailable(name):
        raise ImportError(name)

    monkeypatch.setattr(dxf_entities.importlib, "import_module", unavailable)
    with pytest.raises(ImportError, match=r"digitalmodel\[drawings\]"):
        read_dxf_body_plan("unused.dxf", config())


@pytest.mark.parametrize("kind", ["ARC", "LWPOLYLINE", "REVERSED"])
def test_flattening_chord_bound(kind):
    from digitalmodel.hydrodynamics.hull_library.line_generator.dxf_entities import (
        DxfReadReport,
        flatten,
    )

    msp = ezdxf.new().modelspace()
    entity = (
        msp.add_arc((0, 1), 1, 270, 360)
        if kind == "ARC"
        else msp.add_lwpolyline([(0, 0, np.tan(np.pi / 8)), (1, 1, 0)], format="xyb")
    )
    if kind == "REVERSED":
        entity.set_points([(1, 1, -np.tan(np.pi / 8)), (0, 0, 0)], format="xyb")
    points = flatten(entity, 1e-4, DxfReadReport(), 0).points
    assert np.allclose(points[0], [1, 1] if kind == "REVERSED" else [0, 0])
    assert np.max(abs(np.linalg.norm(points - [0, 1], axis=1) - 1)) < 1e-8
    midpoints = (points[:-1] + points[1:]) / 2
    assert np.max(abs(np.linalg.norm(midpoints - [0, 1], axis=1) - 1)) <= 1e-4


def test_cli_refuses_stale_outputs(wigley, tmp_path):
    assert cli_run(tmp_path, wigley).returncode == 0
    result = cli_run(tmp_path, wigley)
    assert result.returncode == 1
    assert "fresh output directory" in result.stdout


@pytest.mark.parametrize("offsets", [[(0, 1), (1, 2)], [(1, 1), (2, 2)]])
def test_profile_rejects_truncated_submerged_span(tmp_path, offsets):
    path, _ = simple_drawing(
        tmp_path / "partial.dxf",
        curves=[[(y, z) for z, y in offsets], [(3, 0), (4, 2)]],
    )
    defn = read_dxf_body_plan(path, config(station_x=[0, 10]))
    with pytest.raises(DxfLinesError, match="coverage"):
        hull_line_definition_to_profile(
            defn,
            name="partial",
            hull_type="ship",
            length_bp=10,
            beam=8,
            draft=2,
            depth=2,
        )
