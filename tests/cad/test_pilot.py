# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic DXF batch exports; no external drawings or geometry kernels."""

import builtins
import hashlib
import json
import subprocess
import sys
import xml.etree.ElementTree as ET

import ezdxf
import pytest

from digitalmodel.cad.pilot import run_dxf_pilot


def drawing(directory, name="drawing.dxf", *, units=4, kind="line"):
    """Hand: a 1000 by 2000 mm outline spans 1 by 2 m after conversion."""
    directory.mkdir(parents=True, exist_ok=True)
    path = directory / name
    doc = ezdxf.new()
    doc.units = units
    model = doc.modelspace()
    if kind == "line":
        model.add_line((0, 0), (1000, 2000))
    elif kind == "polyline":
        model.add_lwpolyline([(0, 0), (1000, 0), (1000, 2000)], close=True)
    elif kind == "point":
        model.add_point((1000, 2000))
    elif kind == "face":
        model.add_3dface([(0, 0, 0), (1000, 0, 0), (0, 2000, 0)])
    else:
        model.add_circle((0, 0), 1000)
    doc.saveas(path)
    return path


def test_normalized_json_svg_and_check_log(tmp_path):
    path = drawing(tmp_path / "inputs", name="drawing.DXF")
    output = tmp_path / "exports"
    digest = hashlib.sha256(path.read_bytes()).hexdigest()
    report = run_dxf_pilot([path], output)
    assert report["total"] == report["succeeded"] == 1
    assert report["success_percent"] == 100.0
    assert report["accepted"] is True
    assert report["elapsed_seconds"] >= 0
    row = report["files"][0]
    assert row["index"] == 1
    assert row["input_sha256"] == digest
    assert row["status"] == "pass"
    assert list(row["checks"]) == ["input", "geometry_and_units", "json", "svg", "artifacts"]
    assert set(row["checks"].values()) == {"pass"}
    geometry = json.loads((output / row["outputs"]["json"]).read_text())
    assert geometry["points"] == [[0, 0, 0], [1, 2, 0]]
    assert geometry["units"] == "m"
    assert geometry["unit_decision"] == {
        "source_units": "mm", "metres_per_unit": 0.001, "basis": "file"
    }
    assert geometry["entities"][0]["indices"] == [0, 1]
    svg = ET.parse(output / row["outputs"]["svg"]).getroot()
    assert svg.tag == "{http://www.w3.org/2000/svg}svg"
    assert svg.findall(".//{http://www.w3.org/2000/svg}path")
    assert json.loads((output / "report.json").read_text()) == report
    assert path.read_bytes() and hashlib.sha256(path.read_bytes()).hexdigest() == digest
    assert str(tmp_path) not in json.dumps(report)


@pytest.mark.parametrize("kind", ["polyline", "point", "face"])
def test_other_supported_primitives_have_visible_svg(tmp_path, kind):
    path = drawing(tmp_path / "inputs", kind=kind)
    report = run_dxf_pilot([path], tmp_path / "exports")
    assert report["accepted"] is True


@pytest.mark.parametrize("bad_count, accepted", [(0, True), (1, True), (2, False)])
def test_synthetic_ten_file_conversion_threshold(tmp_path, bad_count, accepted):
    """The issue's local 90% threshold means 9/10 passes and 8/10 fails."""
    paths = [drawing(tmp_path / "inputs", f"{index}.dxf") for index in range(10)]
    for path in paths[:bad_count]:
        path.write_text("malformed DXF\n", encoding="utf-8")
    report = run_dxf_pilot(paths, tmp_path / "exports")
    assert report["total"] == len(report["files"]) == 10
    assert report["succeeded"] == 10 - bad_count
    assert report["success_percent"] == (10 - bad_count) * 10
    assert report["accepted"] is accepted


@pytest.mark.parametrize("failure", ["missing", "malformed", "units", "circle", "suffix"])
def test_every_failed_input_is_recorded_and_batch_continues(tmp_path, failure):
    path = drawing(tmp_path / "inputs", units=0 if failure == "units" else 4,
                   kind="circle" if failure == "circle" else "line")
    if failure == "missing":
        path = path.with_name("absent.dxf")
    elif failure == "malformed":
        path.write_text("invalid\n", encoding="utf-8")
    elif failure == "suffix":
        path = drawing(tmp_path / "inputs", "unsupported.step")
    valid = drawing(tmp_path / "other")
    report = run_dxf_pilot([path, valid], tmp_path / "exports")
    assert report["succeeded"] == 1
    assert report["accepted"] is False
    bad, good = report["files"]
    assert bad["status"] == "fail"
    assert list(bad["checks"].values()).count("fail") == 1
    assert bad["checks"]["json"] == bad["checks"]["svg"] == "not_run"
    assert bad["outputs"] == {}
    assert good["status"] == "pass"


def test_unitless_input_accepts_explicit_basis(tmp_path):
    path = drawing(tmp_path / "inputs", units=0)
    output = tmp_path / "exports"
    report = run_dxf_pilot([path], output, units="mm")
    assert report["accepted"] is True
    geometry = json.loads((output / "0001" / "geometry.json").read_text())
    assert geometry["points"][-1] == [1, 2, 0]
    assert geometry["unit_decision"]["basis"] == "caller"


def test_duplicate_basenames_get_separate_artifacts(tmp_path):
    paths = [drawing(tmp_path / "a"), drawing(tmp_path / "b")]
    report = run_dxf_pilot(paths, tmp_path / "exports")
    assert report["succeeded"] == 2
    assert report["files"][0]["outputs"] != report["files"][1]["outputs"]


def test_existing_output_is_preserved(tmp_path):
    output = tmp_path / "exports"
    output.mkdir()
    sentinel = output / "report.json"
    sentinel.write_text("retained", encoding="utf-8")
    with pytest.raises(FileExistsError):
        run_dxf_pilot([drawing(tmp_path / "inputs")], output)
    assert sentinel.read_text() == "retained"
    assert list(output.iterdir()) == [sentinel]


def test_empty_batch_is_refused_before_creating_output(tmp_path):
    output = tmp_path / "exports"
    with pytest.raises(ValueError, match="at least one"):
        run_dxf_pilot([], output)
    assert not output.exists()


def test_svg_failure_records_failed_stage_and_continues(tmp_path, monkeypatch):
    from digitalmodel.cad import pilot

    actual = pilot._write_svg
    calls = 0

    def fail_once(*args):
        nonlocal calls
        calls += 1
        if calls == 1:
            raise OSError("untrusted diagnostic payload")
        return actual(*args)

    monkeypatch.setattr(pilot, "_write_svg", fail_once)
    report = run_dxf_pilot([drawing(tmp_path / "a"), drawing(tmp_path / "b")],
                           tmp_path / "exports")
    assert report["succeeded"] == 1
    assert report["files"][0]["checks"]["svg"] == "fail"
    assert report["files"][0]["checks"]["artifacts"] == "not_run"
    assert report["files"][0]["outputs"] == {}
    assert "untrusted" not in json.dumps(report)


def test_missing_svg_dependency_is_logged(tmp_path, monkeypatch):
    original = builtins.__import__

    def missing(name, *args, **kwargs):
        if name.startswith("ezdxf.addons.drawing"):
            raise ImportError("untrusted payload")
        return original(name, *args, **kwargs)

    monkeypatch.setattr(builtins, "__import__", missing)
    report = run_dxf_pilot([drawing(tmp_path / "inputs")], tmp_path / "exports")
    row = report["files"][0]
    assert row["checks"]["svg"] == "fail"
    assert row["error"] == "dependency_unavailable: install digitalmodel[drawings]"


@pytest.mark.parametrize("valid, expected", [(True, 0), (False, 1)])
def test_module_cli_exit_status(tmp_path, valid, expected):
    path = drawing(tmp_path / "inputs", kind="line" if valid else "circle")
    output = tmp_path / "exports"
    result = subprocess.run(
        [sys.executable, "-m", "digitalmodel.cad.pilot", "--output", str(output), str(path)],
        check=False, capture_output=True, text=True,
    )
    assert result.returncode == expected, result.stderr
    assert json.loads((output / "report.json").read_text())["accepted"] is valid
    assert str(tmp_path) not in result.stdout + result.stderr


@pytest.mark.parametrize("end", [(1000, 0), (0, 2000), (1, 1)])
def test_axis_aligned_and_small_drawings_have_preview(tmp_path, end):
    path = drawing(tmp_path / "inputs")
    doc = ezdxf.readfile(path)
    doc.modelspace().query("LINE").first.dxf.end = end
    doc.saveas(path)
    assert run_dxf_pilot([path], tmp_path / "exports")["accepted"] is True


def test_invalid_svg_artifact_is_not_counted_as_success(tmp_path, monkeypatch):
    from digitalmodel.cad import pilot

    def empty_svg(geometry, destination):
        destination.write_text('<svg xmlns="http://www.w3.org/2000/svg"/>', encoding="utf-8")

    monkeypatch.setattr(pilot, "_write_svg", empty_svg)
    report = run_dxf_pilot([drawing(tmp_path / "inputs")], tmp_path / "exports")
    assert report["succeeded"] == 0
    assert report["files"][0]["checks"]["artifacts"] == "fail"
    assert report["files"][0]["outputs"] == {}


def test_conversion_reads_the_bytes_identified_by_digest(tmp_path, monkeypatch):
    from digitalmodel.cad import pilot

    source = drawing(tmp_path / "inputs")
    digest = hashlib.sha256(source.read_bytes()).hexdigest()
    original_read = pilot.read_cad

    def change_original_then_read(path, **kwargs):
        source.write_text("source changed after snapshot\n", encoding="utf-8")
        return original_read(path, **kwargs)

    monkeypatch.setattr(pilot, "read_cad", change_original_then_read)
    report = run_dxf_pilot([source], tmp_path / "exports")
    assert report["accepted"] is True
    assert report["files"][0]["input_sha256"] == digest


def test_cli_existing_destination_is_setup_error(tmp_path):
    from digitalmodel.cad.pilot import main

    output = tmp_path / "exports"
    output.mkdir()
    assert main(["--output", str(output), str(drawing(tmp_path / "inputs"))]) == 2
    assert list(output.iterdir()) == []


def test_svg_with_entity_declaration_fails_artifact_check(tmp_path, monkeypatch):
    from digitalmodel.cad import pilot

    def entity_svg(geometry, destination):
        destination.write_text(
            '<!DOCTYPE svg [<!ENTITY label "synthetic">]>'
            '<svg xmlns="http://www.w3.org/2000/svg">'
            '<text>&label;</text><path d="M 0 0 L 1 1"/></svg>', encoding="utf-8"
        )

    monkeypatch.setattr(pilot, "_write_svg", entity_svg)
    report = run_dxf_pilot([drawing(tmp_path / "inputs")], tmp_path / "exports")
    assert report["succeeded"] == 0
    assert report["files"][0]["checks"]["artifacts"] == "fail"
