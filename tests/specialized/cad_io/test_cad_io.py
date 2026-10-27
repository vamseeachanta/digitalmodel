# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Synthetic CAD and failure-isolation acceptance checks in the specialized shard."""

from __future__ import annotations

import importlib
import json
import math
import shutil
import subprocess
import tomllib
from importlib.metadata import PackageNotFoundError, requires, version
from pathlib import Path

import pytest

from digitalmodel.cad_io import (
    dxf_to_json,
    dxf_to_svg,
    dwg_to_dxf,
    read_dwg_version,
    roundtrip_check,
    step_checks,
)
from digitalmodel.cad_io.cli import main


def test_installed_cad_extra_satisfies_shared_runtime_requirements():
    """Check the CAD toolchain against the package's shared dependencies."""
    try:
        version("build123d")
    except PackageNotFoundError:
        pytest.skip("the optional CAD extra is not installed")
    importlib.import_module("build123d")
    from packaging.requirements import Requirement

    project = tomllib.loads(
        (Path(__file__).resolve().parents[3] / "pyproject.toml").read_text()
    )["project"]
    runtime = {r.name: r for r in map(Requirement, project["dependencies"])}
    for spec in project["optional-dependencies"]["cad-io"]:
        dependency = Requirement(spec)
        assert version(dependency.name) in dependency.specifier
        for transitive_spec in requires(dependency.name) or []:
            transitive = Requirement(transitive_spec)
            if transitive.marker and not transitive.marker.evaluate({"extra": ""}):
                continue
            if transitive.name in runtime:
                installed = version(transitive.name)
                assert installed in runtime[transitive.name].specifier, (
                    f"{transitive.name} {installed} conflicts with {runtime[transitive.name]}"
                )


@pytest.fixture
def drawings(tmp_path):
    """Create three independent DXF drawings without external fixtures."""
    ezdxf = pytest.importorskip("ezdxf")
    paths = []
    for code in ("lines", "attributes", "dimensions"):
        doc = ezdxf.new("R2000")
        doc.layers.new("TEST")
        model = doc.modelspace()
        if code == "lines":
            model.add_line((0, 0), (10, 5), dxfattribs={"layer": "TEST"})
            model.add_text("SYNTHETIC", dxfattribs={"height": 1})
            model.add_mtext("SECOND\\PPARAGRAPH")
        elif code == "attributes":
            block = doc.blocks.new("TITLE")
            block.add_line((0, 0), (20, 0))
            block.add_attdef("ID", (1, 1), text="EMPTY")
            model.add_blockref("TITLE", (0, 0)).add_auto_attribs({"ID": "T01"})
        else:
            dimension = model.add_linear_dim(
                base=(0, 3), p1=(0, 0), p2=(10, 0), angle=0
            )
            dimension.render()
        path = tmp_path / f"{code}.dxf"
        doc.saveas(path)
        paths.append(path)
    return paths


@pytest.fixture
def solids(tmp_path):
    """Create a box and a cylinder with a through hole in STEP format."""
    cad = pytest.importorskip("build123d")
    shapes = [cad.Box(2, 3, 4), cad.Cylinder(4, 5) - cad.Cylinder(1, 5)]
    paths = []
    for index, shape in enumerate(shapes):
        path = tmp_path / f"solid{index}.step"
        cad.export_step(shape, str(path))
        paths.append(path)
    return paths


@pytest.mark.parametrize(
    ("tag", "release"),
    [("AC1014", "R14"), ("AC1015", "2000"), ("AC1018", "2004"),
     ("AC1021", "2007"), ("AC1024", "2010"), ("AC1027", "2013"),
     ("AC1032", "2018")],
)
def test_version_tags(tmp_path, tag, release):
    path = tmp_path / "header.dwg"
    path.write_bytes(tag.encode("ascii") + b"\x00" * 10)
    assert read_dwg_version(path) == {"tag": tag, "release": release}


@pytest.mark.parametrize("data", [b"", b"AC10", b"garbag", b"AC9999"])
def test_invalid_header_is_explicit(tmp_path, data):
    path = tmp_path / "bad.dwg"
    path.write_bytes(data)
    with pytest.raises(ValueError):
        read_dwg_version(path)


def test_dxf_metadata_and_rendering(drawings, tmp_path):
    lines, attributes, dimensions = map(dxf_to_json, drawings)
    assert lines["entity_counts"] == {"LINE": 1, "MTEXT": 1, "TEXT": 1}
    assert "TEST" in {row["name"] for row in lines["layers"]}
    assert {row["text"] for row in lines["text"]} == {
        "SYNTHETIC", "SECOND\nPARAGRAPH"
    }
    assert attributes["block_attributes"][0]["attributes"] == {"ID": "T01"}
    assert attributes["block_attributes"][0]["block"] == "TITLE"
    assert dimensions["entity_counts"]["DIMENSION"] == 1
    assert dimensions["dimensions"][0]["measurement"] == pytest.approx(10)
    for index, drawing in enumerate(drawings):
        svg = dxf_to_svg(drawing, tmp_path / f"drawing{index}.svg")
        assert "<svg" in svg.read_text()
    json.dumps([lines, attributes, dimensions], allow_nan=False)


def test_cli_continues_and_separates_same_stems(drawings, tmp_path, capsys):
    inputs = tmp_path / "inputs"
    inputs.mkdir()
    for index, drawing in enumerate(drawings):
        folder = inputs / str(index)
        folder.mkdir()
        shutil.copyfile(drawing, folder / "same.dxf")
    (inputs / "bad.dxf").write_text("invalid")
    (inputs / "notes.txt").write_text("ignored")
    out = tmp_path / "outputs"
    assert main([str(inputs), "--out", str(out)]) == 1
    report = json.loads((out / "results.json").read_text())
    assert len(report["files"]) == 4
    assert report["summary"]["converted"] == 3
    assert report["summary"]["tried"] == 4
    assert len({row["code"] for row in report["files"]}) == 4
    assert sum(row["status"] == "ok" for row in report["files"]) == 3
    assert len(list(out.glob("F*/*.svg"))) == 3
    assert "3/4 (75.0%)" in capsys.readouterr().out


def test_cli_rejects_output_inside_input_without_touching_input(drawings):
    folder = drawings[0].parent
    original = drawings[0].read_bytes()
    assert main([str(folder), "--out", str(folder)]) == 2
    assert drawings[0].read_bytes() == original
    assert not (folder / "results.json").exists()


@pytest.mark.parametrize("name", [
    "results.json", "results.json.pending",
    "F01/checkpoint.json", "F01/checkpoint.json.pending",
    "F01/drawing.json", "F01/drawing.json.pending", "F01/drawing.svg",
    "F01/conversion/drawing.dxf", "F01/conversion/dwg2dxf.log",
    "F01/roundtrip/first/drawing.dxf", "F01/roundtrip/first/dwg2dxf.log",
    "F01/roundtrip/encoded/drawing.dwg", "F01/roundtrip/encoded/dxf2dwg.log",
    "F01/roundtrip/last/drawing.dxf", "F01/roundtrip/last/dwg2dxf.log",
    "F01/roundtrip/resaved.dxf", "F01/part.glb", "F01/part.stl", "F01/part.inp",
])
@pytest.mark.parametrize("alias", ["direct", "symlink", "hardlink"])
def test_cli_rejects_source_record_alias_before_writes(tmp_path, name, alias):
    out = tmp_path / "out"
    target = out / name
    target.parent.mkdir(parents=True)
    source = target if alias == "direct" else tmp_path / "source.json"
    original = b'{"input": "preserve these bytes"}\n'
    source.write_bytes(original)
    if alias == "symlink":
        target.symlink_to(source)
    elif alias == "hardlink":
        target.hardlink_to(source)
    before = {p.relative_to(tmp_path): p.read_bytes()
              for p in tmp_path.rglob("*") if p.is_file()}

    status = main([str(source), "--out", str(out)])

    assert source.read_bytes() == original
    assert {p.relative_to(tmp_path): p.read_bytes()
            for p in tmp_path.rglob("*") if p.is_file()} == before
    assert status == 2


@pytest.mark.parametrize("alias", ["symlink", "hardlink"])
@pytest.mark.parametrize("name", [
    "checkpoint.json", "drawing.json.pending", "conversion/dwg2dxf.log",
])
def test_cli_rejects_report_checkpoint_alias_before_writes(tmp_path, alias, name):
    source = tmp_path / "source.json"
    source.write_text("unsupported input")
    out = tmp_path / "out"
    (out / "F01").mkdir(parents=True)
    report = out / "results.json"
    original = b'{"existing": "report"}\n'
    report.write_bytes(original)
    checkpoint = out / "F01" / name
    checkpoint.parent.mkdir(parents=True, exist_ok=True)
    if alias == "symlink":
        checkpoint.symlink_to(report)
    else:
        checkpoint.hardlink_to(report)

    status = main([str(source), "--out", str(out)])

    assert report.read_bytes() == checkpoint.read_bytes() == original
    assert status == 2


@pytest.mark.parametrize("name", ["drawing.json", "drawing.json.pending"])
@pytest.mark.parametrize("alias", ["symlink", "hardlink"])
def test_cli_preserves_dxf_with_metadata_alias(drawings, tmp_path, name, alias):
    source = drawings[0]
    original = source.read_bytes()
    out = tmp_path / "out"
    target = out / "F01" / name
    target.parent.mkdir(parents=True)
    if alias == "symlink":
        target.symlink_to(source)
    else:
        target.hardlink_to(source)

    status = main([str(source), "--out", str(out)])

    assert source.read_bytes() == target.read_bytes() == original
    assert status == 2
    assert sorted(p.relative_to(out).as_posix() for p in out.rglob("*")) == [
        "F01", f"F01/{name}"
    ]


@pytest.mark.parametrize("name", [
    "conversion/dwg2dxf.log", "roundtrip/first/dwg2dxf.log",
    "roundtrip/encoded/dxf2dwg.log", "roundtrip/last/dwg2dxf.log",
])
@pytest.mark.parametrize("alias", ["symlink", "hardlink"])
def test_cli_rejects_converter_log_alias_before_writes(tmp_path, monkeypatch, name, alias):
    from digitalmodel.cad_io import cli

    source = tmp_path / "source.dwg"
    original = b"AC1015 synthetic input"
    source.write_bytes(original)
    out = tmp_path / "out"
    target = out / "F01" / name
    target.parent.mkdir(parents=True)
    if alias == "symlink":
        target.symlink_to(source)
    else:
        target.hardlink_to(source)
    before = sorted(p.relative_to(out).as_posix() for p in out.rglob("*"))

    def unexpected(*args, **kwargs):
        pytest.fail("preflight must reject the alias before inspection")

    monkeypatch.setattr(cli, "_inspect", unexpected)
    assert main([str(source), "--out", str(out)]) == 2
    assert source.read_bytes() == target.read_bytes() == original
    assert sorted(p.relative_to(out).as_posix() for p in out.rglob("*")) == before


@pytest.mark.parametrize("alias", ["direct", "symlink", "hardlink"])
def test_converter_preserves_source_aliased_by_log(tmp_path, monkeypatch, alias):
    folder = tmp_path / "conversion"
    folder.mkdir()
    log = folder / "dwg2dxf.log"
    source = log if alias == "direct" else tmp_path / "source.dwg"
    original = b"AC1015 synthetic input"
    source.write_bytes(original)
    if alias == "symlink":
        log.symlink_to(source)
    elif alias == "hardlink":
        log.hardlink_to(source)
    monkeypatch.setattr(shutil, "which", lambda _: "synthetic-tool")
    calls = []

    def convert(*args, **kwargs):
        calls.append(args)
        return subprocess.CompletedProcess(args, 1)

    monkeypatch.setattr(subprocess, "run", convert)
    with pytest.raises(ValueError, match="alias"):
        dwg_to_dxf(source, folder)
    assert source.read_bytes() == log.read_bytes() == original
    assert calls == []
    assert not (folder / "drawing.dxf").exists()


@pytest.mark.parametrize("name", [
    "first/dwg2dxf.log", "encoded/dxf2dwg.log", "last/dwg2dxf.log", "resaved.dxf",
])
def test_roundtrip_checks_later_artifacts_before_conversion(tmp_path, monkeypatch, name):
    from digitalmodel.cad_io import drawings as drawing_api

    source = tmp_path / "source.dwg"
    original = b"AC1015 synthetic input"
    source.write_bytes(original)
    folder = tmp_path / "roundtrip"
    target = folder / name
    target.parent.mkdir(parents=True)
    target.hardlink_to(source)

    def unexpected(*args, **kwargs):
        pytest.fail("all round-trip paths must be checked before conversion")

    monkeypatch.setattr(drawing_api, "dwg_to_dxf", unexpected)
    with pytest.raises(ValueError, match="alias"):
        roundtrip_check(source, folder)
    assert source.read_bytes() == original


def test_cli_checks_all_inputs_before_metadata_writes(drawings, tmp_path):
    inputs = tmp_path / "inputs"
    inputs.mkdir()
    for index, drawing in enumerate(drawings[:2]):
        shutil.copyfile(drawing, inputs / f"{index}.dxf")
    before = {p.name: p.read_bytes() for p in inputs.iterdir()}
    out = tmp_path / "out"
    target = out / "F01" / "drawing.json.pending"
    target.parent.mkdir(parents=True)
    target.hardlink_to(inputs / "1.dxf")

    assert main([str(inputs), "--out", str(out)]) == 2
    assert {p.name: p.read_bytes() for p in inputs.iterdir()} == before
    assert not (out / "results.json").exists()


def test_missing_tool_has_clear_error(drawings, tmp_path, monkeypatch):
    monkeypatch.setattr(shutil, "which", lambda _: None)
    with pytest.raises(RuntimeError, match="dwg2dxf"):
        dwg_to_dxf(drawings[0], tmp_path / "out")


def test_converter_timeout_and_missing_output(drawings, tmp_path, monkeypatch):
    monkeypatch.setattr(shutil, "which", lambda _: "/synthetic/tool")

    def timeout(*args, **kwargs):
        raise subprocess.TimeoutExpired("tool", 120)

    monkeypatch.setattr(subprocess, "run", timeout)
    with pytest.raises(RuntimeError, match="timeout"):
        dwg_to_dxf(drawings[0], tmp_path / "timeout")
    monkeypatch.setattr(
        subprocess, "run", lambda *a, **k: subprocess.CompletedProcess(a, 0)
    )
    with pytest.raises(RuntimeError, match="output"):
        dwg_to_dxf(drawings[0], tmp_path / "missing")


def make_dwg(drawing, folder):
    """Encode a synthetic DXF with the same permitted toolchain."""
    encoder = shutil.which("dxf2dwg")
    if not encoder or not shutil.which("dwg2dxf"):
        pytest.skip("LibreDWG dxf2dwg and dwg2dxf are required")
    path = folder / "synthetic.dwg"
    result = subprocess.run(
        [encoder, "--as", "r2000", "-o", str(path), str(drawing)],
        capture_output=True, timeout=120, check=False,
    )
    assert result.returncode == 0, "synthetic DWG encoding failed"
    assert path.is_file()
    return path


def test_libredwg_roundtrip(drawings, tmp_path):
    dwg = make_dwg(drawings[0], tmp_path)
    converted = dwg_to_dxf(dwg, tmp_path / "converted")
    original = converted.read_bytes()
    metadata = dxf_to_json(converted)
    assert metadata["entity_counts"]["LINE"] == 1
    assert metadata["recovery"]["null_endblk_handles_removed"] == 1
    assert converted.read_bytes() == original
    result = roundtrip_check(dwg, tmp_path / "roundtrip")
    assert result["mode"] == "dwg-dxf-dwg-dxf"
    assert result["agreement_percent"] == 100.0
    assert result["counts_before"] == result["counts_after"]


def test_ezdxf_roundtrip_fallback_is_named(drawings, tmp_path, monkeypatch):
    dwg = make_dwg(drawings[0], tmp_path)
    which = shutil.which
    monkeypatch.setattr(
        shutil, "which", lambda name: None if name == "dxf2dwg" else which(name)
    )
    result = roundtrip_check(dwg, tmp_path / "fallback")
    assert result["mode"] == "dwg-dxf-dxf"
    assert result["agreement_percent"] == 100.0


def test_cli_resumes_completed_digest_without_reconversion(drawings, tmp_path, monkeypatch):
    out = tmp_path / "resume"
    assert main([str(drawings[0]), "--out", str(out)]) == 0
    from digitalmodel.cad_io import cli

    def unexpected(*args, **kwargs):
        raise AssertionError("completed input was run twice")

    monkeypatch.setattr(cli, "_inspect", unexpected)
    assert main([str(drawings[0]), "--out", str(out)]) == 0
    drawings[0].write_text("changed")
    assert main([str(drawings[0]), "--out", str(out)]) == 1
    result = json.loads((out / "results.json").read_text())
    assert "input or schema changed" in result["files"][0]["reason"]


def test_render_refuses_to_overwrite_input(drawings):
    original = drawings[0].read_bytes()
    with pytest.raises(FileExistsError):
        dxf_to_svg(drawings[0], drawings[0])
    assert drawings[0].read_bytes() == original


def test_step_geometry_and_exports(solids, tmp_path):
    expected = [(24, [2, 3, 4]), (math.pi * 15 * 5, [8, 8, 5])]
    for index, (path, (volume, size)) in enumerate(zip(solids, expected)):
        result = step_checks(path, tmp_path / f"exports{index}", mesh=False)
        assert result["valid"] is True
        assert result["volume"] == pytest.approx(volume)
        assert result["bounding_box"]["size"] == pytest.approx(size)
        assert result["steps"]["stl"]["status"] == "ok"
        assert result["steps"]["gltf"]["status"] == "ok"
        assert (tmp_path / f"exports{index}" / "part.stl").stat().st_size > 0
        assert (tmp_path / f"exports{index}" / "part.glb").stat().st_size > 0


def test_gmsh_volume_mesh_and_inp(solids, tmp_path):
    try:
        import gmsh
    except (ImportError, OSError):
        pytest.skip("gmsh Python module and its shared libraries are required")
    result = step_checks(solids[0], tmp_path / "mesh")
    mesh = result["steps"]["mesh"]
    assert mesh["status"] == "ok"
    assert mesh["nodes"] > 0
    assert mesh["elements"] > 0
    text = (tmp_path / "mesh" / "part.inp").read_text().upper()
    assert "*NODE" in text and "*ELEMENT" in text
    assert not gmsh.isInitialized()


def test_step_cli_logs_bad_solid_and_continues(solids, tmp_path):
    (solids[0].parent / "bad.step").write_text("invalid STEP")
    out = tmp_path.parent / (tmp_path.name + "-outputs")
    assert main([str(solids[0].parent), "--out", str(out)]) == 1
    report = json.loads((out / "results.json").read_text())
    assert len(report["files"]) == 3
    assert report["summary"]["converted"] == 2
    assert sum(row["status"] == "failed" for row in report["files"]) == 1
