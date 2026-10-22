# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.
"""Synthetic CAD and failure-isolation acceptance checks in the specialized shard."""

from __future__ import annotations

import json
import math
import shutil
import subprocess
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
        cad.export_step(shape, path)
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
