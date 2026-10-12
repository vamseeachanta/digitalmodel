"""Coordinate-only export regressions; CAD payloads never enter assertions."""

import importlib.util
import json
import os
from pathlib import Path

import ezdxf
import pytest

ROOT = Path(__file__).resolve().parents[3]
SCRIPT = ROOT / "scripts/hull_library/abstracted_hulls.py"


def exporter():
    assert SCRIPT.exists(), "coordinate-only exporter is missing"
    spec = importlib.util.spec_from_file_location("abstracted_hulls", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_allowlist_excludes_metadata_and_unsupported_entities():
    module = exporter()
    doc = ezdxf.new()
    model = doc.modelspace()
    model.add_line((0, 0, 0), (10, 1, 2))
    model.add_text("synthetic private sentinel")
    model.add_point((9, 9, 9))
    curves = module.coordinate_curves(doc)
    assert curves == [[[0.0, 0.0, 0.0], [10.0, 1.0, 2.0]]]


def test_sections_interpolate_coordinates():
    module = exporter()
    curves = [[[0.0, 0.0, 0.0], [10.0, 2.0, 4.0]]]
    assert module.intersections(curves, 0, 5.0) == [[5.0, 1.0, 2.0]]
    assert module.intersections(curves, 1, 1.0) == [[5.0, 1.0, 2.0]]
    assert module.intersections(curves, 2, 2.0) == [[5.0, 1.0, 2.0]]


def test_nonfinite_coordinates_fail_closed():
    module = exporter()
    doc = ezdxf.new()
    doc.modelspace().add_line((float("nan"), 0, 0), (1, 1, 1))
    with pytest.raises(ValueError, match="Nonfinite coordinate"):
        module.coordinate_curves(doc)


def test_units_and_source_digest_gate():
    module = exporter()
    doc = ezdxf.new()
    doc.header["$INSUNITS"] = 0
    record = {
        "source_sha256": "a",
        "unit_inference": {"status": "declared"},
        "dxf": {"insunits": 4},
    }
    assert module.units_status(doc, record, "a") == "units_unverified"
    doc.header["$INSUNITS"] = 4
    assert module.units_status(doc, record, "b") == "units_unverified"
    assert module.units_status(doc, record, "a") == "mm_declared"


def test_real_sources_and_committed_output(tmp_path):
    module = exporter()
    converter = os.environ.get("LIBREDWG_DWGREAD")
    if not converter:
        pytest.skip("Set LIBREDWG_DWGREAD for all-six-source privacy regression")
    probe = json.loads((ROOT / "docs/reports/dwg-conversion/probe.json").read_text())
    configured = os.environ.get("HULL_DWG_SOURCE_DIR")
    if not configured:
        pytest.skip("Set HULL_DWG_SOURCE_DIR for privately retained source DWGs")
    source_dir = Path(configured)
    actual = sorted(p.name for p in source_dir.iterdir() if p.suffix.lower() == ".dwg")
    assert actual == sorted(r["source"] for r in probe["sources"])
    observations = []
    for record in probe["sources"]:
        source = source_dir / record["source"]
        before = module.digest(source)
        assert before == record["source_sha256"], (
            "private source differs from recorded digest"
        )
        doc, audit = module.read_dwg(source, converter)
        assert bool(module.coordinate_curves(doc)), "no coordinate curves"
        assert module.digest(source) == before, "source changed"
        observations.append((doc, audit, record, before))
    verify_real_export(module, observations, tmp_path)


def verify_real_export(module, observations, tmp_path):
    documents = [entry[0] for entry in observations]
    counts = {"mm_declared": 0, "units_unverified": 0}
    for index, (doc, audit, record, before) in enumerate(observations):
        status = module.units_status(doc, record, before)
        counts[status] += 1
        target = tmp_path / f"hull-{index + 1:02d}.json"
        if status == "units_unverified":
            written = module.export_document(doc, record, before, target, audit)
            assert written is False
        else:
            # All-six literal comparison fails closed on incidental numeric
            # matches. No attribute values appear in this assertion or logs.
            with pytest.raises(ValueError, match="Metadata collision; output withheld"):
                module.export_document(
                    doc, record, before, target, audit, privacy_documents=documents
                )
        assert not target.exists(), "unqualified or colliding output was written"
    assert counts == {"mm_declared": 1, "units_unverified": 5}
    for committed in (ROOT / "data/hull_geometry/abstracted").glob("*.json"):
        for doc in documents:
            clean = module.attribute_values_absent(doc, committed.read_bytes())
            assert clean, "committed output failed runtime attribute exclusion"


def test_block_metadata_is_not_expanded_and_defaults_are_checked():
    module = exporter()
    doc = ezdxf.new()
    block = doc.blocks.new("synthetic block")
    block.add_line((0, 0), (1, 1))
    block.add_attdef("synthetic tag", text="synthetic restricted value")
    doc.modelspace().add_blockref("synthetic block", (0, 0))
    assert module.coordinate_curves(doc) == []
    assert (
        module.attribute_values_absent(doc, b'{"synthetic restricted value":1}')
        is False
    )
    assert module.attribute_values_absent(doc, b'{"curves":[]}') is True


def test_decoder_failure_does_not_reveal_diagnostics(tmp_path, capsys):
    module = exporter()
    decoder = tmp_path / "decoder"
    decoder.write_text("#!/bin/sh\necho synthetic-private-sentinel >&2\nexit 1\n")
    decoder.chmod(0o700)
    with pytest.raises(ValueError, match="DWG decoding failed; diagnostics withheld"):
        module.read_dwg(tmp_path / "source.dwg", decoder)
    captured = capsys.readouterr()
    assert not captured.out and not captured.err


def test_parser_failure_does_not_reveal_diagnostics(tmp_path, capsys):
    module = exporter()
    decoder = tmp_path / "decoder"
    decoder.write_text("#!/bin/sh\necho synthetic-private-sentinel\n")
    decoder.chmod(0o700)
    with pytest.raises(ValueError, match="DWG decoding failed; diagnostics withheld"):
        module.read_dwg(tmp_path / "source.dwg", decoder)
    captured = capsys.readouterr()
    assert not captured.out and not captured.err


def test_spatial_mesh_topology_does_not_connect_unrelated_vertices():
    module = exporter()
    doc = ezdxf.new()
    mesh = doc.modelspace().add_polymesh((2, 2))
    points = {
        (0, 0): (0, 0, 0),
        (0, 1): (0, 1, 1),
        (1, 0): (2, 0, 0),
        (1, 1): (2, 1, 1),
    }
    for key, point in points.items():
        mesh.set_mesh_vertex(key, point)
    curves = module.coordinate_curves(doc, spatial_only=True)
    assert len(curves) == 4
    assert [[0.0, 0.0, 0.0], [2.0, 0.0, 0.0]] in curves
    assert [[0.0, 1.0, 1.0], [2.0, 0.0, 0.0]] not in curves


def test_polyface_exports_face_coordinates_without_face_record_origins():
    module = exporter()
    doc = ezdxf.new()
    face = doc.modelspace().add_polyface()
    face.append_face([(10, 0, 0), (10, 1, 0), (10, 1, 1)])
    curves = module.coordinate_curves(doc, spatial_only=True)
    assert curves == [
        [[10.0, 0.0, 0.0], [10.0, 1.0, 0.0], [10.0, 1.0, 1.0], [10.0, 0.0, 0.0]]
    ]


def test_export_checks_other_drawing_attributes_before_writing(tmp_path):
    module = exporter()
    doc = ezdxf.new()
    doc.header["$INSUNITS"] = 4
    doc.modelspace().add_polyline3d([(0, 0, 0), (10, 1, 2)])
    other = ezdxf.new()
    block = other.blocks.new("synthetic block")
    block.add_attdef("tag", text="mm_declared")
    record = {
        "source_sha256": "a",
        "unit_inference": {"status": "declared"},
        "dxf": {"insunits": 4},
    }
    target = tmp_path / "output.json"
    with pytest.raises(ValueError, match="Metadata collision; output withheld"):
        module.export_document(
            doc,
            record,
            "a",
            target,
            {"errors": 0, "fixes": 0},
            privacy_documents=[doc, other],
        )
    assert not target.exists()


def test_caller_cannot_omit_exported_source_from_privacy_check(tmp_path):
    module = exporter()
    doc = ezdxf.new()
    doc.header["$INSUNITS"] = 4
    doc.modelspace().add_polyline3d([(0, 0, 0), (10, 1, 2)])
    block = doc.blocks.new("synthetic block")
    block.add_attdef("tag", text="mm_declared")
    record = {
        "source_sha256": "a",
        "unit_inference": {"status": "declared"},
        "dxf": {"insunits": 4},
    }
    with pytest.raises(ValueError, match="Metadata collision; output withheld"):
        module.export_document(
            doc,
            record,
            "a",
            tmp_path / "output.json",
            {"errors": 0, "fixes": 0},
            privacy_documents=[ezdxf.new()],
        )


def test_raw_sources_removed_and_case_insensitive_ignore_guard():
    import subprocess

    folder = ROOT / "docs/domains/freecad/src/hulls"
    assert not any(p.suffix.lower() == ".dwg" for p in folder.iterdir())
    env = {
        key: value
        for key, value in os.environ.items()
        if key not in {"GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"}
    }
    tracked = subprocess.run(
        ["git", "ls-files", "-z", "--", "docs/domains/freecad/src/hulls"],
        cwd=ROOT,
        env=env,
        capture_output=True,
        check=True,
    ).stdout.split(b"\0")
    assert b"docs/domains/freecad/src/hulls/.gitignore" in tracked
    assert not any(name.lower().endswith(b".dwg") for name in tracked)
    for name in ("future.dwg", "future.DWG", "future.DwG"):
        result = subprocess.run(
            ["git", "check-ignore", "--no-index", str(folder / name)],
            cwd=ROOT,
            env=env,
            capture_output=True,
            check=False,
        )
        assert result.returncode == 0, "raw source ignore guard missing"


def test_cli_reads_explicit_private_source_directory(tmp_path, monkeypatch, capsys):
    module = exporter()
    probe = json.loads((ROOT / "docs/reports/dwg-conversion/probe.json").read_text())
    for record in probe["sources"]:
        (tmp_path / record["source"]).write_bytes(b"synthetic input")
    observed = []

    def read_synthetic(source, converter):
        observed.append(source)
        assert source.read_bytes() == b"synthetic input"
        return ezdxf.new(), None

    monkeypatch.setattr(module, "read_dwg", read_synthetic)
    monkeypatch.setattr(
        "sys.argv",
        [
            "exporter",
            "--converter",
            "synthetic-converter",
            "--source-dir",
            str(tmp_path),
            "--output",
            str(tmp_path / "output"),
        ],
    )
    assert module.main() == 0
    assert observed == [tmp_path / r["source"] for r in probe["sources"]]
    assert not (tmp_path / "output").exists()
    output = capsys.readouterr().out
    assert str(tmp_path) not in output
    assert all(r["source"] not in output for r in probe["sources"])


def test_cli_requires_private_source_directory(monkeypatch, capsys):
    module = exporter()
    monkeypatch.setattr(
        "sys.argv", ["exporter", "--converter", "synthetic", "--output", "output"]
    )
    with pytest.raises(SystemExit) as failure:
        module.main()
    assert failure.value.code == 2
    assert "--source-dir" in capsys.readouterr().err


def test_cli_missing_source_directory_reports_neutral_precondition(
    tmp_path, monkeypatch, capsys
):
    module = exporter()
    missing = tmp_path / "missing"
    monkeypatch.setattr(
        "sys.argv",
        [
            "exporter",
            "--converter",
            "synthetic",
            "--source-dir",
            str(missing),
            "--output",
            str(tmp_path / "output"),
        ],
    )
    assert module.main() == 1
    output = capsys.readouterr().out
    assert "source directory unavailable" in output
    assert str(missing) not in output


def test_real_source_configuration_missing_directory_fails(tmp_path, monkeypatch):
    monkeypatch.setenv("LIBREDWG_DWGREAD", "synthetic-converter")
    monkeypatch.setenv("HULL_DWG_SOURCE_DIR", str(tmp_path / "missing"))
    with pytest.raises(FileNotFoundError):
        test_real_sources_and_committed_output(tmp_path)


def test_real_source_configuration_digest_mismatch_fails(tmp_path, monkeypatch):
    probe = json.loads((ROOT / "docs/reports/dwg-conversion/probe.json").read_text())
    for record in probe["sources"]:
        (tmp_path / record["source"]).write_bytes(b"synthetic mismatched input")
    monkeypatch.setenv("LIBREDWG_DWGREAD", "synthetic-converter")
    monkeypatch.setenv("HULL_DWG_SOURCE_DIR", str(tmp_path))
    with pytest.raises(
        AssertionError, match="private source differs from recorded digest"
    ):
        test_real_sources_and_committed_output(tmp_path)
