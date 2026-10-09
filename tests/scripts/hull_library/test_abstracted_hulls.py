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
    actual = sorted(
        p.name
        for p in (ROOT / "docs/domains/freecad/src/hulls").iterdir()
        if p.suffix.lower() == ".dwg"
    )
    assert actual == sorted(r["source"] for r in probe["sources"])
    observations = []
    for record in probe["sources"]:
        source = ROOT / "docs/domains/freecad/src/hulls" / record["source"]
        before = module.digest(source)
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
