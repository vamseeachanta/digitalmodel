# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Parametric pilot criteria, geometry, extraction and deterministic lookup tests."""

import hashlib
import json
from pathlib import Path

import numpy as np
import pytest


ROOT = Path(__file__).resolve().parents[3]
BASIS = ROOT / "examples/hydrodynamics/parametric_rao/basis.json"
CRITERIA = BASIS.with_name("criteria.json")
REVISIONS = BASIS.with_name("revisions.json")


def test_cartesian_table_covers_both_forms_and_every_dimension():
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    basis = json.loads(BASIS.read_text())
    table = pilot.case_table(basis)
    assert len(table) == 16
    assert len({c["case_id"] for c in table}) == 16
    for form in ("box", "analytic_ship"):
        selected = [c for c in table if c["form"] == form]
        assert len(selected) == 8
        for key in ("length_m", "beam_m", "draft_m"):
            assert {c[key] for c in selected} == set(basis["dimensions"][key])


@pytest.mark.parametrize("form", ["box", "analytic_ship"])
def test_generated_mesh_has_outward_normals_and_dimension_bound_digest(form):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    case = pilot.case_table(json.loads(BASIS.read_text()))[0] | {"form": form}
    vertices, panels = pilot.geometry(case)
    quads = vertices[panels]
    normals = np.cross(quads[:, 1] - quads[:, 0], quads[:, 2] - quads[:, 0])
    assert (np.linalg.norm(normals, axis=1) > 0).all()
    assert vertices[:, 0].min() == pytest.approx(-case["length_m"] / 2)
    assert vertices[:, 0].max() == pytest.approx(case["length_m"] / 2)
    assert vertices[:, 1].min() == pytest.approx(-case["beam_m"] / 2, rel=0.01)
    assert vertices[:, 2].min() == pytest.approx(-case["draft_m"])
    assert vertices[:, 2].max() == 0
    centers = quads.mean(axis=1)
    interior = np.array([0, 0, -case["draft_m"] / 2])
    assert (np.einsum("ij,ij->i", normals, centers - interior) >= -1e-9).all()
    digest = pilot.geometry_digest(vertices, panels)
    assert len(digest) == 64
    assert digest == pilot.geometry_digest(*pilot.geometry(case))
    assert digest != pilot.geometry_digest(*pilot.geometry(case | {"beam_m": case["beam_m"] * 1.1}))


def fixture_record(case=None):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    if case is None:
        case = pilot.case_table(json.loads(BASIS.read_text()))[0]
    values = np.ones((2, 2, 6), complex) * (1 + 1j)
    return {
        "case": case, "geometry_sha256": "a" * 64,
        "solver_version": "fixture-only", "panel_count": 8,
        "omega_rad_s": [2.0, 1.0], "heading_deg": [90.0, 0.0],
        "rao_real": values.real.tolist(), "rao_imag": values.imag.tolist(),
        "input_sha256": "b" * 64,
    }


def test_extract_sorts_aligned_axes_and_converts_rotations_to_database_units(tmp_path):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
    record = fixture_record()
    record["rao_real"][1][1][0] = 7
    entry = pilot.database_entry(record)
    assert list(entry.rao_data.frequencies) == [1.0, 2.0]
    assert list(entry.rao_data.directions) == [0.0, 90.0]
    assert entry.rao_data.amplitudes[0, 0, 0] == pytest.approx(np.hypot(7, 1))
    assert entry.rao_data.amplitudes[0, 0, 3] == pytest.approx(np.rad2deg(np.sqrt(2)))
    assert entry.rao_data.phases[0, 0, 3] == pytest.approx(45)
    assert entry.metadata["geometry_sha256"] == "a" * 64
    assert entry.metadata["solver_version"] == "fixture-only"
    db = RAODatabase()
    db.store(entry)
    path = tmp_path / "database.parquet"
    db.save_to_disk(path)
    restored = RAODatabase()
    restored.load_from_disk(path)
    loaded = restored.get_by_id(entry.variation_id)
    np.testing.assert_array_equal(loaded.rao_data.amplitudes, entry.rao_data.amplitudes)
    assert loaded.metadata == entry.metadata
    assert len(list(restored.query({"length_m": (0, entry.hull_params["length_m"])}))) == 1


@pytest.mark.parametrize("field,value", [
    ("geometry_sha256", "invalid"), ("solver_version", ""),
    ("rao_real", [[[float("nan")] * 6] * 2] * 2),
    ("omega_rad_s", [1.0, 1.0]), ("heading_deg", [0.0, 0.0]),
])
def test_extraction_rejects_invalid_results(field, value):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    with pytest.raises(ValueError):
        pilot.database_entry(fixture_record() | {field: value})


def test_criteria_receipt_precedes_first_run_and_detects_tampering(tmp_path):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    criteria = tmp_path / "criteria.json"
    criteria.write_bytes(CRITERIA.read_bytes())
    before = pilot.digest(criteria)
    receipt = pilot.freeze_criteria(criteria, tmp_path / "run.jsonl")
    pilot.validate_receipt(criteria, receipt, "9999-01-01T00:00:00+00:00")
    assert receipt["criteria_sha256"] == hashlib.sha256(criteria.read_bytes()).hexdigest()
    with pytest.raises(ValueError):
        pilot.validate_receipt(criteria, receipt, "2000-01-01T00:00:00+00:00")
    criteria.write_bytes(criteria.read_bytes() + b" ")
    tampered = pilot.digest(criteria)
    with pytest.raises(ValueError):
        pilot.validate_receipt(criteria, receipt, "9999-01-01T00:00:00+00:00")
    criteria.write_bytes(CRITERIA.read_bytes())
    restored = pilot.digest(criteria)
    assert before == restored and before != tampered
    pilot.write_json(tmp_path / "criteria-probe.json", {"before": before, "tampered": tampered, "restored": restored})


def test_benchmark_uses_frozen_bounds_and_changed_response_fails():
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    ref = {"omega_rad_s": [1.0, 2.0], "heave_rao": [1.0, 0.1]}
    criteria = json.loads(CRITERIA.read_text())
    assert pilot.compare_heave([1.0, 0.1], ref, criteria)["passed"]
    assert not pilot.compare_heave([2.0, 0.1], ref, criteria)["passed"]
    with pytest.raises(ValueError):
        pilot.compare_heave([1.0], ref, criteria)


def test_convergence_reports_all_panels_and_actual_response_change():
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    records = [fixture_record() | {"panel_count": n, "heave_rao": h}
               for n, h in [(8, [1.2, 0.3]), (16, [1.0, 0.2]), (32, [1.01, 0.21])]]
    result = pilot.convergence_record(records, json.loads(CRITERIA.read_text()))
    assert result["panel_counts"] == [8, 16, 32]
    assert result["max_absolute_change_m_per_m"] == pytest.approx(0.01)
    with pytest.raises(ValueError):
        pilot.convergence_record(records[:1], json.loads(CRITERIA.read_text()))


def test_lookup_builds_twice_identically_from_database_and_revisions(tmp_path):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
    from digitalmodel.hydrodynamics.hull_library.parametric_rao_report import build_lookup
    db = RAODatabase()
    db.store(pilot.database_entry(fixture_record()))
    path = tmp_path / "database.parquet"
    db.save_to_disk(path)
    revisions = json.loads(REVISIONS.read_text())
    first = build_lookup(path, revisions, {"revision": 0, "summary": "Fixture only."})
    second = build_lookup(path, revisions, {"revision": 0, "summary": "Fixture only."})
    assert first == second
    assert first.index("Summary") < first.index("Revision history") < first.index("Lookup")
    assert "deg/m" in first and "m/m" in first
    assert "Head-sea comparison" in first and "comparison-line" in first
    assert "src=" not in first and "href=" not in first
    with pytest.raises(ValueError):
        build_lookup(path, revisions, {"revision": 99, "summary": "Fixture only."})


def test_native_pilot_artifacts_criteria_and_database_readback(tmp_path):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
    output = ROOT / "docs/domains/hydrodynamics/parametric-rao"
    manifest = json.loads((output / "manifest.json").read_text())
    for name, evidence in manifest["outputs"].items():
        path = output / name
        assert pilot.digest(path) == evidence["sha256"]
        assert path.stat().st_size == evidence["bytes"]
    assert manifest["criteria_sha256"] == pilot.digest(CRITERIA)
    pilot.validate_receipt(CRITERIA, manifest["criteria_receipt"], manifest["first_run_started_at"])
    results = json.loads((output / "results.json").read_text())
    assert results["benchmark"]["passed"]
    assert results["convergence"]["passed"]
    assert len(results["records"]) == 16
    db = RAODatabase()
    db.load_from_disk(output / "pilot.parquet")
    for record in results["records"]:
        entry = db.get_by_id(record["case"]["case_id"])
        assert record["geometry_sha256"] == pilot.geometry_digest(*pilot.geometry(record["case"]))
        mesh_file = tmp_path / "geometry.gdf"
        pilot._write_mesh(mesh_file, *pilot.geometry(record["case"]))
        assert pilot.digest(mesh_file) == record["geometry_input_sha256"]
        assert entry.metadata["solver_version"] == record["solver_version"]
        assert entry.metadata["solver_version"] != "fixture-only"
        np.testing.assert_array_equal(entry.rao_data.amplitudes, pilot.database_entry(record).rao_data.amplitudes)
        response = np.array(record["rao_real"]) + 1j*np.array(record["rao_imag"])
        excitation = np.array(record["excitation_real"]) + 1j*np.array(record["excitation_imag"])
        for index, omega in enumerate(record["omega_rad_s"]):
            operator = -omega**2*(np.array(record["inertia"]) + np.array(record["added_mass"])[index]) - 1j*omega*np.array(record["radiation_damping"])[index] + np.array(record["hydrostatic_stiffness"])
            for heading in range(len(record["heading_deg"])):
                residual = operator @ response[index, heading] - excitation[index, heading]
                assert np.linalg.norm(residual) <= 1e-8 * max(np.linalg.norm(excitation[index, heading]), 1)


@pytest.mark.parametrize("key,value", [("length_m", [0]), ("beam_m", [float("nan")]), ("draft_m", [1, 1])])
def test_case_table_rejects_invalid_dimensions_before_a_solve(key, value):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    basis = json.loads(BASIS.read_text())
    basis["dimensions"][key] = value
    with pytest.raises(ValueError):
        pilot.case_table(basis)


def test_resume_rejects_tampering_and_changed_basis(tmp_path):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    basis = json.loads(BASIS.read_text())
    record = fixture_record()
    basis = basis | {"omega_rad_s": record["omega_rad_s"], "heading_deg": record["heading_deg"]}
    case = record["case"]
    receipt = {"criteria_sha256": pilot.digest(CRITERIA)}
    record["panel_count"] = len(pilot.geometry(case)[1])
    record["criteria_sha256"] = receipt["criteria_sha256"]
    record["geometry_sha256"] = pilot.geometry_digest(*pilot.geometry(case))
    record["input_sha256"] = pilot.input_digest(case, basis, record["geometry_sha256"], record["solver_version"], receipt)
    path = tmp_path / "native.json"
    def save():
        checksum = hashlib.sha256(json.dumps(record, sort_keys=True, allow_nan=False).encode()).hexdigest()
        pilot.write_json(path, record | {"record_sha256": checksum})
    save()
    before = pilot.digest(path)
    assert pilot.read_record(path, case, basis, receipt) == record
    altered = json.loads(path.read_text())
    altered["rao_real"][0][0][0] += 1
    pilot.write_json(path, altered)
    tampered = pilot.digest(path)
    with pytest.raises(ValueError):
        pilot.read_record(path, case, basis, receipt)
    save()
    restored = pilot.digest(path)
    assert before == restored and before != tampered
    pilot.write_json(tmp_path / "resume-probe.json", {"before": before, "tampered": tampered, "restored": restored})
    with pytest.raises(ValueError):
        pilot.read_record(path, case, basis | {"rho_kg_m3": 1}, receipt)


def test_solver_rejects_path_traversal_before_optional_solver_import(tmp_path):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    with pytest.raises(ValueError):
        pilot.solve_case({"case_id": "../escape"}, {}, tmp_path, {})
    assert list(tmp_path.iterdir()) == []


def test_current_body_api_returns_immersed_body(monkeypatch):
    import sys
    from types import SimpleNamespace
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    sentinel = object()
    calls = []
    class Body:
        def __init__(self, **kwargs):
            calls.append(kwargs)
        def immersed_part(self):
            return sentinel
    library = SimpleNamespace(load_mesh=lambda path: path, FloatingBody=Body,
                              rigid_body_dofs=lambda **kwargs: kwargs)
    monkeypatch.setitem(sys.modules, "capytaine", library)
    config = SimpleNamespace(mesh=SimpleNamespace(path="geometry.gdf", name="box"),
                             rotation_center_or_com=(0, 0, -2), center_of_mass=(0, 0, -2))
    assert pilot.floating_body(config) is sentinel
    assert calls[0]["center_of_mass"] == (0, 0, -2)


def test_current_engine_api_places_green_function_on_engine():
    from types import SimpleNamespace
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    calls = []
    marker = object()
    def engine(**kwargs):
        calls.append(kwargs)
        return marker
    def solver(**kwargs):
        assert kwargs == {"engine": marker, "method": "indirect"}
        return marker
    library = SimpleNamespace(Delhommeau=lambda: "green", DefaultMatrixEngine=engine,
                              BEMSolver=solver)
    config = SimpleNamespace(linear_solver="lu_decomposition", method="indirect")
    assert pilot.bem_solver(library, config) is marker
    assert calls == [{"green_function": "green", "linear_solver": "lu_decomposition"}]


def test_pilot_database_declares_partial_readiness_and_heading_frame():
    from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
    db = RAODatabase()
    db.load_from_disk(ROOT / "docs/domains/hydrodynamics/parametric-rao/pilot.parquet")
    entries = list(db.query({}))
    assert len(entries) == 16
    for entry in entries:
        assert entry.metadata["readiness_status"] == "partial"
        assert "design use remains unverified" in entry.metadata["intended_use"]
        assert entry.metadata["heading_convention"] == "wave propagation angle measured counterclockwise from +x"


def test_source_links_resolve_to_the_pinned_repository_paths():
    from urllib.parse import urljoin
    documents = [BASIS, ROOT / "docs/domains/hydrodynamics/parametric-rao/manifest.json"]
    for document in documents:
        data = json.loads(document.read_text())
        revision = json.loads(BASIS.read_text())["source_revision"]
        prefix = "https://example.invalid/owner/repository/"
        document_url = prefix + "blob/" + revision + "/" + document.relative_to(ROOT).as_posix()
        for source in data["sources"]:
            if "repository_path" in source:
                expected = prefix + "blob/" + revision + "/" + source["repository_path"]
                assert urljoin(document_url, source["source_url"]) == expected
                assert hashlib.sha256((ROOT / source["repository_path"]).read_bytes()).hexdigest() == source["sha256"]
