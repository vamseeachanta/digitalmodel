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
    first = build_lookup(path, revisions, {"revision": revisions[-1]["revision"], "summary": "Fixture only."})
    second = build_lookup(path, revisions, {"revision": revisions[-1]["revision"], "summary": "Fixture only."})
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
            operator = -omega**2*(np.array(record["inertia"]) + np.array(record["added_mass"])[index].T) - 1j*omega*np.array(record["radiation_damping"])[index].T + np.array(record["hydrostatic_stiffness"])
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


def test_response_uses_force_rows_for_asymmetric_added_mass_and_damping(monkeypatch, tmp_path):
    import sys
    from types import SimpleNamespace
    import xarray as xr
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot

    basis = json.loads(BASIS.read_text()) | {"omega_rad_s": [1.0, 2.0]}
    case = pilot.case_table(basis)[0]
    n = len(pilot.geometry(case)[1])
    added = np.zeros((2, 6, 6))
    damping = np.zeros_like(added)
    added[:, 0, 1], added[:, 1, 0] = 2, 1
    damping[:, 0, 1], damping[:, 1, 0] = 3, 1
    force = np.zeros((2, 2, 6), complex)
    force[:, :, :2] = [1, 2]
    coords = {"omega": basis["omega_rad_s"], "radiating_dof": pilot.DOFS,
              "influenced_dof": pilot.DOFS, "wave_direction": [0, np.pi / 2]}
    ds = xr.Dataset({
        "added_mass": (("omega", "radiating_dof", "influenced_dof"), added.transpose(0, 2, 1)),
        "radiation_damping": (("omega", "radiating_dof", "influenced_dof"), damping.transpose(0, 2, 1)),
        "excitation_force": (("omega", "wave_direction", "influenced_dof"), force),
    }, coords=coords)
    matrix_coords = {"influenced_dof": pilot.DOFS, "radiating_dof": pilot.DOFS}
    body = SimpleNamespace(mesh=SimpleNamespace(nb_faces=n), center_of_mass=(0, 0, -2),
        inertia_matrix=xr.DataArray(np.eye(6), dims=matrix_coords, coords=matrix_coords),
        hydrostatic_stiffness=xr.DataArray(10*np.eye(6), dims=matrix_coords, coords=matrix_coords))
    class Solver:
        def __init__(self, *args):
            self.body, self.dataset = body, ds
        def solve(self):
            pass
    monkeypatch.setitem(sys.modules, "capytaine", SimpleNamespace(__version__="fixture-only"))
    monkeypatch.setattr(pilot, "_solver_type", lambda *args: Solver)
    record = pilot.solve_case(case, basis, tmp_path, {"criteria_sha256": pilot.digest(CRITERIA)})
    response = np.array(record["rao_real"]) + 1j*np.array(record["rao_imag"])
    # At omega=1: Z=[[9,-2-3i],[-1-i,9]], F=[1,2].
    # det Z=81-(2+3i)(1+i)=82-5i; adj(Z) F=[13+6i,19+i].
    expected = np.array([13+6j, 19+1j]) / (82-5j)
    np.testing.assert_allclose(response[0, 0, :2], expected, rtol=1e-13)
    assert record["hydrodynamic_matrix_axes"] == ["omega", "radiating_dof", "influenced_dof"]
    assert record["added_mass"][0][1][0] == 2
    assert record["radiation_damping"][0][1][0] == 3


def test_ship_inertia_matches_closed_form_uniform_density_wigley():
    results = json.loads((ROOT / "docs/domains/hydrodynamics/parametric-rao/results.json").read_text())
    for record in results["records"]:
        if record["case"]["form"] != "analytic_ship":
            continue
        case = record["case"]
        length, beam, draft = (case[k] for k in ("length_m", "beam_m", "draft_m"))
        # Half breadth h=(B/2)(1-4x^2/L^2)(1-z^2/T^2), -T<=z<=0.
        # Integrating 2h gives V=4LBT/9, E[x^2]=L^2/20,
        # E[z]=-3T/8, E[z^2]=T^2/5. For E[y^2], integrate 2h^3/3:
        # each ratio integral(f^3)/integral(f)=24/35, hence E[y^2]=48B^2/1225.
        # About (0,0,c), E[(z-c)^2]=T^2/5+3cT/4+c^2.
        cog = record.get("center_of_mass_m", [0, 0, results["basis"]["cog_draft_fraction"]*draft])
        vertical = draft**2/5 + 3*cog[2]*draft/4 + cog[2]**2
        expected = np.array([48*beam**2/1225 + vertical,
                             length**2/20 + vertical, length**2/20 + 48*beam**2/1225])
        inertia = np.array(record["inertia"])
        actual = np.diag(inertia)[3:] / inertia[0, 0]
        # Six percent allows the retained coarse faceted mesh, not an open-surface deficit.
        np.testing.assert_allclose(actual, expected, rtol=0.06)
        np.testing.assert_allclose(cog, [0, 0, -3*draft/8], atol=0.03*draft)


def test_build_gate_rejects_unmasked_negative_heave_damping(tmp_path):
    import importlib.util
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    spec = importlib.util.spec_from_file_location("pilot_run", BASIS.with_name("run.py"))
    run = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(run)
    record = fixture_record()
    record["added_mass"] = np.zeros((2, 6, 6)).tolist()
    damping = np.zeros((2, 6, 6))
    damping[0, 2, 2] = -1
    record["radiation_damping"] = damping.tolist()
    record["started_at"] = "2026-10-15T00:00:01+00:00"
    reference = {"omega_rad_s": record["omega_rad_s"], "heave_rao": [np.sqrt(2)]*2}
    criteria = json.loads(CRITERIA.read_text())
    receipt = {"criteria_sha256": pilot.digest(CRITERIA), "recorded_at": "2026-10-15T00:00:00+00:00"}
    with pytest.raises(ValueError, match="Unmasked negative"):
        run.build([record], json.loads(BASIS.read_text()), criteria, receipt,
                  reference, pilot.convergence_record([{"panel_count": n, "heave_rao": [1.0, 1.0]} for n in (1, 2, 3)], criteria), tmp_path / "invalid")
    record["masked_points"] = [{"omega_rad_s": 2.0, "reasons": ["negative diagonal radiation damping: Heave"]}]
    # The same native coefficient is allowed only when its response point is removed.
    record["masked_points"][0]["index"] = 0
    assert run.build([record], json.loads(BASIS.read_text()), criteria, receipt,
                     reference, pilot.convergence_record([{"panel_count": n, "heave_rao": [1.0, 1.0]} for n in (1, 2, 3)], criteria), tmp_path / "masked")
    entry = pilot.database_entry(record)
    assert list(entry.rao_data.frequencies) == [1.0]
    assert entry.metadata["masked_points"] == record["masked_points"]


def test_waterplane_origin_integrals_match_closed_box_centroid():
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    case = pilot.case_table(json.loads(BASIS.read_text()))[0]
    mass, centroid, inertia = pilot.homogeneous_mass_properties(*pilot.geometry(case), 1025)
    length, beam, draft = (case[k] for k in ("length_m", "beam_m", "draft_m"))
    assert mass == pytest.approx(1025*length*beam*draft)
    np.testing.assert_allclose(centroid, [0, 0, -draft/2], atol=1e-12)
    np.testing.assert_allclose(np.diag(inertia), mass*np.array(
        [beam**2+draft**2, length**2+draft**2, length**2+beam**2])/12, rtol=1e-12)


def test_box_mask_uses_mode_band_and_coefficient_signs():
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    basis = json.loads(BASIS.read_text())
    masked = set()
    for case in pilot.case_table(basis)[:8]:
        record = fixture_record(case)
        record["omega_rad_s"] = basis["omega_rad_s"]
        record["added_mass"] = np.zeros((6, 6, 6)).tolist()
        record["radiation_damping"] = np.zeros((6, 6, 6)).tolist()
        result = pilot.mask_irregular_points(record, basis)
        if result["masked_points"]:
            masked.add(case["case_id"])
            assert [p["omega_rad_s"] for p in result["masked_points"]] == [1.32]
        assert result["interior_mode_omega_rad_s"] > 0
        if case["case_id"] == "box_03":
            record["radiation_damping"][-1][4][4] = -1
            flagged = pilot.mask_irregular_points(record, basis)
            assert "negative diagonal radiation damping: Pitch" in flagged["masked_points"][0]["reasons"]
            with pytest.raises(ValueError, match="Unmasked negative"):
                pilot.validate_physical_points(record, basis)
            pilot.validate_physical_points(flagged, basis)
        if case["case_id"] == "box_02":
            record["added_mass"][-1][2][2] = -1
            flagged = pilot.mask_irregular_points(record, basis)
            assert "negative heave added mass" in flagged["masked_points"][0]["reasons"]
    assert masked == {"box_01", "box_02", "box_03", "box_05", "box_06", "box_07"}



def test_box_mask_has_inclusive_asymmetric_band_edges():
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    basis = json.loads(BASIS.read_text())
    record = fixture_record()
    _, mode = pilot.point_reasons(record, basis)
    for fraction, expected in ((-0.0100001, False), (-0.01, True),
                               (-0.0099999, True), (0.0299999, True),
                               (0.03, True), (0.0300001, False)):
        record["omega_rad_s"] = [mode * (1 + fraction), 0.2]
        flagged = pilot.mask_irregular_points(record, basis)
        assert (0 in pilot.masked_indices(flagged)) == expected, fraction


def test_build_gate_rejects_unmasked_box_07_interior_mode(tmp_path):
    import importlib.util
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    spec = importlib.util.spec_from_file_location("pilot_run", BASIS.with_name("run.py"))
    run = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(run)
    basis = json.loads(BASIS.read_text())
    record = fixture_record(pilot.case_table(basis)[7])
    record["omega_rad_s"] = [1.32, 0.2]
    record["started_at"] = "2026-10-15T00:00:01+00:00"
    reference = {"omega_rad_s": record["omega_rad_s"], "heave_rao": [np.sqrt(2)]*2}
    criteria = json.loads(CRITERIA.read_text())
    receipt = {"criteria_sha256": pilot.digest(CRITERIA), "recorded_at": "2026-10-15T00:00:00+00:00"}
    convergence = pilot.convergence_record(
        [{"panel_count": n, "heave_rao": [1.0, 1.0]} for n in (1, 2, 3)], criteria)
    with pytest.raises(ValueError, match="Unmasked within declared interior-mode band at 1.32"):
        run.build([record], basis, criteria, receipt, reference, convergence, tmp_path)



def test_mass_integration_accepts_closed_box_and_waterline_open_meshes():
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    cases = pilot.case_table(json.loads(BASIS.read_text()))
    for case in (cases[0], cases[8]):
        vertices, panels = pilot.geometry(case)
        mass, _, _ = pilot.homogeneous_mass_properties(vertices, panels, 1025)
        assert mass > 0
    case = cases[0]
    vertices, panels = pilot.geometry(case)
    quads = vertices[panels]
    lid = quads[np.all(quads[:, :, 2] == -case["draft_m"], axis=1)][:, ::-1].copy()
    lid[:, :, 2] = 0
    closed = np.concatenate((quads, lid))
    vertices, inverse = np.unique(closed.reshape(-1, 3), axis=0, return_inverse=True)
    mass, _, _ = pilot.homogeneous_mass_properties(vertices, inverse.reshape(-1, 4), 1025)
    assert mass == pytest.approx(1025*case["length_m"]*case["beam_m"]*case["draft_m"])


@pytest.mark.parametrize("duplicate_vertices", [False, True])
def test_mass_integration_rejects_open_boundary_off_waterline(duplicate_vertices):
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    case = pilot.case_table(json.loads(BASIS.read_text()))[0]
    vertices, panels = pilot.geometry(case)
    if duplicate_vertices:
        vertices = vertices[panels].reshape(-1, 3)
        panels = np.arange(len(vertices)).reshape(-1, 4)
    vertices = vertices.copy()
    vertices[:, 2] -= 0.125
    with pytest.raises(ValueError, match=r"120 boundary edges.*largest \|z\|=0.125"):
        pilot.homogeneous_mass_properties(vertices, panels, 1025)
    # Lift a corner shared by two open edges; both endpoint positions matter.
    vertices, panels = pilot.geometry(case)
    if duplicate_vertices:
        vertices = vertices[panels].reshape(-1, 3)
        panels = np.arange(len(vertices)).reshape(-1, 4)
    vertices = vertices.copy()
    corner = np.unique(vertices[vertices[:, 2] == 0], axis=0)[-1]
    vertices[np.all(vertices == corner, axis=1), 2] = 2e-9
    with pytest.raises(ValueError, match=r"2 boundary edges.*largest \|z\|=2e-09"):
        pilot.homogeneous_mass_properties(vertices, panels, 1025)


def test_repeatability_limits_are_visible_and_retained_in_metadata():
    """Disclose unexplained retained-point scatter without implying a new solve."""
    from digitalmodel.hydrodynamics.hull_library.rao_database import RAODatabase
    from digitalmodel.hydrodynamics.hull_library.parametric_rao_report import build_lookup
    output = ROOT / "docs/domains/hydrodynamics/parametric-rao"
    results = json.loads((output / "results.json").read_text())
    revisions = json.loads(REVISIONS.read_text())
    html = build_lookup(output / "pilot.parquet", revisions, results)
    for text in ("1.3 %", "0.46 %", "cause is not established",
                 "does not discriminate", "face-centre quadrature",
                 "beam-sea roll is not mesh-qualified"):
        assert text in html
    assert "uses 0.5%" in html
    assert "0.03 %" in revisions[2]["change"]
    assert "cause is not established" in revisions[2]["change"]
    environment = revisions[2]["rebuild_environment"]
    assert environment["solver_environment"] == "Historical package versions beyond Capytaine are not recorded."
    assert environment["builder_revision"]
    assert set(environment["packages"]) >= {"numpy", "scipy", "xarray", "pandas", "pyarrow"}
    db = RAODatabase()
    db.load_from_disk(output / "pilot.parquet")
    for entry in db.query({}):
        assert "cause is not established" in entry.metadata["repeatability_limit"]
        assert "0.46 %" in entry.metadata["repeatability_limit"]


def test_retained_records_remain_bound_to_basis_and_implementation():
    """Reapply screening and input binding to all retained native coefficients."""
    from digitalmodel.hydrodynamics.hull_library import parametric_rao_pilot as pilot
    output = ROOT / "docs/domains/hydrodynamics/parametric-rao"
    basis = json.loads(BASIS.read_text())
    manifest = json.loads((output / "manifest.json").read_text())
    results = json.loads((output / "results.json").read_text())
    convergence = json.loads((output / "convergence-native.json").read_text())
    for record in results["records"] + convergence:
        local_basis = basis
        if record in convergence:
            local_basis = basis | {"omega_rad_s": record["omega_rad_s"]}
        pilot.validate_physical_points(record, local_basis)
        assert record["input_sha256"] == pilot.input_digest(
            record["case"], local_basis, record["geometry_sha256"],
            record["solver_version"], manifest["criteria_receipt"],
        )
