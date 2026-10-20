"""Solver-free diagnostics specified by agent-pack section 5 (G0/G4/G5)."""

import ast
import importlib.util
import logging
import subprocess
import sys
from collections import Counter
from pathlib import Path

import numpy as np
import pytest

PACKAGE = Path(__file__).resolve().parents[4] / "src/digitalmodel/solvers/orcawave"


def load_helper(name, relative_path):
    """Load only the audited helper, without unrelated solver initializers."""
    spec = importlib.util.spec_from_file_location(name, PACKAGE / relative_path)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


orchestrator = load_helper("pack_orchestrator", "diffraction/orchestrator.py")
OrcaFlexConverter = load_helper(
    "pack_converter", "diffraction/scripts/convert_to_orcaflex.py"
).OrcaFlexConverter
GeometryValidator = load_helper(
    "pack_geometry", "diffraction/scripts/validate_geometry.py"
).GeometryValidator


@pytest.fixture
def workflow(tmp_path):
    config = tmp_path / "config.yml"
    config.write_text("analysis: {}\n", encoding="utf-8")
    result = orchestrator.OrcaWaveOrchestrator(config_path=str(config))
    result.results_dir = tmp_path / "results"
    result.vessel_name = "synthetic"
    result._create_output_directories()
    return result


def test_g0_syntax():
    paths = sorted(PACKAGE.rglob("*.py"))
    assert paths
    for path in paths:
        tree = ast.parse(path.read_text(encoding="utf-8-sig"), filename=str(path))
        assert isinstance(tree, ast.Module)


def test_g0_geometry_rejects_empty_directory(tmp_path):
    with pytest.raises((FileNotFoundError, ValueError)):
        GeometryValidator(str(tmp_path)).validate_all(parallel=False)


@pytest.mark.parametrize("excel_present", [False, True])
def test_g0_components_empty_csv_rejected(tmp_path, excel_present):
    (tmp_path / "csv_outputs").mkdir()
    if excel_present:
        # Selection must refuse the empty CSV source, not silently switch formats.
        (tmp_path / "results.xlsx").write_bytes(b"unread synthetic workbook")
    converter = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))
    with pytest.raises(FileNotFoundError, match="No CSV results"):
        converter.load_orcawave_results()
    assert converter.vessel_data is None
    assert not list(converter.output_dir.iterdir())


def test_g0_components_direct_csv_rejected(tmp_path):
    converter = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))
    with pytest.raises(FileNotFoundError, match="No CSV results"):
        converter._load_from_csv(tmp_path)


def test_g0_components_properties_only_csv_rejected(tmp_path):
    csv_dir = tmp_path / "csv_outputs"
    csv_dir.mkdir()
    properties = ["displacement", "cog_x", "cog_y", "cog_z", "cob_x",
                  "cob_y", "cob_z", "waterplane_area", "metacentric_height"]
    (csv_dir / "vessel_properties.csv").write_text(
        "property,value\n" + "".join(f"{key},1\n" for key in properties),
        encoding="utf-8",
    )
    converter = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))
    with pytest.raises(FileNotFoundError, match="No CSV results"):
        converter.load_orcawave_results()
    assert converter.vessel_data is None
    assert not list(converter.output_dir.iterdir())


@pytest.mark.parametrize("files", [
    {"unrelated.csv"},
    {"frequencies.csv", "directions.csv"},
    {"directions.csv", "added_mass_11.csv"},
    {"frequencies.csv", "added_mass_11.csv"},
    {"frequencies.csv", "directions.csv", "added_mass_unused.csv"},
])
def test_g0_components_missing_sources_rejected_before_allocation(tmp_path, monkeypatch, files):
    contents = {"frequencies.csv": "frequency\n0.5\n",
                "directions.csv": "direction\n30\n"}
    for name in files:
        (tmp_path / name).write_text(contents.get(name, "value\n1\n"), encoding="utf-8")

    def unexpected_allocation(*args, **kwargs):
        pytest.fail("coefficient allocation preceded source validation")

    monkeypatch.setattr(np, "zeros", unexpected_allocation)
    converter = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))
    with pytest.raises(FileNotFoundError, match="No CSV results"):
        converter._load_from_csv(tmp_path)


@pytest.mark.parametrize("value", [0.0, 7.25])
@pytest.mark.parametrize("family", ["added_mass", "damping", "excitation_force"])
def test_nonempty_csv_values_are_preserved(tmp_path, value, family):
    """Nonempty legacy input is readable; this does not qualify missing channels."""
    (tmp_path / "frequencies.csv").write_text("frequency\n0.5\n", encoding="utf-8")
    (tmp_path / "directions.csv").write_text("direction\n30\n", encoding="utf-8")
    component = "1" if family == "excitation_force" else "11"
    content = f"real,imag\n{value},0\n" if family == "excitation_force" else f"value\n{value}\n"
    (tmp_path / f"{family}_{component}.csv").write_text(content, encoding="utf-8")
    result = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))._load_from_csv(tmp_path)
    np.testing.assert_array_equal(result.frequencies, [0.5])
    np.testing.assert_array_equal(result.directions, [30])
    assert getattr(result, family).flat[0] == value


def test_g0_components_phase3_refuses_empty_output(workflow, monkeypatch, caplog):
    real_run = subprocess.run
    calls = []

    def converter_only(command, **kwargs):
        assert Path(command[1]) == workflow.scripts_dir / "convert_to_orcaflex.py"
        # Only the solver-free Python converter may run, with captured output.
        kwargs["creationflags"] = getattr(subprocess, "CREATE_NO_WINDOW", 0)
        result = real_run(command, **kwargs)
        calls.append(result)
        return result

    monkeypatch.setattr(orchestrator.subprocess, "run", converter_only)
    with caplog.at_level(logging.ERROR):
        success = workflow._phase3_results_processing()
    assert calls
    assert calls[0].returncode == 1
    assert (workflow.results_dir / "csv_outputs").is_dir()
    assert not success
    assert workflow.workflow_state["results_converted"] is False
    assert "No CSV results" in caplog.text
    assert not list((workflow.results_dir / "orcaflex").iterdir())


@pytest.mark.parametrize("source", ["hdf5", "csv", "excel"])
def test_format_selection_preserved(tmp_path, monkeypatch, source):
    converter = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))
    expected = object()
    if source == "hdf5":
        (tmp_path / "results.h5").touch()
        (tmp_path / "csv_outputs").mkdir()
    elif source == "csv":
        csv_dir = tmp_path / "csv_outputs"
        csv_dir.mkdir()
        (csv_dir / "frequencies.csv").write_text("frequency\n0.5\n", encoding="utf-8")
        (csv_dir / "directions.csv").write_text("direction\n30\n", encoding="utf-8")
        (csv_dir / "added_mass_11.csv").write_text("value\n7.25\n", encoding="utf-8")
    (tmp_path / "results.xlsx").touch()
    monkeypatch.setattr(converter, f"_load_from_{source}", lambda path: expected)
    for other in {"hdf5", "csv", "excel"} - {source}:
        monkeypatch.setattr(converter, f"_load_from_{other}",
                            lambda path: pytest.fail("lower-priority format selected"))
    assert converter.load_orcawave_results() is expected


@pytest.fixture
def open_triangle():
    return np.eye(3), np.array([[0, 1, 2]])


@pytest.fixture
def open_triangle_stats(tmp_path, open_triangle):
    vertices, faces = open_triangle
    return GeometryValidator(str(tmp_path))._compute_mesh_statistics(
        "synthetic.obj", "obj", vertices, faces
    )


def test_g4_open_triangle_preconditions(open_triangle, open_triangle_stats):
    _, faces = open_triangle
    edges = Counter(tuple(sorted((face[i], face[(i + 1) % 3])))
                    for face in faces for i in range(3))
    assert sum(count == 1 for count in edges.values()) == 3
    assert open_triangle_stats.volume == pytest.approx(1 / 6)


def test_g4_closure_rejects_open_triangle(open_triangle_stats):
    assert not open_triangle_stats.is_watertight


def test_g5_incomplete_results_precondition(workflow):
    assert workflow._check_data_completeness() is False


def test_g5_state_refuses_incomplete_results(workflow):
    success = workflow._phase4_quality_assurance()
    assert (success, workflow.workflow_state["quality_checked"]) == (False, False)


# Synthetic topology expands the open-triangle fixture above, from issue 2738.
# Counts here describe connectivity, not engineering tolerances.
@pytest.fixture
def tetrahedron():
    vertices = np.vstack((np.zeros(3), np.eye(3)))
    faces = np.array([[0, 2, 1], [0, 1, 3], [0, 3, 2], [1, 2, 3]])
    return vertices, faces


@pytest.mark.parametrize("separate_vertices", [False, True])
def test_closed_tetrahedron_topology(tmp_path, tetrahedron, separate_vertices):
    vertices, faces = tetrahedron
    if separate_vertices:
        # STL represents each triangle with its own coordinate records.
        vertices = vertices[faces].reshape(-1, 3)
        faces = np.arange(len(vertices)).reshape(-1, 3)
    stats = GeometryValidator(str(tmp_path))._compute_mesh_statistics(
        "synthetic", "obj", vertices, faces
    )
    assert stats.is_watertight
    assert stats.num_edges == 6  # Six distinct edges of the fixture tetrahedron.


@pytest.mark.parametrize("defect", ["hole", "duplicate", "nonmanifold"])
def test_topology_defects_are_not_closed(tmp_path, tetrahedron, defect):
    vertices, faces = tetrahedron
    if defect == "hole":
        faces = faces[:-1]
    elif defect == "duplicate":
        faces = np.array([faces[0], faces[0][::-1]])
    else:
        # A second tetrahedron shares the base face, with a mirrored apex.
        # Distinct triangles give excessive incidence without duplicate faces.
        vertices = np.vstack((vertices, -vertices[-1]))
        faces = np.vstack((faces, [[0, 1, 4], [1, 2, 4], [2, 0, 4]]))
        edges = Counter(tuple(sorted((face[i], face[(i + 1) % 3])))
                        for face in faces for i in range(3))
        assert set(edges.values()) == {2, 3}
        assert len({tuple(sorted(face)) for face in faces}) == len(faces)
    stats = GeometryValidator(str(tmp_path))._compute_mesh_statistics(
        "synthetic", "obj", vertices, faces
    )
    assert not stats.is_watertight


@pytest.mark.parametrize("bad_faces", [[], [[0, 1]], [[0, 1, 2, 3]],
                                       [[-1, 1, 2]], [[0, 1, 4]]])
def test_unsupported_connectivity_rejected(tmp_path, tetrahedron, bad_faces):
    vertices, _ = tetrahedron
    with pytest.raises(ValueError):
        GeometryValidator(str(tmp_path))._compute_mesh_statistics(
            "synthetic", "obj", vertices, np.array(bad_faces)
        )


@pytest.mark.parametrize("parallel", [False, True])
def test_geometry_discovery_clears_stale_results(tmp_path, parallel):
    validator = GeometryValidator(str(tmp_path))
    validator.results["stale"] = object()
    with pytest.raises(FileNotFoundError):
        validator.validate_all(parallel=parallel)
    assert validator.results == {}


@pytest.mark.parametrize("prior", [False, True])
@pytest.mark.parametrize("passed", [False, True])
def test_quality_state_matches_checks(workflow, monkeypatch, prior, passed):
    workflow.workflow_state["quality_checked"] = prior
    monkeypatch.setattr(workflow, "_run_quality_checks", lambda: passed)
    assert workflow._phase4_quality_assurance() is passed
    assert workflow.workflow_state["quality_checked"] is passed


def test_quality_exception_clears_stale_success(workflow, monkeypatch):
    workflow.workflow_state["quality_checked"] = True

    def fail():
        raise ValueError("synthetic invalid results")

    monkeypatch.setattr(workflow, "_run_quality_checks", fail)
    with pytest.raises(ValueError, match="synthetic invalid results"):
        workflow._phase4_quality_assurance()
    assert workflow.workflow_state["quality_checked"] is False


@pytest.mark.parametrize("method", ["_check_reciprocity", "_check_energy_conservation",
                                   "_check_asymptotes", "_generate_visualizations"])
def test_uncomputed_checks_never_pass(workflow, method, caplog):
    with caplog.at_level(logging.WARNING):
        assert getattr(workflow, method)() is False
    assert "NOT_EVALUATED" in caplog.text


@pytest.mark.parametrize("value", [np.nan, np.inf, -np.inf])
def test_nonfinite_geometry_rejected(tmp_path, tetrahedron, value):
    vertices, faces = tetrahedron
    vertices[0, 0] = value
    with pytest.raises(ValueError, match="finite"):
        GeometryValidator(str(tmp_path))._compute_mesh_statistics(
            "synthetic", "obj", vertices, faces
        )


def test_ascii_stl_partial_triangle_rejected(tmp_path):
    source = tmp_path / "synthetic.stl"
    source.write_text("vertex 1 0 0\n", encoding="utf-8")
    with pytest.raises(ValueError, match="Incomplete"):
        GeometryValidator(str(tmp_path)).validate_file(source, "stl_ascii")


def test_binary_stl_missing_attribute_rejected(tmp_path):
    import struct

    # Record sizes/encoding come from _validate_stl_binary's existing reader.
    source = tmp_path / "synthetic.stl"
    source.write_bytes(bytes(80) + struct.pack("<I", 1) + bytes(48))
    with pytest.raises(ValueError, match="Truncated"):
        GeometryValidator(str(tmp_path)).validate_file(source, "stl_binary")


@pytest.mark.parametrize("parallel", [False, True])
@pytest.mark.parametrize("fail", [False, True])
def test_geometry_results_are_transactional(tmp_path, monkeypatch, parallel, fail):
    module = sys.modules[GeometryValidator.__module__]
    validator = GeometryValidator(str(tmp_path))
    validator.results["stale"] = object()
    calls = []

    # Exercise the legacy request set without duplicating identifying filenames.
    monkeypatch.setattr(Path, "is_file", lambda path: True)

    def parse(path, kind):
        calls.append(kind)
        if fail and kind == "obj":
            raise ValueError("synthetic parse failure")
        return kind

    class Future:
        def __init__(self, function, args):
            self.function, self.args = function, args

        def result(self):
            return self.function(*self.args)

    class Pool:
        def __init__(self, **kwargs):
            pass

        def __enter__(self):
            return self

        def __exit__(self, *args):
            return False

        def submit(self, function, *args):
            return Future(function, args)

    monkeypatch.setattr(validator, "validate_file", parse)
    monkeypatch.setattr(module, "ProcessPoolExecutor", Pool)
    monkeypatch.setattr(module, "as_completed", lambda futures: iter(futures))
    if fail:
        with pytest.raises(ValueError, match="synthetic parse failure"):
            validator.validate_all(parallel=parallel)
        assert validator.results == {}
        assert calls == ["stl_ascii", "stl_binary", "obj"]
    else:
        results = validator.validate_all(parallel=parallel)
        assert set(results.values()) == {"stl_ascii", "stl_binary", "obj"}
        assert "stale" not in results


@pytest.mark.parametrize("parallel", [False, True])
def test_partial_geometry_request_refused_before_parsing(tmp_path, monkeypatch, parallel):
    validator = GeometryValidator(str(tmp_path))
    monkeypatch.setattr(Path, "is_file", lambda path: path.suffix == ".obj")
    monkeypatch.setattr(validator, "validate_file",
                        lambda *args: pytest.fail("partial request was parsed"))
    with pytest.raises(FileNotFoundError, match="Missing requested geometry"):
        validator.validate_all(parallel=parallel)
    assert validator.results == {}


def test_collinear_face_rejects_closed_connectivity(tmp_path, tetrahedron):
    vertices, faces = tetrahedron
    # Synthetic midpoint of the fixture's basis vectors; no physical tolerance.
    vertices[-1] = vertices[1:3].mean(axis=0)
    edges = Counter(tuple(sorted((face[i], face[(i + 1) % 3])))
                    for face in faces for i in range(3))
    assert set(edges.values()) == {2}
    assert len(np.unique(vertices, axis=0)) == len(vertices)
    stats = GeometryValidator(str(tmp_path))._compute_mesh_statistics(
        "synthetic", "obj", vertices, faces
    )
    assert stats.min_panel_area == 0
    assert not stats.is_watertight
