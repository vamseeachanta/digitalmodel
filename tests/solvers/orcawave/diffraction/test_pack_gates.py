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


@pytest.mark.xfail(
    strict=True, raises=pytest.fail.Exception,
    reason="https://github.com/vamseeachanta/digitalmodel/issues/2738: G0-GEOMETRY",
)
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


@pytest.mark.xfail(strict=True, raises=AssertionError, reason="#2738: G4-CLOSURE")
def test_g4_closure_rejects_open_triangle(open_triangle_stats):
    assert not open_triangle_stats.is_watertight


def test_g5_incomplete_results_precondition(workflow):
    assert workflow._check_data_completeness() is False


@pytest.mark.xfail(
    strict=True, raises=AssertionError,
    reason="#2738: G5-STATE",
)
def test_g5_state_refuses_incomplete_results(workflow):
    success = workflow._phase4_quality_assurance()
    assert (success, workflow.workflow_state["quality_checked"]) == (False, False)
