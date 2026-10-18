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


@pytest.mark.parametrize("value", [0.0, 7.25])
def test_nonempty_csv_values_are_preserved(tmp_path, value):
    """Nonempty legacy input is readable; this does not qualify missing channels."""
    (tmp_path / "frequencies.csv").write_text("frequency\n0.5\n", encoding="utf-8")
    (tmp_path / "directions.csv").write_text("direction\n30\n", encoding="utf-8")
    (tmp_path / "added_mass_11.csv").write_text(f"value\n{value}\n", encoding="utf-8")
    result = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))._load_from_csv(tmp_path)
    np.testing.assert_array_equal(result.frequencies, [0.5])
    np.testing.assert_array_equal(result.directions, [30])
    assert result.added_mass[0, 0, 0] == value


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


@pytest.mark.parametrize("source", ["hdf5", "excel"])
def test_format_selection_preserved(tmp_path, monkeypatch, source):
    converter = OrcaFlexConverter(str(tmp_path), str(tmp_path / "out"))
    expected = object()
    if source == "hdf5":
        (tmp_path / "results.h5").touch()
        (tmp_path / "csv_outputs").mkdir()
    (tmp_path / "results.xlsx").touch()
    monkeypatch.setattr(converter, f"_load_from_{source}", lambda path: expected)
    assert converter.load_orcawave_results() is expected


@pytest.mark.xfail(
    strict=True, raises=AssertionError,
    reason="https://github.com/vamseeachanta/digitalmodel/issues/2738: G4-CLOSURE",
)
def test_g4_closure_rejects_open_triangle(tmp_path):
    vertices = np.eye(3)
    faces = np.array([[0, 1, 2]])
    edges = Counter(tuple(sorted(edge)) for edge in [(0, 1), (1, 2), (2, 0)])
    assert sum(count == 1 for count in edges.values()) == 3
    stats = GeometryValidator(str(tmp_path))._compute_mesh_statistics(
        "synthetic.obj", "obj", vertices, faces
    )
    assert stats.volume == pytest.approx(1 / 6)
    assert not stats.is_watertight


@pytest.mark.xfail(
    strict=True, raises=AssertionError,
    reason="https://github.com/vamseeachanta/digitalmodel/issues/2738: G5-STATE",
)
def test_g5_state_refuses_incomplete_results(workflow):
    assert workflow._check_data_completeness() is False
    success = workflow._phase4_quality_assurance()
    assert (success, workflow.workflow_state["quality_checked"]) == (False, False)
