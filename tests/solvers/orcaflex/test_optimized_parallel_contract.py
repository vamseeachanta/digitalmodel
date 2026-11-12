# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Optimized worker contracts from issue #2521 (2026).

Expected statuses and empty output lists follow the issue's refusal contract:
without an interface and without explicit opt-in, no solve or output exists.
All interface imports are intercepted before the module loads; no solver is
imported, discovered or constructed by these tests.
"""

import importlib.util
import sys
from pathlib import Path

import pytest


@pytest.fixture
def runner(monkeypatch, capsys):
    """Load the real worker with the interface made unavailable in isolation."""
    monkeypatch.setitem(sys.modules, "OrcFxAPI", None)
    source = (
        Path(__file__).resolve().parents[3]
        / "src/digitalmodel/solvers/orcaflex/orcaflex_optimized_parallel_v2.py"
    )
    spec = importlib.util.spec_from_file_location("optimized_contract_runner", source)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    module.import_output = capsys.readouterr().out
    return module


@pytest.mark.parametrize("config", [{}, {"mock_mode": False}])
def test_unavailable_interface_refuses_implicit_mock(runner, tmp_path, config):
    """Issue #2521: no native run means no success or output to report."""
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")

    result = runner.process_single_file_worker(
        {"file_path": str(model), "config": config}
    )

    assert result["status"] == "failed"
    assert "OrcFxAPI is unavailable" in result["error"]
    assert result["output_files"] == []
    assert result["end_time"] is not None
    assert result["duration"] is not None
    assert list(tmp_path.iterdir()) == [model]


def test_import_warning_does_not_promise_implicit_mock(runner):
    """Import-time diagnostics must agree with the issue #2521 refusal."""
    assert "Running in mock mode" not in runner.import_output


@pytest.mark.parametrize("available", [False, True])
@pytest.mark.parametrize(
    ("save_sim", "save_dat", "names"),
    [
        (True, False, ["model.mock.sim"]),
        (False, True, ["model_processed.mock.dat"]),
        (True, True, ["model.mock.sim", "model_processed.mock.dat"]),
        (False, False, []),
    ],
)
def test_explicit_mock_writes_only_marked_artifacts(
    runner, monkeypatch, tmp_path, available, save_sim, save_dat, names
):
    """Issue #2521 and core/mock_artifacts.py define non-native provenance.

    Expected filenames are constructed by hand: append .mock before the suffix,
    put outputs in out_mock, and retain the established in-band marker. No
    engineering quantity is evaluated. A forced mock must also bypass an
    available interface, which has no Model attribute in this double.
    """
    from types import SimpleNamespace

    monkeypatch.setattr(runner, "ORCAFLEX_AVAILABLE", available)
    monkeypatch.setattr(runner, "OrcFxAPI", SimpleNamespace(), raising=False)
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")
    native_dir = tmp_path / "out"

    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {
                "mock_mode": True,
                "save_sim": save_sim,
                "save_dat": save_dat,
                "output_dir": str(native_dir),
            },
        }
    )

    assert result["status"] == "mock_success"
    assert result["is_mock"] is True
    assert result["error"] is None
    paths = [Path(path) for path in result["output_files"]]
    assert [path.name for path in paths] == names
    for path in paths:
        assert path.is_file()
        assert path.parent == tmp_path / "out_mock"
        assert path.read_text().startswith("Mock simulation for model.dat")
    assert not native_dir.exists()
    assert model.read_text() == "regression input\n"


def test_explicit_mock_defaults_to_input_directory_sibling(runner, tmp_path):
    """The established artifact contract uses a marked sibling directory."""
    model = tmp_path / "inputs" / "model.dat"
    model.parent.mkdir()
    model.write_text("regression input\n")

    result = runner.process_single_file_worker(
        {"file_path": str(model), "config": {"mock_mode": True}}
    )

    assert result["output_files"] == [
        str(tmp_path / "inputs_mock" / "model.mock.sim")
    ]
    assert Path(result["output_files"][0]).is_file()


@pytest.mark.parametrize("mock_mode", ["true", "false", 1, None])
def test_mock_request_requires_boolean(runner, tmp_path, mock_mode):
    """Issue #2521 requires explicit opt-in; truthy coercion is ambiguous."""
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")

    result = runner.process_single_file_worker(
        {"file_path": str(model), "config": {"mock_mode": mock_mode}}
    )

    assert result["status"] == "failed"
    assert "mock_mode must be a boolean" in result["error"]
    assert result["output_files"] == []


def test_mock_with_missing_input_does_not_claim_completion(runner, tmp_path):
    """No input means no completed regression case, per issue #2521."""
    result = runner.process_single_file_worker(
        {"file_path": str(tmp_path / "missing.dat"), "config": {"mock_mode": True}}
    )

    assert result["status"] == "failed"
    assert result["error"]
    assert result["output_files"] == []
    assert list(tmp_path.iterdir()) == []


def test_mock_write_failure_has_no_phantom_output(runner, tmp_path):
    """Issue #2521: paths are reported only after a file has been written."""
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")
    (tmp_path / "out_mock").write_text("blocks artifact directory creation\n")

    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {"mock_mode": True, "output_dir": str(tmp_path / "out")},
        }
    )

    assert result["status"] == "failed"
    assert result["is_mock"] is True
    assert result["error"]
    assert result["output_files"] == []


@pytest.fixture
def native_double(runner, monkeypatch):
    """Provide only a synthetic interface; a licensed binding is never loaded."""
    from types import SimpleNamespace

    class ModelDouble:
        state = "Reset"
        write_sim = True
        write_dat = True

        def __init__(self):
            self.calls = []

        def LoadData(self, path):
            Path(path).read_text()
            self.calls.append("load")

        def CalculateStatics(self):
            self.calls.append("statics")
            self.state = "InStaticState"

        def RunSimulation(self):
            self.calls.append("dynamics")
            self.state = "SimulationStopped"

        def SaveSimulation(self, path):
            self.calls.append("save_sim")
            if self.write_sim:
                Path(path).write_text("synthetic interface output\n")

        def SaveData(self, path):
            self.calls.append("save_dat")
            if self.write_dat:
                Path(path).write_text("synthetic interface output\n")

    model = ModelDouble()
    monkeypatch.setattr(runner, "ORCAFLEX_AVAILABLE", True)
    monkeypatch.setattr(
        runner, "OrcFxAPI", SimpleNamespace(Model=lambda: model), raising=False
    )
    return model


def test_native_double_preserves_analysis_and_save_order(runner, native_double, tmp_path):
    """Existing worker order is load, statics, dynamics, simulation, data.

    The expected states come from run_state.py's named-state contract. The
    output names follow the worker's existing stem and _processed naming;
    synthetic file contents are supplied entirely by the interface double.
    """
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")
    output = tmp_path / "out"

    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {"dynamic": True, "save_dat": True, "output_dir": str(output)},
        }
    )

    assert result["status"] == "success"
    assert result["is_mock"] is False
    assert result["error"] is None
    assert native_double.calls == [
        "load", "statics", "dynamics", "save_sim", "save_dat"
    ]
    assert result["output_files"] == [
        str(output / "model.sim"), str(output / "model_processed.dat")
    ]
    assert all(Path(path).is_file() for path in result["output_files"])


@pytest.mark.parametrize("save_kind", ["sim", "dat"])
def test_native_save_must_create_reported_file(
    runner, native_double, tmp_path, save_kind
):
    """Issue #2521: a save returning without a file proves no output exists."""
    setattr(native_double, f"write_{save_kind}", False)
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")

    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {
                "save_sim": save_kind == "sim",
                "save_dat": save_kind == "dat",
                "output_dir": str(tmp_path / "out"),
            },
        }
    )

    assert result["status"] == "failed"
    assert "did not create" in result["error"]
    assert result["output_files"] == []
    assert result["is_mock"] is False


def test_failed_second_save_reports_only_existing_first_output(
    runner, native_double, tmp_path
):
    """Issue #2521: a partial failure can retain only the file actually saved."""
    native_double.write_dat = False
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")
    output = tmp_path / "out"

    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {"save_dat": True, "output_dir": str(output)},
        }
    )

    assert result["status"] == "failed"
    assert result["output_files"] == [str(output / "model.sim")]
    assert Path(result["output_files"][0]).is_file()
    assert not (output / "model_processed.dat").exists()


@pytest.mark.parametrize("mode", ["unavailable", "mock", "native_double"])
def test_batch_counts_only_native_success_as_successful(
    runner, monkeypatch, tmp_path, request, mode
):
    """Hand count of two inputs, per issue #2521's native/mock distinction.

    With no interface: zero native successes, zero mocks, two failures.
    With explicit mock: zero native successes, two mocks, zero failures.
    With the native double: two native successes, zero mocks, zero failures.
    The single worker is a test resource choice, not an engineering coefficient.
    """
    if mode == "native_double":
        request.getfixturevalue("native_double")
    monkeypatch.setattr(
        runner.SimpleResourceManager, "calculate_optimal_threads", lambda files: 1
    )
    files = []
    for name in ("first.dat", "second.dat"):
        path = tmp_path / name
        path.write_text("regression input\n")
        files.append(str(path))
    analysis = runner.OrcaFlexOptimizedParallelAnalysis(
        num_threads=1, use_processes=False
    )

    summary = analysis.process_files_parallel(
        files, {"mock_mode": mode == "mock", "output_dir": str(tmp_path / "out")}
    )

    assert summary["total_files"] == 2
    assert summary["successful"] == (2 if mode == "native_double" else 0)
    assert summary["mock_successful"] == (2 if mode == "mock" else 0)
    assert summary["failed"] == (2 if mode == "unavailable" else 0)
    assert len(summary["results"]) == 2
    assert sorted(analysis.failed_files) == (sorted(files) if mode == "unavailable" else [])
    assert all(
        Path(path).is_file()
        for result in summary["results"]
        for path in result["output_files"]
    )


def test_refusal_does_not_adopt_or_modify_existing_native_outputs(runner, tmp_path):
    """Issue #2521: an older file cannot establish a completion for this call.

    Hand calculation: one unavailable-interface request produces one failure,
    no successes and no output paths, even when both native filenames exist.
    """
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")
    output = tmp_path / "out"
    output.mkdir()
    previous = [output / "model.sim", output / "model_processed.dat"]
    for path in previous:
        path.write_bytes(b"previous output\n")

    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {"save_dat": True, "output_dir": str(output)},
        }
    )

    assert result["status"] == "failed"
    assert result["is_mock"] is False
    assert result["output_files"] == []
    assert all(path.read_bytes() == b"previous output\n" for path in previous)
    assert model.read_text() == "regression input\n"


def test_mock_preserves_existing_native_outputs(runner, tmp_path):
    """The repository mock-artifact contract isolates both output save types."""
    model = tmp_path / "model.dat"
    model.write_text("regression input\n")
    output = tmp_path / "out"
    output.mkdir()
    previous = [output / "model.sim", output / "model_processed.dat"]
    for path in previous:
        path.write_bytes(b"previous output\n")

    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {
                "mock_mode": True, "save_dat": True, "output_dir": str(output)
            },
        }
    )

    assert result["status"] == "mock_success"
    assert result["is_mock"] is True
    assert result["output_files"] == [
        str(tmp_path / "out_mock" / "model.mock.sim"),
        str(tmp_path / "out_mock" / "model_processed.mock.dat"),
    ]
    assert all(path.read_bytes() == b"previous output\n" for path in previous)
    assert all(Path(path).is_file() for path in result["output_files"])
    assert model.read_text() == "regression input\n"


def test_failed_second_mock_write_reports_only_first_artifact(
    runner, monkeypatch, tmp_path
):
    """Issue #2521: one completed write plus one failed write yields one path.

    Expected paths follow core/mock_artifacts.py; the injected second-write
    failure is independent of the worker and does not emulate a solver.
    """
    from digitalmodel.solvers.orcaflex.core import mock_artifacts

    model = tmp_path / "model.dat"
    model.write_text("regression input\n")
    writer = mock_artifacts.write_mock_artifact

    def fail_data_write(path, name, detail):
        if path.suffix == ".dat":
            raise OSError("injected data artifact write failure")
        return writer(path, name, detail)

    monkeypatch.setattr(mock_artifacts, "write_mock_artifact", fail_data_write)
    result = runner.process_single_file_worker(
        {
            "file_path": str(model),
            "config": {
                "mock_mode": True,
                "save_dat": True,
                "output_dir": str(tmp_path / "out"),
            },
        }
    )

    artifact = tmp_path / "out_mock" / "model.mock.sim"
    assert result["status"] == "failed"
    assert result["is_mock"] is True
    assert "injected data artifact write failure" in result["error"]
    assert result["output_files"] == [str(artifact)]
    assert artifact.read_text().startswith("Mock simulation for model.dat")
    assert not (artifact.parent / "model_processed.mock.dat").exists()


def test_worked_example_matches_independent_hand_count(tmp_path):
    """Issue #2521 (2026): hand-count the example's two synthetic inputs.

    Without opt-in, two unavailable requests give (native, mock, failed) =
    (0, 0, 2). With opt-in, the same two requests give (0, 2, 0); saving both
    types produces 2 inputs * 2 artifacts = 4 marked files. The CLI blocks
    OrcFxAPI before any worker import and performs no numerical analysis.
    """
    import json
    import os
    import subprocess

    root = Path(__file__).resolve().parents[3]
    example = root / "examples/orcaflex/optimized_worker_contract.py"
    work_dir = tmp_path / "walkthrough"
    env = dict(os.environ, PYTHONPATH=str(root / "src"))
    completed = subprocess.run(
        [sys.executable, str(example), str(work_dir)],
        capture_output=True, text=True, env=env, check=True,
    )
    payload = next(
        line.removeprefix("RESULT: ")
        for line in completed.stdout.splitlines()
        if line.startswith("RESULT: ")
    )
    result = json.loads(payload)

    assert result["refused"] == {"successful": 0, "mock_successful": 0, "failed": 2}
    assert result["regression"] == {"successful": 0, "mock_successful": 2, "failed": 0}
    assert result["artifacts"] == [
        "results_mock/first.mock.sim", "results_mock/first_processed.mock.dat",
        "results_mock/second.mock.sim", "results_mock/second_processed.mock.dat",
    ]
    for relative in result["artifacts"]:
        artifact = work_dir / relative
        assert artifact.is_file()
        assert artifact.read_text().startswith("Mock simulation for ")
    assert not (work_dir / "results").exists()
    for name in ("first", "second"):
        assert (work_dir / f"{name}.dat").read_text() == (
            "Synthetic regression input; not a native model.\n"
        )


def test_worked_example_documented_answer():
    """The HTML answer is the independent hand count from issue #2521.

    Two refused requests give two failures and no outputs. Two explicit mock
    requests each save a simulation and data artifact, giving four files.
    """
    import json
    import xml.etree.ElementTree as element_tree

    root = Path(__file__).resolve().parents[3]
    page = element_tree.parse(
        root / "docs/domains/orcaflex/optimized-worker-output-contract.html"
    )
    answer = json.loads(page.find(".//pre[@id='expected-answer']").text)

    assert answer == {
        "refused": {"successful": 0, "mock_successful": 0, "failed": 2},
        "regression": {"successful": 0, "mock_successful": 2, "failed": 0},
        "artifacts": [
            "results_mock/first.mock.sim", "results_mock/first_processed.mock.dat",
            "results_mock/second.mock.sim", "results_mock/second_processed.mock.dat",
        ],
    }


def test_example_refuses_existing_directory_without_overwriting(tmp_path):
    """A walkthrough must preserve existing data rather than reuse a case."""
    import os
    import subprocess

    root = Path(__file__).resolve().parents[3]
    model = tmp_path / "first.dat"
    model.write_text("existing input\n")
    completed = subprocess.run(
        [
            sys.executable, str(root / "examples/orcaflex/optimized_worker_contract.py"),
            str(tmp_path),
        ],
        capture_output=True, text=True,
        env=dict(os.environ, PYTHONPATH=str(root / "src")),
    )

    assert completed.returncode != 0
    assert "FileExistsError" in completed.stderr
    assert "RESULT: " not in completed.stdout
    assert model.read_text() == "existing input\n"
    assert list(tmp_path.iterdir()) == [model]


def test_example_refuses_a_preloaded_native_worker(monkeypatch, tmp_path):
    """An imported example must never reuse a cached native-enabled worker."""
    from types import SimpleNamespace

    root = Path(__file__).resolve().parents[3]
    spec = importlib.util.spec_from_file_location(
        "contract_example", root / "examples/orcaflex/optimized_worker_contract.py"
    )
    example = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(example)

    def forbidden_native_call(*args, **kwargs):
        pytest.fail("A native-enabled worker must not be constructed")

    monkeypatch.setitem(
        sys.modules,
        "digitalmodel.solvers.orcaflex.orcaflex_optimized_parallel_v2",
        SimpleNamespace(
            ORCAFLEX_AVAILABLE=True,
            OrcaFlexOptimizedParallelAnalysis=forbidden_native_call,
        ),
    )
    with pytest.raises(RuntimeError, match="isolated interpreter"):
        example.run_example(tmp_path / "walkthrough")
    assert list(tmp_path.iterdir()) == []
