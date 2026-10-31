"""Coefficient transport regressions; no engineering qualification is implied."""

import io
import json
import os
from pathlib import Path
import subprocess
import sys

import pytest


WORKFLOW = (
    "digitalmodel.marine_ops.marine_engineering.mooring_analysis.fpso_full_workflow"
)


def test_first_response_does_not_write(tmp_path):
    """Issue 2442 requires an empty output set, independent of response magnitude.

    The input is the existing synthetic grid knot in test_atlas.py's
    test_fpso_value_atlas; that unchanged test checks numerical regression.
    """
    work = tmp_path / "working"
    work.mkdir()
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_COMMON_DIR"):
        env.pop(key, None)
    env["PYTHONDONTWRITEBYTECODE"] = "1"
    env["MPLCONFIGDIR"] = str(tmp_path / "plot-config")
    env["PYTHONPATH"] = os.pathsep.join(str(path) for path in sys.path if path)
    script = """
import math
import os
from pathlib import Path
import sys
from digitalmodel.parametric.generate import RESPONSE_FUNCS
# Load dependencies before observing the first computation, not just imports.
from digitalmodel.marine_ops.marine_engineering.mooring_analysis import fpso_full_workflow
from io import StringIO

def reject_write(event, args):
    if event == "open":
        mode, flags = args[1:3]
        assert not (mode and any(char in mode for char in "wax+")), event
        assert not (flags & (os.O_WRONLY | os.O_RDWR | os.O_CREAT | os.O_TRUNC)), event
    assert event not in {"os.mkdir", "os.remove", "os.rename", "os.rmdir"}, event

assert list(Path.cwd().iterdir()) == []
# A compute-only response must also work when stdout has been closed.
sys.stdout = StringIO()
sys.stdout.close()
sys.addaudithook(reject_write)
value = RESPONSE_FUNCS["fpso_mooring_full"](
    {"Hs": 4.0, "Tp": 11.0, "water_depth_m": 1000.0}
)
assert math.isfinite(value)
assert list(Path.cwd().iterdir()) == []
"""
    result = subprocess.run(
        [sys.executable, "-c", script],
        cwd=work,
        env=env,
        capture_output=True,
        text=True,
        check=False,
    )
    assert result.returncode == 0, result.stdout + result.stderr
    assert list(work.iterdir()) == []


@pytest.mark.parametrize("source", ["stream", "path"])
def test_database_accepts_csv(tmp_path, source):
    """Hand calculation: a singleton table returns its literal coefficients.

    The dimensionless test vector (1, 2, 3, 4, 5, 6) is arbitrary, not a
    published design coefficient set. No interpolation or force model is needed.
    """
    from digitalmodel.marine_ops.marine_engineering.environmental_loading import (
        OCIMFDatabase,
    )

    csv = "displacement,heading,CXw,CYw,CMw,CXc,CYc,CMc\n1,0,1,2,3,4,5,6\n"
    stream = io.StringIO(csv)
    path = tmp_path / "coefficients.csv"
    if source == "path":
        path.write_text(csv, encoding="utf-8")
    database = OCIMFDatabase(stream if source == "stream" else str(path))
    assert database.csv_path == (None if source == "stream" else path)
    coefficients = database.get_coefficients(heading=0, displacement=1)
    assert [
        getattr(coefficients, key) for key in ("CXw", "CYw", "CMw", "CXc", "CYc", "CMc")
    ] == [1, 2, 3, 4, 5, 6]
    assert not stream.closed


def test_stream_database_validates_columns():
    """Missing required headings must fail just as they do for a file input."""
    from digitalmodel.marine_ops.marine_engineering.environmental_loading import (
        OCIMFDatabase,
    )

    with pytest.raises(ValueError, match="Missing required columns"):
        OCIMFDatabase(io.StringIO("unrelated\ntext\n"))


@pytest.mark.parametrize("location", ["analysis", "config"])
def test_explicit_directory_preserves_sample_and_force_values(
    tmp_path,
    monkeypatch,
    location,
):
    """Transport identity: identical CSV bytes give identical force results.

    This checks storage behavior, not the engineering validity of the synthetic
    sample. Inputs are reused from generate.py's reference demonstration case.
    """
    from importlib import import_module

    from digitalmodel.parametric.generate import _FPSO_REF_ENV, _FPSO_REF_VESSEL

    workflow = import_module(WORKFLOW).FPSOMooringFullWorkflow()
    environment = dict(_FPSO_REF_ENV)
    # The wave contribution is zero by definition for the supplied zero moment.
    spectrum = {"m0": 0.0}
    work = tmp_path / "working"
    work.mkdir()
    monkeypatch.chdir(work)
    output = tmp_path / "chosen"
    cfg = (
        {"Analysis": {"result_folder": str(output)}}
        if location == "analysis"
        else {"_config_dir_path": str(output)}
    )
    expected_folder = output if location == "analysis" else output / "results"
    implicit = workflow._environmental_forces({}, _FPSO_REF_VESSEL, environment, spectrum)
    assert list(work.iterdir()) == []
    explicit = workflow._environmental_forces(cfg, _FPSO_REF_VESSEL, environment, spectrum)
    assert explicit == implicit
    sample = expected_folder / "ocimf_coefficients_sample.csv"
    before = sample.read_bytes()
    assert (
        workflow._environmental_forces(cfg, _FPSO_REF_VESSEL, environment, spectrum)
        == explicit
    )
    assert sample.read_bytes() == before
    assert list(expected_folder.iterdir()) == [sample]


def test_compute_only_sample_is_silent(capsys):
    from importlib import import_module

    workflow = import_module(WORKFLOW).FPSOMooringFullWorkflow()
    capsys.readouterr()
    workflow._ocimf_database({})
    workflow._ocimf_database({})
    assert capsys.readouterr().out == ""


@pytest.mark.parametrize("destination", ["stream", "path"])
def test_sample_generation_can_be_silent(tmp_path, capsys, destination):
    from digitalmodel.marine_ops.marine_engineering.environmental_loading import (
        create_sample_database,
    )

    target = io.StringIO() if destination == "stream" else tmp_path / "sample.csv"
    capsys.readouterr()
    frame = create_sample_database(target, verbose=False)
    assert capsys.readouterr().out == ""
    if destination == "stream":
        assert not target.closed
        contents = target.getvalue()
    else:
        contents = target.read_bytes().decode("utf-8")
    assert contents == frame.to_csv(index=False)


def test_sample_path_output_keeps_creation_message(tmp_path, capsys):
    from digitalmodel.marine_ops.marine_engineering.environmental_loading import (
        create_sample_database,
    )

    target = tmp_path / "sample.csv"
    capsys.readouterr()
    frame = create_sample_database(target)
    assert capsys.readouterr().out == (
        f"Created sample OCIMF database: {target}\n"
        "  Vessels: 5\n"
        f"  Total entries: {len(frame)}\n"
    )


@pytest.mark.parametrize(
    "cfg,expected",
    [
        ({}, None),
        ({"Analysis": {}}, None),
        ({"Analysis": {"result_folder": ""}, "_config_dir_path": ""}, None),
        ({"Analysis": {"result_folder": "chosen"}}, Path("chosen")),
        ({"_config_dir_path": "config"}, Path("config/results")),
        (
            {"Analysis": {"result_folder": ""}, "_config_dir_path": "config"},
            Path("config/results"),
        ),
        (
            {"Analysis": {"result_folder": Path("chosen")}, "_config_dir_path": "config"},
            Path("chosen"),
        ),
    ],
)
def test_configured_result_folder_matches_output_routing(cfg, expected):
    from digitalmodel.marine_ops.marine_engineering.mooring_analysis.fpso_full_output import (
        configured_result_folder,
        result_folder,
    )

    assert configured_result_folder(cfg) == expected
    assert result_folder(cfg) == (expected if expected is not None else Path("results"))


def test_unconfigured_sample_ignores_existing_working_directory_csv(tmp_path, monkeypatch):
    from importlib import import_module

    workflow = import_module(WORKFLOW).FPSOMooringFullWorkflow()
    expected = workflow._ocimf_database({}).data.to_csv(index=False)
    sample = tmp_path / "results" / "ocimf_coefficients_sample.csv"
    sample.parent.mkdir()
    original = "unrelated\nignored\n"
    sample.write_text(original, encoding="utf-8")
    monkeypatch.chdir(tmp_path)
    assert workflow._ocimf_database({}).data.to_csv(index=False) == expected
    assert sample.read_text(encoding="utf-8") == original


def test_explicit_router_writes_configured_outputs(tmp_path):
    """The shipped example retains its configured output contract.

    Inputs come from examples/workflows/fpso-mooring-full/input.yml. The
    expected summary equals the returned result, and the line count equals
    the configured count; no independent engineering accuracy is asserted.
    """
    from importlib import import_module

    import yaml

    source = Path(__file__).resolve().parents[2] / (
        "examples/workflows/fpso-mooring-full/input.yml"
    )
    cfg = yaml.safe_load(source.read_text(encoding="utf-8"))
    cfg["Analysis"] = {"result_folder": str(tmp_path)}
    summary = tmp_path / "summary.json"
    tensions = tmp_path / "tensions.csv"
    cfg["output"] = {"summary_json": str(summary), "line_tensions_csv": str(tensions)}
    workflow = import_module(WORKFLOW).FPSOMooringFullWorkflow()
    result = workflow.router(cfg)["fpso_mooring_full"]
    assert json.loads(summary.read_text(encoding="utf-8")) == result
    assert len(result["line_tensions"]) == cfg["mooring"]["n_lines"]
    assert {path.name for path in tmp_path.iterdir()} == {
        "summary.json",
        "tensions.csv",
        "ocimf_coefficients_sample.csv",
    }


def test_existing_configured_sample_is_used(tmp_path):
    """An existing singleton CSV supplies its literal coefficients unchanged.

    As in test_database_accepts_csv, the dimensionless vector is arbitrary
    test data; a singleton lookup returns (1, 2, 3, 4, 5, 6) by definition.
    """
    from importlib import import_module

    from digitalmodel.parametric.generate import _FPSO_REF_ENV, _FPSO_REF_VESSEL

    sample = tmp_path / "ocimf_coefficients_sample.csv"
    sample.write_text(
        "displacement,heading,CXw,CYw,CMw,CXc,CYc,CMc\n1,0,1,2,3,4,5,6\n",
        encoding="utf-8",
    )
    before = sample.read_bytes()
    workflow = import_module(WORKFLOW).FPSOMooringFullWorkflow()
    environment = {**_FPSO_REF_ENV, "wind_heading": 0, "current_heading": 0}
    vessel = {**_FPSO_REF_VESSEL, "displacement": 1}
    forces = workflow._environmental_forces(
        {"Analysis": {"result_folder": str(tmp_path)}},
        vessel,
        environment,
        {"m0": 0.0},
    )
    assert [
        forces["coefficients"][key]
        for key in ("CXw", "CYw", "CMw", "CXc", "CYc", "CMc")
    ] == [1, 2, 3, 4, 5, 6]
    assert sample.read_bytes() == before
