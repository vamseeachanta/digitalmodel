# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Characterize the existing CLI before extracting its synthetic single-case seam."""

import importlib
import json
from dataclasses import replace
from pathlib import Path
from types import SimpleNamespace

import pytest
from click.testing import CliRunner

from digitalmodel.subsea.catenary_riser.cli import cli

CASES = json.loads((Path(__file__).parent / "fixtures/simple_cli.json").read_text())["cases"]


@pytest.mark.parametrize("case", CASES, ids=lambda c: c["name"])
def test_cli_characterization(case, tmp_path):
    output = tmp_path / "result.json"
    run = CliRunner().invoke(cli, case["args"] + ["--output", str(output)])
    assert run.exit_code == 0, run.output
    assert run.stdout == case["stdout"] + f"\nResults saved to: {output}\n"
    assert json.loads(output.read_text()) == case["output"]


@pytest.mark.parametrize("options,message", [
    (["--thickness", "0.06"], "wall_thickness"),
    (["--coating-thickness", "0.002"], "coating_density"),
    (["--length", "3", "--top-tension", "1000"],
     "catenary did not converge or exceeds available length"),
])
def test_cli_characterization_errors(options, message, tmp_path):
    output = tmp_path / "result.json"
    run = CliRunner().invoke(cli, CASES[0]["args"] + options + ["--output", str(output)])
    assert run.exit_code == 1
    assert message in run.output
    assert not output.exists()


@pytest.fixture
def workflow():
    return importlib.import_module("digitalmodel.subsea.catenary_riser.workflow")


@pytest.mark.parametrize("case", CASES, ids=lambda c: c["name"])
def test_service_delegates_once_and_preserves_raw_result(workflow, case, monkeypatch):
    original = workflow.SimpleCatenaryAnalyzer.analyze_riser
    calls = []

    def spy(self, riser, **controls):
        raw = original(self, riser, **controls)
        calls.append((riser, controls, raw))
        return raw

    monkeypatch.setattr(workflow.SimpleCatenaryAnalyzer, "analyze_riser", spy)
    request = workflow.SimpleCatenaryRequest(**case["inputs"])
    outcome = workflow.analyze_simple(request)
    assert len(calls) == 1
    riser, controls, raw = calls[0]
    expected = ({"top_tension": request.top_tension} if request.top_tension is not None
                else {"water_depth": request.water_depth, "horizontal_offset": request.offset})
    assert controls == expected
    assert (riser.name, riser.outer_diameter, riser.wall_thickness, riser.length) == (
        "Riser1", request.diameter, request.thickness, request.length)
    assert (riser.water_depth, riser.horizontal_offset) == (request.water_depth, request.offset)
    assert riser.material == workflow.get_material(request.material)
    assert riser.internal_fluid == workflow.get_fluid(request.internal_fluid)
    assert (riser.coating_thickness, riser.coating_density) == (
        request.coating_thickness, request.coating_density)
    assert riser.gravity == 9.81 and riser.external_fluid.density == 1025
    assert outcome.result is raw
    assert raw.arc_length != round(raw.arc_length, 2)
    assert outcome.presentation == case["output"]


@pytest.mark.parametrize("changes,message", [
    ({"thickness": .06}, "wall_thickness"),
    ({"coating_thickness": .002}, "coating_density"),
    ({"length": 3, "top_tension": 1000}, "catenary did not converge"),
])
def test_service_preserves_validation(workflow, changes, message):
    request = workflow.SimpleCatenaryRequest(**CASES[0]["inputs"])
    with pytest.raises(ValueError, match=message):
        workflow.analyze_simple(replace(request, **changes))


def test_service_defaults_match_cli(workflow, tmp_path):
    inputs = {k: v for k, v in CASES[0]["inputs"].items()
              if k not in ("material", "internal_fluid")}
    assert workflow.analyze_simple(workflow.SimpleCatenaryRequest(**inputs)).presentation == CASES[0]["output"]
    output = tmp_path / "default.json"
    run = CliRunner().invoke(cli, CASES[0]["args"][:-4] + ["--output", str(output)])
    assert run.exit_code == 0
    assert json.loads(output.read_text()) == CASES[0]["output"]


def test_unconverged_refuses_output(workflow, monkeypatch, tmp_path):
    monkeypatch.setattr(workflow.SimpleCatenaryAnalyzer, "analyze_riser",
                        lambda *args, **kwargs: SimpleNamespace(converged=False))
    with pytest.raises(ValueError, match="catenary did not converge or exceeds available length"):
        workflow.analyze_simple(workflow.SimpleCatenaryRequest(**CASES[0]["inputs"]))
    output = tmp_path / "result.json"
    run = CliRunner().invoke(cli, CASES[0]["args"] + ["--output", str(output)])
    assert run.exit_code == 1
    assert "catenary did not converge or exceeds available length" in run.output
    assert not output.exists()


def test_cli_delegates_to_service(workflow, monkeypatch):
    module = importlib.import_module("digitalmodel.subsea.catenary_riser.cli")
    calls = []

    def spy(request):
        calls.append(request)
        return workflow.analyze_simple(request)

    monkeypatch.setattr(module, "analyze_simple", spy)
    run = CliRunner().invoke(cli, CASES[1]["args"])
    assert run.exit_code == 0, run.output
    assert calls == [workflow.SimpleCatenaryRequest(**CASES[1]["inputs"])]
    assert run.stdout == CASES[1]["stdout"]
