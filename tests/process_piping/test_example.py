"""The public example is executable and its reported results meet named criteria."""

import importlib.util
from pathlib import Path

import pytest


def test_worked_example():
    path = (
        Path(__file__).resolve().parents[2]
        / "examples/workflows/process-piping/example.py"
    )
    spec = importlib.util.spec_from_file_location("process_piping_example", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    result = module.run_example()
    assert result["input_version"] == 1
    assert result["wall_meets_pressure_only_requirement"]
    assert result["hydrotest"]["target_pressure_pa"] == pytest.approx(6.52173913e6)
    assert result["hydrotest"]["component_limit_satisfied"]
    assert all(result["bolt_up_design"]["checks"].values())
    assert all(result["bolt_up_test"]["checks"].values())
    assert result["selected_water_line"]["nps"] == 4
    assert result["gas_nps4"]["method"] == "isothermal"
    assert result["mixture_nps4"]["pressure_drop_pa"] is None
