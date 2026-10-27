# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Regress the synthetic free-body calculation in the adjacent usage note."""

import importlib.util
import os
from pathlib import Path
import subprocess
import sys

import pytest


ROOT = Path(__file__).resolve().parents[2]
EXAMPLE = ROOT / "examples/domains/installation/shore_approach_pull_in.py"


@pytest.fixture
def example():
    spec = importlib.util.spec_from_file_location("shore_approach_example", EXAMPLE)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def execute_example(tmp_path, *arguments):
    # Deny network, file writes and licensed bindings while executing the CLI.
    code = '''
import os, runpy, sys
def audit(event, args):
    if event.startswith("socket."):
        raise AssertionError("Example attempted network access")
    if event == "import" and args[0].startswith("OrcFxAPI"):
        raise AssertionError("Example attempted a licensed solver import")
    if event == "open":
        mode, flags = args[1:3]
        if (mode and any(c in mode for c in "wax+")) or flags & (os.O_WRONLY | os.O_RDWR | os.O_CREAT):
            raise AssertionError("Example attempted a file write")
    if event in {"os.mkdir", "os.remove", "os.rename", "os.rmdir"}:
        raise AssertionError("Example attempted a filesystem change")
sys.addaudithook(audit)
sys.argv = sys.argv[1:]
runpy.run_path(sys.argv[0], run_name="__main__")
'''
    env = dict(os.environ, PYTHONPATH=str(ROOT / "src"), PYTHONDONTWRITEBYTECODE="1")
    result = subprocess.run(
        [sys.executable, "-c", code, str(EXAMPLE), *arguments],
        cwd=tmp_path, env=env, capture_output=True, text=True, check=False,
    )
    assert result.returncode == 0, result.stderr
    assert list(tmp_path.iterdir()) == []
    return result.stdout


def test_cli_prints_hand_balance_with_units_offline(tmp_path):
    # Source: shore_approach_pull_in.md, Hand calculation, kN throughout.
    output = execute_example(tmp_path, "--capacity-kn", "100")
    for line in (
        "Tail tension: 10.0 kN",
        "Segment 1: normal=40.0 kN, friction=10.0 kN, gravity=0.0 kN, tension=10.0 -> 20.0 kN",
        "Segment 2: normal=80.0 kN, friction=20.0 kN, gravity=60.0 kN, tension=20.0 -> 100.0 kN",
        "Segment 3: normal=80.0 kN, friction=20.0 kN, gravity=-60.0 kN, tension=100.0 -> 60.0 kN",
        "Total friction: 50.0 kN",
        "Total gravity: 0.0 kN",
        "Pipe head tension: 60.0 kN",
        "Peak pipe tension: 100.0 kN",
        "Winch efficiency: 0.8 (dimensionless)",
        "Winch demand: 75.0 kN",
        "Declared winch capacity: 100.0 kN",
        "Capacity utilisation: 75.0 %",
        "Capacity status: within_declared_capacity",
        "Unevaluated: HDD, bends, breakout and dynamic loads.",
    ):
        assert line in output.splitlines()


def test_result_matches_independent_endpoint_and_efficiency_balance(example, capsys):
    # Source: note's Hand calculation. W=2*50=100 kN, triangle 30:40:50.
    # N=100*(40/50)=80 kN; F=80/4=20 kN; G=+/-100*(30/50)=+/-60 kN.
    result = example.run_example(
        example.synthetic_route(net_weight_kn_m=2.0, friction=0.25),
        tail_tension_kn=10.0, efficiency=0.8, capacity_kn=100.0,
    )
    assert result["segment_normal_loads_kn"] == pytest.approx([40, 80, 80])
    assert result["segment_end_tensions_kn"] == pytest.approx([20, 100, 60])
    assert result["friction_kn"] == pytest.approx(50)
    assert result["gravity_kn"] == pytest.approx(0)
    assert result["pipe_head_tension_kn"] == pytest.approx(60)
    assert result["peak_pipe_tension_kn"] == pytest.approx(100)
    assert result["winch_demand_kn"] == pytest.approx(75)
    assert result["capacity_utilisation"] == pytest.approx(0.75)
    capsys.readouterr()


def test_missing_capacity_is_explicit_in_cli(tmp_path):
    output = execute_example(tmp_path)
    assert "Winch demand: 75.0 kN" in output
    assert "Declared winch capacity: not supplied" in output
    assert "Capacity utilisation: not evaluated" in output
    assert "Capacity status: not_evaluated" in output
    assert "within_declared_capacity" not in output


def test_missing_capacity_result_is_unknown(example, capsys):
    # Source: note's Capacity status; omission supplies no capacity criterion.
    result = example.run_example(
        example.synthetic_route(net_weight_kn_m=2.0, friction=0.25),
        tail_tension_kn=10.0, efficiency=0.8,
    )
    assert result["capacity_utilisation"] is None
    assert result["capacity_status"] == "not_evaluated"
    capsys.readouterr()


@pytest.mark.parametrize("capacity, utilisation, status", [
    # Source: note's Capacity status: demand 75 kN divided by declared capacity.
    (100.0, 0.75, "within_declared_capacity"),
    (75.0, 1.0, "within_declared_capacity"),
    (60.0, 1.25, "exceeded"),
])
def test_capacity_uses_winch_demand(example, capsys, capacity, utilisation, status):
    result = example.run_example(
        example.synthetic_route(net_weight_kn_m=2.0, friction=0.25),
        tail_tension_kn=10.0, efficiency=0.8, capacity_kn=capacity,
    )
    assert result["capacity_utilisation"] == pytest.approx(utilisation)
    assert result["capacity_status"] == status
    capsys.readouterr()


def test_cli_uses_all_caller_supplied_inputs(tmp_path):
    # Source: note's Alternate inputs: F=10+20+20=50 kN, G=0 kN;
    # endpoints=30,80,70 kN; demand=70/0.5=140 kN; utilisation=140/200.
    output = execute_example(
        tmp_path, "--friction", "0.5", "--net-weight-kn-m", "1",
        "--tail-tension-kn", "20", "--efficiency", "0.5", "--capacity-kn", "200",
    )
    for line in (
        "Tail tension: 20.0 kN",
        "Total friction: 50.0 kN",
        "Pipe head tension: 70.0 kN",
        "Peak pipe tension: 80.0 kN",
        "Winch demand: 140.0 kN",
        "Capacity utilisation: 70.0 %",
    ):
        assert line in output.splitlines()


def test_compression_requiring_route_refuses_before_printing(example, capsys):
    # Source: note's Compression refusal: downhill alone, T=10+20-60=-30 kN.
    downhill = example.synthetic_route(net_weight_kn_m=2.0, friction=0.25)[-1]
    with pytest.raises(ValueError, match="compression"):
        example.run_example([downhill], tail_tension_kn=10.0, efficiency=0.8)
    assert capsys.readouterr().out == ""
