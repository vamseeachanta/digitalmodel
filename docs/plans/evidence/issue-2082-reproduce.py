"""Reproduce issue 2082 with a fake API; never import the native OrcFxAPI.

Run with the existing interpreter. No package installation or environment
modification is required. Output contains no private absolute paths.
This is a planning reproduction, not a production acceptance test.
"""

import argparse
import importlib.util
import json
import math
from pathlib import Path
import subprocess
import sys
import tempfile
from types import SimpleNamespace


EXPECTED_REVISION = "80da6e09b649707ef7bdb09c9d112f1650bf3d6a"
REPO_ROOT = Path(__file__).resolve().parents[3]
PROBE_PATH = "src/digitalmodel/solvers/smoke/probes.py"
SCENARIOS = (
    ("finite_control", 1.0, [1.0, 2.0], "SimulationStopped", False),
    ("static_nan", float("nan"), [1.0, 2.0], "SimulationStopped", False),
    ("static_inf", float("inf"), [1.0, 2.0], "SimulationStopped", False),
    ("dynamic_nan", 1.0, [1.0, float("nan")], "SimulationStopped", False),
    ("dynamic_inf", 1.0, [1.0, float("inf")], "SimulationStopped", False),
    ("single_sample", 1.0, [1.0], "SimulationStopped", False),
    ("wrong_state", 1.0, [1.0, 2.0], "SimulationRunning", False),
    ("corrupt_sim", 1.0, [1.0, 2.0], "SimulationStopped", True),
)


class FakeHistory(list):
    """Small stand-in for the native array reductions used by the probe."""

    def min(self):
        return float("nan") if any(math.isnan(v) for v in self) else min(self)

    def max(self):
        return float("nan") if any(math.isnan(v) for v in self) else max(self)


def fake_api(static, history, state, corrupt, calls):
    class Line:
        def __init__(self):
            self.Length = [0.0]
            self.TargetSegmentLength = [0.0]

        def StaticResult(self, *args):
            return static

        def TimeHistory(self, *args):
            return FakeHistory(history)

    class Model:
        def __init__(self, path=None, **kwargs):
            suffix = Path(path).suffix if path is not None else None
            calls.append({"input_suffix": suffix, "kwargs": kwargs})
            if suffix == ".sim" and corrupt:
                raise RuntimeError("fake simulation file cannot be reloaded")
            self.general = SimpleNamespace()
            self.environment = SimpleNamespace()
            self.state = "InStaticState"

        def CreateObject(self, *args):
            return Line()

        def CalculateStatics(self):
            pass

        def RunSimulation(self):
            self.state = state

        def SaveData(self, path):
            Path(path).write_text("fake data", encoding="utf-8")

        def SaveSimulation(self, path):
            Path(path).write_text("fake simulation", encoding="utf-8")

    return SimpleNamespace(
        Model=Model, otLine=1, oeEndA=1, Period=lambda value: value,
        DLLVersion=lambda: "FAKE-NO-LICENCE",
    )


def load_probe():
    spec = importlib.util.spec_from_file_location("issue_2082_probe", REPO_ROOT / PROBE_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def public_number(value):
    if isinstance(value, float) and not math.isfinite(value):
        return str(value)
    return value


def run_scenario(probe, root, scenario):
    name, static, history, state, corrupt = scenario
    calls = []
    fake = fake_api(static, history, state, corrupt, calls)
    previous = sys.modules.get("OrcFxAPI")
    sys.modules["OrcFxAPI"] = fake
    try:
        result = probe.check_orcaflex(root / name)
        assert sys.modules["OrcFxAPI"] is fake
    finally:
        if previous is None:
            sys.modules.pop("OrcFxAPI", None)
        else:
            sys.modules["OrcFxAPI"] = previous
    try:
        json.dumps(result, allow_nan=False)
        strict_json = True
    except ValueError:
        strict_json = False
    return {
        "scenario": name,
        "observed_ok": result["ok"],
        "expected_ok_after_repair": name == "finite_control",
        "static_tension": public_number(result.get("static_tension_kN")),
        "dynamic_range": [public_number(v) for v in result.get("dynamic_tension_kN", [])],
        "dynamic_samples": result.get("dynamic_samples"),
        "simulation_state": result.get("simulation_state"),
        "constructor_calls": calls,
        "sim_reload_attempted": any(call["input_suffix"] == ".sim" for call in calls),
        "raw_report_is_strict_json": strict_json,
    }


def reproduce():
    source_check = subprocess.run(
        ["git", "diff", "--quiet", EXPECTED_REVISION, "--", PROBE_PATH],
        cwd=REPO_ROOT,
    )
    if source_check.returncode:
        raise RuntimeError("Probe source differs from its recorded source revision")
    probe = load_probe()
    with tempfile.TemporaryDirectory(prefix="orcaflex-smoke-fake-") as scratch:
        results = [run_scenario(probe, Path(scratch), case) for case in SCENARIOS]
    return {
        "issue": "https://github.com/vamseeachanta/digitalmodel/issues/2082",
        "source_revision": EXPECTED_REVISION,
        "source_file": PROBE_PATH,
        "method": "file-loaded source plus injected fake API; no native imports or licences",
        "native_verification": False,
        "scratch_removed": not Path(scratch).exists(),
        "results": results,
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    output = json.dumps(reproduce(), indent=2, allow_nan=False) + "\n"
    if args.output:
        args.output.write_text(output, encoding="utf-8")
    else:
        print(output, end="")


if __name__ == "__main__":
    main()
