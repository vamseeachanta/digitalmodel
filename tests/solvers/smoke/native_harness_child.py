"""Fake CLI copied into an isolated repository by harness tests."""
import argparse
import json
from pathlib import Path
import subprocess
import sys
import time

parser = argparse.ArgumentParser()
parser.add_argument("--solver", choices=["orcaflex"], required=True)
parser.add_argument("--json", action="store_true", required=True)
parser.add_argument("--output-dir", required=True)
args = parser.parse_args()
mode = Path("mode").read_text()
if mode == "timeout":
    child = subprocess.Popen([sys.executable, "-c", "import time; time.sleep(60)"])
    Path("descendant.pid").write_text(str(child.pid))
    time.sleep(60)
directory = Path(args.output_dir) / "orcaflex"
directory.mkdir()
for suffix in ("dat", "sim"):
    (directory / ("smoke." + suffix)).write_text("fake retained artifact")
entry = dict(solver="orcaflex", ok=mode != "failure", dll_version="FAKE",
             module=str(Path.cwd() / "private"), thread_count_requested=1,
             thread_counts_observed=dict(solve=1, data_reader=1, simulation_reader=1),
             dynamic_samples=2, reloaded_dynamic_samples=2, sim_bytes=22,
             static_tension_kN=1.0, dynamic_tension_kN=[1.0, 2.0])
for key in ("static_finite", "dynamic_finite", "simulation_complete",
            "saved_sim_reloaded", "reloaded_simulation_complete", "reloaded_dynamic_finite"):
    entry[key] = True
if mode == "badproof":
    entry["thread_counts_observed"]["simulation_reader"] = 8
if mode == "nonfinite":
    entry["static_tension_kN"] = float("nan")
print(json.dumps(dict(ok=entry["ok"], results=[entry], python=sys.executable)))
print("fake diagnostic", file=sys.stderr)
sys.exit(1 if mode == "failure" else 0)
