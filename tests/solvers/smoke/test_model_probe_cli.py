"""CLI must expose preflight failure without native model construction."""
import json
import os
from pathlib import Path
import subprocess
import sys


def test_invalid_manifest_returns_failure_json(tmp_path):
    script = Path(__file__).resolve().parents[3] / "scripts/orcaflex_native_model_probe.py"
    manifest = tmp_path / "manifest.json"
    manifest.write_text("{}")
    output = tmp_path / "output"
    run = subprocess.run([sys.executable, "-B", str(script), "--manifest", str(manifest),
                          "--phase", "solve", "--output", str(output)], capture_output=True,
                         text=True, timeout=20, env={**os.environ, "PYTHONHASHSEED": "0"})
    assert run.returncode == 1
    proof = json.loads(run.stdout)
    assert proof["ok"] is False and proof["stage"] == "preflight"
    assert proof == json.loads((output / "solve/results.json").read_text())
