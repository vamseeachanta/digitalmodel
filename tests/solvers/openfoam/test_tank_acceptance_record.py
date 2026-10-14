"""Read back the native bridge acceptance packet without rerunning CFD."""

import hashlib
import json
from datetime import datetime
from pathlib import Path

ROOT = Path(__file__).resolve().parents[3]
PACKET = ROOT / "docs/validation/gmsh-tank"


def test_committed_evidence_digests():
    manifest = json.loads((PACKET / "manifest.json").read_text())
    assert manifest["files"]
    for item in manifest["files"]:
        data = (PACKET / item["path"]).read_bytes()
        assert len(data) == item["size"]
        assert hashlib.sha256(data).hexdigest() == item["sha256"]


def test_criteria_precede_native_run():
    manifest = json.loads((PACKET / "manifest.json").read_text())
    data = (PACKET / "criteria.json").read_bytes()
    assert hashlib.sha256(data).hexdigest() == manifest["criteria_sha256"]
    assert datetime.fromisoformat(manifest["criteria_recorded_utc"]) < datetime.fromisoformat(manifest["first_run_started_utc"])
    log = (PACKET / "run-record.txt").read_text()
    assert manifest["criteria_sha256"] in log
    assert manifest["criteria_recorded_utc"] in log


def test_native_run_meets_frozen_criteria():
    criteria = json.loads((PACKET / "criteria.json").read_text())
    manifest = json.loads((PACKET / "manifest.json").read_text())
    result = manifest["result"]
    assert result["mesh_ok"] is True
    assert result["failed_mesh_checks"] == criteria["failed_mesh_checks"]
    assert result["cells"] > 0
    assert 1 <= result["ranks"] <= criteria["maximum_ranks"]
    assert result["steps"] >= criteria["minimum_steps"]
    assert abs(result["final_time_s"] - criteria["end_time_s"]) <= criteria["time_tolerance_s"]
    assert result["rotation_error_m"] <= criteria["rotation_tolerance_m"]
    assert result["wall_time_s"] > 0
    assert result["source_mesh_rebuild_identical"] is True


def test_probe_and_force_histories_are_finite_and_advance():
    import math
    import re

    for name in ("probe-pressure.txt", "force.txt", "moment.txt"):
        rows = []
        for line in (PACKET / name).read_text().splitlines():
            if line.strip() and not line.startswith("#"):
                rows.append([float(value) for value in re.findall(r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?", line)])
        assert len(rows) >= 2
        assert all(row and all(math.isfinite(value) for value in row) for row in rows)
        assert all(first[0] < second[0] for first, second in zip(rows, rows[1:]))
        assert abs(rows[-1][0] - 0.3) <= 1e-8
