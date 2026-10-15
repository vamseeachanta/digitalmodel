"""Read back the native bridge acceptance packet without rerunning CFD."""

import hashlib
import json
import math
import re
import shutil
from datetime import datetime
from pathlib import Path

import pytest

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


def assert_native_result_matches_manifest(packet: Path) -> None:
    result = json.loads((packet / "manifest.json").read_text())["result"]
    mesh_check = (packet / "mesh-check.txt").read_text()
    cells = re.search(r"^\s*cells:\s*(\d+)\s*$", mesh_check, re.MULTILINE)
    bridge = json.loads((packet / "bridge-manifest.json").read_text())
    solver_log = (packet / "solver-log.txt").read_text()
    times = re.findall(r"^Time = (.+)$", solver_log, re.MULTILINE)
    iterations = re.findall(r"No Iterations (\d+)", solver_log)
    observations = (
        ("cells", "mesh-check.txt", int(cells[1]) if cells else None),
        ("cells", "bridge-manifest.json", bridge["contract"]["cells"]),
        ("steps", "solver-log.txt", len(times)),
        ("linear_solver_iterations", "solver-log.txt", sum(map(int, iterations))),
        ("final_time_s", "solver-log.txt", float(times[-1]) if times else None),
    )
    for quantity, source, native in observations:
        assert result[quantity] == native, (
            f"{quantity} ({source}): manifest={result[quantity]!r}, native={native!r}"
        )


def test_native_run_meets_frozen_criteria():
    assert_native_result_matches_manifest(PACKET)
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
    criteria = json.loads((PACKET / "criteria.json").read_text())
    for name in ("probe-pressure.txt", "force.txt", "moment.txt"):
        rows = []
        for line in (PACKET / name).read_text().splitlines():
            if line.strip() and not line.startswith("#"):
                rows.append([
                    float(value)
                    for value in re.findall(
                        r"[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?", line
                    )
                ])
        assert len(rows) >= 2
        assert all(row and all(math.isfinite(value) for value in row) for row in rows)
        assert all(first[0] < second[0] for first, second in zip(rows, rows[1:]))
        assert (
            abs(rows[-1][0] - criteria["end_time_s"])
            <= criteria["time_tolerance_s"]
        )


@pytest.mark.parametrize(
    "quantity", ["cells", "steps", "linear_solver_iterations", "final_time_s"]
)
def test_manifest_disagreement_is_rejected(tmp_path: Path, quantity: str):
    packet = shutil.copytree(PACKET, tmp_path / "packet")
    path = packet / "manifest.json"
    manifest = json.loads(path.read_text())
    criteria = json.loads((packet / "criteria.json").read_text())
    native = manifest["result"][quantity]
    altered = native + (
        criteria["time_tolerance_s"] / 2 if quantity == "final_time_s" else 1
    )
    manifest["result"][quantity] = altered
    path.write_text(json.dumps(manifest))
    if quantity == "final_time_s":
        assert abs(altered - criteria["end_time_s"]) <= criteria["time_tolerance_s"]
    elif quantity == "steps":
        assert altered >= criteria["minimum_steps"]
    else:
        assert altered > 0

    with pytest.raises(AssertionError, match=quantity) as error:
        assert_native_result_matches_manifest(packet)
    assert f"manifest={altered!r}" in str(error.value)
    assert f"native={native!r}" in str(error.value)


@pytest.mark.parametrize(
    ("source", "quantity", "offset"),
    [
        ("step", "steps", -1),
        ("iteration", "linear_solver_iterations", 1),
        ("mesh-check", "cells", 1),
        ("bridge-manifest", "cells", 1),
    ],
)
def test_native_disagreement_is_rejected(
    tmp_path: Path, source: str, quantity: str, offset: int
):
    packet = shutil.copytree(PACKET, tmp_path / "packet")
    manifest = json.loads((packet / "manifest.json").read_text())
    expected = manifest["result"][quantity]
    if source == "step":
        path = packet / "solver-log.txt"
        path.write_text(re.sub(
            r"^Time = .*\n", "", path.read_text(), count=1, flags=re.MULTILINE
        ))
    elif source == "iteration":
        path = packet / "solver-log.txt"
        path.write_text(re.sub(
            r"No Iterations (\d+)",
            lambda match: f"No Iterations {int(match[1]) + 1}",
            path.read_text(), count=1,
        ))
    elif source == "mesh-check":
        path = packet / "mesh-check.txt"
        path.write_text(re.sub(
            r"^(\s*cells:\s*)(\d+)",
            lambda match: f"{match[1]}{int(match[2]) + 1}",
            path.read_text(), count=1, flags=re.MULTILINE,
        ))
    else:
        path = packet / "bridge-manifest.json"
        bridge = json.loads(path.read_text())
        bridge["contract"]["cells"] += 1
        path.write_text(json.dumps(bridge))

    with pytest.raises(AssertionError, match=quantity) as error:
        assert_native_result_matches_manifest(packet)
    assert f"manifest={expected!r}" in str(error.value)
    assert f"native={expected + offset!r}" in str(error.value)
