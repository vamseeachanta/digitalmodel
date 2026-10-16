"""CLI contract tests for the single-tank forced-roll response sweep (#1911)."""

from __future__ import annotations

import importlib.util
import json
from pathlib import Path
from types import SimpleNamespace

import pytest


SCRIPT = (
    Path(__file__).resolve().parents[4]
    / "scripts"
    / "cfd"
    / "run_sloshing_response_sweep.py"
)


@pytest.fixture()
def sweep_module():
    spec = importlib.util.spec_from_file_location("run_sloshing_response_sweep", SCRIPT)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_build_threads_custom_roll_amplitude_into_case_and_spec(
    tmp_path, sweep_module
) -> None:
    case = sweep_module._build(
        tmp_path,
        {
            "kind": "forced",
            "hl": 0.5,
            "ratio": 1.0,
            "drive_period": 1.12136,
            "t1_analytical": 1.12136,
            "name": "custom_amplitude",
        },
        roll_amplitude_deg=8.0,
    )

    motion = (case / "constant" / "dynamicMeshDict").read_text()
    case_spec = json.loads((case / "_spec.json").read_text())
    assert "amplitude   (0 0 8)" in motion
    assert case_spec["roll_amplitude_deg"] == 8.0


def test_collect_records_custom_roll_amplitude_in_manifest(
    tmp_path, monkeypatch, sweep_module
) -> None:
    manifest_path = tmp_path / "manifest.json"
    monkeypatch.setattr(sweep_module, "_MANIFEST", manifest_path)

    manifest = sweep_module._collect(tmp_path, roll_amplitude_deg=2.0)

    assert manifest["meta"]["roll_amplitude_deg"] == 2.0
    assert json.loads(manifest_path.read_text())["meta"]["roll_amplitude_deg"] == 2.0


def test_collect_rejects_result_from_different_roll_amplitude(
    tmp_path, sweep_module
) -> None:
    case = tmp_path / "resp_fr_hl50_r100"
    case.mkdir()
    (case / "_result.json").write_text(
        json.dumps(
            {
                "kind": "forced",
                "hl": 0.5,
                "ratio": 1.0,
                "roll_amplitude_deg": 4.0,
                "status": "completed",
            }
        )
    )

    with pytest.raises(ValueError, match="roll amplitude"):
        sweep_module._collect(tmp_path, roll_amplitude_deg=8.0)


def test_run_one_does_not_reuse_completed_result_from_different_amplitude(
    tmp_path, monkeypatch, sweep_module
) -> None:
    case = tmp_path / "case"
    case.mkdir()
    (case / "_spec.json").write_text(
        json.dumps(
            {
                "kind": "forced",
                "hl": 0.5,
                "drive_period": 1.12136,
                "roll_amplitude_deg": 8.0,
            }
        )
    )
    (case / "_result.json").write_text(
        json.dumps(
            {
                "kind": "forced",
                "hl": 0.5,
                "drive_period": 1.12136,
                "roll_amplitude_deg": 4.0,
                "status": "completed",
            }
        )
    )

    class FakeRunner:
        def __init__(self, config):
            pass

        def run(self, case_dir):
            return SimpleNamespace(
                status=SimpleNamespace(value="failed"),
                duration_seconds=0.1,
                error_message="intentional test sentinel",
            )

    monkeypatch.setattr(sweep_module, "OpenFOAMRunner", FakeRunner)
    result = sweep_module._run_one(case)

    assert result["status"] == "failed"
    assert result["roll_amplitude_deg"] == 8.0


def test_cli_default_roll_amplitude_remains_four_degrees(
    tmp_path, monkeypatch, sweep_module
) -> None:
    seen = []
    monkeypatch.setattr(sweep_module, "_specs", lambda: [])
    monkeypatch.setattr(
        sweep_module,
        "_collect",
        lambda work_dir, roll_amplitude_deg: seen.append(roll_amplitude_deg)
        or {"fills": []},
    )

    assert sweep_module.main(["all", "--work-dir", str(tmp_path)]) == 0
    assert seen == [4.0]


def test_cli_forwards_requested_roll_amplitude(
    tmp_path, monkeypatch, sweep_module
) -> None:
    seen = []
    monkeypatch.setattr(sweep_module, "_specs", lambda: [])
    monkeypatch.setattr(
        sweep_module,
        "_collect",
        lambda work_dir, roll_amplitude_deg: seen.append(roll_amplitude_deg)
        or {"fills": []},
    )

    assert (
        sweep_module.main(
            [
                "all",
                "--work-dir",
                str(tmp_path),
                "--roll-amplitude-deg",
                "8",
            ]
        )
        == 0
    )
    assert seen == [8.0]


def test_build_accepts_longitudinal_dimensions_and_fill_depth(tmp_path, sweep_module):
    spec = {"kind": "forced", "hl": 1 / 3, "ratio": 1.0,
            "drive_period": 1.4882, "name": "longitudinal"}
    case = sweep_module._build(tmp_path, spec, breadth=1.35, tank_height=0.9, fill_depth=0.45)
    mesh = (case / "system/blockMeshDict").read_text()
    control = (case / "system/controlDict").read_text()
    assert "(1.35" in mesh
    assert "(0.01125 0 0.005)" in control
    stored = json.loads((case / "_spec.json").read_text())
    assert stored["breadth"] == 1.35
    assert stored["tank_height"] == 0.9
    assert stored["fill_depth"] == 0.45


def test_default_cases_match_independent_base_hashes(tmp_path, sweep_module):
    import hashlib
    fixture = Path(__file__).parent / "fixtures/sloshing_response_default_sha256.json"
    for spec in sweep_module._specs():
        sweep_module._build(tmp_path, spec)
    actual = {p.relative_to(tmp_path).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
              for p in sorted(tmp_path.rglob("*")) if p.is_file()}
    assert actual == json.loads(fixture.read_text())
