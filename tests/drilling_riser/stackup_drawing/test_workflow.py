"""``riser_stackup_drawing`` workflow tests (#2152)."""

from __future__ import annotations

import json
import shutil
from pathlib import Path

import pytest
import yaml

from digitalmodel.drilling_riser.stackup_drawing import from_json, reconcile
from digitalmodel.drilling_riser.stackup_drawing.workflow import (
    StackupReconcileError,
    router,
)
from digitalmodel.engine import engine

from .conftest import SYNTHETIC_SPEC

REPO_ROOT = Path(__file__).resolve().parents[3]
EXAMPLE_DIR = REPO_ROOT / "examples" / "workflows" / "riser-stackup-drawing"


def _stage_example(tmp_path: Path) -> Path:
    """Copy the example input + spec into ``tmp_path``; return the input path."""
    for name in ("input.yml", "synthetic_spec.json"):
        shutil.copy(EXAMPLE_DIR / name, tmp_path / name)
    return tmp_path / "input.yml"


def test_example_spec_matches_test_fixture():
    example = json.loads((EXAMPLE_DIR / "synthetic_spec.json").read_text("utf-8"))
    fixture = json.loads(SYNTHETIC_SPEC.read_text("utf-8"))
    assert example == fixture


def test_engine_runs_the_example_and_writes_reconciled_outputs(tmp_path):
    input_path = _stage_example(tmp_path)

    cfg = engine(inputfile=str(input_path))

    summary = cfg["riser_stackup_drawing"]
    assert cfg["basename"] == "riser_stackup_drawing"
    assert summary["result"] == "pass"
    svg_path = Path(summary["svg"])
    spec_path = Path(summary["spec_json"])
    report_path = Path(summary["reconcile_report"])
    for path in (svg_path, spec_path, report_path):
        assert path.exists(), path
        assert path.resolve().is_relative_to((tmp_path / "results").resolve())

    report = json.loads(report_path.read_text("utf-8"))
    assert report["result"] == "pass"
    # the written artifacts reconcile on their own, independent of the run
    spec = from_json(spec_path.read_text("utf-8"))
    assert reconcile(spec, svg_path.read_text("utf-8"))["result"] == "pass"


def test_router_fails_when_reconcile_fails(tmp_path):
    data = json.loads(SYNTHETIC_SPEC.read_text("utf-8"))
    pup = next(c for c in data["components"] if c["id"] == "c06-pup-joint")
    pup["joint_length_m"] += 0.5  # breaks length closure
    (tmp_path / "bad_spec.json").write_text(json.dumps(data), encoding="utf-8")
    input_path = tmp_path / "bad.yml"
    input_path.write_text(
        yaml.safe_dump(
            {
                "basename": "riser_stackup_drawing",
                "riser_stackup_drawing": {
                    "spec": "bad_spec.json",
                    "output_dir": "results",
                },
            }
        ),
        encoding="utf-8",
    )
    cfg = {
        "basename": "riser_stackup_drawing",
        "riser_stackup_drawing": {"spec": "bad_spec.json", "output_dir": "results"},
        "_config_file_path": str(input_path),
        "_config_dir_path": str(tmp_path),
    }

    with pytest.raises(StackupReconcileError, match="d_totals"):
        router(cfg)

    # the report is still written so the failure can be inspected
    report = json.loads((tmp_path / "results" / "bad_reconcile.json").read_text("utf-8"))
    assert report["result"] == "fail"
    assert report["checks"]["d_totals"]["status"] == "fail"


def test_router_requires_a_spec(tmp_path):
    with pytest.raises(ValueError, match="spec"):
        router({"riser_stackup_drawing": {}, "_config_dir_path": str(tmp_path)})
