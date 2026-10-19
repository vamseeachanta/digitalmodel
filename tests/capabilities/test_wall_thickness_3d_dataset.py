"""The full load-grid export preserves the tidy per-check contract."""

import importlib.util
import json
from pathlib import Path

import pytest


SCRIPT = Path(__file__).resolve().parents[2] / "scripts/hf/build_capability_datasets.py"


def _publisher():
    spec = importlib.util.spec_from_file_location("capability_datasets", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_explicit_full_study_preserves_rows_and_per_check_values(tmp_path):
    """Synthetic transport values; this checks no engineering equation."""
    rows = [{"code": "example", "combined_utilisation": 0.5, "collapse_utilisation": None}]
    source = tmp_path / "study.json"
    source.write_text(json.dumps({"meta": {"grid": {"row_count": 1}}, "rows": rows}))
    assert _publisher().wall_thickness_3d_rows(source) == rows


def test_full_study_export_rejects_a_truncated_grid(tmp_path):
    source = tmp_path / "study.json"
    source.write_text(json.dumps({"meta": {"grid": {"row_count": 1}}, "rows": []}))
    with pytest.raises(ValueError, match="row count"):
        _publisher().wall_thickness_3d_rows(source)
