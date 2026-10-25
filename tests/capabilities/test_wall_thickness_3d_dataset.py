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


def test_document_snapshot_is_used_without_reopening_source(monkeypatch):
    publisher = _publisher()
    rows = [{"combined_utilisation": 0.5, "collapse_utilisation": None}]

    def unexpected_load(_path):
        pytest.fail("the supplied document snapshot must be used directly")

    monkeypatch.setattr(publisher, "_load", unexpected_load)
    assert publisher.wall_thickness_3d_rows(doc={"rows": rows}) is rows


def test_explicit_source_keyword_preserves_full_study_rows(tmp_path):
    rows = [{"combined_utilisation": 0.5}]
    source = tmp_path / "study.json"
    source.write_text(json.dumps({"meta": {"grid": {"row_count": 1}}, "rows": rows}))
    assert _publisher().wall_thickness_3d_rows(source=source) == rows


@pytest.mark.parametrize("document", [{"rows": []}, Path("other.json")])
def test_multiple_study_inputs_are_rejected_before_reading(document, tmp_path):
    with pytest.raises(ValueError, match="one study input"):
        _publisher().wall_thickness_3d_rows(doc=document, source=tmp_path / "missing.json")


def test_default_full_study_rejects_a_truncated_grid(monkeypatch):
    publisher = _publisher()
    monkeypatch.setattr(
        publisher, "_load", lambda _path: {"meta": {"grid": {"row_count": 1}}, "rows": []}
    )
    with pytest.raises(ValueError, match="row count"):
        publisher.wall_thickness_3d_rows()
