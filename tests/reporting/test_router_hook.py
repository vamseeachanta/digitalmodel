# ABOUTME: Tests for the engine router's standard-report hook (#2212 part 2).
# ABOUTME: report: {kind} renders beside results; no report / no kind is a no-op.
"""The ``digitalmodel.engine`` hook that renders ``report:`` blocks.

Runs the real cathodic_protection arm on the jacket fixture through
``engine(cfg=..., config_flag=False)`` (the pattern used by
``tests/cathodic_protection/test_engine_adapter.py``) with ``pdf: off``.
"""

from __future__ import annotations

import copy
import json
import warnings
from pathlib import Path
from typing import Any

import pytest
import yaml  # type: ignore[import-untyped]

from digitalmodel import engine as engine_module

INPUT = (
    Path(__file__).resolve().parents[1]
    / "fixtures"
    / "cathodic_protection"
    / "workflow_inputs"
    / "jacket.yml"
)
DOCUMENT = {
    "number": "B0000-RPT-042-01",
    "revision": "01",
    "title": "Jacket CP anode design",
    "project": "B0000",
    "client": "Client",
}


@pytest.fixture
def jacket_cfg(monkeypatch: pytest.MonkeyPatch) -> dict[str, Any]:
    monkeypatch.setattr(engine_module.app_manager, "save_cfg", lambda cfg_base: None)
    with INPUT.open(encoding="utf-8") as stream:
        cfg: dict[str, Any] = yaml.safe_load(stream)
    cfg["_config_file_path"] = str(INPUT)
    cfg["_config_dir_path"] = str(INPUT.parent)
    return cfg


def _engine(cfg: dict[str, Any]) -> dict[str, Any]:
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        out: dict[str, Any] = engine_module.engine(cfg=cfg, config_flag=False)
    return out


def test_hook_renders_report_beside_results(jacket_cfg: dict[str, Any], tmp_path: Path) -> None:
    out_dir = tmp_path / "reports"
    jacket_cfg["report"] = {
        "kind": "anode_design",
        "document": DOCUMENT,
        "pdf": "off",
        "output_dir": str(out_dir),
        "stem": "jacket_cp",
    }
    out = _engine(jacket_cfg)

    assert out["results"]["status"]["result"] == "FAIL"
    assert out["report"]["artifacts"] == {
        "html": "jacket_cp.html",
        "pdf": None,
        "citations": "jacket_cp_citations.json",
        "manifest": "jacket_cp_manifest.json",
    }
    assert "disabled" in out["report"]["pdf_status"]
    assert sorted(p.name for p in out_dir.iterdir()) == [
        "jacket_cp.html",
        "jacket_cp_citations.json",
        "jacket_cp_manifest.json",
    ]
    html = (out_dir / "jacket_cp.html").read_text(encoding="utf-8")
    assert "B0000-RPT-042-01" in html
    assert "Jacket CP anode design" in html
    assert '<div class="st fail">' in html
    assert "<script src=" not in html
    assert "workflow_inputs/jacket.yml" not in html  # identifier is config-dir relative
    assert "<td>jacket.yml</td>" in html
    manifest = json.loads((out_dir / "jacket_cp_manifest.json").read_text(encoding="utf-8"))
    assert manifest["document"]["number"] == "B0000-RPT-042-01"
    assert manifest["standards"][0]["code_id"] == "DNV-RP-B401"
    assert manifest["citations_count"] == 5
    assert manifest["pdf"]["rendered"] is False


def test_hook_output_dir_is_relative_to_the_config_dir(jacket_cfg: dict[str, Any], tmp_path: Path) -> None:
    jacket_cfg["_config_dir_path"] = str(tmp_path)
    jacket_cfg["report"] = {"kind": "anode_design", "pdf": "off", "output_dir": "results"}
    out = _engine(jacket_cfg)
    assert Path(out["report"]["output_dir"]) == tmp_path / "results"
    assert (tmp_path / "results" / "cathodic_protection.html").is_file()
    html = (tmp_path / "results" / "cathodic_protection.html").read_text(encoding="utf-8")
    assert "X0000-CP-000-00" in html  # placeholder document number, no report.document


def test_hook_is_a_noop_without_report_block(jacket_cfg: dict[str, Any], tmp_path: Path) -> None:
    before = copy.deepcopy(jacket_cfg)
    out = _engine(jacket_cfg)
    assert "report" not in out
    assert out["results"]["status"]["result"] == "FAIL"
    assert out["inputs"] == before["inputs"]
    assert not list(tmp_path.iterdir())


def test_hook_leaves_legacy_report_blocks_without_kind_alone(jacket_cfg: dict[str, Any], tmp_path: Path) -> None:
    jacket_cfg["report"] = {"html": True}
    out = _engine(jacket_cfg)
    assert out["report"] == {"html": True}
    assert not list(tmp_path.iterdir())


def test_hook_rejects_unknown_kind(jacket_cfg: dict[str, Any], tmp_path: Path) -> None:
    from digitalmodel.reporting import AdapterError

    jacket_cfg["report"] = {"kind": "nope", "pdf": "off", "output_dir": str(tmp_path)}
    with pytest.raises(AdapterError, match="cathodic_protection.nope"):
        _engine(jacket_cfg)
    assert not list(tmp_path.iterdir())
