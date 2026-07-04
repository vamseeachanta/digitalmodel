"""Tests for skeleton-first reporting + the CLI (#1021)."""

from __future__ import annotations

import json

import pytest

from digitalmodel.reporting import Provenance, ReportSkeleton
from digitalmodel.reporting.__main__ import main as cli_main


def _skeleton() -> ReportSkeleton:
    return ReportSkeleton.model_validate(
        {
            "title": "Test report",
            "sections": [
                {
                    "key": "summary",
                    "label": "Summary",
                    "blocks": [
                        {"key": "scope", "label": "Scope", "required": True},
                        {"key": "extra", "label": "Extra", "required": False},
                    ],
                },
                {
                    "key": "results",
                    "label": "Results",
                    "blocks": [{"key": "rao", "label": "RAOs", "required": True}],
                },
            ],
        }
    )


def test_required_block_keys() -> None:
    assert _skeleton().required_block_keys() == ["scope", "rao"]


def test_empty_skeleton_renders_pending_slots() -> None:
    html = _skeleton().build_html()
    assert "<!DOCTYPE html>" in html
    # required + optional pending placeholders present
    assert "slot pending (required)" in html
    assert "slot pending (optional)" in html
    assert 'id="scope"' in html and 'id="rao"' in html


def test_filled_slots_replace_placeholders() -> None:
    content = {"scope": "<p>FPSO RAOs</p>", "rao": "<p>RAO table</p>"}
    html = _skeleton().build_html(content)
    assert "FPSO RAOs" in html and "RAO table" in html
    # only the optional block remains pending
    assert "slot pending (required)" not in html
    assert "slot pending (optional)" in html


def test_completeness_tracks_required_blocks_and_provenance() -> None:
    sk = _skeleton()
    # nothing filled, no provenance
    c0 = sk.completeness()
    assert not c0.complete
    assert c0.required_total == 2 and c0.required_filled == 0
    assert set(c0.missing_blocks) == {"scope", "rao"}
    assert c0.provenance_ok is False

    # required blocks filled but still no provenance -> incomplete
    filled = {"scope": "x", "rao": "y"}
    c1 = sk.completeness(filled)
    assert c1.required_filled == 2 and not c1.complete and not c1.provenance_ok

    # filled + provenance -> complete
    prov = Provenance().add("spec", "spec.yml")
    c2 = sk.completeness(filled, provenance=prov)
    assert c2.complete and c2.provenance_ok and c2.missing_blocks == []


def test_require_provenance_false_completes_without_source() -> None:
    sk = _skeleton()
    sk.require_provenance = False
    c = sk.completeness({"scope": "x", "rao": "y"})
    assert c.complete and c.provenance_ok


def test_provenance_block_appended_when_supplied() -> None:
    html = _skeleton().build_html(provenance=Provenance().add("spec", "s.yml"))
    assert 'id="provenance"' in html and "s.yml" in html


def test_unknown_section_mode_rejected() -> None:
    sk = ReportSkeleton.model_validate(
        {
            "title": "t",
            "sections": [{"key": "s", "label": "S", "mode": "bogus", "blocks": []}],
        }
    )
    with pytest.raises(ValueError):
        sk.build_html()


# --- CLI -------------------------------------------------------------------


def _write_skeleton_yaml(tmp_path) -> str:
    p = tmp_path / "sk.yml"
    p.write_text(
        "title: T\n"
        "sections:\n"
        "  - key: s\n"
        "    label: S\n"
        "    blocks:\n"
        "      - {key: a, label: A, required: true}\n"
    )
    return str(p)


def test_cli_check_incomplete_exits_nonzero(tmp_path, capsys) -> None:
    rc = cli_main([_write_skeleton_yaml(tmp_path), "--check"])
    assert rc == 1
    out = json.loads(capsys.readouterr().out)
    assert out["complete"] is False and out["missing_blocks"] == ["a"]


def test_cli_renders_html_to_file(tmp_path) -> None:
    out = tmp_path / "r.html"
    rc = cli_main([_write_skeleton_yaml(tmp_path), "-o", str(out)])
    assert rc == 0 and out.exists()
    assert "slot pending (required)" in out.read_text()


def test_cli_check_complete_with_content_and_provenance(tmp_path) -> None:
    sk = _write_skeleton_yaml(tmp_path)
    content = tmp_path / "c.yml"
    content.write_text("a: '<p>filled</p>'\n")
    prov = tmp_path / "p.yml"
    prov.write_text("sources:\n  - {kind: spec, identifier: s.yml}\n")
    rc = cli_main([sk, "--content", str(content), "--provenance", str(prov), "--check"])
    assert rc == 0
