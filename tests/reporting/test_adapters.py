# ABOUTME: Tests for the report adapter registry and the config-driven hook.
# ABOUTME: Registration, build_spec, and maybe_render_report no-op vs render.
"""Tests for :mod:`digitalmodel.reporting.adapters`."""

from __future__ import annotations

import json
import sys
from collections.abc import Iterator
from pathlib import Path

import pytest

from digitalmodel.reporting import (
    ADAPTERS,
    AdapterError,
    DocumentMeta,
    Provenance,
    ReportSpec,
    Section,
    StatusBlock,
    TextBlock,
    build_spec,
    maybe_render_report,
    report_adapter,
)
from digitalmodel.reporting.adapters import import_domain_adapters


@pytest.fixture(autouse=True)
def clean_registry() -> Iterator[None]:
    saved = dict(ADAPTERS)
    ADAPTERS.clear()
    yield
    ADAPTERS.clear()
    ADAPTERS.update(saved)


def _dummy(cfg: dict) -> ReportSpec:
    # Adapters receive the whole routed cfg (#2212 part 2); this one reads
    # its domain block only.
    results = cfg.get("dummy", {})
    return ReportSpec(
        document=DocumentMeta(number="B0000-RPT-001-00", revision="00",
                              title="Adapter default", project="B0000", client="C"),
        sections=[
            Section(
                key="summary",
                title="Summary",
                blocks=[
                    TextBlock(markdown=f"Mass {results.get('mass', 0)} kg."),
                    StatusBlock(label="Mass", status="PASS"),
                ],
            )
        ],
        provenance=Provenance().add("results", "cfg[dummy]"),
        input_echo=dict(results),
        tool_version="test",
    )


def test_register_and_build_spec() -> None:
    report_adapter("dummy.design")(_dummy)
    assert ADAPTERS["dummy.design"] is _dummy
    spec = build_spec("dummy.design", {"dummy": {"mass": 42}})
    assert spec.sections[0].blocks[0].markdown == "Mass 42 kg."


def test_build_spec_imports_domain_adapters_on_demand() -> None:
    # "cathodic_protection.anode_design" lives in
    # digitalmodel.cathodic_protection.report_adapters; the registry was
    # cleared by the fixture, so build_spec must import it (a fresh import
    # registers only when the module is not cached).
    assert "cathodic_protection.anode_design" not in ADAPTERS
    sys.modules.pop("digitalmodel.cathodic_protection.report_adapters", None)
    with pytest.raises(ValueError, match=r"cfg\['results'\]"):
        build_spec("cathodic_protection.anode_design", {"inputs": {}})
    assert "cathodic_protection.anode_design" in ADAPTERS
    assert "cathodic_protection.assessment" in ADAPTERS
    assert import_domain_adapters("no_such_domain_xyz") is False


def test_registration_validates_key_and_duplicates() -> None:
    with pytest.raises(AdapterError, match="<basename>.<kind>"):
        report_adapter("nodot")
    report_adapter("dummy.design")(_dummy)
    report_adapter("dummy.design")(_dummy)  # same function is idempotent

    def other(cfg: dict) -> ReportSpec:  # pragma: no cover - never called
        return _dummy(cfg)

    with pytest.raises(AdapterError, match="already registered"):
        report_adapter("dummy.design")(other)


def test_build_spec_unknown_key_lists_known() -> None:
    report_adapter("dummy.design")(_dummy)
    with pytest.raises(AdapterError, match="known: dummy.design"):
        build_spec("dummy.other", {})


def test_build_spec_rejects_non_spec_return() -> None:
    report_adapter("dummy.bad")(lambda cfg: {"not": "a spec"})  # type: ignore[arg-type]
    with pytest.raises(AdapterError, match="not ReportSpec"):
        build_spec("dummy.bad", {})


def test_maybe_render_report_noop_without_report_block(tmp_path: Path) -> None:
    cfg = {"basename": "dummy", "dummy": {"mass": 1}}
    assert maybe_render_report(cfg, "dummy") is None
    assert cfg == {"basename": "dummy", "dummy": {"mass": 1}}
    assert not list(tmp_path.iterdir())


def test_maybe_render_report_requires_kind_and_valid_pdf_mode(tmp_path: Path) -> None:
    with pytest.raises(AdapterError, match="report.kind"):
        maybe_render_report({"report": {}}, "dummy")
    with pytest.raises(AdapterError, match="report.pdf"):
        maybe_render_report({"report": {"kind": "design", "pdf": "maybe"}}, "dummy")


def test_maybe_render_report_renders_with_yaml_document_control(tmp_path: Path) -> None:
    report_adapter("dummy.design")(_dummy)
    cfg = {
        "basename": "dummy",
        "dummy": {"mass": 42, "zone": "Z1"},
        "report": {
            "kind": "design",
            "document": {
                "number": "B0000-RPT-007-01",
                "revision": "01",
                "title": "From YAML",
                "project": "B0000",
                "client": "Client",
            },
            "manifest": {
                "issue": "https://github.com/example/repo/issues/1",
                "project": "B0000",
                "artifact_class": "test",
                "privacy_classification": "test",
                "publishability_decision": "test only",
                "input_source_ids": ["SRC-1"],
                "raw_output_path": "repo:tmp/raw",
                "final_output_path": "repo:tmp/final",
            },
            "pdf": "off",
            "output_dir": "results",
        },
        "_config_dir_path": str(tmp_path),
    }
    artifacts = maybe_render_report(cfg, "dummy")
    assert artifacts is not None
    out = tmp_path / "results"
    assert artifacts.html_path == out / "dummy.html"
    assert artifacts.pdf_path is None
    html = artifacts.html_path.read_text(encoding="utf-8")
    assert "From YAML" in html
    assert "B0000-RPT-007-01" in html
    assert "Mass 42 kg." in html
    manifest = json.loads(artifacts.manifest_path.read_text(encoding="utf-8"))
    assert manifest["document"]["number"] == "B0000-RPT-007-01"
    assert manifest["report_layer"]["input_source_ids"] == ["SRC-1"]
    assert cfg["report"]["artifacts"] == {
        "html": "dummy.html",
        "pdf": None,
        "citations": "dummy_citations.json",
        "manifest": "dummy_manifest.json",
    }
    assert "disabled" in cfg["report"]["pdf_status"]


def test_maybe_render_report_invalid_document_rejected(tmp_path: Path) -> None:
    report_adapter("dummy.design")(_dummy)
    cfg = {
        "dummy": {},
        "report": {"kind": "design", "pdf": "off", "output_dir": str(tmp_path),
                   "document": {"number": "bad", "revision": "00", "title": "t",
                                "project": "p", "client": "c"}},
    }
    with pytest.raises(ValueError, match="document.number"):
        maybe_render_report(cfg, "dummy")
