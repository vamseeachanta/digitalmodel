# ABOUTME: Tests for the fail-soft PDF chain moved into digitalmodel.reporting.pdf.
# ABOUTME: off / auto-without-browser / require paths, plus a real Edge/Chrome render.
"""Tests for :mod:`digitalmodel.reporting.pdf`."""

from __future__ import annotations

import shutil
import sys
import types
from pathlib import Path

import pytest

from digitalmodel.reporting import pdf as pdf_module
from digitalmodel.reporting import (
    DocumentMeta,
    PdfRenderError,
    PdfStatus,
    Provenance,
    ReportSpec,
    Section,
    TextBlock,
    render_pdf,
    write_report,
)
from digitalmodel.report_pack import workflow as report_pack_workflow

MINIMAL_HTML = "<!DOCTYPE html><html><body><p>hello</p></body></html>\n"


def _spec():
    return ReportSpec(
        document=DocumentMeta(number="B0000-RPT-001-00", revision="00", title="T",
                              project="B0000", client="C"),
        sections=[Section(key="s1", title="S", blocks=[TextBlock(markdown="x")])],
        provenance=Provenance().add("file", "in.yml"),
        tool_version="test",
    )


@pytest.fixture
def no_browser(monkeypatch):
    """Simulate a host with neither Playwright nor Edge/Chrome."""
    monkeypatch.setitem(sys.modules, "playwright", None)
    monkeypatch.setitem(sys.modules, "playwright.sync_api", None)
    monkeypatch.setattr(pdf_module, "shutil", types.SimpleNamespace(which=lambda n: None))
    monkeypatch.setattr(pdf_module, "_EDGE_DEFAULT_PATHS", ())


def _browser_available() -> bool:
    names = ("msedge", "microsoft-edge", "chrome", "google-chrome", "chromium",
             "chromium-browser")
    return any(shutil.which(n) for n in names) or any(
        Path(p).is_file() for p in pdf_module._EDGE_DEFAULT_PATHS
    )


def test_report_pack_imports_the_moved_chain():
    assert report_pack_workflow._render_pdf is pdf_module._render_pdf
    assert report_pack_workflow._pdf_via_browser_cli is pdf_module._pdf_via_browser_cli
    assert report_pack_workflow._pdf_via_playwright is pdf_module._pdf_via_playwright


def test_mode_off_is_disabled_without_touching_the_host(tmp_path, no_browser):
    html = tmp_path / "r.html"
    html.write_text(MINIMAL_HTML, encoding="utf-8")
    status = render_pdf(html, tmp_path / "r.pdf", "off")
    assert status == PdfStatus(rendered=False, engine=None,
                               message="pdf rendering disabled (pdf: off)")
    assert "disabled" in status.message
    assert not (tmp_path / "r.pdf").exists()


def test_invalid_mode_rejected(tmp_path):
    with pytest.raises(ValueError, match="pdf mode"):
        render_pdf(tmp_path / "r.html", tmp_path / "r.pdf", "maybe")  # type: ignore[arg-type]


def test_auto_without_browser_fails_soft(tmp_path, no_browser):
    html = tmp_path / "r.html"
    html.write_text(MINIMAL_HTML, encoding="utf-8")
    status = render_pdf(html, tmp_path / "r.pdf", "auto")
    assert status.rendered is False
    assert status.engine is None
    assert status.message.startswith("PDF not rendered")
    assert "playwright (not installed)" in status.message
    assert "no browser executable found" in status.message
    assert not (tmp_path / "r.pdf").exists()


def test_require_without_browser_raises(tmp_path, no_browser):
    html = tmp_path / "r.html"
    html.write_text(MINIMAL_HTML, encoding="utf-8")
    with pytest.raises(PdfRenderError, match="PDF not rendered"):
        render_pdf(html, tmp_path / "r.pdf", "require")
    # report_pack's legacy wrapper keeps raising a plain RuntimeError subclass.
    with pytest.raises(RuntimeError, match="PDF not rendered"):
        pdf_module._render_pdf(html, tmp_path / "r.pdf", "require")


def test_write_report_auto_without_browser_still_completes(tmp_path, no_browser):
    artifacts = write_report(_spec(), tmp_path, "case", pdf="auto")
    assert artifacts.pdf_path is None
    assert artifacts.pdf_status.rendered is False
    assert "PDF not rendered" in artifacts.pdf_status.message
    for path in (artifacts.html_path, artifacts.citations_json_path,
                 artifacts.manifest_path):
        assert path.is_file()
    manifest = artifacts.manifest_path.read_text(encoding="utf-8")
    assert '"pdf": null' in manifest
    assert "PDF not rendered" in manifest


def test_browser_cli_passes_print_flags(tmp_path, monkeypatch):
    """Edge/Chrome get the no-header and virtual-time flags so Plotly draws."""
    recorded: dict[str, list[str]] = {}

    def fake_run(command, **kwargs):
        recorded["command"] = command
        Path(command[-2].split("=", 1)[1]).write_bytes(b"%PDF-1.7 fake")
        return types.SimpleNamespace(returncode=0)

    monkeypatch.setattr(pdf_module, "shutil",
                        types.SimpleNamespace(which=lambda n: "C:/fake/msedge.exe"
                                              if n == "msedge" else None))
    monkeypatch.setattr(pdf_module, "_EDGE_DEFAULT_PATHS", ())
    monkeypatch.setattr(pdf_module.subprocess, "run", fake_run)
    attempts: list[str] = []
    html = tmp_path / "r.html"
    html.write_text(MINIMAL_HTML, encoding="utf-8")
    assert pdf_module._pdf_via_browser_cli(html, tmp_path / "r.pdf", attempts) is True
    assert "--no-pdf-header-footer" in recorded["command"]
    assert "--virtual-time-budget=10000" in recorded["command"]
    assert attempts == ["msedge headless --print-to-pdf"]


@pytest.mark.skipif(not _browser_available(), reason="no Edge/Chrome on this host")
def test_real_render_produces_a_pdf(tmp_path):
    artifacts = write_report(_spec(), tmp_path, "real", pdf="auto")
    assert artifacts.pdf_status.rendered is True, artifacts.pdf_status.message
    assert artifacts.pdf_path is not None
    assert artifacts.pdf_path.read_bytes()[:4] == b"%PDF"
    assert artifacts.pdf_path.stat().st_size > 1000
