#!/usr/bin/env python3
"""PDF rendering for the standard report engine — best-effort, fail-soft.

ABOUTME: The renderer chain moved from ``report_pack.workflow`` (#2212):
Playwright/Chromium, then Microsoft Edge headless (the documented Windows
fallback: ``msedge --headless --print-to-pdf``), then Chrome/Chromium headless.
No new installs: if nothing is available the HTML stays the approved source
and the status says so.

Modes: ``auto`` (try, record status), ``off`` (skip), ``require`` (raise).
The browser CLI gets ``--virtual-time-budget`` so Plotly draws before print.
"""

from __future__ import annotations

import shutil
import subprocess
from dataclasses import dataclass
from pathlib import Path
from typing import Literal

PdfMode = Literal["auto", "off", "require"]
PDF_MODES = ("auto", "off", "require")


class PdfRenderError(RuntimeError):
    """Raised in ``require`` mode when no renderer produced a PDF."""


@dataclass(frozen=True)
class PdfStatus:
    """Outcome of one :func:`render_pdf` call (serialisable into a manifest)."""

    rendered: bool
    engine: str | None
    message: str


def render_pdf(html_path: Path, pdf_path: Path, mode: PdfMode = "auto") -> PdfStatus:
    """Render ``html_path`` to ``pdf_path``; never raises unless ``require``."""
    if mode not in PDF_MODES:
        raise ValueError(f"pdf mode must be one of {PDF_MODES}: got {mode!r}")
    if mode == "off":
        return PdfStatus(False, None, "pdf rendering disabled (pdf: off)")

    attempts: list[str] = []
    written = _pdf_via_playwright(html_path, pdf_path, attempts)
    if not written:
        written = _pdf_via_browser_cli(html_path, pdf_path, attempts)
    if written:
        return PdfStatus(True, attempts[-1], f"pdf rendered via {attempts[-1]}")
    message = _no_renderer_message(attempts)
    if mode == "require":
        raise PdfRenderError(message)
    return PdfStatus(False, None, message)


def _no_renderer_message(attempts: list[str]) -> str:
    return (
        "PDF not rendered — no PDF renderer available on this host. "
        "Tried: " + "; ".join(attempts) + ". "
        "Install Playwright (pip install playwright && playwright install chromium) "
        "or ensure Microsoft Edge / Chrome is on PATH "
        "(Windows fallback: msedge --headless --print-to-pdf). "
        "The md/html pack is complete; PDFs are limited derivatives of the "
        "approved HTML source."
    )


def _render_pdf(html_path: Path, pdf_path: Path, mode: str) -> tuple[bool, str]:
    """Render ``html_path`` to ``pdf_path`` using the first available renderer.

    Renderer chain: Playwright/Chromium, then Microsoft Edge headless (the
    documented Windows fallback: ``msedge --headless --print-to-pdf=...``),
    then Chrome/Chromium headless. Returns ``(written, status_message)``.
    ``mode='off'`` skips entirely; ``mode='require'`` raises if every renderer
    is unavailable or fails; ``mode='auto'`` fails soft with a clear message.
    """
    if mode == "off":
        return False, "pdf rendering disabled (report_pack.pdf: off)"

    attempts: list[str] = []

    written = _pdf_via_playwright(html_path, pdf_path, attempts)
    if not written:
        written = _pdf_via_browser_cli(html_path, pdf_path, attempts)

    if written:
        return True, f"pdf rendered via {attempts[-1]}"
    message = _no_renderer_message(attempts)
    if mode == "require":
        raise RuntimeError(message)
    return False, message


def _pdf_via_playwright(html_path: Path, pdf_path: Path, attempts: list[str]) -> bool:
    try:
        from playwright.sync_api import sync_playwright
    except ImportError:
        attempts.append("playwright (not installed)")
        return False
    try:
        with sync_playwright() as playwright:
            browser = playwright.chromium.launch(headless=True)
            page = browser.new_page()
            page.goto(html_path.resolve().as_uri(), wait_until="networkidle")
            page.emulate_media(media="print")
            page.pdf(path=str(pdf_path), format="A4", print_background=True)
            browser.close()
    except Exception as exc:  # pragma: no cover - browser availability varies
        attempts.append(f"playwright (failed: {exc})")
        return False
    attempts.append("playwright/chromium")
    return pdf_path.is_file()


_EDGE_DEFAULT_PATHS = (
    r"C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe",
    r"C:\Program Files\Microsoft\Edge\Application\msedge.exe",
)


def _pdf_via_browser_cli(html_path: Path, pdf_path: Path, attempts: list[str]) -> bool:
    candidates: list[tuple[str, str]] = []
    for name in ("msedge", "microsoft-edge", "chrome", "google-chrome", "chromium",
                 "chromium-browser"):
        located = shutil.which(name)
        if located:
            candidates.append((name, located))
    for default in _EDGE_DEFAULT_PATHS:
        if Path(default).is_file():
            candidates.append(("msedge", default))
            break
    if not candidates:
        attempts.append("edge/chrome headless (no browser executable found)")
        return False
    for name, executable in candidates:
        command = [
            executable,
            "--headless",
            "--disable-gpu",
            "--no-sandbox",
            "--no-pdf-header-footer",
            "--virtual-time-budget=10000",
            f"--print-to-pdf={pdf_path.resolve()}",
            html_path.resolve().as_uri(),
        ]
        try:
            completed = subprocess.run(
                command, capture_output=True, timeout=120, check=False
            )
        except (OSError, subprocess.TimeoutExpired) as exc:
            attempts.append(f"{name} headless (failed: {exc})")
            continue
        if completed.returncode == 0 and pdf_path.is_file():
            attempts.append(f"{name} headless --print-to-pdf")
            return True
        attempts.append(f"{name} headless (exit {completed.returncode})")
    return False


__all__ = [
    "PDF_MODES",
    "PdfMode",
    "PdfRenderError",
    "PdfStatus",
    "render_pdf",
]
