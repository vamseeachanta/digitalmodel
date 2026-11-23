#!/usr/bin/env python3
# ABOUTME: Renders the small-operator HTML deliverables to client-ready PDFs
# ABOUTME: Wraps the artifact fragments into standalone print documents via headless Chrome

"""
Build client-ready PDFs
=======================

The HTML deliverables in this directory are written as **artifact fragments** —
no ``<!doctype>``, ``<html>``, ``<head>`` or ``<body>``, because the Artifact
publisher supplies that wrapper. Handing a fragment straight to a browser gives
you quirks-mode rendering and, worse, the dark theme on a client's printer.

So this script does three things before printing:

1. wraps the fragment in a real HTML document,
2. pins ``data-theme="light"`` on the root so the page cannot render dark
   regardless of the rendering machine's OS setting, and
3. injects print CSS — page size, margins, and break rules that keep a matrix
   row or a table from splitting across a page boundary.

Usage::

    python docs/small_operator/build_pdfs.py            # build all
    python docs/small_operator/build_pdfs.py brochure   # build one

Output lands in ``docs/small_operator/pdf/``.
"""

from __future__ import annotations

import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT_DIR = HERE / "pdf"

CHROME_CANDIDATES = [
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/usr/bin/google-chrome",
    "/usr/bin/chromium",
    "/usr/bin/chromium-browser",
]


@dataclass(frozen=True)
class Document:
    """One deliverable: a source fragment and how it should print."""

    key: str
    source: str
    output: str
    landscape: bool
    audience: str

    @property
    def page_size(self) -> str:
        # Letter, since the audience is US operators.
        return "11in 8.5in" if self.landscape else "8.5in 11in"


DOCUMENTS = [
    Document(
        key="brochure",
        source="brochure.html",
        output="AceEngineer-small-operator-field-note.pdf",
        landscape=False,
        audience="Any small / marginal operator. Nothing in it is specific to one company.",
    ),
]


# The Artifact runtime ships a CSS reset; locally we supply the small part of it
# these pages actually rely on, plus the print rules.
#
# NOTE: substitution here is a plain string replace, deliberately. The CSS
# contains literal percent signs ("width: 100%"), so %-formatting blows up on it.
PRINT_CSS = """
  *, *::before, *::after { box-sizing: border-box; }
  @page {
    size: __PAGE_SIZE__;
    margin: 12mm 12mm 14mm;
  }
  html, body { background: #ffffff !important; }
  .sheet {
    border-left: 0 !important;
    border-right: 0 !important;
    max-width: none !important;
    padding-left: 0 !important;
    padding-right: 0 !important;
    padding-bottom: 0 !important;
  }
  .rule-top { margin-left: 0 !important; margin-right: 0 !important; }
  header { padding-top: 0 !important; }

  /* Screen type is set for a lit display at arm's length; print wants it a
     couple of steps down. Scale the whole ramp rather than individual pieces,
     so the hierarchy that holds on screen still holds on paper — the standfirst
     must stay LARGER than body copy, not smaller. */
  body        { font-size: 13px !important; }
  .standfirst { font-size: 15px !important; max-width: 88ch !important; }
  .lede       { font-size: 14px !important; }
  h1 { font-size: 25px !important; line-height: 1.1 !important; margin-top: 2px !important; }

  /* On screen the masthead can breathe; on paper that whitespace cost a whole
     sheet, because the first content block then would not fit beneath it. */
  .sheet  { gap: 22px !important; }
  header  { gap: 10px !important; }

  /* Keep units of meaning intact across page boundaries — but only the small
     ones. Protecting a whole .entry meant any entry that did not fit in the
     remaining space jumped to the next sheet, leaving half-empty pages through
     the document. Protect the quote and the answer block instead, and let the
     entry itself flow. */
  .panel, .step, .case-title, blockquote, .answer { break-inside: avoid; }
  .entry, section { break-inside: auto; }
  h1, h2, h3 { break-after: avoid; }

  /* Tables scroll on screen; on paper they must simply fit. */
  .scroller { overflow: visible !important; }
  table.plain { min-width: 0 !important; }

  /* The matrix must stay a MATRIX on paper.
     Printed landscape Letter gives ~700px of content width, which is BELOW the
     900px breakpoint where the page collapses to stacked cards for phones. On
     screen that is right; on paper it destroys the whole point of the document,
     which is four columns read across. So the table layout is re-asserted here
     and the type is stepped down to fit four columns in the width available. */
  /* table-layout:fixed is required, not cosmetic. Under automatic layout the
     browser sizes columns by content, and the Case column's pull-quote is one
     long unbroken line, so it claimed over half the sheet and squeezed the four
     columns that carry the actual answer into unreadable ribbons. Fixed layout
     makes the colgroup percentages authoritative. */
  table.matrix { display: table !important; table-layout: fixed !important;
                 min-width: 0 !important; width: 100% !important; }
  table.matrix col.c-case  { width: 16% !important; }
  table.matrix col.c-input { width: 19% !important; }
  table.matrix col.c-meth  { width: 19% !important; }
  table.matrix col.c-diag  { width: 27% !important; }
  table.matrix col.c-fwd   { width: 19% !important; }
  table.matrix thead { display: table-header-group !important; }
  table.matrix tbody { display: table-row-group !important; }
  table.matrix tr    { display: table-row !important; }
  table.matrix td    { display: table-cell !important; }
  table.matrix td[data-col]::before { content: none !important; }
  table.matrix tr {
    border-bottom: 0 !important;
    padding: 0 !important;
  }
  table.matrix td {
    border-bottom: 1px solid var(--rule) !important;
    padding: 11px 11px 11px 0 !important;
    font-size: 10.5px !important;
    line-height: 1.42 !important;
  }
  table.matrix th { font-size: 9px !important; }
  table.matrix .case-title { font-size: 11.5px !important; line-height: 1.2 !important; }
  table.matrix .case-meta  { font-size: 8px !important; }
  table.matrix .said       { font-size: 9.5px !important; }
  table.matrix .tag        { font-size: 7.5px !important; }
  table.matrix ul          { padding-left: 13px !important; gap: 3px !important; }

  /* A matrix row can be taller than a landscape page; let it split rather than
     overflow off the sheet. The repeating header keeps a split row readable. */
  table.matrix tr { break-inside: auto; }

  /* Links are dead on paper — keep them readable, drop the affordance. */
  a { text-decoration: none; color: inherit; }
"""


def find_chrome() -> str:
    """Locate a Chrome/Chromium binary, or explain what to install."""
    for path in CHROME_CANDIDATES:
        if Path(path).exists():
            return path
    found = shutil.which("google-chrome") or shutil.which("chromium")
    if found:
        return found
    raise SystemExit(
        "No Chrome or Chromium found. Checked:\n  "
        + "\n  ".join(CHROME_CANDIDATES)
        + "\nInstall Chrome, or render the HTML manually with Print to PDF."
    )


def wrap(fragment: str, doc: Document) -> str:
    """Wrap an artifact fragment into a standalone, light-themed print document."""
    title = "AceEngineer"
    if "<title>" in fragment:
        title = fragment.split("<title>", 1)[1].split("</title>", 1)[0]
    return (
        "<!doctype html>\n"
        '<html lang="en" data-theme="light">\n'
        "<head>\n"
        '<meta charset="utf-8">\n'
        f"<title>{title}</title>\n"
        f"<style>{PRINT_CSS.replace('__PAGE_SIZE__', doc.page_size)}</style>\n"
        "</head>\n"
        f"<body>\n{fragment}\n</body>\n</html>\n"
    )


def build(doc: Document, chrome: str) -> Path:
    """Render one document to PDF. Returns the output path."""
    src = HERE / doc.source
    if not src.exists():
        raise SystemExit(f"Missing source: {src}")

    OUT_DIR.mkdir(parents=True, exist_ok=True)
    out = OUT_DIR / doc.output

    with tempfile.TemporaryDirectory() as tmp:
        tmp_path = Path(tmp)
        page = tmp_path / "page.html"
        page.write_text(wrap(src.read_text(), doc), encoding="utf-8")

        cmd = [
            chrome,
            "--headless",
            "--disable-gpu",
            "--no-sandbox",
            "--disable-extensions",
            "--disable-background-networking",
            f"--user-data-dir={tmp_path / 'profile'}",
            "--no-pdf-header-footer",
            "--virtual-time-budget=4000",
            f"--print-to-pdf={out}",
            page.as_uri(),
        ]

        if out.exists():
            out.unlink()

        # Headless Chrome on macOS reliably WRITES the PDF and then fails to
        # exit, so waiting for a clean return code hangs forever. Treat the
        # timeout as expected: kill it, then judge success by the artefact on
        # disk rather than by the exit status.
        stderr = ""
        try:
            proc = subprocess.run(cmd, capture_output=True, text=True, timeout=45)
            stderr = proc.stderr
        except subprocess.TimeoutExpired as exc:
            stderr = (exc.stderr or b"").decode(errors="replace") if exc.stderr else ""

    if not out.exists() or out.stat().st_size == 0:
        raise SystemExit(
            f"Chrome produced no PDF for {doc.source}.\nstderr: {stderr}"
        )
    return out


def main() -> None:
    wanted = sys.argv[1:]
    docs = [d for d in DOCUMENTS if not wanted or d.key in wanted]
    if not docs:
        raise SystemExit(
            f"Unknown document(s): {', '.join(wanted)}. "
            f"Known: {', '.join(d.key for d in DOCUMENTS)}"
        )

    chrome = find_chrome()
    print(f"Renderer: {chrome}\n")

    for doc in docs:
        out = build(doc, chrome)
        size_kb = out.stat().st_size / 1024
        orient = "landscape" if doc.landscape else "portrait"
        print(f"  {out.name}")
        print(f"    {size_kb:,.0f} KB · {orient}")
        print(f"    Send to: {doc.audience}\n")


if __name__ == "__main__":
    main()
