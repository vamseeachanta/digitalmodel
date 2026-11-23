# ABOUTME: Post-processes the ship-plate buckling HTML report into a fully self-contained file.
# ABOUTME: Inlines plotly.js (replacing the CDN <script src>) so the report opens offline / via email.

"""Make the buckling report self-contained.

The GTM report template loads plotly.js from the CDN, which needs internet on
first open. For sharing with management offline (email, USB, no-network rooms)
we inline plotly's bundled minified JS instead.

Usage:
    .venv/bin/python examples/demos/structural/make_standalone_report.py
"""

from __future__ import annotations

from pathlib import Path

from plotly.offline import get_plotlyjs

HERE = Path(__file__).resolve().parent
SRC = HERE / "output" / "ship_plate_buckling_report.html"
DST = HERE / "output" / "ship_plate_buckling_report_standalone.html"

CDN_TAG = '<script src="https://cdn.plot.ly/plotly-2.32.0.min.js"></script>'


def main() -> None:
    html = SRC.read_text(encoding="utf-8")
    if CDN_TAG not in html:
        raise SystemExit(f"CDN plotly tag not found in {SRC}; template may have changed.")
    inline = f"<script>{get_plotlyjs()}</script>"
    html = html.replace(CDN_TAG, inline)
    DST.write_text(html, encoding="utf-8")
    print(f"Wrote {DST} ({DST.stat().st_size / 1_048_576:.2f} MB, self-contained)")


if __name__ == "__main__":
    main()
