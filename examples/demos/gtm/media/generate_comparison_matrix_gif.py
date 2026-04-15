#!/usr/bin/env python3.12
"""Generate comparison matrix GIF from vessel + structure comparison JSON data.

Renders an HTML summary page of the cross-demo comparison matrices and
captures an animated scroll-through GIF using Playwright + Pillow.

Usage:
    cd digitalmodel
    python examples/demos/gtm/media/generate_comparison_matrix_gif.py
"""

import json
import math
import tempfile
from pathlib import Path
from io import BytesIO

from PIL import Image
from playwright.sync_api import sync_playwright

SCRIPT_DIR = Path(__file__).resolve().parent
RESULTS_DIR = SCRIPT_DIR.parent / "results"
OUTPUT_GIF = SCRIPT_DIR / "demo_comparison_matrix.gif"

WIDTH = 1024
HEIGHT = 640
FRAME_DURATION_MS = 120  # ms per frame in GIF
SCROLL_STEP = 80  # pixels per scroll step
PAUSE_FRAMES = 8  # extra frames at top/bottom for readability


def load_json(name: str) -> dict:
    path = RESULTS_DIR / name
    with open(path) as f:
        return json.load(f)


def go_nogo_color(val: str) -> str:
    val = str(val).upper().strip()
    if val == "GO":
        return "background:#22c55e;color:#fff;font-weight:700;"
    elif val == "NO_GO":
        return "background:#ef4444;color:#fff;font-weight:700;"
    elif val == "MARGINAL":
        return "background:#f59e0b;color:#000;font-weight:700;"
    return ""


def build_vessel_matrix_html(vessel_data: dict) -> str:
    """Build HTML for vessel capability comparison."""
    html_parts = []
    html_parts.append('<div class="section"><h2>Vessel Capability Matrix</h2>')
    html_parts.append(f'<p class="desc">{vessel_data.get("_description", "")}</p>')

    for vessel in vessel_data.get("vessels", []):
        name = vessel["name"]
        vtype = vessel.get("type", "")
        score = vessel.get("overall_score", {})
        pass_rate = score.get("pass_rate_pct", "N/A")
        strongest = score.get("strongest_capability", "").replace("_", " ").title()
        weakest = score.get("weakest_capability", "").replace("_", " ").title()

        html_parts.append(f'<div class="vessel-card">')
        html_parts.append(f'<h3>{name} <span class="vtype">({vtype})</span></h3>')
        html_parts.append(
            f'<div class="score-bar">'
            f'<span>Pass rate: <b>{pass_rate}%</b></span> · '
            f'<span>Strongest: {strongest}</span> · '
            f'<span>Weakest: {weakest}</span>'
            f'</div>'
        )

        for cap_name, cap_data in vessel.get("capabilities", {}).items():
            matrix = cap_data.get("go_nogo_matrix", {})
            if not matrix:
                continue
            cap_label = cap_name.replace("_", " ").title()
            html_parts.append(f'<h4>{cap_label}</h4>')

            # Get all depth columns
            first_key = next(iter(matrix))
            depths = list(matrix[first_key].keys())

            html_parts.append('<table><thead><tr><th>Structure</th>')
            for d in depths:
                html_parts.append(f'<th>{d}</th>')
            html_parts.append('</tr></thead><tbody>')

            for struct_name, depth_results in matrix.items():
                html_parts.append(f'<tr><td class="struct-name">{struct_name}</td>')
                for d in depths:
                    val = depth_results.get(d, "—")
                    style = go_nogo_color(val)
                    label = str(val).replace("_", " ")
                    html_parts.append(f'<td style="{style}">{label}</td>')
                html_parts.append('</tr>')

            html_parts.append('</tbody></table>')

        html_parts.append('</div>')

    # Head-to-head
    h2h = vessel_data.get("head_to_head", {})
    if h2h:
        html_parts.append('<div class="h2h"><h3>Head-to-Head Comparisons</h3>')
        for group_name, comparisons in h2h.items():
            if group_name.startswith("_"):
                continue
            for matchup, details in comparisons.items():
                html_parts.append(f'<div class="matchup"><h4>{matchup}</h4>')
                summary = details.get("summary", "")
                if summary:
                    html_parts.append(f'<p>{summary}</p>')
                html_parts.append('</div>')
        html_parts.append('</div>')

    html_parts.append('</div>')
    return "\n".join(html_parts)


def build_structure_matrix_html(struct_data: dict) -> str:
    """Build HTML for structure comparison."""
    html_parts = []
    html_parts.append('<div class="section"><h2>Structure Comparison Matrix</h2>')
    html_parts.append(f'<p class="desc">{struct_data.get("_description", "")}</p>')

    for comp in struct_data.get("comparisons", []):
        category = comp.get("category", "").replace("_", " ").title()
        demo_src = comp.get("demo_source", "")
        structures = comp.get("structures", [])

        html_parts.append(f'<div class="comp-card"><h3>{category} <span class="vtype">({demo_src})</span></h3>')

        # Render by_vessel tables if present
        by_vessel = comp.get("by_vessel", {})
        if by_vessel:
            for vessel_name, vessel_info in by_vessel.items():
                html_parts.append(f'<h4>{vessel_name}</h4>')

                # Pass count table
                pass_counts = vessel_info.get("pass_count_by_structure", {})
                if pass_counts and any(v is not None for v in pass_counts.values()):
                    html_parts.append('<table><thead><tr><th>Structure</th><th>Cases Passed</th><th>Status</th></tr></thead><tbody>')
                    for s, count in pass_counts.items():
                        if count is None:
                            count = 0
                        status = "GO" if count > 0 else "NO GO"
                        style = go_nogo_color(status.replace(" ", "_"))
                        html_parts.append(f'<tr><td>{s}</td><td>{count}</td><td style="{style}">{status}</td></tr>')
                    html_parts.append('</tbody></table>')

                # Max depth table
                max_depths = vessel_info.get("max_depth_by_structure", {})
                if max_depths and any(v is not None for v in max_depths.values()):
                    html_parts.append('<table><thead><tr><th>Structure</th><th>Max Depth (m)</th></tr></thead><tbody>')
                    for s, depth in max_depths.items():
                        val = f"{depth}" if depth is not None else "—"
                        html_parts.append(f'<tr><td>{s}</td><td>{val}</td></tr>')
                    html_parts.append('</tbody></table>')

                # Max pipe size by depth
                mps = vessel_info.get("max_pipe_size_by_depth", {})
                if mps:
                    html_parts.append('<table><thead><tr><th>Depth</th><th>Max Pipe Size</th></tr></thead><tbody>')
                    for depth, size in mps.items():
                        val = size if size else "—"
                        html_parts.append(f'<tr><td>{depth}</td><td>{val}</td></tr>')
                    html_parts.append('</tbody></table>')

        # Render freespan-specific tables
        max_span = comp.get("max_allowable_span_by_current", {})
        if max_span:
            html_parts.append('<h4>Max Allowable Span by Current</h4>')
            currents = list(next(iter(max_span.values())).keys())
            html_parts.append('<table><thead><tr><th>Pipe</th>')
            for c in currents:
                html_parts.append(f'<th>{c}</th>')
            html_parts.append('</tr></thead><tbody>')
            for pipe, curr_data in max_span.items():
                html_parts.append(f'<tr><td class="struct-name">{pipe}</td>')
                for c in currents:
                    val = curr_data.get(c, "—")
                    html_parts.append(f'<td>{val}</td>')
                html_parts.append('</tr>')
            html_parts.append('</tbody></table>')

        html_parts.append('</div>')

    html_parts.append('</div>')
    return "\n".join(html_parts)


def build_html(vessel_data: dict, struct_data: dict) -> str:
    """Build complete HTML page."""
    vessel_html = build_vessel_matrix_html(vessel_data)
    struct_html = build_structure_matrix_html(struct_data)

    return f"""<!DOCTYPE html>
<html><head><meta charset="utf-8">
<style>
* {{ margin: 0; padding: 0; box-sizing: border-box; }}
body {{
    font-family: -apple-system, 'Segoe UI', Roboto, sans-serif;
    background: #0f172a; color: #e2e8f0; padding: 32px;
    width: {WIDTH}px;
}}
h1 {{ font-size: 28px; margin-bottom: 4px; color: #38bdf8; }}
.subtitle {{ color: #94a3b8; font-size: 14px; margin-bottom: 24px; }}
h2 {{ font-size: 22px; color: #38bdf8; margin: 24px 0 12px; border-bottom: 2px solid #1e293b; padding-bottom: 6px; }}
h3 {{ font-size: 18px; color: #f1f5f9; margin: 16px 0 8px; }}
h4 {{ font-size: 15px; color: #cbd5e1; margin: 12px 0 6px; }}
.vtype {{ color: #64748b; font-weight: 400; font-size: 14px; }}
.desc {{ color: #94a3b8; font-size: 13px; margin-bottom: 16px; line-height: 1.5; }}
.section {{ margin-bottom: 32px; }}
.vessel-card, .comp-card {{ background: #1e293b; border-radius: 8px; padding: 16px; margin-bottom: 16px; }}
.score-bar {{ font-size: 13px; color: #94a3b8; margin-bottom: 12px; }}
.score-bar b {{ color: #38bdf8; }}
table {{ width: 100%; border-collapse: collapse; margin: 8px 0 16px; font-size: 13px; }}
th {{ background: #334155; color: #e2e8f0; padding: 8px 10px; text-align: center; font-weight: 600; }}
td {{ padding: 6px 10px; text-align: center; border-bottom: 1px solid #334155; }}
.struct-name {{ text-align: left; font-weight: 600; color: #f1f5f9; }}
tbody tr:hover {{ background: #334155; }}
.h2h {{ background: #1e293b; border-radius: 8px; padding: 16px; margin-top: 16px; }}
.matchup {{ margin-bottom: 12px; }}
.matchup p {{ color: #94a3b8; font-size: 13px; line-height: 1.5; }}
.footer {{ text-align: center; color: #475569; font-size: 12px; margin-top: 32px; padding-top: 16px; border-top: 1px solid #1e293b; }}
</style></head><body>
<h1>GTM Demo Suite — Comparison Matrix</h1>
<p class="subtitle">Cross-demo vessel and structure capability comparison · 5 demos · 1,292 cases</p>
{vessel_html}
{struct_html}
<div class="footer">digitalmodel · GTM Demo Suite · Generated from validated results</div>
</body></html>"""


def capture_scroll_frames(page, total_height: int) -> list[Image.Image]:
    """Capture frames while scrolling down the page."""
    frames = []

    # Pause at top
    for _ in range(PAUSE_FRAMES):
        png = page.screenshot(clip={"x": 0, "y": 0, "width": WIDTH, "height": HEIGHT})
        frames.append(Image.open(BytesIO(png)).convert("RGB"))

    # Scroll through
    max_scroll = total_height - HEIGHT
    if max_scroll <= 0:
        return frames

    num_steps = math.ceil(max_scroll / SCROLL_STEP)
    for i in range(num_steps + 1):
        y = min(i * SCROLL_STEP, max_scroll)
        png = page.screenshot(clip={"x": 0, "y": y, "width": WIDTH, "height": HEIGHT})
        frames.append(Image.open(BytesIO(png)).convert("RGB"))

    # Pause at bottom
    for _ in range(PAUSE_FRAMES):
        png = page.screenshot(
            clip={"x": 0, "y": max_scroll, "width": WIDTH, "height": HEIGHT}
        )
        frames.append(Image.open(BytesIO(png)).convert("RGB"))

    return frames


def frames_to_gif(frames: list[Image.Image], output_path: Path):
    """Convert RGB frames to a palette-mode animated GIF."""
    palette_frames = [f.quantize(colors=256, method=Image.Quantize.MEDIANCUT) for f in frames]
    palette_frames[0].save(
        output_path,
        save_all=True,
        append_images=palette_frames[1:],
        duration=FRAME_DURATION_MS,
        loop=0,
        optimize=True,
    )


def main():
    vessel_data = load_json("vessel_comparison_matrix.json")
    struct_data = load_json("structure_comparison_matrix.json")
    html_content = build_html(vessel_data, struct_data)

    # Write HTML to temp file for Playwright
    with tempfile.NamedTemporaryFile(suffix=".html", delete=False, mode="w") as f:
        f.write(html_content)
        html_path = f.name

    print(f"Rendering HTML from {html_path} ...")

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": WIDTH, "height": HEIGHT})
        page.goto(f"file://{html_path}")
        page.wait_for_load_state("networkidle")

        # Get full page height
        total_height = page.evaluate("document.body.scrollHeight")
        print(f"Page height: {total_height}px, viewport: {WIDTH}x{HEIGHT}")

        # Use full_page screenshot for Playwright, then slice into frames
        full_png = page.screenshot(full_page=True)
        browser.close()

    full_img = Image.open(BytesIO(full_png)).convert("RGB")
    actual_width, actual_height = full_img.size
    print(f"Full screenshot: {actual_width}x{actual_height}")

    # Slice into scroll frames
    frames = []
    max_scroll = actual_height - HEIGHT

    # Pause at top
    for _ in range(PAUSE_FRAMES):
        frames.append(full_img.crop((0, 0, WIDTH, HEIGHT)))

    if max_scroll > 0:
        num_steps = math.ceil(max_scroll / SCROLL_STEP)
        for i in range(num_steps + 1):
            y = min(i * SCROLL_STEP, max_scroll)
            frames.append(full_img.crop((0, y, WIDTH, y + HEIGHT)))

    # Pause at bottom
    for _ in range(PAUSE_FRAMES):
        y = max(0, actual_height - HEIGHT)
        frames.append(full_img.crop((0, y, WIDTH, y + HEIGHT)))

    print(f"Captured {len(frames)} frames")
    frames_to_gif(frames, OUTPUT_GIF)
    size_kb = OUTPUT_GIF.stat().st_size / 1024
    print(f"Saved: {OUTPUT_GIF} ({size_kb:.0f} KB, {len(frames)} frames)")

    # Clean up
    Path(html_path).unlink(missing_ok=True)


if __name__ == "__main__":
    main()
