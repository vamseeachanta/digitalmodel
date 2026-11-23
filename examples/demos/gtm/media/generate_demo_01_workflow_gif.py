#!/usr/bin/env python3.12
"""Generate Demo 1 end-to-end workflow GIF.

Composes a multi-scene animated GIF that illustrates the full workflow:
  Scene 1 — Natural-language prompt (user intent)
  Scene 2 — Repo/command setup (how to run it)
  Scene 3 — Analysis execution (terminal output)
  Scene 4 — Report generation complete (output artifacts)
  Scene 5 — Browser walkthrough (scroll through validated HTML report)

Uses Playwright for rendering + Pillow for GIF assembly, same stack as
the Demo 2 workflow GIF generator.

Usage:
    cd digitalmodel
    uv run python examples/demos/gtm/media/generate_demo_01_workflow_gif.py
"""

import math
import tempfile
from io import BytesIO
from pathlib import Path

from PIL import Image
from playwright.sync_api import sync_playwright

SCRIPT_DIR = Path(__file__).resolve().parent
REPORT_HTML = SCRIPT_DIR.parent / "output" / "demo_01_freespan_report.html"
OUTPUT_GIF = SCRIPT_DIR / "demo_01_freespan_workflow.gif"

WIDTH = 1024
HEIGHT = 640
FRAME_DURATION_MS = 120
SCROLL_STEP = 80
SCENE_HOLD_FRAMES = 12  # hold each slide scene ~1.4 s
REPORT_PAUSE_FRAMES = 6  # shorter pause at top/bottom of report scroll
TRANSITION_FRAMES = 3  # brief blank between scenes

# -- Shared CSS ----------------------------------------------------------------

_BASE_CSS = f"""
* {{ margin: 0; padding: 0; box-sizing: border-box; }}
body {{
    font-family: 'SF Mono', 'Fira Code', 'Cascadia Code', 'Consolas', monospace;
    background: #0c1021; color: #e2e8f0;
    width: {WIDTH}px; height: {HEIGHT}px;
    overflow: hidden;
}}
.scene {{
    width: {WIDTH}px; height: {HEIGHT}px;
    display: flex; flex-direction: column;
    justify-content: center; align-items: center;
    padding: 48px 64px;
}}
.badge {{
    display: inline-block;
    background: #38bdf8; color: #0c1021;
    font-size: 11px; font-weight: 700;
    letter-spacing: 2px; text-transform: uppercase;
    padding: 4px 14px; border-radius: 4px;
    margin-bottom: 20px;
}}
.title {{
    font-family: -apple-system, 'Segoe UI', Roboto, sans-serif;
    font-size: 26px; font-weight: 700; color: #f1f5f9;
    margin-bottom: 12px; text-align: center;
}}
.subtitle {{
    font-family: -apple-system, 'Segoe UI', Roboto, sans-serif;
    font-size: 14px; color: #94a3b8; text-align: center;
    margin-bottom: 28px; line-height: 1.5;
}}
.terminal {{
    background: #1a1e2e; border: 1px solid #334155;
    border-radius: 8px; padding: 20px 24px;
    width: 100%; max-width: 880px;
    font-size: 14px; line-height: 1.7;
}}
.terminal .prompt {{ color: #22c55e; }}
.terminal .cmd {{ color: #f1f5f9; }}
.terminal .comment {{ color: #64748b; }}
.terminal .output {{ color: #94a3b8; }}
.terminal .highlight {{ color: #38bdf8; font-weight: 600; }}
.terminal .success {{ color: #22c55e; font-weight: 600; }}
.terminal .accent {{ color: #f59e0b; }}
.progress-row {{ display: flex; align-items: center; gap: 12px; margin: 6px 0; }}
.progress-bar {{
    flex: 1; height: 14px; background: #334155; border-radius: 7px;
    overflow: hidden;
}}
.progress-fill {{
    height: 100%; background: linear-gradient(90deg, #22c55e, #38bdf8);
    border-radius: 7px;
}}
.step-indicator {{
    position: absolute; top: 20px; right: 32px;
    font-family: -apple-system, 'Segoe UI', Roboto, sans-serif;
    font-size: 12px; color: #475569;
}}
.watermark {{
    position: absolute; bottom: 16px; left: 0; right: 0;
    text-align: center;
    font-family: -apple-system, 'Segoe UI', Roboto, sans-serif;
    font-size: 11px; color: #334155; letter-spacing: 1px;
}}
"""


# -- Scene builders ------------------------------------------------------------

def _wrap(body: str, step: str) -> str:
    return f"""<!DOCTYPE html><html><head><meta charset="utf-8">
<style>{_BASE_CSS}</style></head><body>
<div class="scene" style="position:relative;">
  <span class="step-indicator">{step}</span>
  {body}
  <span class="watermark">digitalmodel · GTM Demo Suite</span>
</div></body></html>"""


def scene_prompt() -> str:
    body = """
    <span class="badge">Step 1 — Prompt</span>
    <div class="title">Natural-Language Request</div>
    <div class="subtitle">
        The engineer describes the analysis goal in plain English.<br>
        The agent resolves it to the correct demo and parameters.
    </div>
    <div class="terminal">
        <span class="prompt">user@ace-linux ~/digitalmodel $</span><br>
        <span class="comment"># Ask the AI agent for a freespan VIV screening</span><br><br>
        <span class="accent">▶</span>
        <span class="cmd"> "Run a DNV-RP-F105 freespan VIV screening</span><br>
        <span class="cmd">&nbsp;&nbsp;for 3 pipeline sizes (8″, 12″, 16″) plus an 8″ rigid jumper.</span><br>
        <span class="cmd">&nbsp;&nbsp;Sweep 8 span lengths, 5 current speeds, and 4–5 gap ratios.</span><br>
        <span class="cmd">&nbsp;&nbsp;Generate an HTML report with interactive charts."</span>
    </div>
    """
    return _wrap(body, "1 / 5")


def scene_setup() -> str:
    body = """
    <span class="badge">Step 2 — Setup</span>
    <div class="title">Repository &amp; Command Resolution</div>
    <div class="subtitle">
        The agent maps the request to the Demo 1 script and resolves dependencies.
    </div>
    <div class="terminal">
        <span class="prompt">agent $</span>
        <span class="output"> Resolving analysis …</span><br>
        <span class="output">  → Script: </span>
        <span class="highlight">examples/demos/gtm/demo_01_dnv_freespan_viv.py</span><br>
        <span class="output">  → Code:   DNV-RP-F105 (Freespan / VIV)</span><br>
        <span class="output">  → Pipes:  8″ 12″ 16″ (pipeline) + 8″ (jumper)</span><br>
        <span class="output">  → Cases:  </span>
        <span class="highlight">680</span>
        <span class="output"> (480 pipeline + 200 jumper)</span><br><br>
        <span class="prompt">agent $</span>
        <span class="cmd"> PYTHONPATH=examples/demos/gtm:src \\</span><br>
        <span class="cmd">&nbsp;&nbsp;uv run python \\</span><br>
        <span class="cmd">&nbsp;&nbsp;examples/demos/gtm/demo_01_dnv_freespan_viv.py</span>
    </div>
    """
    return _wrap(body, "2 / 5")


def scene_execution() -> str:
    body = """
    <span class="badge">Step 3 — Execution</span>
    <div class="title">Parametric VIV Screening Running</div>
    <div class="subtitle">
        680 freespan cases evaluated per DNV-RP-F105 simplified methodology.
    </div>
    <div class="terminal">
        <span class="output">[INFO] Loading pipeline data … 3 sizes + 1 jumper</span><br>
        <span class="output">[INFO] Computing natural frequencies …</span><br>
        <span class="output">[INFO] Evaluating in-line VIV onset …</span><br>
        <span class="output">[INFO] Evaluating cross-flow VIV onset …</span><br>
        <span class="output">[INFO] Screening pass/fail against V<sub>R,onset</sub> …</span><br>
        <span class="output">[INFO] Computing max allowable spans …</span><br><br>
        <div class="progress-row">
            <span class="output" style="min-width:100px">Pipeline:</span>
            <div class="progress-bar"><div class="progress-fill" style="width:100%"></div></div>
            <span class="success">480 / 480</span>
        </div>
        <div class="progress-row">
            <span class="output" style="min-width:100px">Jumper:</span>
            <div class="progress-bar"><div class="progress-fill" style="width:100%"></div></div>
            <span class="success">200 / 200</span>
        </div>
        <div class="progress-row">
            <span class="output" style="min-width:100px">Charts:</span>
            <div class="progress-bar"><div class="progress-fill" style="width:100%"></div></div>
            <span class="success">5 / 5</span>
        </div><br>
        <span class="success">✓ 680 cases screened</span>
        <span class="output"> — VIV onset mapped for all span/current/gap-ratio combinations</span>
    </div>
    """
    return _wrap(body, "3 / 5")


def scene_report_gen() -> str:
    body = """
    <span class="badge">Step 4 — Report</span>
    <div class="title">Report &amp; Artifacts Generated</div>
    <div class="subtitle">
        Branded HTML report with 5 interactive Plotly charts, plus JSON for downstream tools.
    </div>
    <div class="terminal">
        <span class="success">✓</span>
        <span class="output"> HTML report written:</span><br>
        <span class="highlight">&nbsp;&nbsp;examples/demos/gtm/output/demo_01_freespan_report.html</span><br><br>
        <span class="success">✓</span>
        <span class="output"> JSON results cached:</span><br>
        <span class="highlight">&nbsp;&nbsp;examples/demos/gtm/results/demo_01_freespan_results.json</span><br><br>
        <span class="output">Artifacts:</span><br>
        <span class="output">&nbsp;&nbsp;📊 Chart 1 — Natural Frequency vs Span Length</span><br>
        <span class="output">&nbsp;&nbsp;📊 Chart 2 — VIV Onset Screening Map</span><br>
        <span class="output">&nbsp;&nbsp;📊 Chart 3 — Max Allowable Span Heatmap</span><br>
        <span class="output">&nbsp;&nbsp;📊 Chart 4 — Pass/Fail Screening Matrix</span><br>
        <span class="output">&nbsp;&nbsp;📊 Chart 5 — Jumper vs Pipeline Comparison</span><br><br>
        <span class="prompt">agent $</span>
        <span class="cmd"> xdg-open examples/demos/gtm/output/demo_01_freespan_report.html</span>
    </div>
    """
    return _wrap(body, "4 / 5")


def scene_browser_label() -> str:
    """Transition slide before the report scroll-through."""
    body = """
    <span class="badge">Step 5 — Walkthrough</span>
    <div class="title">Browser Report Preview</div>
    <div class="subtitle">
        Scrolling through the validated HTML report.<br>
        Interactive charts, screening tables, and full parameter matrix.
    </div>
    """
    return _wrap(body, "5 / 5")


# -- Frame capture helpers -----------------------------------------------------

def render_slide(page, html: str) -> Image.Image:
    """Render a single-viewport HTML slide and return as PIL Image."""
    with tempfile.NamedTemporaryFile(suffix=".html", delete=False, mode="w") as f:
        f.write(html)
        path = f.name
    page.goto(f"file://{path}")
    page.wait_for_load_state("networkidle")
    png = page.screenshot(clip={"x": 0, "y": 0, "width": WIDTH, "height": HEIGHT})
    Path(path).unlink(missing_ok=True)
    return Image.open(BytesIO(png)).convert("RGB")


def capture_report_scroll(page) -> list[Image.Image]:
    """Load the real Demo 1 report and capture scroll-through frames."""
    page.goto(f"file://{REPORT_HTML}", wait_until="domcontentloaded")
    # Give Plotly charts time to render (CDN load + JS execution)
    page.wait_for_timeout(5000)

    full_png = page.screenshot(full_page=True)
    full_img = Image.open(BytesIO(full_png)).convert("RGB")
    actual_w, actual_h = full_img.size

    frames: list[Image.Image] = []
    max_scroll = actual_h - HEIGHT

    # Pause at top
    for _ in range(REPORT_PAUSE_FRAMES):
        frames.append(full_img.crop((0, 0, WIDTH, HEIGHT)))

    # Scroll through
    if max_scroll > 0:
        num_steps = math.ceil(max_scroll / SCROLL_STEP)
        for i in range(num_steps + 1):
            y = min(i * SCROLL_STEP, max_scroll)
            frames.append(full_img.crop((0, y, WIDTH, y + HEIGHT)))

    # Pause at bottom
    for _ in range(REPORT_PAUSE_FRAMES):
        y = max(0, actual_h - HEIGHT)
        frames.append(full_img.crop((0, y, WIDTH, y + HEIGHT)))

    return frames


def make_transition(width: int = WIDTH, height: int = HEIGHT) -> Image.Image:
    """A dark blank frame used as a brief scene transition."""
    return Image.new("RGB", (width, height), (12, 16, 33))  # matches #0c1021


# -- GIF assembly --------------------------------------------------------------

def frames_to_gif(frames: list[Image.Image], output_path: Path):
    """Convert RGB frames to a palette-mode animated GIF."""
    palette_frames = [
        f.quantize(colors=256, method=Image.Quantize.MEDIANCUT)
        for f in frames
    ]
    palette_frames[0].save(
        output_path,
        save_all=True,
        append_images=palette_frames[1:],
        duration=FRAME_DURATION_MS,
        loop=0,
        optimize=True,
    )


# -- Main ----------------------------------------------------------------------

def main():
    if not REPORT_HTML.exists():
        raise FileNotFoundError(
            f"Validated report not found: {REPORT_HTML}\n"
            "Run Demo 1 first (or use --from-cache) to generate it."
        )

    scenes_html = [
        ("Prompt", scene_prompt()),
        ("Setup", scene_setup()),
        ("Execution", scene_execution()),
        ("Report", scene_report_gen()),
        ("Walkthrough label", scene_browser_label()),
    ]

    all_frames: list[Image.Image] = []
    transition = make_transition()

    with sync_playwright() as p:
        browser = p.chromium.launch()
        page = browser.new_page(viewport={"width": WIDTH, "height": HEIGHT})

        # Render slide scenes
        for name, html in scenes_html:
            print(f"Rendering scene: {name}")
            slide = render_slide(page, html)
            # Hold the slide
            for _ in range(SCENE_HOLD_FRAMES):
                all_frames.append(slide)
            # Brief transition
            for _ in range(TRANSITION_FRAMES):
                all_frames.append(transition)

        # Report scroll-through (the real walkthrough)
        print("Capturing report scroll-through …")
        report_frames = capture_report_scroll(page)
        all_frames.extend(report_frames)

        browser.close()

    print(f"Total frames: {len(all_frames)}")
    frames_to_gif(all_frames, OUTPUT_GIF)
    size_kb = OUTPUT_GIF.stat().st_size / 1024
    print(f"Saved: {OUTPUT_GIF} ({size_kb:.0f} KB, {len(all_frames)} frames)")


if __name__ == "__main__":
    main()
