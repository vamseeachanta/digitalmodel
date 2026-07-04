"""Shared house-style pieces for the interactive CFD verification reports.

Keeps the suite's existing CSS (extracted from the current reports) and adds:
- Plotly (CDN, same delivery as the rudder explorer)
- a self-contained animation "flipbook" component (base64 JPEG frames +
  play/pause + scrub slider), used for the VIV vortex street and the
  dam-break collapse.
"""
import base64
import io
import json
import re
from pathlib import Path

PLOTLY_CDN = '<script src="https://cdn.plot.ly/plotly-2.35.2.min.js" charset="utf-8"></script>'

_STYLE_EXTRA = """
<style>
.plotbox{border:1px solid var(--line);border-radius:6px;margin:14px 0;padding:4px;background:#fff}
.anim{border:1px solid var(--line);border-radius:6px;margin:14px 0;background:#fff;overflow:hidden}
.anim img{display:block;width:100%;height:auto}
.anim-controls{display:flex;gap:10px;align-items:center;padding:8px 12px;border-top:1px solid var(--line);background:var(--bg)}
.anim-controls button{border:1px solid var(--line);background:#fff;border-radius:4px;padding:3px 12px;cursor:pointer;font-size:1rem}
.anim-controls input[type=range]{flex:1}
.anim-controls .tlabel{font-size:.85rem;color:var(--mut);min-width:110px;text-align:right;font-variant-numeric:tabular-nums}
.usecase{background:#f2f7ee;border:1px solid #cfe3c2;border-radius:8px;padding:16px 20px;margin:16px 0}
.usecase h3{margin-top:0;color:#3a6b1f}
.calc{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));gap:10px;margin:12px 0}
.calc label{font-size:.72rem;text-transform:uppercase;letter-spacing:.05em;color:var(--mut);display:block}
.calc input{width:100%;padding:6px 8px;border:1px solid var(--line);border-radius:4px;font-size:.95rem}
.calc .out{background:var(--bg);border:1px solid var(--line);border-radius:6px;padding:8px 10px}
.calc .out b{display:block;font-size:.68rem;text-transform:uppercase;color:var(--mut)}
.calc .out span{font-weight:700;font-size:1.05rem}
.bad{color:#d1242f;font-weight:700}
.badge.warn{background:var(--warn)}
</style>
"""


def house_style(repo_root: Path) -> str:
    """The suite's existing <style> block + interactive-component additions."""
    src = (repo_root / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text()
    style = re.search(r"<style>.*?</style>", src, re.S).group(0)
    return style + _STYLE_EXTRA


def png_img(path: Path, alt: str) -> str:
    b64 = base64.b64encode(path.read_bytes()).decode()
    return (f'<img src="data:image/png;base64,{b64}" alt="{alt}" '
            'style="max-width:100%;border:1px solid var(--line);border-radius:6px;margin:10px 0">')


def fig_to_jpeg_b64(fig, quality=70) -> str:
    buf = io.BytesIO()
    fig.savefig(buf, format="jpg", dpi=110, pil_kwargs={"quality": quality})
    return base64.b64encode(buf.getvalue()).decode()


def plot_div(div_id: str, traces: list, layout: dict, height: int = 460) -> str:
    """A Plotly chart div + inline data. Traces/layout are plain dicts."""
    layout = {
        "margin": {"l": 60, "r": 20, "t": 40, "b": 55},
        "font": {"family": "-apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif",
                 "color": "#1a2332", "size": 13},
        "paper_bgcolor": "#ffffff", "plot_bgcolor": "#ffffff",
        "hovermode": "closest",
        **layout,
    }
    cfg = {"responsive": True, "displaylogo": False,
           "modeBarButtonsToRemove": ["select2d", "lasso2d", "autoScale2d"]}
    return (
        f'<div class="plotbox"><div id="{div_id}" style="height:{height}px"></div></div>\n'
        f"<script>Plotly.newPlot('{div_id}', {json.dumps(traces)}, "
        f"{json.dumps(layout)}, {json.dumps(cfg)});</script>"
    )


def flipbook(div_id: str, frames_b64: list, labels: list, fps: float = 8.0,
             caption: str = "") -> str:
    """Self-contained looping animation with play/pause + scrub slider."""
    n = len(frames_b64)
    frames_js = json.dumps(frames_b64)
    labels_js = json.dumps(labels)
    cap = f'<div style="padding:6px 12px;font-size:.85rem;color:var(--mut)">{caption}</div>' if caption else ""
    return f"""
<div class="anim" id="{div_id}">
  <img id="{div_id}-img" src="data:image/jpeg;base64,{frames_b64[0]}" alt="animation frame">
  <div class="anim-controls">
    <button id="{div_id}-btn" aria-label="play/pause">&#10074;&#10074;</button>
    <input type="range" id="{div_id}-rng" min="0" max="{n - 1}" value="0" step="1">
    <span class="tlabel" id="{div_id}-lbl"></span>
  </div>
  {cap}
</div>
<script>
(function() {{
  const F = {frames_js}, L = {labels_js};
  const img = document.getElementById("{div_id}-img"),
        rng = document.getElementById("{div_id}-rng"),
        btn = document.getElementById("{div_id}-btn"),
        lbl = document.getElementById("{div_id}-lbl");
  let i = 0, playing = true, timer = null;
  function show(k) {{
    i = (k + F.length) % F.length;
    img.src = "data:image/jpeg;base64," + F[i];
    rng.value = i; lbl.textContent = L[i];
  }}
  function start() {{ playing = true; btn.innerHTML = "&#10074;&#10074;";
    timer = setInterval(() => show(i + 1), {int(1000 / fps)}); }}
  function stop() {{ playing = false; btn.innerHTML = "&#9654;";
    clearInterval(timer); }}
  btn.addEventListener("click", () => playing ? stop() : start());
  rng.addEventListener("input", () => {{ stop(); show(+rng.value); }});
  show(0); start();
}})();
</script>"""


def page(title: str, kicker: str, style: str, body: str) -> str:
    return f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1">
<title>{title}</title>
{style}
{PLOTLY_CDN}
</head><body><div class="wrap">
{body}
</div></body></html>
"""
