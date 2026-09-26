"""Vector PDF snapshots from the installation envelope dashboard payload."""

from __future__ import annotations

import json
import math
from copy import deepcopy
from pathlib import Path

from reportlab.lib import colors
from reportlab.lib.pagesizes import A4
from reportlab.lib.styles import ParagraphStyle
from reportlab.pdfgen.canvas import Canvas
from reportlab.platypus import Paragraph
from xml.sax.saxutils import escape

WIDTH, HEIGHT = A4
INK = colors.HexColor("#17384d")
BLUE = colors.HexColor("#1867a0")
ORANGE = colors.HexColor("#bc610a")
RED = colors.HexColor("#aa3030")
STATUS_COLORS = {
    "WITHIN_ASSUMPTIONS": colors.HexColor("#b7d6eb"),
    "EXCEEDS_ASSUMPTIONS": colors.HexColor("#edbd8c"),
    "NOT_EVALUATED": colors.HexColor("#d9dfe5"),
}


def select_snapshot(payload, hs_m=2, tp_s=10, now_s=360):
    """Select an exact scenario and origin; never silently substitute a case."""
    demo = payload["demo"]
    scenarios = demo.get("scenarios", [demo])
    selected = [s for s in scenarios if s["hs_m"] == hs_m and s["tp_s"] == tp_s]
    if len(selected) != 1:
        raise ValueError("Snapshot scenario missing or ambiguous")
    scenario = selected[0]
    frames = [f for f in scenario["frames"] if f["now_s"] == now_s]
    if len(frames) != 1:
        raise ValueError("Snapshot origin missing or ambiguous")
    matches = [c for c in payload["cases"] if c["index"] == scenario["case_index"]]
    if len(matches) != 1 or (matches[0]["hs_m"], matches[0]["tp_s"]) != (hs_m, tp_s):
        raise ValueError("Snapshot case identity disagrees with envelope")
    return scenario, frames[0]


def _series(channel, name, now):
    series = channel[name]
    times, values = series["times"], series["values"]
    if len(times) < 2 or len(times) != len(values):
        raise ValueError("Invalid chart series lengths")
    if not all(math.isfinite(float(v)) for v in times + values):
        raise ValueError("Nonfinite chart data")
    if any(b <= a for a, b in zip(times, times[1:])):
        raise ValueError("Chart times must increase")
    if name == "history" and times[-1] != now:
        raise ValueError("History must end at prediction origin")
    if name != "history" and (times[0] <= now or times[-1] != now + 120):
        raise ValueError("Prediction must span the next 120 seconds")
    return times, values


def plot_bounds(channel, now_s, include_limit=True):
    """Return finite time/value bounds; withheld truth never determines axes."""
    ht, hv = _series(channel, "history", now_s)
    ft, fv = _series(channel, "forecast", now_s)
    values = list(hv) + list(fv)
    limit = channel.get("assumed_limit")
    if limit is not None and not math.isfinite(float(limit)):
        raise ValueError("Nonfinite assumed limit")
    if include_limit and limit is not None:
        values.append(limit)
    low, high = min(values), max(values)
    pad = max((high - low) * 0.1, abs(high) * 0.001, 0.01)
    return ht[0], ft[-1], low - pad, high + pad


def _text(canvas, text, y, size=10, width=WIDTH - 88):
    style = ParagraphStyle("body", fontName="Helvetica", fontSize=size,
                           leading=size * 1.4, textColor=INK)
    para = Paragraph(escape(str(text)).replace("\n", "<br/>"), style)
    _, height = para.wrap(width, HEIGHT)
    if y - height < 45:
        raise ValueError("PDF page text exceeds available space")
    para.drawOn(canvas, 44, y - height)
    return y - height - 10


def _heading(canvas, title, page):
    canvas.setFillColor(INK)
    canvas.setFont("Helvetica-Bold", 17)
    canvas.drawString(44, HEIGHT - 47, title)
    canvas.setFont("Helvetica", 8)
    canvas.drawString(44, 24, "ASSUMED-CRITERIA SCREENING | No operating approval")
    canvas.drawRightString(WIDTH - 44, 24, str(page))


def _description(item):
    if isinstance(item, str):
        return item
    if "label" in item and "limit" in item:
        units = "dimensionless" if item.get("units") == "1" else item.get("units", "")
        return f"{item['label']}: {item['limit']} {units} (assumed)"
    return "; ".join(f"{key}: {value}" for key, value in item.items())


def _heatmap(canvas, cases, scenario, top):
    hs = sorted({c["hs_m"] for c in cases})
    tp = sorted({c["tp_s"] for c in cases})
    lookup = {(c["hs_m"], c["tp_s"]): c for c in cases}
    if len(lookup) != len(cases):
        raise ValueError("Duplicate envelope coordinates")
    left, width, height = 82, WIDTH - 140, 220
    cw, ch = width / len(tp), height / len(hs)
    canvas.setFont("Helvetica", 8)
    for i, h in enumerate(hs):
        canvas.setFillColor(INK)
        canvas.drawRightString(left - 9, top - height + i * ch + ch / 2, f"{h:g}")
        for j, period in enumerate(tp):
            status = lookup.get((h, period), {}).get("status", "NOT_EVALUATED")
            canvas.setFillColor(STATUS_COLORS[status])
            canvas.setStrokeColor(colors.white)
            canvas.rect(left + j * cw, top - height + i * ch, cw, ch, fill=1)
    canvas.setFillColor(INK)
    for j, period in enumerate(tp):
        canvas.drawCentredString(left + (j + .5) * cw, top - height - 14, f"{period:g}")
    x = left + (tp.index(scenario["tp_s"]) + .5) * cw
    y = top - height + (hs.index(scenario["hs_m"]) + .5) * ch
    canvas.setStrokeColor(INK)
    canvas.setLineWidth(2)
    canvas.circle(x, y, min(cw, ch) * .32, fill=0)
    canvas.drawString(44, top + 7, "Hs (m)")
    canvas.drawCentredString(left + width / 2, top - height - 29, "Tp (s)")
    return top - height - 49


def _line(canvas, times, values, bounds, box, color, dashed=False):
    xmin, xmax, ymin, ymax = bounds
    x, y, width, height = box
    canvas.setStrokeColor(color)
    canvas.setLineWidth(1.1)
    canvas.setDash(3, 2) if dashed else canvas.setDash()
    path = canvas.beginPath()
    for index, (t, value) in enumerate(zip(times, values)):
        point = (x + width * (t - xmin) / (xmax - xmin),
                 y + height * (value - ymin) / (ymax - ymin))
        path.moveTo(*point) if index == 0 else path.lineTo(*point)
    canvas.drawPath(path)
    canvas.setDash()


def _axes(canvas, bounds, box, now, labels=True):
    xmin, xmax, ymin, ymax = bounds
    x, y, width, height = box
    canvas.setStrokeColor(colors.HexColor("#b2bdc6"))
    canvas.setLineWidth(.5)
    canvas.rect(x, y, width, height)
    split = x + width * (now - xmin) / (xmax - xmin)
    canvas.setStrokeColor(INK)
    canvas.setDash(3, 2)
    canvas.line(split, y, split, y + height)
    canvas.setDash()
    canvas.setFillColor(INK)
    canvas.setFont("Helvetica", 7)
    if labels:
        for value in [ymin, (ymin + ymax) / 2, ymax]:
            canvas.drawRightString(x - 4, y + height * (value-ymin)/(ymax-ymin), f"{value:.2f}")
        for value in [xmin, now, xmax]:
            canvas.drawCentredString(x + width*(value-xmin)/(xmax-xmin), y - 11, f"{value:g}")
    canvas.drawCentredString(split, y + height + 5, "NOW")


def _traces(canvas, channel, bounds, box, now):
    _axes(canvas, bounds, box, now)
    for name, color in [("history", BLUE), ("forecast", ORANGE)]:
        series = channel[name]
        _line(canvas, series["times"], series["values"], bounds, box, color, name == "forecast")
    limit = channel.get("assumed_limit")
    if limit is not None and bounds[2] <= limit <= bounds[3]:
        _line(canvas, bounds[:2], [limit, limit], bounds, box, RED, True)


def _chart(canvas, channel, now, top):
    full = plot_bounds(channel, now)
    zoom = plot_bounds(channel, now, include_limit=False)
    inset = (full[3] - full[2]) > 3 * (zoom[3] - zoom[2])
    canvas.setFillColor(INK)
    canvas.setFont("Helvetica-Bold", 10)
    canvas.drawString(44, top, f"{channel['label']} ({channel['units']})")
    box = (85, top - 115, WIDTH - (260 if inset else 140), 88)
    _traces(canvas, channel, zoom if inset else full, box, now)
    if inset:
        _traces(canvas, channel, full, (WIDTH - 135, top - 115, 85, 88), now)
        canvas.setFont("Helvetica", 7)
        canvas.drawString(85, top - 16, "Response zoom")
        canvas.drawString(WIDTH - 135, top - 16, "Full limit context")
    canvas.setFont("Helvetica", 8)
    limit = channel.get("assumed_limit")
    caption = f"Assumed limit: {limit:g} {channel['units']}" if limit is not None else "No wave-elevation acceptance threshold"
    canvas.drawString(44, top - 145, caption)
    canvas.drawRightString(WIDTH - 44, top - 145, "Time (s)")
    return top - 175


def _summary(canvas, payload, scenario):
    _heading(canvas, "Installation capability screening", 1)
    y = _text(canvas, payload["title"], HEIGHT - 70, 12)
    y = _text(canvas, "Assumed criteria; results are screening classifications, not an issued operating release.", y)
    y = _text(canvas, "Assumed criteria", y, 12)
    for item in payload.get("criteria", []):
        y = _text(canvas, _description(item), y, 8)
    if y < 395:
        raise ValueError("Criteria too long for envelope page; shorten display text")
    y = _heatmap(canvas, payload["cases"], scenario, y - 8)
    y = _text(canvas, "Blue: within assumptions. Orange: exceeds assumptions. Gray: not evaluated. Circle: selected SIMULATED monitoring case.", y, 9)
    _text(canvas, "Passing cells at the upper study edge are edge-censored, not a failure boundary. No capability beyond the sampled grid is established.", y, 9)


def _monitoring(canvas, scenario, frame, mode):
    _heading(canvas, "Simulated near-real-time monitoring", 2)
    y = _text(canvas, f"SIMULATED case {scenario['case_index']}: Hs {scenario['hs_m']:g} m, Tp {scenario['tp_s']:g} s; origin {frame['now_s']:g} s.", HEIGHT - 70)
    if mode == "wave_preview":
        y = _text(canvas, "SIMULATED WAVE PREVIEW INPUT; conditional load forecast; offshore wave prediction not validated.", y, 9)
    y = _text(canvas, "Not connected offshore. Blue: history. Dashed orange: next 120 s. Red: assumed limit. Load truth is hidden.", y, 8)
    y = _text(canvas, scenario["source_label"], y, 8)
    for channel in frame["channels"]:
        y = _chart(canvas, channel, frame["now_s"], y - 3)


def _forecast_quality(channel):
    metrics = channel.get("metrics", {})
    preview = channel.get("_preview_mode", False)
    if preview:
        metrics = channel.get("wave_preview_metrics")
        if metrics is None:
            return "Supplied simulated wave preview; no wave forecast skill assessed."
    forecast = metrics.get("oracle_wave_fir" if preview else "autoregression", {}).get("rmse")
    naive = [metrics.get(name, {}).get("rmse") for name in ["persistence", "history_mean"]]
    naive = [value for value in naive if value is not None and math.isfinite(value)]
    if forecast is None or not naive or not math.isfinite(forecast):
        return "Forecast comparison not available."
    best = min(naive)
    qualification = ("Conditional oracle comparison; no field skill established."
                     if preview else "Withheld simulated truth; single snapshot.")
    return (f"120 s RMSE {forecast:.4g} {channel['units']}; "
            f"best naive {best:.4g} {channel['units']}. "
            + qualification)


def _evidence(canvas, payload, scenario, frame, mode):
    _heading(canvas, "Limitations and evidence", 3)
    y = _text(canvas, "No operating approval", HEIGHT - 73, 12)
    for item in payload.get("limitations", []):
        y = _text(canvas, _description(item), y, 9)
    y = _text(canvas, "Snapshot provenance", y, 12)
    provenance = payload.get("provenance", {})
    if isinstance(provenance, dict):
        provenance = [f"{key}: {json.dumps(value, ensure_ascii=True)}" for key, value in provenance.items()]
    elif isinstance(provenance, str):
        provenance = [provenance]
    for item in provenance:
        y = _text(canvas, _description(item), y, 8)
    y = _text(canvas, f"Payload created: {payload['created_utc']}; case {scenario['case_index']}; origin {frame['now_s']} s; mode {mode}.", y, 8)
    for channel in frame["channels"]:
        y = _text(canvas, f"Channel {channel['id']}: {channel.get('fit_status', 'not recorded')}. "
                  + _forecast_quality(channel), y, 8)


def _selected_frame(payload, frame, mode):
    mode = mode or payload["demo"].get("default_mode", "history_only")
    if mode not in {"history_only", "wave_preview"}:
        raise ValueError("Unknown forecast mode")
    frame = deepcopy(frame)
    if mode == "wave_preview":
        for channel in frame["channels"]:
            if not channel.get("wave_preview"):
                raise ValueError("Selected channel has no wave preview")
            channel["forecast"] = channel["wave_preview"]
            channel["fit_status"] = channel.get("preview_fit_status", "not recorded")
            channel["_preview_mode"] = True
    return frame, mode


def render_pdf(payload, output, hs_m=2, tp_s=10, now_s=360, mode=None):
    """Write a three-page snapshot to a path or binary stream; return identity."""
    scenario, frame = select_snapshot(payload, hs_m, tp_s, now_s)
    frame, mode = _selected_frame(payload, frame, mode)
    if not 1 <= len(frame["channels"]) <= 3:
        raise ValueError("Snapshot requires one to three chart channels")
    for channel in frame["channels"]:
        plot_bounds(channel, now_s)
    for case in payload["cases"]:
        if case["status"] not in STATUS_COLORS:
            raise ValueError("Unknown envelope classification")
    from io import BytesIO
    buffer = BytesIO()
    canvas = Canvas(buffer, pagesize=A4, pageCompression=1)
    canvas.setTitle(payload["title"])
    _summary(canvas, payload, scenario)
    canvas.showPage()
    _monitoring(canvas, scenario, frame, mode)
    canvas.showPage()
    _evidence(canvas, payload, scenario, frame, mode)
    canvas.save()
    data = buffer.getvalue()
    if hasattr(output, "write"):
        output.write(data)
    else:
        Path(output).write_bytes(data)
    return {"case_index": scenario["case_index"], "now_s": now_s, "pages": 3, "mode": mode}
