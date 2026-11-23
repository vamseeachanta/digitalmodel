# ABOUTME: Regenerates the committed demo data for the hull-girder report pack —
# ABOUTME: screening run -> station/utilization CSVs + compact report tables + margin plot SVG.
"""Regenerate the committed demo data for the hull-girder report pack.

Runs the ``hull_girder_screening`` workflow on ``screening_input.yml`` (the
public box-barge fixture with IACS UR S11 wave loads), writes the raw
station / utilisation / summary CSVs into ``data/``, then derives the two
compact report tables and the deterministic utilisation-margin SVG that
``input.yml`` (the report_pack config) embeds.

Run from the repo root::

    python examples/workflows/hull-girder-report-pack/make_demo_data.py

All outputs are deterministic (fixed float formatting, no timestamps), so a
regeneration on any host reproduces the committed files exactly (up to the
git CRLF/LF newline normalization).
"""

from __future__ import annotations

import csv
import sys
from pathlib import Path

import yaml

DEMO_DIR = Path(__file__).resolve().parent
REPO_SRC = DEMO_DIR.parents[2] / "src"

REPORT_UTILIZATION_CSV = "hull_girder_report_utilization.csv"
REPORT_SUMMARY_CSV = "hull_girder_report_summary.csv"
MARGIN_PLOT_SVG = "hull_girder_margin_plot.svg"

# Validated categorical palette (reference instance, slots 1-2; the aqua
# contrast warning is relieved by direct value labels + the report table).
COLOR_BENDING = "#2a78d6"
COLOR_SHEAR = "#1baf7a"
COLOR_INK = "#172033"
COLOR_INK_MUTED = "#6b6a60"
COLOR_GRID = "#d7dee8"


def generate(data_dir: Path) -> dict:
    """Run the screening workflow into ``data_dir`` and emit the report inputs."""
    if str(REPO_SRC) not in sys.path:
        sys.path.insert(0, str(REPO_SRC))
    from digitalmodel.hull_girder_screening.workflow import router

    config = yaml.safe_load(
        (DEMO_DIR / "screening_input.yml").read_text(encoding="utf-8")
    )
    settings = config["hull_girder_screening"]
    settings["output_dir"] = str(data_dir)
    cfg = router(
        {
            "basename": "hull_girder_screening",
            "hull_girder_screening": settings,
            "_config_dir_path": str(DEMO_DIR),
            "_config_file_path": str(DEMO_DIR / "screening_input.yml"),
        }
    )
    result = cfg["hull_girder_screening"]
    _write_report_utilization(data_dir / REPORT_UTILIZATION_CSV, result)
    _write_report_summary(data_dir / REPORT_SUMMARY_CSV, result)
    (data_dir / MARGIN_PLOT_SVG).write_text(
        _margin_plot_svg(result), encoding="utf-8", newline="\n"
    )
    return result


def _fmt(value, decimals: int = 3) -> str:
    if value is None:
        return "n/a"
    return f"{value:.{decimals}f}"


def _write_report_utilization(path: Path, result: dict) -> None:
    """Compact total (sw + wave) utilisation table for the report body."""
    rows = []
    for row in result["total_utilization"]:
        rows.append(
            {
                "frame": row["frame"],
                "x_m": _fmt(row["x_m"], 1),
                "total_hogging_t_m": _fmt(row["total_hogging_t_m"], 0),
                "total_sagging_t_m": _fmt(row["total_sagging_t_m"], 0),
                "bending_utilization": _fmt(row["bending_utilization"]),
                "governing_bending": row["governing_bending"],
                "shear_utilization": _fmt(row["shear_utilization"]),
                "status": row["status"],
            }
        )
    _write_csv(path, rows)


def _write_report_summary(path: Path, result: dict) -> None:
    """Key screening figures as a metric/value table for the report body."""
    sm_utils = [s["check"]["utilization"] for s in result["section_modulus"]]
    rows = [
        ("Displacement (t)", _fmt(result["displacement_t"], 0)),
        ("Max still-water sagging BM (t·m)", _fmt(result["max_sagging_t_m"], 0)),
        ("Max still-water shear force (t)", _fmt(result["max_shear_t"], 0)),
        ("S11 wave coefficient C", _fmt(result["wave_coefficient"])),
        (
            "S11 wave hogging BM amidships (t·m)",
            _fmt(result["wave_hogging_amidships_t_m"], 0),
        ),
        (
            "S11 wave sagging BM amidships (t·m)",
            _fmt(result["wave_sagging_amidships_t_m"], 0),
        ),
        ("S11 applicability", result["wave_applicability_note"]),
        (
            "Max still-water bending utilisation",
            _fmt(result["max_bending_utilization"]),
        ),
        (
            "Max total (sw + wave) bending utilisation",
            _fmt(result["max_total_bending_utilization"]),
        ),
        (
            "Max total (sw + wave) shear utilisation",
            _fmt(result["max_total_shear_utilization"]),
        ),
        (
            "Max hull-girder SM utilisation (IACS 175/k)",
            _fmt(max(sm_utils) if sm_utils else None),
        ),
        ("Screening status", result["screening_status"]),
    ]
    _write_csv(path, [{"metric": m, "value": v} for m, v in rows])


def _write_csv(path: Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def _margin_plot_svg(result: dict) -> str:
    """Deterministic grouped-bar SVG: total SF/BM utilisation per frame vs the
    unity limit (the gap to the dashed line is the screening margin)."""
    frames = result["total_utilization"]
    width, height = 640.0, 360.0
    left, right, top, bottom = 62.0, 16.0, 46.0, 54.0
    plot_w = width - left - right
    plot_h = height - top - bottom
    max_util = max(
        [1.0]
        + [r["bending_utilization"] or 0.0 for r in frames]
        + [r["shear_utilization"] or 0.0 for r in frames]
    )
    y_max = max(1.2, 1.1 * max_util)

    def y_of(value: float) -> float:
        return top + plot_h * (1.0 - value / y_max)

    parts: list[str] = []
    parts.append(
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width:g}" '
        f'height="{height:g}" viewBox="0 0 {width:g} {height:g}" '
        'font-family="Segoe UI, system-ui, sans-serif">'
    )
    parts.append(f'<rect width="{width:g}" height="{height:g}" fill="#ffffff"/>')
    parts.append(
        f'<text x="{left:.1f}" y="22" font-size="14" font-weight="600" '
        f'fill="{COLOR_INK}">Total (still-water + IACS UR S11 wave) '
        "utilisation by frame</text>"
    )
    # y grid + labels
    ticks = [0.0, 0.25, 0.5, 0.75, 1.0]
    if y_max > 1.25:
        ticks.append(round(y_max, 2))
    for tick in ticks:
        y = y_of(tick)
        parts.append(
            f'<line x1="{left:.1f}" y1="{y:.1f}" x2="{left + plot_w:.1f}" '
            f'y2="{y:.1f}" stroke="{COLOR_GRID}" stroke-width="1"/>'
        )
        parts.append(
            f'<text x="{left - 8:.1f}" y="{y + 4:.1f}" font-size="11" '
            f'text-anchor="end" fill="{COLOR_INK_MUTED}">{tick:.2f}</text>'
        )
    # unity limit reference
    y_limit = y_of(1.0)
    parts.append(
        f'<line x1="{left:.1f}" y1="{y_limit:.1f}" x2="{left + plot_w:.1f}" '
        f'y2="{y_limit:.1f}" stroke="{COLOR_INK_MUTED}" stroke-width="1.5" '
        'stroke-dasharray="6 4"/>'
    )
    parts.append(
        f'<text x="{left + plot_w:.1f}" y="{y_limit - 6:.1f}" font-size="11" '
        f'text-anchor="end" fill="{COLOR_INK_MUTED}">limit = 1.00</text>'
    )
    # grouped bars: bending (slot 1) then shear (slot 2), fixed order
    group_w = plot_w / len(frames)
    bar_w = min(28.0, 0.32 * group_w)
    gap = 2.0  # spacer between adjacent bars
    baseline = y_of(0.0)
    for index, row in enumerate(frames):
        x_mid = left + (index + 0.5) * group_w
        for offset, (value, color) in enumerate(
            (
                (row["bending_utilization"], COLOR_BENDING),
                (row["shear_utilization"], COLOR_SHEAR),
            )
        ):
            value = value or 0.0
            x0 = x_mid - bar_w - gap / 2.0 + offset * (bar_w + gap)
            y1 = y_of(value)
            bar_h = max(baseline - y1, 0.0)
            radius = min(4.0, bar_h)  # rounded data end, baseline anchored
            parts.append(
                f'<path d="M {x0:.1f} {baseline:.1f} V {y1 + radius:.1f} '
                f"Q {x0:.1f} {y1:.1f} {x0 + radius:.1f} {y1:.1f} "
                f"H {x0 + bar_w - radius:.1f} "
                f"Q {x0 + bar_w:.1f} {y1:.1f} {x0 + bar_w:.1f} {y1 + radius:.1f} "
                f'V {baseline:.1f} Z" fill="{color}"/>'
            )
            parts.append(
                f'<text x="{x0 + bar_w / 2.0:.1f}" y="{y1 - 5:.1f}" '
                f'font-size="10" text-anchor="middle" fill="{COLOR_INK}">'
                f"{value:.2f}</text>"
            )
        parts.append(
            f'<text x="{x_mid:.1f}" y="{baseline + 16:.1f}" font-size="11" '
            f'text-anchor="middle" fill="{COLOR_INK}">{row["frame"]}</text>'
        )
    # axis line + legend (2 series -> legend required)
    parts.append(
        f'<line x1="{left:.1f}" y1="{baseline:.1f}" x2="{left + plot_w:.1f}" '
        f'y2="{baseline:.1f}" stroke="{COLOR_INK_MUTED}" stroke-width="1"/>'
    )
    legend_y = height - 14.0
    for offset, (label, color) in enumerate(
        (("Bending moment", COLOR_BENDING), ("Shear force", COLOR_SHEAR))
    ):
        x0 = left + offset * 150.0
        parts.append(
            f'<rect x="{x0:.1f}" y="{legend_y - 9:.1f}" width="12" height="12" '
            f'rx="3" fill="{color}"/>'
        )
        parts.append(
            f'<text x="{x0 + 18:.1f}" y="{legend_y + 1:.1f}" font-size="11" '
            f'fill="{COLOR_INK}">{label}</text>'
        )
    parts.append(
        f'<text x="18" y="{top + plot_h / 2.0:.1f}" font-size="11" '
        f'fill="{COLOR_INK_MUTED}" text-anchor="middle" '
        f'transform="rotate(-90 18 {top + plot_h / 2.0:.1f})">'
        "utilisation (-)</text>"
    )
    parts.append("</svg>")
    return "\n".join(parts) + "\n"


if __name__ == "__main__":
    outputs = generate(DEMO_DIR / "data")
    print(f"screening status: {outputs['screening_status']}")
    print(f"data written to {DEMO_DIR / 'data'}")
