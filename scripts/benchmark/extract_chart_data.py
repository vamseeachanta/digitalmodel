#!/usr/bin/env python3
"""
extract_chart_data.py — Digitize multi-panel matplotlib chart PNGs to CSV.

Detects colored curves in each subplot using HSV-based masking, extracts
per-column y-centroids for solid lines, and uses column-count peak detection
to identify discrete marker (x / square) positions.

Usage
-----
    uv run python scripts/benchmark/extract_chart_data.py \\
        --image docs/modules/orcawave/L00_validation_wamit/3.1/QTF_plot.png \\
        [--config scripts/benchmark/qtf_plot_config.yaml] \\
        [--output docs/modules/orcawave/L00_validation_wamit/3.1/digitized/] \\
        [--grid 26] \\
        [--debug]

Output
------
    One CSV per subplot.  Columns: omega_rad_s, <series1>, <series2>, ...
    Header comment rows carry subplot metadata (title, units, axis limits).
"""
from __future__ import annotations

import argparse
import csv
import sys
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage, signal

try:
    import yaml as _yaml
    _HAS_YAML = True
except ImportError:
    _HAS_YAML = False


# ── default config for QTF 5-panel surge chart ──────────────────────────────
# Pixel boxes (x0, y0, x1, y1) = inner axes region, derived from 535×759 image.
_DEFAULT_CONFIG: dict = {
    "image_title": "QTF Plot - Surge Components",
    "series": [
        {"name": "OrcaWave_real", "hue_lo": 175, "hue_hi": 230,
         "sat_min": 0.25, "val_min": 0.30, "kind": "line"},
        {"name": "OrcaWave_imag", "hue_lo": 340, "hue_hi": 380,
         "sat_min": 0.30, "val_min": 0.30, "kind": "line"},
        {"name": "Wamit_real",    "hue_lo": 175, "hue_hi": 230,
         "sat_min": 0.25, "val_min": 0.30, "kind": "marker"},
        {"name": "Wamit_imag",    "hue_lo": 340, "hue_hi": 380,
         "sat_min": 0.30, "val_min": 0.30, "kind": "marker"},
    ],
    "subplots": [
        {
            "title":       "Diffraction load RAO",
            "output_name": "surge_diffraction_rao",
            "y_unit":      "kN/m",
            "x_min": 0.5, "x_max": 3.0,
            "y_min": 0.0, "y_max": 35.0,
            "pixel_box":   [184, 28, 357, 188],
        },
        {
            "title":       "Mean drift load (PI)",
            "output_name": "surge_mean_drift_pi",
            "y_unit":      "kN/m2",
            "x_min": 0.5, "x_max": 3.0,
            "y_min": 0.0, "y_max": 11.0,
            "pixel_box":   [50, 237, 222, 397],
        },
        {
            "title":       "Quadratic load (PI)",
            "output_name": "surge_quadratic_load_pi",
            "y_unit":      "kN/m2",
            "x_min": 0.5, "x_max": 3.0,
            "y_min": -15.0, "y_max": 8.0,
            "pixel_box":   [316, 237, 490, 397],
        },
        {
            "title":       "Direct potential load",
            "output_name": "surge_direct_potential",
            "y_unit":      "kN/m2",
            "x_min": 0.5, "x_max": 3.0,
            "y_min": 0.0, "y_max": 650.0,
            "pixel_box":   [50, 443, 222, 572],
        },
        {
            "title":       "Indirect potential load",
            "output_name": "surge_indirect_potential",
            "y_unit":      "kN/m2",
            "x_min": 0.5, "x_max": 3.0,
            "y_min": 0.0, "y_max": 650.0,
            "pixel_box":   [316, 443, 490, 572],
        },
    ],
}


# ── image helpers ────────────────────────────────────────────────────────────

def load_image(path: str | Path) -> np.ndarray:
    """Load image to uint8 RGB array (H, W, 3)."""
    return np.asarray(Image.open(path).convert("RGB"), dtype=np.uint8)


def _hsv_from_rgb(img: np.ndarray) -> tuple[np.ndarray, np.ndarray, np.ndarray]:
    """Return (hue_deg, sat, val) arrays from uint8 RGB image."""
    f = img.astype(np.float32) / 255.0
    r, g, b = f[:, :, 0], f[:, :, 1], f[:, :, 2]
    cmax = np.maximum(np.maximum(r, g), b)
    cmin = np.minimum(np.minimum(r, g), b)
    delta = cmax - cmin

    hue = np.zeros_like(cmax)
    m = (cmax == r) & (delta > 0)
    hue[m] = (60.0 * ((g[m] - b[m]) / delta[m])) % 360.0
    m = (cmax == g) & (delta > 0)
    hue[m] = 60.0 * ((b[m] - r[m]) / delta[m] + 2.0)
    m = (cmax == b) & (delta > 0)
    hue[m] = 60.0 * ((r[m] - g[m]) / delta[m] + 4.0)
    hue[hue < 0] += 360.0

    sat = np.where(cmax > 0, delta / cmax, 0.0)
    return hue, sat, cmax  # val = cmax


def build_hsv_mask(
    img: np.ndarray,
    hue_lo: float,
    hue_hi: float,
    sat_min: float = 0.25,
    val_min: float = 0.30,
) -> np.ndarray:
    """
    Boolean mask where pixels fall within the HSV hue range [hue_lo, hue_hi].
    hue_hi may exceed 360 to wrap around (e.g., 340..380 catches red at 350-360 + 0-20).
    """
    hue, sat, val = _hsv_from_rgb(img)
    if hue_hi > 360:
        in_hue = (hue >= hue_lo) | (hue <= hue_hi - 360)
    else:
        in_hue = (hue >= hue_lo) & (hue <= hue_hi)
    return in_hue & (sat >= sat_min) & (val >= val_min)


# ── curve extraction ─────────────────────────────────────────────────────────

def extract_line_and_markers(
    crop: np.ndarray,
    hue_lo: float,
    hue_hi: float,
    sat_min: float,
    val_min: float,
    xmin: float,
    xmax: float,
    ymin: float,
    ymax: float,
    marker_peak_factor: float = 1.4,
    marker_min_distance: int = 4,
) -> tuple[list[tuple[float, float]], list[tuple[float, float]]]:
    """
    Extract (x_data, y_data) for a solid line and for discrete markers.

    Strategy
    --------
    1. Build HSV mask for the target colour range.
    2. Per-column pixel count: spikes above `marker_peak_factor * mean` → markers.
    3. Per-column y-centroid across all columns → dense line data.

    Returns
    -------
    line_pts   : dense list, one per non-empty column
    marker_pts : sparse list, one per detected marker spike
    """
    H, W = crop.shape[:2]
    mask = build_hsv_mask(crop, hue_lo, hue_hi, sat_min, val_min)

    if not mask.any():
        return [], []

    x_scale = (xmax - xmin) / W
    y_scale = (ymax - ymin) / H
    col_count = mask.sum(axis=0).astype(float)  # (W,)

    # ── solid line: per-column median y ──────────────────────────────────────
    line_pts: list[tuple[float, float]] = []
    for col in range(W):
        rows = np.where(mask[:, col])[0]
        if len(rows) == 0:
            continue
        row_c = float(np.median(rows))
        x_d = xmin + (col + 0.5) * x_scale
        y_d = ymax - (row_c + 0.5) * y_scale
        line_pts.append((x_d, y_d))

    # ── markers: peaks in column-count profile ───────────────────────────────
    active = col_count[col_count > 0]
    if len(active) == 0:
        return line_pts, []

    mean_cnt = float(active.mean())
    height_thr = mean_cnt * marker_peak_factor

    peaks, _ = signal.find_peaks(
        col_count,
        height=height_thr,
        distance=marker_min_distance,
    )

    marker_pts: list[tuple[float, float]] = []
    for p in peaks:
        rows = np.where(mask[:, p])[0]
        if len(rows) == 0:
            continue
        row_c = float(np.median(rows))
        x_d = xmin + (p + 0.5) * x_scale
        y_d = ymax - (row_c + 0.5) * y_scale
        marker_pts.append((x_d, y_d))

    line_pts.sort(key=lambda pt: pt[0])
    marker_pts.sort(key=lambda pt: pt[0])
    return line_pts, marker_pts


# ── resampling ───────────────────────────────────────────────────────────────

def resample_to_grid(
    pts: list[tuple[float, float]],
    x_grid: np.ndarray,
) -> np.ndarray:
    """Nearest-neighbour resample (x,y) pairs onto x_grid. Returns NaN where empty."""
    if not pts:
        return np.full(len(x_grid), np.nan)
    xs = np.array([p[0] for p in pts])
    ys = np.array([p[1] for p in pts])
    out = np.full(len(x_grid), np.nan)
    spacing = float(x_grid[1] - x_grid[0]) if len(x_grid) > 1 else 1.0
    for i, xq in enumerate(x_grid):
        idx = int(np.argmin(np.abs(xs - xq)))
        if abs(float(xs[idx]) - float(xq)) <= spacing:
            out[i] = float(ys[idx])
    return out


# ── CSV writer ───────────────────────────────────────────────────────────────

def write_subplot_csv(
    output_path: Path,
    sp_cfg: dict,
    series_pts: dict[str, list[tuple[float, float]]],
    n_grid: int,
) -> None:
    """Write one CSV file for a subplot with all series on a common x grid."""
    xmin = float(sp_cfg["x_min"])
    xmax = float(sp_cfg["x_max"])
    x_grid = np.linspace(xmin, xmax, n_grid)

    output_path.parent.mkdir(parents=True, exist_ok=True)
    with open(output_path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["# title",   sp_cfg["title"]])
        w.writerow(["# y_unit",  sp_cfg.get("y_unit", "")])
        w.writerow(["# x_label", "omega (rad/s)"])
        w.writerow(["# y_min",   sp_cfg["y_min"]])
        w.writerow(["# y_max",   sp_cfg["y_max"]])
        w.writerow(["omega_rad_s"] + list(series_pts.keys()))

        cols = {name: resample_to_grid(pts, x_grid) for name, pts in series_pts.items()}
        for i, xq in enumerate(x_grid):
            row = [f"{xq:.4f}"]
            for name in series_pts:
                v = cols[name][i]
                row.append("" if np.isnan(v) else f"{v:.5f}")
            w.writerow(row)

    print("  [ok] %-40s  %d series x %d pts" % (
        output_path.name, len(series_pts), n_grid))


# ── debug overlay ────────────────────────────────────────────────────────────

def save_debug_overlay(img_path: Path, subplots: list[dict], out: Path) -> None:
    """Save annotated PNG showing the subplot bounding boxes."""
    pil = Image.open(img_path).convert("RGB")
    draw = ImageDraw.Draw(pil)
    colours = ["red", "lime", "blue", "yellow", "cyan"]
    for i, sp in enumerate(subplots):
        box = sp.get("pixel_box", [0, 0, pil.width, pil.height])
        x0, y0, x1, y1 = [int(v) for v in box]
        col = colours[i % len(colours)]
        draw.rectangle([x0, y0, x1, y1], outline=col, width=2)
        draw.text((x0 + 3, y0 + 3), sp["title"][:18], fill=col)
    out.parent.mkdir(parents=True, exist_ok=True)
    pil.save(out)
    print("  [debug] overlay -> %s" % out.name)


# ── config ───────────────────────────────────────────────────────────────────

def load_config(path: str | Path) -> dict:
    if not _HAS_YAML:
        print("WARNING: PyYAML unavailable, using built-in defaults")
        return _DEFAULT_CONFIG
    with open(path, encoding="utf-8") as f:
        return _yaml.safe_load(f)


# ── main ─────────────────────────────────────────────────────────────────────

def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--image",  required=True, help="Input PNG path")
    ap.add_argument("--config", default=None,  help="YAML config file (optional)")
    ap.add_argument("--output", default=None,  help="Output directory")
    ap.add_argument("--grid",   type=int, default=26,
                    help="Number of x grid points (default 26 = 0.5:0.1:3.0)")
    ap.add_argument("--debug",  action="store_true",
                    help="Save annotated overlay image showing subplot boxes")
    args = ap.parse_args()

    img_path = Path(args.image)
    if not img_path.exists():
        sys.exit("ERROR: image not found: %s" % img_path)

    # Resolve config
    if args.config:
        cfg = load_config(args.config)
    else:
        auto_cfg = img_path.with_name(img_path.stem + "_digitize.yaml")
        if auto_cfg.exists():
            cfg = load_config(auto_cfg)
            print("Config: %s" % auto_cfg.name)
        else:
            cfg = _DEFAULT_CONFIG
            print("Config: built-in QTF defaults")

    output_dir = Path(args.output) if args.output else img_path.parent / "digitized"
    img = load_image(img_path)
    print("Image: %dx%d  Output: %s" % (img.shape[1], img.shape[0], output_dir))

    if args.debug:
        save_debug_overlay(img_path, cfg["subplots"], output_dir / "debug_subplot_boxes.png")

    series_specs = cfg["series"]
    print("Subplots: %d   Series: %d" % (len(cfg["subplots"]), len(series_specs)))

    for sp in cfg["subplots"]:
        print("\n[subplot] %s" % sp["title"])
        box = sp.get("pixel_box", [0, 0, img.shape[1], img.shape[0]])
        x0, y0, x1, y1 = [int(v) for v in box]
        crop = img[y0:y1 + 1, x0:x1 + 1].copy()
        xmin, xmax = float(sp["x_min"]), float(sp["x_max"])
        ymin, ymax = float(sp["y_min"]), float(sp["y_max"])

        # Cache per-colour extraction (line + marker share same mask computation)
        _cache: dict[tuple, tuple] = {}

        series_pts: dict[str, list] = {}
        for spec in series_specs:
            name = spec["name"]
            hlo  = float(spec.get("hue_lo", 0))
            hhi  = float(spec.get("hue_hi", 360))
            smin = float(spec.get("sat_min", 0.25))
            vmin = float(spec.get("val_min", 0.30))
            kind = spec.get("kind", "line")

            key = (hlo, hhi, smin, vmin)
            if key not in _cache:
                _cache[key] = extract_line_and_markers(
                    crop, hlo, hhi, smin, vmin, xmin, xmax, ymin, ymax)
            line_pts, marker_pts = _cache[key]

            pts = line_pts if kind == "line" else marker_pts
            print("  %-22s  kind=%-6s  %3d pts" % (name, kind, len(pts)))
            series_pts[name] = pts

        out_path = output_dir / (sp["output_name"] + ".csv")
        write_subplot_csv(out_path, sp, series_pts, args.grid)

    print("\nDone. Output directory: %s" % output_dir)


if __name__ == "__main__":
    main()
