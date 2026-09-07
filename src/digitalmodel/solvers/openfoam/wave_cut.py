"""Reduce OpenFOAM ``surfaces`` raw VOF output to wave-cut metrics."""

from __future__ import annotations

import argparse
import csv
import json
import math
from pathlib import Path
from typing import Iterable, Mapping

try:
    import numpy as np
except ImportError:  # pragma: no cover - supported lightweight installation
    np = None


def read_raw(path: str | Path) -> list[tuple[float, float, float, float]]:
    """Read numeric ``x y z alpha`` rows, ignoring OpenFOAM comment headers."""
    rows = []
    with Path(path).open(encoding="utf-8") as stream:
        for line in stream:
            fields = line.split()
            if not fields or fields[0].startswith("#") or len(fields) < 4:
                continue
            try:
                rows.append(tuple(float(value) for value in fields[:4]))
            except ValueError:
                continue
    if not rows:
        raise ValueError(f"{path}: no numeric rows of x y z alpha data")
    return rows


def _is_iso_surface(rows: Iterable[tuple[float, float, float, float]]) -> bool:
    rows = list(rows)
    y_values = [row[1] for row in rows]
    alpha_values = [row[3] for row in rows]
    return (max(y_values) - min(y_values) > 1.0e-6
            and max(abs(value - 0.5) for value in alpha_values) < 0.05)


def extract_eta(rows, waterline: float, bin_width: float) -> list[tuple[float, float]]:
    """Bin a vertical cut by x and interpolate its alpha=0.5 elevation."""
    columns: dict[float, list[tuple[float, float]]] = {}
    for row in rows:
        columns.setdefault(round(row[0], 9), []).append((row[2], row[3]))
    crossings = []
    for x, column in columns.items():
        column.sort()
        candidates = []
        for (z0, a0), (z1, a1) in zip(column, column[1:]):
            if (a0 - 0.5) * (a1 - 0.5) > 0.0 or a0 == a1:
                continue
            candidates.append(z0 + (0.5 - a0) * (z1 - z0) / (a1 - a0))
        if candidates:
            crossings.append((x, _median(candidates)))
    bins: dict[int, list[tuple[float, float]]] = {}
    for x, z in crossings:
        bins.setdefault(math.floor(x / bin_width + 1.0e-9), []).append((x, z))
    profile = []
    for points in bins.values():
        profile.append((sum(point[0] for point in points) / len(points),
                        sum(point[1] for point in points) / len(points) - waterline))
    profile.sort()
    if len(profile) < 2:
        raise ValueError("cut has fewer than two alpha=0.5 crossings")
    return profile


def _crossing_wavelength(profile: list[tuple[float, float]]) -> float | None:
    crossings = []
    for (x0, y0), (x1, y1) in zip(profile, profile[1:]):
        if y0 == 0.0:
            crossings.append(x0)
        elif y0 * y1 < 0.0:
            crossings.append(x0 - y0 * (x1 - x0) / (y1 - y0))
    spans = [crossings[i + 2] - crossings[i] for i in range(len(crossings) - 2)]
    return _median(spans) if spans else None


def _median(values: list[float]) -> float:
    ordered = sorted(values)
    middle = len(ordered) // 2
    if len(ordered) % 2:
        return ordered[middle]
    return (ordered[middle - 1] + ordered[middle]) / 2.0


def _fft_wavelength(profile: list[tuple[float, float]]) -> float | None:
    if np is None or len(profile) < 4:
        return None
    x = np.asarray([point[0] for point in profile])
    eta = np.asarray([point[1] for point in profile])
    spacing = float(np.median(np.diff(x)))
    uniform_x = np.arange(x[0], x[-1] + spacing * 0.25, spacing)
    signal = np.interp(uniform_x, x, eta)
    signal -= signal.mean()
    spectrum = np.abs(np.fft.rfft(signal))
    frequencies = np.fft.rfftfreq(signal.size, spacing)
    if spectrum.size < 2 or not np.any(spectrum[1:] > 0):
        return None
    frequency = float(frequencies[1 + int(np.argmax(spectrum[1:]))])
    return 1.0 / frequency if frequency else None


def _rms_bins(profile, stern: float, width: float) -> list[dict]:
    bins: dict[int, list[float]] = {}
    for x, eta in profile:
        distance = stern - x
        if distance >= 0.0:
            bins.setdefault(math.floor(distance / width), []).append(eta)
    return [{"distance_start_m": index * width,
             "distance_end_m": (index + 1) * width,
             "rms_eta_m": math.sqrt(sum(v * v for v in values) / len(values)),
             "point_count": len(values)}
            for index, values in sorted(bins.items())]


def summarize_cut(profile, stern: float, wavelength: float) -> dict:
    astern = [point for point in profile if point[0] <= stern]
    if not astern:
        raise ValueError(f"no wave-cut points astern of stern x={stern}")
    crest = max(astern, key=lambda point: point[1])
    trough = min(astern, key=lambda point: point[1])
    return {
        "point_count": len(profile),
        "crest": {"x_m": crest[0], "eta_m": crest[1]},
        "trough": {"x_m": trough[0], "eta_m": trough[1]},
        "dominant_wavelength_zero_crossing_m": _crossing_wavelength(astern),
        "dominant_wavelength_fft_m": _fft_wavelength(astern),
        "amplitude_decay": _rms_bins(astern, stern, wavelength),
    }


def summarize_iso(rows, waterline: float, stern: float, wavelength: float) -> dict:
    """Compute RMS elevation in half-wavelength bins inside a 45-degree aft wedge."""
    width = wavelength / 2.0
    bins: dict[int, list[float]] = {}
    for x, y, z, _alpha in rows:
        aft = stern - x
        if aft < 0.0 or abs(y) > aft:
            continue
        radius = math.hypot(aft, y)
        bins.setdefault(math.floor(radius / width), []).append(z - waterline)
    metrics = [{"radius_start_m": index * width,
                "radius_end_m": (index + 1) * width,
                "rms_eta_m": math.sqrt(sum(v * v for v in values) / len(values)),
                "point_count": len(values)}
               for index, values in sorted(bins.items())]
    return {"kind": "iso_surface", "radial_bin_width_m": width,
            "wedge_half_angle_deg": 45.0, "wedge_rms": metrics}


def _write_csv(path: Path, profile) -> None:
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.writer(stream)
        writer.writerow(("x", "eta"))
        writer.writerows(profile)


def _plot(path: Path, profiles: Mapping[str, list], stern: float, wavelength: float) -> None:
    try:
        import matplotlib.pyplot as plt
    except ImportError:  # pragma: no cover
        _plot_svg(path, profiles, stern, wavelength)
        return
    figure, axis = plt.subplots(figsize=(10, 5))
    for label, profile in profiles.items():
        axis.plot(*zip(*profile), label=label)
    axis.axhline(0.0, color="black", linewidth=0.8)
    if profiles:
        xmin = min(x for profile in profiles.values() for x, _ in profile)
        marker = stern
        while marker >= xmin:
            axis.axvline(marker, color="gray", linestyle=":", linewidth=0.6)
            marker -= wavelength
    axis.set(xlabel="x (m; astern is negative)", ylabel="eta (m)")
    axis.grid(alpha=0.2)
    axis.legend()
    figure.tight_layout()
    figure.savefig(path, format="svg")
    plt.close(figure)


def _plot_svg(path: Path, profiles: Mapping[str, list], stern: float, wavelength: float) -> None:
    points = [point for profile in profiles.values() for point in profile]
    xmin, xmax = min(x for x, _ in points), max(x for x, _ in points)
    ymin, ymax = min(y for _, y in points), max(y for _, y in points)
    span_x, span_y = max(xmax - xmin, 1e-9), max(ymax - ymin, 1e-9)
    lines = ['<?xml version="1.0" encoding="UTF-8"?>',
             '<svg xmlns="http://www.w3.org/2000/svg" width="1000" height="500">',
             '<rect width="100%" height="100%" fill="white"/>']
    for index, (label, profile) in enumerate(profiles.items()):
        coords = " ".join(f"{60+880*(x-xmin)/span_x:.1f},{440-380*(y-ymin)/span_y:.1f}"
                          for x, y in profile)
        colour = ("#1f77b4", "#d62728", "#2ca02c")[index % 3]
        lines.append(f'<polyline points="{coords}" fill="none" stroke="{colour}"/>')
        lines.append(f'<text x="{70+index*150}" y="25" fill="{colour}">{label}</text>')
    lines.append('</svg>')
    path.write_text("\n".join(lines), encoding="utf-8")


def reduce_files(inputs: Mapping[str, str | Path], out_dir: str | Path,
                 waterline: float, stern: float, wavelength: float,
                 bin_width: float = 0.5) -> dict:
    if wavelength <= 0.0 or bin_width <= 0.0:
        raise ValueError("wavelength and bin width must be positive")
    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    summary = {"waterline_m": waterline, "stern_x_m": stern,
               "reference_wavelength_m": wavelength, "bin_width_m": bin_width,
               "cuts": {}, "iso_surfaces": {}}
    profiles = {}
    for label, path in inputs.items():
        rows = read_raw(path)
        if _is_iso_surface(rows):
            summary["iso_surfaces"][label] = summarize_iso(
                rows, waterline, stern, wavelength)
        else:
            profile = extract_eta(rows, waterline, bin_width)
            profiles[label] = profile
            summary["cuts"][label] = summarize_cut(profile, stern, wavelength)
            _write_csv(out / f"{label}.csv", profile)
    if profiles:
        _plot(out / "wave_cut.svg", profiles, stern, wavelength)
    (out / "summary.json").write_text(
        json.dumps(summary, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    return summary


def _arguments(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--waterline", type=float, required=True)
    parser.add_argument("--stern", type=float, required=True)
    parser.add_argument("--lam", type=float, required=True, dest="wavelength")
    parser.add_argument("--bin", type=float, default=0.5, dest="bin_width")
    parser.add_argument("--out", type=Path, required=True)
    parser.add_argument("inputs", nargs="+", metavar="LABEL=PATH")
    return parser.parse_args(argv)


def main(argv=None) -> int:
    args = _arguments(argv)
    inputs = {}
    for item in args.inputs:
        if "=" not in item:
            raise SystemExit(f"input must be LABEL=PATH: {item}")
        label, path = item.split("=", 1)
        inputs[label] = Path(path)
    reduce_files(inputs, args.out, args.waterline, args.stern,
                 args.wavelength, args.bin_width)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
