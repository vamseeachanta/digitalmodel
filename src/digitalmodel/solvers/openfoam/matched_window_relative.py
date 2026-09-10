"""Matched-window relative force reduction for free-surface resistance runs.

The pressure force in an unconverged free-surface calculation can retain a slowly
decaying wobble for thousands of iterations.  A cycle average remains the preferred
absolute estimate for one run.  For a relative comparison, this module averages both
runs over identical iteration windows and sweeps the common window end.  Correlated
drift then appears in both windowed absolutes instead of masquerading as a change
between runs.

The method assumes that the runs use the same speed, mesh family and numerical
method, that their wobbles have not fully decayed, and that the remaining drift is
substantially common-mode.  The reported correlation checks the last assumption.
"""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Any, Sequence

import numpy as np

from .force_cycle_average import extrema, load_force

COMPONENTS = ("total", "pressure", "viscous")
FALLBACK_WINDOW = 2_500.0


class WindowRangeError(ValueError):
    """Raised when the histories cannot supply a requested common window."""


def _period(t: np.ndarray, pressure: np.ndarray) -> float | None:
    """Estimate a full pressure-wobble period using cycle-average extrema logic."""
    start = max(float(t[0]), 500.0)
    _, points = extrema(t, pressure, start=start, smooth=25)
    if len(points) < 3:
        return None
    full_periods = [points[index][0] - points[index - 2][0] for index in range(2, len(points))]
    return float(np.mean(full_periods))


def _window_mean(t: np.ndarray, values: np.ndarray, end: float, window: float) -> float:
    selected = (t >= end - window) & (t <= end)
    if not np.any(selected):
        raise WindowRangeError(f"no samples in window ending at {end:g}")
    return float(np.mean(values[selected]))


def _correlation(a: Sequence[float], b: Sequence[float]) -> float | None:
    if len(a) < 2 or np.std(a) == 0.0 or np.std(b) == 0.0:
        return None
    value = float(np.corrcoef(a, b)[0, 1])
    return value if np.isfinite(value) else None


def _relative_at_end(
    histories: dict[str, tuple[np.ndarray, np.ndarray]], end: float, window: float
) -> float | None:
    a = _window_mean(*histories["a"], end, window)
    b = _window_mean(*histories["b"], end, window)
    return None if b == 0.0 else float((a / b - 1.0) * 100.0)


def analyse(
    case_a: str | Path,
    case_b: str | Path,
    *,
    window: float | None = None,
    end: float | None = None,
    sweep: float = 800.0,
    step: float = 100.0,
    component: str = "total",
    labels: tuple[str, str] = ("A", "B"),
) -> dict[str, Any]:
    """Compare two force histories over matched iteration windows.

    Relative statistics are computed placement-by-placement before their sample
    standard deviation is taken.  Both files must span the full selected window in
    a common iteration range; sweep placements that predate that range are omitted.
    """
    path_a, ta, total_a, pressure_a, viscous_a = load_force(Path(case_a))
    path_b, tb, total_b, pressure_b, viscous_b = load_force(Path(case_b))
    period_a, period_b = _period(ta, pressure_a), _period(tb, pressure_b)
    if period_a is None or period_b is None:
        estimated_period = FALLBACK_WINDOW / 2.0
        default_window = FALLBACK_WINDOW
        period_source = "fallback"
    else:
        estimated_period = (period_a + period_b) / 2.0
        default_window = 2.0 * estimated_period
        period_source = "pressure_extrema"
    selected_window = float(default_window if window is None else window)
    if selected_window <= 0 or sweep < 0 or step <= 0:
        raise ValueError("window and step must be positive, and sweep must be non-negative")

    for label, t in zip(labels, (ta, tb)):
        if float(t[-1] - t[0]) < selected_window:
            raise WindowRangeError(
                f"history {label} is shorter than the {selected_window:g}-iteration window"
            )
    common_start = max(float(ta[0]), float(tb[0]))
    common_end = min(float(ta[-1]), float(tb[-1]))
    if common_end - common_start < selected_window:
        raise WindowRangeError(
            f"available iteration ranges do not overlap by one {selected_window:g}-iteration window"
        )
    selected_end = common_end if end is None else float(end)
    if selected_end > common_end or selected_end - selected_window < common_start:
        raise WindowRangeError(
            f"window ending at {selected_end:g} is outside the common iteration range"
        )

    first_end = max(selected_end - float(sweep), common_start + selected_window)
    ends = list(np.arange(first_end, selected_end + step * 0.5, step, dtype=float))
    if not ends or ends[-1] > selected_end + 1e-9:
        ends = [value for value in ends if value <= selected_end + 1e-9]
    if not ends or ends[-1] < selected_end - 1e-9:
        ends.append(selected_end)

    raw = {
        "total": ((ta, total_a), (tb, total_b)),
        "pressure": ((ta, pressure_a), (tb, pressure_b)),
        "viscous": ((ta, viscous_a), (tb, viscous_b)),
    }
    names = COMPONENTS if component == "all" else (component,)
    rows: dict[str, Any] = {}
    for name in names:
        hist = {"a": raw[name][0], "b": raw[name][1]}
        means_a = [_window_mean(*hist["a"], place, selected_window) for place in ends]
        means_b = [_window_mean(*hist["b"], place, selected_window) for place in ends]
        relatives = [
            (a / b - 1.0) * 100.0 for a, b in zip(means_a, means_b) if b != 0.0
        ]
        if len(relatives) != len(ends):
            raise ValueError(f"{labels[1]} {name} window mean is zero")
        correlation = _correlation(means_a, means_b)
        rows[name] = {
            "mean_a": float(np.mean(means_a)),
            "mean_b": float(np.mean(means_b)),
            "relative_pct": float(np.mean(relatives)),
            "relative_sd_pct": float(np.std(relatives, ddof=1)) if len(relatives) > 1 else 0.0,
            "placements": len(ends),
            "window": selected_window,
            "wobble_periods_covered": selected_window / estimated_period,
            "correlation": correlation,
        }

    primary = "total" if component == "all" else component
    sensitivity_histories = {"a": raw[primary][0], "b": raw[primary][1]}
    sensitivity: dict[str, float | None] = {}
    for multiplier in (0.5, 1.0, 1.5, 2.0):
        length = default_window * multiplier
        key = f"{multiplier:g}x"
        sensitivity[key] = (
            _relative_at_end(sensitivity_histories, selected_end, length)
            if selected_end - length >= common_start
            else None
        )
    return {
        "files": [str(path_a), str(path_b)],
        "labels": list(labels),
        "periods": {"a": period_a, "b": period_b, "pair": estimated_period, "source": period_source},
        "default_window": default_window,
        "window": selected_window,
        "end": selected_end,
        "sweep": float(sweep),
        "step": float(step),
        "placement_ends": ends,
        "components": rows,
        "sensitivity_component": primary,
        "window_sensitivity": sensitivity,
    }


def _parse_labels(value: str) -> tuple[str, str]:
    labels = tuple(part.strip() for part in value.split(","))
    if len(labels) != 2 or not all(labels):
        raise argparse.ArgumentTypeError("labels must be two non-empty names separated by a comma")
    return labels[0], labels[1]


def _print(result: dict[str, Any]) -> None:
    label_a, label_b = result["labels"]
    print(
        f"matched windows: {label_a}/{label_b}; end {result['end']:.0f}; "
        f"window {result['window']:.0f} iterations"
    )
    for name, row in result["components"].items():
        corr = "n/a" if row["correlation"] is None else f"{row['correlation']:.3f}"
        print(
            f"{name:8s}: {label_a} {row['mean_a']:+.6g}  {label_b} {row['mean_b']:+.6g}  "
            f"relative {row['relative_pct']:+.3f} % +/- {row['relative_sd_pct']:.3f} %  "
            f"n={row['placements']}  periods={row['wobble_periods_covered']:.2f}  corr={corr}"
        )
        if row["correlation"] is not None and row["correlation"] < 0.5:
            print(
                f"WARNING: {name} correlation is below 0.5; the runs may not be in a "
                "comparable state and the relative is not trustworthy.",
                file=sys.stderr,
            )
    print(f"window-length sensitivity ({result['sensitivity_component']}, same end):")
    for multiplier, relative in result["window_sensitivity"].items():
        value = "unavailable" if relative is None else f"{relative:+.3f} %"
        print(f"  {multiplier:>4s}: {value}")


def main(argv: Sequence[str] | None = None) -> int:
    """Run the ``matched_window_relative`` command-line interface."""
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("case_a")
    parser.add_argument("case_b")
    parser.add_argument("--window", type=float)
    parser.add_argument("--end", type=float)
    parser.add_argument("--sweep", type=float, default=800.0)
    parser.add_argument("--step", type=float, default=100.0)
    parser.add_argument("--component", choices=(*COMPONENTS, "all"), default="total")
    parser.add_argument("--labels", type=_parse_labels, default=("A", "B"))
    parser.add_argument("--json")
    args = parser.parse_args(argv)
    try:
        result = analyse(
            args.case_a, args.case_b, window=args.window, end=args.end,
            sweep=args.sweep, step=args.step, component=args.component, labels=args.labels,
        )
    except WindowRangeError as error:
        print(f"error: {error}", file=sys.stderr)
        return 2
    _print(result)
    if args.json:
        Path(args.json).write_text(json.dumps(result, indent=2) + "\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
