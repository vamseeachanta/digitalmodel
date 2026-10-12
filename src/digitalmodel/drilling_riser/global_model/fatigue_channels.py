"""Wave-fatigue channels of one solved window (the plan's damage path, W5/W7 input).

At each station and at 8 points around the wall (outer fibre, ``THETAS_DEG``) the axial wall stress ``ZZ stress``
history of the main stage is rainflow counted by a conforming counter of ``fatigue/counting_contract.py``
(``digitalmodel.fatigue.rainflow.rainflow_count``: pyLife four-point, the residue counted as half cycles). OrcaFlex
``RainflowHalfCycles`` is kept for information: batch 7 found it 4-9 % short of the history span at 7 of about 70,000
points (``orcaflex_max_range``, ``orcaflex_invariant_ok``). The half-cycle ranges are binned on a
range derived from the observed maximum - bin width = max / ``n_bins``, the maximum in the last (closed) bin - so no
half-cycle is out of range (the defect the plan fixes in ``opp_time_series.py``, which clipped to a configured range).
Each point also carries the peak-to-valley invariant: the largest half-cycle must equal the span (max - min) of the
stress history within 1e-6 (the ``fatigue/counting_contract.py`` clause 1, residuals retained). Counts are half
cycles (W5 weights them 0.5). Units: kPa (OrcaFlex).

Stations: along ``Riser`` every section boundary and at most ``spacing_m`` apart (ends included); along ``Stack`` the
section boundaries; along ``Conductor`` (with a foundation) every 5 m over the top 30 m.
"""

from __future__ import annotations

from typing import Any, Iterable, Sequence

THETAS_DEG = (0.0, 45.0, 90.0, 135.0, 180.0, 225.0, 270.0, 315.0)
VARIABLE = "ZZ stress"
DEFAULT_SPACING_M = 25.0
DEFAULT_BINS = 100
CONDUCTOR_TOP_M = 30.0
CONDUCTOR_STEP_M = 5.0
INVARIANT_RTOL = 1.0e-6


def half_cycle_histogram(half_cycles: Iterable[float], *, n_bins: int = DEFAULT_BINS) -> dict[str, Any]:
    """Counts of half-cycle ranges in ``n_bins`` equal bins over [0, max]; the last bin is closed at the maximum."""
    hc = [float(x) for x in half_cycles]
    mx = max(hc) if hc else 0.0
    counts = [0] * n_bins
    width = mx / n_bins if mx > 0 else 0.0
    out = 0
    for x in hc:
        if x < 0 or x > mx:
            out += 1
            continue
        i = n_bins - 1 if width == 0 else min(int(x / width), n_bins - 1)
        counts[i] += 1
    return {"n_bins": n_bins, "bin_width": width, "max_range": mx, "half_cycles": len(hc), "counts": counts,
            "out_of_range": out}


COUNTER = "digitalmodel.fatigue.rainflow:rainflow_count (pyLife four-point, residue as half cycles)"


def half_cycles(series) -> list[float]:
    """Half-cycle ranges of ``series`` by the conforming counter (a full cycle is two half cycles)."""
    import numpy as np

    from digitalmodel.fatigue.rainflow import rainflow_count

    s = np.asarray(series, dtype=float)
    if s.size < 2:
        return []
    df = rainflow_count(s)
    out: list[float] = []
    for r, c in zip(df["stress_range"].to_numpy(dtype=float), df["cycles"].to_numpy(dtype=float)):
        out.extend([float(r)] * int(round(2.0 * c)))
    return out


def peak_to_valley_ok(half_cycles: Sequence[float], series: Sequence[float], *, rtol: float = INVARIANT_RTOL) -> bool:
    """The largest half-cycle range equals the span of the history (residuals retained as half cycles)."""
    if not series:
        return True
    span = max(series) - min(series)
    top = max(half_cycles) if len(half_cycles) else 0.0
    return abs(top - span) <= rtol * max(abs(span), 1.0)


def _arcs(lengths: Sequence[float], spacing: float) -> list[float]:
    out, arc = [0.0], 0.0
    for L in lengths:
        n = max(1, int(-(-L // spacing)))  # ceil
        for k in range(1, n + 1):
            out.append(arc + L * k / n)
        arc += L
    return out


def stations(spec, *, spacing_m: float = DEFAULT_SPACING_M) -> list[tuple[str, float, str]]:
    """(line, arc length from End A, label) of the fatigue stations."""
    out = [("Riser", a, f"riser@{a:.2f}") for a in _arcs([s.length_m for s in spec.riser], spacing_m)]
    secs = list(reversed(spec.stack))  # the stack line runs up from the datum
    arc = 0.0
    out.append(("Stack", 0.0, "stack:datum"))
    for lo, hi in zip(secs, secs[1:]):
        arc += lo.length_m
        out.append(("Stack", arc, f"stack:{lo.name}|{hi.name}"))
    if getattr(spec, "foundation", None) is not None:
        depth = spec.foundation.depth_m
        z = 0.0
        while z <= min(CONDUCTOR_TOP_M, depth) + 1e-9:
            out.append(("Conductor", depth - z, f"conductor@{z:.1f}m below datum"))
            z += CONDUCTOR_STEP_M
    return out


def extract(model, spec, ofx, *, spacing_m: float = DEFAULT_SPACING_M, n_bins: int = DEFAULT_BINS,
            period=None) -> dict[str, Any]:
    if period is None:
        from .orcaflex_run import main_period

        period = main_period(model, spec)
    doc: dict[str, Any] = {"variable": VARIABLE, "radial_position": "outer", "thetas_deg": list(THETAS_DEG),
                           "units": "kPa", "counts": "half cycles (weight 0.5 in the damage sum)", "counter": COUNTER,
                           "binning": "equal bins over [0, observed maximum], last bin closed", "stations": []}
    fails = out_total = ofx_fails = 0
    for line_name, arc, label in stations(spec, spacing_m=spacing_m):
        line = model[line_name]
        pts = []
        for th in THETAS_DEG:
            ex = ofx.oeLine(ArcLength=arc, RadialPos=1, Theta=th)
            series = [float(x) for x in line.TimeHistory(VARIABLE, period, ex)]
            hc = half_cycles(series)
            ofx_hc = [float(x) for x in line.RainflowHalfCycles(VARIABLE, period, objectExtra=ex)]
            h = half_cycle_histogram(hc, n_bins=n_bins)
            h["orcaflex_max_range"] = max(ofx_hc) if ofx_hc else 0.0
            h["orcaflex_invariant_ok"] = peak_to_valley_ok(ofx_hc, series)
            ofx_fails += not h["orcaflex_invariant_ok"]
            h["theta_deg"] = th
            h["span"] = (max(series) - min(series)) if series else 0.0
            h["mean_stress"] = sum(series) / len(series) if series else 0.0
            h["invariant_ok"] = peak_to_valley_ok(hc, series)
            fails += not h["invariant_ok"]
            out_total += h["out_of_range"]
            pts.append(h)
        doc["stations"].append({"line": line_name, "arc_m": arc, "label": label, "points": pts})
    doc["invariant_failures"] = fails
    doc["out_of_range"] = out_total
    doc["orcaflex_invariant_failures"] = ofx_fails  # information: the OrcaFlex count is not used
    return doc
