#!/usr/bin/env python
"""Coupled vessel + anti-roll-tank two-peak response — the classic tuned-absorber
split and its merging as tank damping rises (issue #1433, epic #1429, backlog
item 2).

Moaleji & Greig (2007): a lightly damped anti-roll tank, tuned near the vessel
roll frequency, splits the vessel roll response into TWO adverse peaks straddling
the tuning; as the tank damping rises the two peaks merge into a single
controlled peak, and beyond the optimum the tank over-damps and the peak grows
back. This is the tuned-vibration-absorber (Den Hartog) physics — a COUPLED
vessel+tank phenomenon, not an isolated-tank effect.

This reduction does NOT run new CFD. It takes the tank damping ladder our
forced-roll CFD already measured (the equivalent damping ratio zeta rises with
fill: ~0.10 / 0.18 / 0.31 at h/L = 0.30 / 0.50 / 0.70, from the SDOF reduction
sloshing-sdof.json, PR #1446) and feeds it into the coupled vessel+tank equations
to show where those measured damping levels sit relative to the two-peak/merge
transition. The tank damping is what an internal baffle or damping screen sets
physically; here it is set by fill, and the fill ladder happens to bracket the
optimum.

Den Hartog tuned-absorber response (primary/vessel roll undamped, absorber/tank
damped) — vessel roll amplitude normalised by its static deflection:

    |X1/Xst|^2 = [ (2 z g)^2 + (g^2 - f^2)^2 ]
               / [ (2 z g)^2 (g^2 - 1 + mu g^2)^2
                   + ( mu f^2 g^2 - (g^2 - 1)(g^2 - f^2) )^2 ]

with g = omega/omega_vessel, f = omega_tank/omega_vessel (tuning ratio, ~1),
mu = tank/vessel mass ratio, z = tank (absorber) damping ratio. The mass ratio
is a representative anti-roll-tank design value; the two-peak split and its
merge are generic in mu (only the peak SEPARATION scales ~sqrt(mu)), while the
transition is governed by z relative to the Den Hartog optimum
z_opt = sqrt(3 mu / (8 (1+mu)^3)).

    uv run python scripts/cfd/reduce_tld_two_peak.py
"""
from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any, Dict, List

_REPO = Path(__file__).resolve().parents[2]
_SDOF = _REPO / "docs" / "api" / "cfd" / "sloshing-sdof.json"
_MANIFEST = _REPO / "docs" / "api" / "structural" / "sloshing-tld-twopeak.json"

MU = 0.05                # representative anti-roll-tank / vessel roll mass ratio
TUNING = 1.0             # f = omega_tank / omega_vessel (tuned)
G_LO, G_HI, N = 0.75, 1.30, 111   # frequency-ratio grid


def _response(g: float, mu: float, f: float, z: float) -> float:
    num = (2 * z * g) ** 2 + (g * g - f * f) ** 2
    den = ((2 * z * g) ** 2 * (g * g - 1 + mu * g * g) ** 2
           + (mu * f * f * g * g - (g * g - 1) * (g * g - f * f)) ** 2)
    return math.sqrt(num / den)


def _z_opt(mu: float) -> float:
    return math.sqrt(3 * mu / (8 * (1 + mu) ** 3))


def _analyse(gs: List[float], ys: List[float]) -> Dict[str, Any]:
    peaks = [(gs[i], ys[i]) for i in range(1, len(ys) - 1)
             if ys[i] > ys[i - 1] and ys[i] > ys[i + 1]]
    return {"n_peaks": len(peaks),
            "peak_freq_ratios": [round(p[0], 4) for p in peaks],
            "peak_response": round(max(ys), 4),
            "regime": ("under-damped (two-peak split)" if len(peaks) >= 2
                       else "single peak")}


def _cfd_damping_ladder() -> List[Dict[str, Any]]:
    """The CFD-measured tank damping ladder (zeta vs fill) from the SDOF
    reduction; degrade to a documented fallback if the manifest is absent."""
    if _SDOF.exists():
        sdof = json.loads(_SDOF.read_text())
        out = []
        for s in sdof.get("sdof_by_fill", []):
            out.append({"label": f"CFD h/L={s['h_over_L']:.2g}",
                        "zeta": round(s["equivalent_damping_ratio_zeta"], 4),
                        "source": "CFD forced-roll SDOF (sloshing-sdof.json)",
                        "h_over_L": s["h_over_L"]})
        if out:
            return out
    return [{"label": "CFD h/L=0.3", "zeta": 0.095, "source": "fallback"},
            {"label": "CFD h/L=0.5", "zeta": 0.180, "source": "fallback"},
            {"label": "CFD h/L=0.7", "zeta": 0.305, "source": "fallback"}]


def build() -> Dict[str, Any]:
    gs = [G_LO + (G_HI - G_LO) * i / (N - 1) for i in range(N)]
    z_opt = _z_opt(MU)
    # A light-damping reference + the CFD ladder + the Den Hartog optimum.
    ladder = ([{"label": "very light", "zeta": 0.03, "source": "reference"}]
              + _cfd_damping_ladder()
              + [{"label": "Den Hartog optimum", "zeta": round(z_opt, 4),
                  "source": f"z_opt(mu={MU})"}])
    # dedupe by rounded zeta, keep first label; sort by zeta
    seen: Dict[float, Dict[str, Any]] = {}
    for d in ladder:
        seen.setdefault(round(d["zeta"], 3), d)
    curves = []
    for d in sorted(seen.values(), key=lambda x: x["zeta"]):
        ys = [_response(g, MU, TUNING, d["zeta"]) for g in gs]
        curves.append({**d, **_analyse(gs, ys),
                       "response": [round(y, 4) for y in ys]})
    # rank the CFD FILL curves specifically (the analytical optimum is a
    # reference line, not a fill you can choose).
    cfd_curves = [c for c in curves if str(c.get("source", "")).startswith("CFD")] or curves
    best = min(cfd_curves, key=lambda c: c["peak_response"])
    manifest = {
        "meta": {"generated_by": "scripts/cfd/reduce_tld_two_peak.py",
                 "epic": "#1429", "issue": "#1433",
                 "model": "Den Hartog tuned-vibration-absorber (vessel roll + anti-roll tank); vessel roll undamped, tank damped",
                 "damping_source": "tank equivalent damping ratio zeta measured by forced-roll CFD (sloshing-sdof.json, PR #1446); rises with fill",
                 "note": "Reproduces the Moaleji & Greig (2007) two-peak anti-roll split and its merging as tank damping rises. mu is a representative design mass ratio; the split/merge transition is governed by zeta relative to z_opt. Tank damping is physically set by an internal baffle/damping screen; here it is set by fill.",
                 "mass_ratio_mu": MU, "tuning_ratio_f": TUNING,
                 "z_opt": round(z_opt, 4)},
        "frequency_ratio_grid": [round(g, 4) for g in gs],
        "curves": curves,
        "summary": {
            "best_fill_label": best["label"],
            "best_fill_zeta": best["zeta"],
            "best_fill_response": best["peak_response"],
            "under_damped_labels": [c["label"] for c in cfd_curves if c["n_peaks"] >= 2],
            "over_damped_labels": [c["label"] for c in cfd_curves
                                   if c["n_peaks"] < 2 and c["peak_response"] > best["peak_response"]],
            "note": "The CFD fill-damping ladder brackets the optimum: light damping (low fill) shows the adverse two-peak split; near-optimal damping (mid fill) gives the lowest single peak; heavier damping (high fill) over-damps and the peak grows back.",
        },
    }
    _MANIFEST.parent.mkdir(parents=True, exist_ok=True)
    _MANIFEST.write_text(json.dumps(manifest, indent=2) + "\n")
    return manifest


def main() -> int:
    m = build()
    print(f"z_opt(mu={MU}) = {m['meta']['z_opt']}")
    for c in m["curves"]:
        print(f"  zeta={c['zeta']:.3f} {c['label']:<20} {c['regime']:<28} "
              f"peaks@{c['peak_freq_ratios']} max={c['peak_response']}")
    print(f"best fill: {m['summary']['best_fill_label']} "
          f"(zeta={m['summary']['best_fill_zeta']}, resp={m['summary']['best_fill_response']})")
    print(f"-> {_MANIFEST.relative_to(_REPO)}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
