#!/usr/bin/env python
"""Reduce the forced-roll CFD to a tuned-liquid-damper SDOF + f1(fill) tuning (#1433).

The anti-roll-tank literature (Carette 2023; Moaleji & Greig 2007) says the design
deliverable is a single-DOF reduction — added roll inertia + damping (an equivalent
damping ratio) — plus an f1(fill) tuning curve intersecting the vessel roll
frequency, NOT a standalone "best fill." This turns the committed forced-roll CFD
into exactly that:

* re-reduces each forced-roll case (raw moment.dat) to the full in-phase / quadrature
  contract (the manifest kept only quad + amplitude), then feeds
  SloshingCouplingModel to get added roll inertia A44 = -in_phase/omega^2 and added
  roll damping B44 = quad at each fill's resonance;
* fits an equivalent damping ratio zeta per fill from the wall-run-up response curve
  (SDOF amplification 1/sqrt((1-r^2)^2+(2 zeta r)^2));
* records the analytical rectangular f1(fill) basis so the page can build an
  interactive "tune to your roll period" curve at any tank size.

Writes docs/api/cfd/sloshing-sdof.json. No new CFD — reads the existing case tree.
"""
from __future__ import annotations

import argparse
import json
import math
from pathlib import Path
from typing import Any, Dict, List

from digitalmodel.solvers.openfoam.validation.sloshing_2d import parse_roll_moment
from digitalmodel.solvers.openfoam.validation.sloshing_sweep import reduce_roll_moment
from digitalmodel.solvers.openfoam.sloshing_coupling import SloshingCouplingModel

_REPO = Path(__file__).resolve().parents[2]
_OUT = _REPO / "docs" / "api" / "cfd" / "sloshing-sdof.json"
_FORCED = _REPO / "docs" / "api" / "structural" / "sloshing-forced-response.json"
ROLL_DEG = 4.0
G = 9.80665


def _fit_zeta(pairs: List[tuple]) -> float:
    """Best damped-oscillator zeta for run-up (r, normalised amplitude), per fill."""
    def amp(r, z):
        return 1.0 / math.sqrt((1 - r * r) ** 2 + (2 * z * r) ** 2)
    best_z, best_e = 0.1, float("inf")
    z = 0.02
    while z <= 0.6:
        a1 = amp(1.0, z)
        e = sum((y - amp(r, z) / a1) ** 2 for r, y in pairs)
        if e < best_e:
            best_e, best_z = e, z
        z += 0.005
    return round(best_z, 3)


def main(argv: List[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description="Sloshing SDOF + tuning reduction (#1433)")
    ap.add_argument("--work-dir", type=Path, default=Path("/mnt/local-analysis/sloshing_cfd_work"))
    args = ap.parse_args(argv)

    forced = json.loads(_FORCED.read_text())
    breadth = forced.get("tank", {}).get("breadth_m") or forced["fills"][0].get("breadth_m", 0.9)

    # 1) re-reduce every forced case to the full contract (in_phase + quad).
    rows: List[Dict[str, Any]] = []
    for f in forced["fills"]:
        hl = f["h_over_L"]
        t1 = f.get("T1_analytical_s")
        for p in f["forced"]:
            if p.get("status") != "completed":
                continue
            spec_dir = args.work_dir / f"resp_fr_hl{round(hl*100):02d}_r{round(p['period_ratio']*100):03d}"
            try:
                mt, mz = parse_roll_moment(spec_dir)
                red = reduce_roll_moment(mt, mz, p["drive_period_s"],
                                         fill_level=hl, roll_amplitude_deg=ROLL_DEG)
                rows.append(red)
            except (FileNotFoundError, RuntimeError, ValueError):
                continue

    model = SloshingCouplingModel.from_rows(rows) if rows else None

    # 2) per-fill SDOF summary at resonance + zeta from the run-up response.
    fills_out: List[Dict[str, Any]] = []
    for f in forced["fills"]:
        hl = f["h_over_L"]
        t1 = f.get("T1_analytical_s")
        omega_n = 2.0 * math.pi / t1 if t1 else None
        # normalised run-up response for the zeta fit
        pts = [(p["drive_period_s"] / t1, p["runup_amp_m"]) for p in f["forced"]
               if p.get("runup_amp_m") and t1]
        zeta = None
        if pts:
            peak = max(a for _, a in pts)
            zeta = _fit_zeta([(r, a / peak) for r, a in pts])
        coeff = None
        if model and omega_n:
            try:
                c = model.moment_coefficients(omega=omega_n, fill_level=hl)
                coeff = {"added_roll_inertia_A44": round(c.added_roll_inertia, 4),
                         "added_roll_damping_B44": round(c.added_roll_damping, 4),
                         "added_roll_stiffness_K44": round(c.added_roll_stiffness, 4)}
            except Exception:  # noqa: BLE001
                coeff = None
        fills_out.append({
            "h_over_L": hl,
            "natural_period_s": t1,
            "omega_n_rad_s": round(omega_n, 4) if omega_n else None,
            "equivalent_damping_ratio_zeta": zeta,
            "resonance_coefficients": coeff,
            "peak_quad_coeff": round(max((p.get("quad_coeff") or 0) for p in f["forced"]), 4),
        })

    manifest = {
        "meta": {"generated_by": "scripts/cfd/reduce_sloshing_sdof.py", "epic": "#1429", "issue": "#1433",
                 "note": "SDOF reduction of the forced-roll CFD; zeta fitted from wall run-up; A44/B44 from SloshingCouplingModel.",
                 "roll_amplitude_deg": ROLL_DEG},
        "tank": {"shape": "rectangular", "breadth_m": breadth},
        "sdof_by_fill": fills_out,
        # analytical rectangular f1(fill) basis (CFD-validated): f1 = sqrt(pi*g/L*tanh(pi*h/L))/(2pi)
        "tuning_basis": {"relation": "omega1^2 = (pi*g/L)*tanh(pi*h/L)",
                         "g": G, "note": "Interactive tuning: for tank length L and target roll period T_roll, the tuning fill is where f1(fill)=1/T_roll."},
    }
    _OUT.parent.mkdir(parents=True, exist_ok=True)
    _OUT.write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"wrote {_OUT.relative_to(_REPO)} ({len(fills_out)} fills, {len(rows)} contract rows)")
    for x in fills_out:
        print(f"  h/L={x['h_over_L']}: T1={x['natural_period_s']}s zeta={x['equivalent_damping_ratio_zeta']} "
              f"coeff={x['resonance_coefficients']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
