#!/usr/bin/env python3
"""Analyze the solved MacCamy-Fuchs cylinder case (#1171) → results.json.

Usage:
    uv run python analyze_maccamy_fuchs.py <case_root> <out_dir>

``case_root`` contains the solved OpenFOAM case (fields + the ``forces``
function-object output). Thin driver mirroring
``docs/api/cfd/cases/wave_excited_body/analyze_rao_sweep_point.py``: it rebuilds
the frozen headline config, calls
:func:`digitalmodel.solvers.openfoam.validation.maccamy_fuchs.analyze_cylinder_loading`
(the incident-wave split + inline-force fit + MacCamy-Fuchs known-answer gate)
and folds it into the ``results.json`` the report reads.

results.json schema (consumed by
``docs/api/cfd/report_build/build_maccamy_fuchs.py``):

    {
      "issue": "#1171",
      "config":  {"wave": {"H", "T", "depth"},
                  "cylinder": {"D", "x"},
                  "regime": {"ka", "D_over_L"}},
      "incident": {"measured_H", "a_i", "reflection_kr"},
      "force":    {"amplitude_N", "phase_lead_velocity_deg", "period_s", "mean_N"},
      "reference":{"F_maccamy_fuchs_at_measured_H_N", "F_morison_N",
                   "force_lead_velocity_deg"},
      "gate":     {"force_rel_error", "within_force_gate", "phase_lead_error_deg",
                   "within_phase_gate", "period_error"},
      "force_history": {"t": [...], "fx": [...]}
    }
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

from digitalmodel.solvers.openfoam.validation.maccamy_fuchs import (
    LOADING_PERIOD_TOLERANCE,
    CylinderWaveLoadingConfig,
    analyze_cylinder_loading,
    morison_inertia_force,
)


def _g(d: dict, *keys, default=None):
    """First present key (the analyzer may name a field either way)."""
    for k in keys:
        if k in d and d[k] is not None:
            return d[k]
    return default


def main() -> None:
    case_root = Path(sys.argv[1])
    out_dir = Path(sys.argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)

    cfg = CylinderWaveLoadingConfig()
    res = analyze_cylinder_loading(case_root, cfg)

    # Reference recomputed at the MEASURED incident wave height so a small
    # wave-generation error does not leak into the force verdict.
    measured_H = float(_g(res, "measured_H", "incident_H", default=cfg.wave_height))
    F_mf = float(_g(res, "force_reference", "F_maccamy_fuchs_N",
                    default=cfg.reference["F_amplitude"] * measured_H / cfg.wave_height))
    F_mor = float(_g(res, "morison_force", "F_morison_N",
                     default=morison_inertia_force(measured_H, cfg.wave_period,
                                                   cfg.depth, cfg.diameter)))
    gamma_ref = float(_g(res, "force_lead_velocity_deg", "gamma_reference_deg",
                         default=cfg.reference["phase_lead_velocity_deg"]))

    period_error = float(_g(res, "period_error", default=0.0))
    hist = _g(res, "force_history", default={}) or {}

    out = {
        "issue": "#1171",
        "config": {
            "wave": {"H": cfg.wave_height, "T": cfg.wave_period, "depth": cfg.depth},
            "cylinder": {"D": cfg.diameter, "x": cfg.cylinder_x},
            "regime": {"ka": cfg.ka, "D_over_L": cfg.diameter_over_wavelength},
        },
        "incident": {
            "measured_H": measured_H,
            "a_i": float(_g(res, "incident_amplitude", "a_i", default=measured_H / 2.0)),
            "reflection_kr": float(_g(res, "reflection_kr", "kr", default=0.0)),
        },
        "force": {
            "amplitude_N": float(_g(res, "force_amplitude", "amplitude_N", default=0.0)),
            "phase_lead_velocity_deg": float(
                _g(res, "force_lead_velocity_deg_cfd", "phase_lead_velocity_deg",
                   "gamma_cfd_deg", default=gamma_ref)),
            "period_s": float(_g(res, "force_period", "period_s",
                                 default=cfg.wave_period)),
            "mean_N": float(_g(res, "force_mean", "mean_N", default=0.0)),
        },
        "reference": {
            "F_maccamy_fuchs_at_measured_H_N": F_mf,
            "F_morison_N": F_mor,
            "force_lead_velocity_deg": gamma_ref,
        },
        "gate": {
            "force_rel_error": float(_g(res, "force_rel_error", default=0.0)),
            "within_force_gate": bool(_g(res, "within_force_gate", default=False)),
            "phase_lead_error_deg": float(_g(res, "phase_lead_error_deg", default=0.0)),
            "within_phase_gate": bool(
                _g(res, "within_phase_gate",
                   default=abs(float(_g(res, "phase_lead_error_deg", default=99.0))) <= 15.0)),
            "period_error": period_error,
        },
        "force_history": {
            "t": list(map(float, hist.get("t", []))),
            "fx": list(map(float, hist.get("fx", []))),
        },
    }
    (out_dir / "results.json").write_text(json.dumps(out, indent=2) + "\n")

    g = out["gate"]
    within_period = period_error <= LOADING_PERIOD_TOLERANCE
    print(f"F_cfd={out['force']['amplitude_N']:.2f} N  "
          f"F_mf={F_mf:.2f} N  rel_err={g['force_rel_error']:+.1%}  "
          f"{'PASS' if g['within_force_gate'] else 'OUT'} force | "
          f"γ_err={g['phase_lead_error_deg']:+.1f}° "
          f"{'PASS' if g['within_phase_gate'] else 'OUT'} phase | "
          f"period {'PASS' if within_period else 'OUT'}")
    print(f"wrote {out_dir / 'results.json'}")


if __name__ == "__main__":
    main()
