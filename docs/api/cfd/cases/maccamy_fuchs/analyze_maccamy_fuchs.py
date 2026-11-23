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
    CylinderWaveLoadingConfig,
    analyze_cylinder_loading,
)


def main() -> None:
    case_root = Path(sys.argv[1])
    out_dir = Path(sys.argv[2])
    out_dir.mkdir(parents=True, exist_ok=True)

    cfg = CylinderWaveLoadingConfig()
    # analyze_cylinder_loading already returns the report schema (nested
    # config/incident/force/reference/gate/force_history) plus flat aliases,
    # and normalises by the MEASURED incident wave. Write it verbatim.
    res = analyze_cylinder_loading(case_root, cfg)
    (out_dir / "results.json").write_text(json.dumps(res, indent=2) + "\n")

    g, f, ref = res["gate"], res["force"], res["reference"]
    print(f"F_cfd={f['amplitude_N']:.2f} N  "
          f"F_mf={ref['F_maccamy_fuchs_at_measured_H_N']:.2f} N (at measured "
          f"H={res['incident']['measured_H']:.4f})  "
          f"rel_err={g['force_rel_error']:+.1%}  "
          f"{'PASS' if g['within_force_gate'] else 'OUT'} force | "
          f"lead={f['phase_lead_velocity_deg']:.1f}° vs {ref['force_lead_velocity_deg']:.1f}° "
          f"(err {g['phase_lead_error_deg']:+.1f}°) "
          f"{'PASS' if g['within_phase_gate'] else 'OUT'} phase | "
          f"period {'PASS' if g.get('within_period_gate') else 'OUT'}")
    print(f"wrote {out_dir / 'results.json'}")


if __name__ == "__main__":
    main()
