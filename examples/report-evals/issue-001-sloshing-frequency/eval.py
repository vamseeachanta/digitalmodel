# Copyright (c) 2026 AceEngineer. Licensed under the MIT License.
"""Eval: 2D tank sloshing, first-mode frequency from OpenFOAM against linear theory.

Newsletter issue 1 states that the sloshing CFD study reproduced the tank's
small-amplitude first-mode frequency within about 0.30 % of linear theory
across the tested fill levels. This eval recomputes the linear theory, the
errors and the Richardson extrapolation from the archived CFD outputs, and
compares them with results.json. Standard library only. It does not rerun
OpenFOAM (see PROMPT.md for that optional step).

    python examples/report-evals/issue-001-sloshing-frequency/eval.py
    python examples/report-evals/issue-001-sloshing-frequency/eval.py --compare my-results.json
"""

from __future__ import annotations

import json
import math
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE.parent))
import evalkit  # noqa: E402

REPO_ROOT = HERE.parents[2]
BENCHMARK = "docs/api/structural/sloshing-cfd-benchmark.json"
CONVERGENCE = "docs/api/cfd/sloshing-convergence.json"


def add_args(ap):
    ap.add_argument("--repo-root", type=Path, default=REPO_ROOT,
                    help="digitalmodel clone that holds the archived CFD outputs")


def omega_linear(h_over_l: float) -> float:
    """Non-dimensional first-mode frequency of a rectangular tank, linear potential theory.

    omega1 * sqrt(L/g) = sqrt(pi * tanh(pi * h / L)) (Faltinsen and Timokha,
    Sloshing, 2009, first natural mode of a 2D rectangular tank).
    """
    return math.sqrt(math.pi * math.tanh(math.pi * h_over_l))


def compute(args):
    root = args.repo_root
    values, notes = {}, {}
    context = {"repo_root": str(root), "repo_head": evalkit.git_head(root),
               "sha256": {p: evalkit.sha256_file(root / p) for p in (BENCHMARK, CONVERGENCE)}}
    bench = json.loads((root / BENCHMARK).read_text(encoding="utf-8"))
    g = float(bench["meta"]["g"])
    errors = []
    for p in bench["free_decay"]["points"]:
        if p.get("solver_status") != "completed":
            continue
        tag = f"hl{round(p['h_over_L'] * 100):02d}"
        om = omega_linear(p["h_over_L"])
        values[f"omega1_linear_{tag}"] = round(om, 4)
        # Measured non-dimensional frequency from the archived FFT frequency.
        breadth = float(bench["free_decay"]["breadth_m"])
        om_meas = 2 * math.pi * p["freq_meas_hz"] * math.sqrt(breadth / g)
        values[f"omega1_cfd_{tag}"] = round(om_meas, 4)
        err = abs(om_meas / om - 1) * 100
        values[f"error_pct_{tag}"] = round(err, 2)
        errors.append(err)
    values["max_error_pct_all_fills"] = round(max(errors), 2)

    conv = json.loads((root / CONVERGENCE).read_text(encoding="utf-8"))
    # L = 1 m, h/L = 0.30 (convergence case).
    f_lin = math.sqrt(g * math.pi * math.tanh(math.pi * 0.30)) / (2 * math.pi)
    values["freq_linear_hz_l1_hl30"] = round(f_lin, 6)
    mesh = {m["cells_per_breadth"]: m["freq_meas_hz"] for m in conv["mesh_sweep"]}
    values["finest_mesh_error_pct"] = round(abs(mesh[160] / f_lin - 1) * 100, 3)
    # Richardson extrapolation on the ratio-2 triple 40, 80, 160 cells per breadth
    # (Roache, 1998; ASME V&V 20-2009 GCI with safety factor 1.25).
    f1, f2, f3 = mesh[160], mesh[80], mesh[40]
    order = math.log((f2 - f3) / (f1 - f2)) / math.log(2)
    values["richardson_order"] = round(order, 3)
    values["richardson_freq_hz"] = round(f1 + (f1 - f2) / (2 ** order - 1), 6)
    values["gci_fine_pct"] = round(1.25 * abs((f1 - f2) / f1) / (2 ** order - 1) * 100, 4)
    return values, notes, context


if __name__ == "__main__":
    raise SystemExit(evalkit.main(HERE / "results.json", compute, add_args))
