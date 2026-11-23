"""Benchmark DOF amplitude/phase table builder helpers.

ABOUTME: _compute_dof_amplitude_rows, _compute_dof_phase_rows, and
_build_solver_column_table. Split from benchmark_correlation.py (WRK-593).

Leaf module — imports from benchmark_helpers and output_schemas only.
"""
from __future__ import annotations

import html as html_mod
from typing import Any, Dict, List

import numpy as np

from digitalmodel.hydrodynamics.diffraction.benchmark_helpers import (
    _AMPLITUDE_UNITS,
)
from digitalmodel.hydrodynamics.diffraction.output_schemas import (
    DOF,
    RAOComponent,
)


def _compute_dof_amplitude_rows(
    dof: DOF,
    h_indices: List[int],
    solver_results: Dict[str, Any],
    solver_names: List[str],
) -> List[Dict[str, Any]]:
    """Compute amplitude summary rows for a DOF using given headings."""
    dof_name = dof.name.lower()
    rows: List[Dict[str, Any]] = []
    for solver in solver_names:
        comp: RAOComponent = getattr(
            solver_results[solver].raos, dof_name,
        )
        periods = comp.frequencies.periods
        for hi in h_indices:
            mag = comp.magnitude[:, hi]
            peak_idx = int(np.argmax(mag))
            rows.append({
                "heading": f"{comp.headings.values[hi]:.0f}",
                "solver": solver,
                "peak_amp": f"{mag[peak_idx]:.4g}",
                "peak_period": f"{periods[peak_idx]:.2f}",
                "long_period_amp": f"{mag[0]:.4g}",
            })
    return rows


def _compute_dof_phase_rows(
    dof: DOF,
    h_indices: List[int],
    solver_results: Dict[str, Any],
    solver_names: List[str],
) -> List[Dict[str, Any]]:
    """Compute phase summary rows for a DOF using given headings."""
    dof_name = dof.name.lower()
    rows: List[Dict[str, Any]] = []
    for solver in solver_names:
        comp: RAOComponent = getattr(
            solver_results[solver].raos, dof_name,
        )
        for hi in h_indices:
            mag = comp.magnitude[:, hi]
            phase = comp.phase[:, hi]
            peak_idx = int(np.argmax(mag))
            rows.append({
                "heading": f"{comp.headings.values[hi]:.0f}",
                "solver": solver,
                "phase_at_peak": f"{float(phase[peak_idx]):.1f}",
                "long_period_phase": f"{float(phase[0]):.1f}",
            })
    return rows


def _build_solver_column_table(
    rows: List[Dict[str, Any]],
    mode: str,
    solver_names: List[str],
) -> str:
    """Build a comparison table with solver names as columns."""
    if not rows:
        return "<p><em>No data</em></p>"

    by_heading: Dict[str, Dict[str, Dict[str, Any]]] = {}
    for r in rows:
        h = r["heading"]
        solver = r.get("solver", "")
        if h not in by_heading:
            by_heading[h] = {}
        by_heading[h][solver] = r

    parts: List[str] = ['<table class="solver-table">']

    if mode == "amplitude":
        parts.append("<tr><th rowspan='2'>Hdg</th>")
        for solver in solver_names:
            parts.append(
                f"<th colspan='3'>"
                f"{html_mod.escape(solver)}</th>"
            )
        parts.append("</tr><tr>")
        for _ in solver_names:
            parts.append("<th>Peak</th><th>T(s)</th><th>LP</th>")
        parts.append("</tr>")

        for h, solvers in by_heading.items():
            parts.append(f"<tr><td>{h}&deg;</td>")
            for solver in solver_names:
                r = solvers.get(solver, {})
                parts.append(
                    f"<td>{r.get('peak_amp', '-')}</td>"
                    f"<td>{r.get('peak_period', '-')}</td>"
                    f"<td>{r.get('long_period_amp', '-')}</td>"
                )
            parts.append("</tr>")
    else:
        parts.append("<tr><th rowspan='2'>Hdg</th>")
        for solver in solver_names:
            parts.append(
                f"<th colspan='2'>"
                f"{html_mod.escape(solver)}</th>"
            )
        parts.append("</tr><tr>")
        for _ in solver_names:
            parts.append("<th>@Peak</th><th>LP</th>")
        parts.append("</tr>")

        for h, solvers in by_heading.items():
            parts.append(f"<tr><td>{h}&deg;</td>")
            for solver in solver_names:
                r = solvers.get(solver, {})
                parts.append(
                    f"<td>{r.get('phase_at_peak', '-')}</td>"
                    f"<td>{r.get('long_period_phase', '-')}</td>"
                )
            parts.append("</tr>")

    parts.append("</table>")
    return "\n".join(parts)
