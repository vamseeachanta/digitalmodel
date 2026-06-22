"""Generic parametric-sweep driver (parametrics P0).

``run_sweep`` expands a ``ParametricStudy`` to its full-factorial case matrix and
applies a per-case runner, collecting results fail-soft (a runner exception marks
that one case ``failed`` and the sweep continues). It adds NO new case-matrix or
parameter machinery — that all comes from ``ParametricStudy`` / ``ParameterSweep``.
"""

from __future__ import annotations

from typing import Callable

from digitalmodel.orcaflex.batch_parametric import (
    CaseResult,
    ParametricResultsSummary,
    ParametricStudy,
)

# A runner turns one expanded case config (base_config + injected params, plus
# ``case_id``) into a CaseResult.
CaseRunner = Callable[[dict], CaseResult]


def run_sweep(
    study: ParametricStudy, case_runner: CaseRunner
) -> ParametricResultsSummary:
    """Run ``case_runner`` over every case in ``study``; collect a summary."""
    summary = ParametricResultsSummary(study_name=study.name)
    param_names = [p.name for p in study.parameters]
    for cfg in study.generate_case_configs():
        case_id = str(cfg["case_id"])
        params = {name: cfg[name] for name in param_names}
        try:
            result = case_runner(cfg)
        except Exception as exc:  # noqa: BLE001 - one bad case must not kill the sweep
            result = CaseResult(
                case_id=case_id,
                parameters=params,
                status="failed",
                notes=f"{type(exc).__name__}: {exc}",
            )
        summary.results.append(result)
    return summary
