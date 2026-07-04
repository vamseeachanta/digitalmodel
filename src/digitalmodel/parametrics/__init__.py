"""Parametric sweeps over ready solver use-cases (parametrics P0, #968 / #943).

Thin harness that REUSES the existing solver-agnostic case-matrix expander
(``orcaflex.batch_parametric.ParametricStudy`` — no OrcFxAPI) rather than adding
a new parametric engine: a study (parameter sweeps + base config) expands to a
full-factorial case matrix; ``run_sweep`` applies a per-case runner and collects
``CaseResult``s into a ``ParametricResultsSummary``.

Pilots (one per solver, all offline / no license) live in ``pilots``:
- ANSYS padeye geometry sweep -> a per-case APDL ``.inp`` (status ``prepared``;
  the MAPDL solve is the licensed P1 step).
- OrcaWave water-depth sweep -> a per-case validated diffraction spec (status
  ``prepared``; the OrcaWave solve is the licensed P1 step).
- OrcaFlex FOWT watch-circle sweep -> real closed-form results (status
  ``completed``), since that workflow is analytical and runs offline.
"""

from digitalmodel.orcaflex.batch_parametric import (
    CaseResult,
    ParameterSweep,
    ParametricResultsSummary,
    ParametricStudy,
)

from digitalmodel.parametrics.sweep import CaseRunner, run_sweep

__all__ = [
    "CaseResult",
    "ParameterSweep",
    "ParametricResultsSummary",
    "ParametricStudy",
    "CaseRunner",
    "run_sweep",
]
