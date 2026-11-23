"""Pipelay durable workflows (J-lay, reel-lay).

Thin, offline workflow router over the analytical pipelay pre-processing
utilities in ``digitalmodel.orcaflex.pipelay_analysis``. No OrcFxAPI / OrcaFlex
license is required.
"""

from digitalmodel.pipelay.workflow import router

__all__ = ["router"]
