# ABOUTME: Thin engine router exposing the #2157 crack-like-flaw coordinator under the
# ABOUTME: engine basename "crack_fe_ffs"; result kind in_memory, key crack_fe_ffs.
"""Engine route for :mod:`crack_fe_assessment`.

The router builds the case from ``cfg["crack_fe_assessment"]`` (paths relative to the
repository root), runs :func:`crack_fe_assessment.run` with full receipt validation, and
parks ``CrackAssessmentResult.to_dict()`` on ``cfg["crack_fe_ffs"]`` so the registry
descriptor ``result: {kind: in_memory, key: crack_fe_ffs}`` can locate it. No
assessment physics lives here.
"""

from __future__ import annotations

from typing import Any

from digitalmodel.asset_integrity.assessment.crack_fe_assessment import (
    case_from_cfg,
    run,
)


class CrackFEWorkflow:
    """Engine adapter for the crack-like-flaw coordinator (basename ``crack_fe_ffs``)."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        result = run(case_from_cfg(cfg))
        cfg[cfg["basename"]] = result.to_dict()
        return cfg
