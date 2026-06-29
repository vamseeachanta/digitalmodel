# ABOUTME: Thin engine router exposing the Phase-1 FFS coordinator (assess_component)
# ABOUTME: under the engine basename "ffs" (workspace-hub#3285). kind: in_memory.
"""Engine route for the canonical FFS metal-loss coordinator.

This adapter gives ``asset_integrity/assessment/ffs_coordinator.assess_component``
an engine basename (``ffs``) so it is callable both from the durable CLI path
(``uv run python -m digitalmodel <input.yml>``) and from the deterministic
``digitalmodel.workflow_api.run_workflow("ffs-metal-loss", ...)`` entrypoint.

No assessment physics lives here -- every number comes from the validated
Phase-1 chain. The router only normalises the cfg inputs into a
:class:`FFSComponent` + grid, runs ``assess_component``, and parks the indexed
16-key ``FFSAssessmentResult.to_dict()`` (the #1066 Deckhand-API surface) on
``cfg["ffs"]`` so the registry ``result: {kind: in_memory, key: ffs}`` descriptor
can locate it.
"""

from __future__ import annotations

from typing import Any

import numpy as np

from digitalmodel.asset_integrity.assessment import (
    FFSComponent,
    assess_component,
)


class FFSWorkflow:
    """Engine adapter for the Phase-1 FFS coordinator (basename ``ffs``)."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        ffs_cfg = cfg.get("ffs_assessment")
        if not ffs_cfg:
            raise KeyError(
                "ffs workflow requires an 'ffs_assessment' block with "
                "'component' and 'grid' keys"
            )

        component = FFSComponent(**ffs_cfg["component"])
        grid = _to_grid(ffs_cfg["grid"])
        result = assess_component(
            component,
            grid,
            input_units=ffs_cfg.get("input_units", "in"),
            force_type=ffs_cfg.get("force_type"),
        )

        # in-memory locator target: cfg[basename] == cfg["ffs"] (the 16-key
        # indexed FFSAssessmentResult.to_dict() Deckhand-API shape, #1066).
        cfg[cfg["basename"]] = result.to_dict()
        return cfg


def _to_grid(grid: Any):
    """Normalise the cfg-supplied grid to a form ``assess_component`` accepts.

    A YAML fixture supplies the measurement lattice as a list-of-lists; convert
    that to a 2-D float ``np.ndarray``. A string is treated as a CSV path and
    passed through unchanged (``assess_component`` handles DataFrame/ndarray/CSV).
    """
    if isinstance(grid, (list, tuple)):
        return np.asarray(grid, dtype=float)
    return grid
