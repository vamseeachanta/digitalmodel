# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Engine-routed slope screening with a versioned JSON summary."""

from __future__ import annotations

from dataclasses import asdict
from typing import Any

from digitalmodel.geotechnical.cfg_workflows import _resolve_dir, _write_summary


class SlopeStabilityWorkflow:
    """Run Bishop and Spencer circular searches from a layered SI profile."""

    def router(self, cfg: dict[str, Any]) -> dict[str, Any]:
        """Calculate slope screening results and write the configured JSON sidecar.

        Args:
            cfg: Configuration with a slope_stability profile and output directory.

        Returns:
            Configuration containing the results and sidecar path.
        """
        from digitalmodel.geotechnical.slope_stability import analyze_profile

        workflow = cfg["slope_stability"]
        outputs = workflow.get("outputs", {})
        directory = _resolve_dir(cfg, outputs.get("directory", "results/slope_stability"))
        directory.mkdir(parents=True, exist_ok=True)
        result = analyze_profile(workflow["profile"], directory,
                                 num_slices=workflow.get("num_slices", 20))
        converged = all(method.search_converged for method in result.methods.values())
        status = "screening_only" if converged else "not_converged"
        payload = {"result": asdict(result), "screening_status": status}
        payload["summary_json"] = str(_write_summary(
            cfg, outputs, "results/slope_stability", "slope_stability_summary.json", payload
        ))
        cfg["slope_stability"] = {**workflow, **payload}
        cfg["screening_status"] = status
        return cfg
