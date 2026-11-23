"""UV-workflow router for the offline FOWT watch-circle vs dynamic-cable check.

Wraps the closed-form, solver-free ``mooring_design_fowt`` watch-circle check so
it is dispatchable from a committed ``input.yml`` via
``uv run python -m digitalmodel <input.yml>`` (basename ``fowt_mooring``), with no
OrcaFlex licence required. This is a preliminary feasibility screen; final
dynamic-cable curvature design needs a licensed FE/lumped-mass dynamic solver
(see ``mooring_design_fowt`` module docstring).
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from digitalmodel.orcaflex.mooring_design_fowt import (
    DynamicCableConfig,
    FloaterType,
    MooringType,
    check_watch_circle_vs_cable,
)


class FOWTMooringWorkflow:
    """Run the FOWT watch-circle vs dynamic-cable MBR check from a cfg dict."""

    def router(self, cfg: dict) -> dict:
        fowt = cfg.get("fowt_mooring") or {}
        watch_circle_radius = float(fowt["watch_circle_radius"])

        cable_cfg = self._build_cable(fowt)
        result = check_watch_circle_vs_cable(watch_circle_radius, cable_cfg)

        cfg["fowt_mooring"] = {
            **fowt,
            "result": self._json_safe(result.model_dump()),
        }
        summary_path = self._write_summary(cfg)
        if summary_path is not None:
            cfg.setdefault("outputs", {})["summary_json"] = str(summary_path)
        return cfg

    def _build_cable(self, fowt: dict) -> DynamicCableConfig:
        cable = dict(fowt["cable"])
        if "mooring_type" in cable:
            cable["mooring_type"] = MooringType(cable["mooring_type"])
        if "floater_type" in cable:
            cable["floater_type"] = FloaterType(cable["floater_type"])
        return DynamicCableConfig(**cable)

    def _json_safe(self, obj: Any) -> Any:
        if isinstance(obj, dict):
            return {k: self._json_safe(v) for k, v in obj.items()}
        if isinstance(obj, (list, tuple)):
            return [self._json_safe(v) for v in obj]
        if isinstance(obj, (MooringType, FloaterType)):
            return obj.value
        return obj

    def _write_summary(self, cfg: dict) -> Path | None:
        output_path = (cfg.get("output") or {}).get("summary_json")
        if not output_path:
            return None
        path = Path(output_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(cfg["fowt_mooring"], indent=2))
        return path
