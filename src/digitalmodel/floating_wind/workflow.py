"""UV-workflow router for floating-wind concept screening (issue #1028).

Makes the offline concept-screening capability (issues #1023/#1024) dispatchable
from a committed ``input.yml`` via::

    uv run python -m digitalmodel examples/workflows/floating-wind-sizing/input.yml

(basename ``floating_wind_sizing``), licence-free. It builds the turbine
topside, site load cases, screening criteria and one-or-more concept sweeps from
the config, runs the screen and writes a JSON summary with the passing shortlist
per archetype. The high-fidelity OrcaWave/OrcaFlex tier (issue #1025) is a
separate, licensed step that consumes the shortlist.
"""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any

from digitalmodel.floating_wind.floaters import TurbineTopside
from digitalmodel.floating_wind.screening import (
    LoadCase,
    ScreeningCriteria,
    ScreeningResults,
    screen_concept,
)
from digitalmodel.orcaflex.batch_parametric import ParameterSweep

__all__ = ["FloatingWindSizingWorkflow"]


class FloatingWindSizingWorkflow:
    """Run a floating-wind concept screen from a cfg dict."""

    def router(self, cfg: dict) -> dict:
        spec = cfg.get("floating_wind_sizing") or {}

        topside = TurbineTopside(**spec["topside"])
        load_cases = [LoadCase(**lc) for lc in spec["load_cases"]]
        criteria = ScreeningCriteria(**(spec.get("criteria") or {}))

        results: list[ScreeningResults] = []
        for concept in spec["concepts"]:
            sweeps = [ParameterSweep(**s) for s in concept.get("sweeps", [])]
            results.append(
                screen_concept(
                    concept["archetype"],
                    dict(concept["base_params"]),
                    sweeps,
                    topside,
                    load_cases,
                    criteria,
                    study_name=concept.get("name", "floating_wind_screen"),
                )
            )

        summary = self._summarise(results, load_cases)
        cfg["floating_wind_sizing"] = {**spec, "result": summary}

        summary_path = self._write_summary(cfg, summary)
        if summary_path is not None:
            cfg.setdefault("outputs", {})["summary_json"] = str(summary_path)
        return cfg

    # -- summary ------------------------------------------------------------

    def _summarise(
        self, results: list[ScreeningResults], load_cases: list[LoadCase]
    ) -> dict[str, Any]:
        concepts = []
        for res in results:
            shortlist = [
                self._variant_row(v)
                for v in sorted(
                    res.passing(),
                    key=lambda v: v.governing_margin,
                    reverse=True,
                )
            ]
            concepts.append(
                {
                    "archetype": res.archetype.value,
                    "n_variants": res.n_variants,
                    "n_passed": res.n_passed,
                    "shortlist": shortlist,
                }
            )
        return {
            "load_cases": [lc.name for lc in load_cases],
            "n_concepts": len(results),
            "total_variants": sum(r.n_variants for r in results),
            "total_passed": sum(r.n_passed for r in results),
            "concepts": concepts,
        }

    def _variant_row(self, v) -> dict[str, Any]:
        p = v.properties
        return {
            "case_id": v.case_id,
            "params": self._json_safe(v.params),
            "displacement_t": round(p.displacement_t, 1),
            "steel_mass_t": round(p.steel_mass_t, 1),
            "ballast_mass_t": round(p.ballast_mass_t, 1),
            "GM_m": round(p.GM_m, 3),
            "heave_period_s": self._json_safe(p.heave_natural_period_s),
            "pitch_period_s": self._json_safe(p.pitch_natural_period_s),
            "governing_check": v.governing_check,
            "governing_margin": round(v.governing_margin, 4),
        }

    def _json_safe(self, obj: Any) -> Any:
        if isinstance(obj, dict):
            return {k: self._json_safe(val) for k, val in obj.items()}
        if isinstance(obj, (list, tuple)):
            return [self._json_safe(val) for val in obj]
        if isinstance(obj, float):
            if math.isinf(obj):
                return "inf"
            if math.isnan(obj):
                return None
            return obj
        return obj

    def _write_summary(self, cfg: dict, summary: dict) -> Path | None:
        output_path = (cfg.get("output") or {}).get("summary_json")
        if not output_path:
            return None
        path = Path(output_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(json.dumps(summary, indent=2))
        return path
