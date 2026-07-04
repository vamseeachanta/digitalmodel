"""UV-workflow router for floating-wind LCOE/TOTEX (issue #1226).

Closing slice of the economics epic (#1220): make the LCOE engine dispatchable
from a committed ``input.yml`` via the digitalmodel engine::

    uv run python -m digitalmodel examples/workflows/floating-wind-economics/input.yml

(basename ``floating_wind_economics``). The router builds a project economics
case from the config, computes LCOE, optionally runs a cost-driver sweep (#1223)
and a reliability scenario (#1222), writes a JSON summary and -- when asked -- a
one-page LCOE/TOTEX data sheet (#1226 report). Licence-free; no OrcaFlex.

Config shape (under the ``floating_wind_economics`` key)::

    floating_wind_economics:
      project:            # ProjectEconomics fields (turbine, capex, financial, ...)
        ...
      reliability:        # optional — failure-rate reduction scenario
        failure_rate_reduction: 0.5
      sensitivities:      # optional — list of {name, changes:[{path, scale|value}]}
        - name: design_life_30yr
          changes: [{path: financial.design_life_years, value: 30}]
    output:
      summary_json: path/to/summary.json     # optional
      data_sheet_html: path/to/datasheet.html # optional

For back-compat the project fields may sit directly under
``floating_wind_economics`` (no ``project`` wrapper) -- the packaged base case
(#1221) is exactly that shape.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from digitalmodel.floating_wind.economics import (
    LCOEResult,
    ProjectEconomics,
    compute_lcoe,
)
from digitalmodel.floating_wind.reliability import (
    ReliabilityScenario,
    lcoe_with_reliability,
)
from digitalmodel.floating_wind.sweep import DriverScenario, run_sweep

__all__ = ["FloatingWindEconomicsWorkflow"]


class FloatingWindEconomicsWorkflow:
    """Run a floating-wind LCOE/TOTEX case from a cfg dict."""

    def router(self, cfg: dict) -> dict:
        spec = cfg.get("floating_wind_economics") or {}
        econ = ProjectEconomics.from_mapping(spec.get("project", spec))
        result = compute_lcoe(econ)

        summary: dict[str, Any] = {
            "farm_capacity_mw": econ.farm_capacity_mw,
            "lcoe": self._lcoe_summary(result),
        }

        rel = spec.get("reliability")
        if rel:
            scenario = ReliabilityScenario(**rel)
            summary["reliability"] = {
                "failure_rate_reduction": scenario.failure_rate_reduction,
                "lcoe": self._lcoe_summary(lcoe_with_reliability(econ, scenario)),
            }

        sweep_rows = []
        sens = spec.get("sensitivities")
        if sens:
            scenarios = [DriverScenario(**s) for s in sens]
            sweep_rows = run_sweep(econ, scenarios)
            summary["sensitivities"] = [
                {
                    "name": row.name,
                    "lcoe_usd_per_mwh": round(row.lcoe_usd_per_mwh, 3),
                    "delta_vs_base": round(row.delta_vs_base, 3),
                    "pct_vs_base": round(row.pct_vs_base, 3),
                }
                for row in sweep_rows
            ]

        output = cfg.get("output") or {}
        summary_path = self._write_summary(output.get("summary_json"), summary)
        if summary_path is not None:
            summary["summary_json"] = str(summary_path)
        sheet_path = self._write_data_sheet(
            output.get("data_sheet_html"), econ, result, sweep_rows
        )
        if sheet_path is not None:
            summary["data_sheet_html"] = str(sheet_path)

        cfg["floating_wind_economics"] = {**spec, "result": summary}
        return cfg

    # -- helpers ------------------------------------------------------------

    def _lcoe_summary(self, result: LCOEResult) -> dict[str, Any]:
        return {
            "lcoe_usd_per_mwh": round(result.lcoe_usd_per_mwh, 3),
            "capex_per_mwh": round(result.capex_per_mwh, 3),
            "opex_per_mwh": round(result.opex_per_mwh, 3),
            "decomex_per_mwh": round(result.decomex_per_mwh, 3),
            "totex_discounted_usd": round(result.totex_discounted_usd, 1),
            "capex_usd": round(result.capex_usd, 1),
        }

    def _write_summary(self, path: str | None, summary: dict) -> Path | None:
        if not path:
            return None
        out = Path(path)
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(json.dumps(summary, indent=2))
        return out

    def _write_data_sheet(
        self,
        path: str | None,
        econ: ProjectEconomics,
        result: LCOEResult,
        sweep_rows: list,
    ) -> Path | None:
        if not path:
            return None
        # Import lazily so the LCOE path never requires the reporting backbone.
        from digitalmodel.floating_wind.economics_report import (
            write_lcoe_data_sheet,
        )

        return write_lcoe_data_sheet(econ, result, path, sweep_rows=sweep_rows)
