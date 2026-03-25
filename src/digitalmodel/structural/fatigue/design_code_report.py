# ABOUTME: Per-standard fatigue design-code HTML report generator (DNV-RP-C203, BS 7608, API RP 2A, IIW)
"""Design code-specific fatigue reporting."""

from __future__ import annotations

import json
import math
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import numpy as np
import pandas as pd

from .sn_curves import PowerLawSNCurve
from .worked_examples import ExampleResult

_SUPPORTED_STANDARDS: Dict[str, Dict[str, Any]] = {
    "DNV-RP-C203": {
        "methodology": "Offshore welded-joint fatigue based on DNV S-N classes with optional thickness/environment adjustments.",
        "scope": "Offshore structures, pipelines, risers, welded details in air, seawater+CP, and free-corrosion environments.",
        "default_curve": "D",
        "environment": "Seawater + CP",
        "references": [
            "DNV-RP-C203 (2016) including 2019 amendments",
            "DNV-RP-C203 Table 2-1 to Table 2-3 (air / seawater+CP / free-corrosion)",
            "DNV-RP-C203 thickness correction using (t/tref)^k with tref=25 mm",
            "DNV-RP-C203 weld improvement guidance (grinding and hammer peening)",
        ],
    },
    "BS 7608": {
        "methodology": "Welded steel detail fatigue design using class-based S-N curves and Miner damage summation.",
        "scope": "Steel structures and welded details using classes B, C, D, E, F, F2, G, W in air or marine conditions.",
        "default_curve": "D",
        "environment": "Air",
        "references": [
            "BS 7608:2014+A1:2015 Guide to fatigue design and assessment of steel products",
            "BS 7608 class framework: B, C, D, E, F, F2, G, W",
        ],
    },
    "API RP 2A": {
        "methodology": "Fixed-platform fatigue assessment with API tubular-joint curve classes and Palmgren-Miner accumulation.",
        "scope": "Offshore fixed platforms and tubular joints using X and X′ class fatigue curves.",
        "default_curve": "X",
        "environment": "Marine",
        "references": [
            "API RP 2A-WSD 22nd Edition",
            "API RP 2A tubular-joint fatigue classes X and X′",
            "Efthymiou SCF equations for tubular joints",
        ],
    },
    "IIW": {
        "methodology": "IIW FAT-class design using stress-range category and optional stress-ratio (R-ratio) mean-stress treatment.",
        "scope": "Welded joints and structural details using FAT classes from FAT 36 to FAT 160.",
        "default_curve": "FAT 90",
        "environment": "Air",
        "references": [
            "IIW Recommendations for Fatigue Design of Welded Joints and Components, Doc. XIII-2460-13",
            "IIW FAT class system (FAT 36 to FAT 160)",
            "IIW mean stress effect handling via stress ratio (R-ratio) correction",
        ],
    },
}


def _normalize_standard(standard: str) -> str:
    text = standard.strip()
    if text in _SUPPORTED_STANDARDS:
        return text
    aliases = {
        "DNV": "DNV-RP-C203",
        "DNV RP C203": "DNV-RP-C203",
        "API": "API RP 2A",
        "API RP2A": "API RP 2A",
        "BS": "BS 7608",
    }
    key = text.upper().replace("-", " ")
    for alias, canonical in aliases.items():
        if key == alias.upper().replace("-", " "):
            return canonical
    raise ValueError(f"Unsupported standard: {standard}")


def _rows_to_histogram(rows: Union[pd.DataFrame, List[Dict[str, Any]], None]) -> pd.DataFrame:
    if isinstance(rows, pd.DataFrame):
        df = rows.copy()
    elif rows is None:
        df = pd.DataFrame(columns=["range", "count"])
    else:
        df = pd.DataFrame(rows)
    if "range" not in df.columns:
        df["range"] = 0.0
    if "count" not in df.columns:
        df["count"] = 0.0
    df = df[["range", "count"]].astype(float)
    return df[df["range"] > 0].copy()


def _build_curve(payload: Dict[str, Any], standard: str) -> PowerLawSNCurve:
    if isinstance(payload.get("sn_curve"), PowerLawSNCurve):
        return payload["sn_curve"]
    default_curve = _SUPPORTED_STANDARDS[standard]["default_curve"]
    curve_class = payload.get("curve_class", default_curve)
    A = float(payload.get("A", 5.73e11))
    m = float(payload.get("m", 3.0))
    fatigue_limit = float(payload.get("fatigue_limit", 0.0))
    return PowerLawSNCurve(
        name=f"{standard}-{curve_class}",
        A=A,
        m=m,
        fatigue_limit=fatigue_limit,
    )


def _compute_contributions(
    histogram: pd.DataFrame,
    curve: PowerLawSNCurve,
    scf: float,
) -> List[Dict[str, float]]:
    total = 0.0
    contribs: List[Dict[str, float]] = []
    for _, row in histogram.iterrows():
        stress = float(row["range"]) * scf
        cycles = float(row["count"])
        if stress <= 0 or cycles <= 0:
            continue
        allowable = float(curve.get_allowable_cycles(stress))
        if not np.isfinite(allowable) or allowable <= 0:
            continue
        increment = cycles / allowable
        total += increment
        contribs.append(
            {
                "stress_range": stress,
                "cycles_applied": cycles,
                "cycles_allowable": allowable,
                "damage_increment": increment,
                "cumulative_damage": total,
            }
        )
    return contribs


def _recommendations_for_damage(damage: float) -> List[str]:
    if damage < 0.1:
        return [
            "Structure is in good condition.",
            "Continue normal operation.",
            "Maintain regular inspection schedule.",
        ]
    if damage < 0.5:
        return [
            "Increase inspection frequency.",
            "Monitor for crack initiation.",
            "Consider load reduction where practical.",
        ]
    if damage < 1.0:
        return [
            "Detailed inspection is required.",
            "Perform non-destructive testing.",
            "Evaluate structural modifications.",
        ]
    return [
        "Immediate inspection is required.",
        "Apply load restrictions if operationally feasible.",
        "Prepare repair or replacement plan.",
    ]


def _infer_curve_class(curve_name: str, standard: str) -> str:
    if standard == "IIW" and "FAT" in curve_name.upper():
        idx = curve_name.upper().find("FAT")
        return curve_name[idx:].split("-")[0].strip()
    chunks = curve_name.replace("_", "-").split("-")
    return chunks[-1] if chunks else _SUPPORTED_STANDARDS[standard]["default_curve"]


class DesignCodeReport:
    """Generate design-code-specific fatigue report HTML."""

    def __init__(
        self,
        standard: str,
        result: Union[ExampleResult, Dict[str, Any]],
        title: Optional[str] = None,
        company: Optional[str] = None,
        project_ref: Optional[str] = None,
        revision: Optional[str] = None,
    ):
        self.standard = _normalize_standard(standard)
        self.meta = _SUPPORTED_STANDARDS[self.standard]
        self.title = title or f"{self.standard} Fatigue Design Report"
        self.company = company or "-"
        self.project_ref = project_ref or "-"
        self.revision = revision or "A"
        self.payload = self._normalize_result(result)

    def _normalize_result(self, result: Union[ExampleResult, Dict[str, Any]]) -> Dict[str, Any]:
        if isinstance(result, ExampleResult):
            payload: Dict[str, Any] = {
                "name": result.name,
                "description": result.description,
                "histogram": _rows_to_histogram(result.histogram),
                "sn_curve": result.sn_curve,
                "scf": float(result.scf),
                "dff": float(result.dff),
                "design_life_years": float(result.design_life_years),
                "damage": float(result.damage),
                "life_years": float(result.life_years),
                "damage_contributions": list(result.damage_contributions),
                "references": list(result.references),
            }
        else:
            payload = dict(result)
            payload["histogram"] = _rows_to_histogram(payload.get("histogram"))
            payload["scf"] = float(payload.get("scf", 1.0))
            payload["dff"] = float(payload.get("dff", 1.0))
            payload["design_life_years"] = float(payload.get("design_life_years", 20.0))
            payload["sn_curve"] = _build_curve(payload, self.standard)
            payload["damage"] = float(payload.get("damage", payload.get("total_damage", 0.0)))
            if "life_years" not in payload:
                d = payload["damage"]
                payload["life_years"] = math.inf if d <= 0 else payload["design_life_years"] / d
            payload["damage_contributions"] = list(payload.get("damage_contributions", []))
            payload["references"] = list(payload.get("references", []))

        if not payload["damage_contributions"] and not payload["histogram"].empty:
            payload["damage_contributions"] = _compute_contributions(
                payload["histogram"], payload["sn_curve"], payload["scf"]
            )
        return payload

    def _sn_curve_table_html(self) -> str:
        curve = self.payload["sn_curve"]
        curve_name = curve.name if curve.name else self.meta["default_curve"]
        curve_class = self.payload.get("curve_class") or _infer_curve_class(curve_name, self.standard)
        log_a = math.log10(curve.A) if curve.A > 0 else 0.0
        env = self.payload.get("environment", self.meta["environment"])
        return (
            "<table><thead><tr><th>Class</th><th>m-Slope</th><th>log(A)</th><th>CAFL (MPa)</th><th>Environment</th></tr></thead>"
            f"<tbody><tr><td>{curve_class}</td><td>{curve.m:.2f}</td><td>{log_a:.3f}</td><td>{curve.fatigue_limit:.2f}</td><td>{env}</td></tr></tbody></table>"
        )

    def _sn_chart_json(self) -> str:
        curve = self.payload["sn_curve"]
        hist = self.payload["histogram"]
        scf = self.payload["scf"]
        n_arr = np.logspace(3, 9, 200)
        s_arr = curve.get_stress_range(n_arr)
        line_trace = {"type": "scatter", "mode": "lines", "name": curve.name, "x": n_arr.tolist(), "y": s_arr.tolist(), "line": {"color": "#1f4e79", "width": 2.5}}
        points_trace = {"type": "scatter", "mode": "markers", "name": "Operating points", "x": [], "y": [], "marker": {"size": [], "color": "#d9534f", "opacity": 0.72}}
        if not hist.empty:
            eff = hist["range"].values * scf
            n_allow = [float(curve.get_allowable_cycles(v)) for v in eff]
            n_allow = [v if np.isfinite(v) else 1e9 for v in n_allow]
            counts = hist["count"].values
            sizes = 8 + 16 * (counts / max(float(counts.max()), 1.0))
            points_trace["x"] = n_allow
            points_trace["y"] = eff.tolist()
            points_trace["marker"]["size"] = sizes.tolist()
        layout = {
            "title": "S-N Curve with Operating Point Cloud",
            "xaxis": {"title": "Allowable Cycles N", "type": "log"},
            "yaxis": {"title": "Stress Range Δσ (MPa)", "type": "log"},
            "height": 430,
            "template": "plotly_white",
        }
        return json.dumps({"data": [line_trace, points_trace], "layout": layout})

    def _damage_table_html(self) -> str:
        contribs = self.payload["damage_contributions"]
        if not contribs:
            return "<p><em>Damage contributions unavailable.</em></p>"
        rows = []
        for item in sorted(contribs, key=lambda x: x.get("stress_range", 0.0)):
            rows.append(
                "<tr>"
                f"<td>{item.get('stress_range', 0.0):.2f}</td>"
                f"<td>{item.get('cycles_applied', 0.0):.3e}</td>"
                f"<td>{item.get('cycles_allowable', 0.0):.3e}</td>"
                f"<td>{item.get('damage_increment', 0.0):.6f}</td>"
                "</tr>"
            )
        header = "<thead><tr><th>Stress Range (MPa)</th><th>ni</th><th>Ni</th><th>ni/Ni</th></tr></thead>"
        return f"<table>{header}<tbody>{''.join(rows)}</tbody></table>"

    def _summary_html(self) -> str:
        damage = float(self.payload["damage"])
        life_years = float(self.payload["life_years"])
        dff = float(self.payload["dff"])
        dff_damage = damage * dff
        status = "PASS" if dff_damage <= 1.0 else "FAIL"
        css = "status-pass" if status == "PASS" else "status-fail"
        life_text = "∞" if not np.isfinite(life_years) else f"{life_years:.2f}"
        return (
            "<div class='summary-box'>"
            f"<div><strong>Total Damage:</strong> {damage:.6f}</div>"
            f"<div><strong>Life (years):</strong> {life_text}</div>"
            f"<div><strong>DFF:</strong> {dff:.2f} (D×DFF={dff_damage:.4f})</div>"
            f"<div class='{css}'><strong>{status}</strong></div>"
            "</div>"
        )

    def _references_html(self) -> str:
        refs = self.meta["references"] + self.payload.get("references", [])
        unique_refs = list(dict.fromkeys(refs))
        body = "".join(f"<li>{item}</li>" for item in unique_refs)
        return f"<ul>{body}</ul>"

    def generate(self, output_path: Optional[str] = None) -> str:
        damage = float(self.payload["damage"])
        recs = _recommendations_for_damage(damage)
        recs_html = "".join(f"<li>{item}</li>" for item in recs)
        chart_json = self._sn_chart_json()
        html = _HTML_TEMPLATE.format(
            title=self.title,
            standard=self.standard,
            company=self.company,
            project_ref=self.project_ref,
            revision=self.revision,
            methodology=self.meta["methodology"],
            scope=self.meta["scope"],
            sn_table=self._sn_curve_table_html(),
            summary_box=self._summary_html(),
            damage_table=self._damage_table_html(),
            recommendations=f"<ul>{recs_html}</ul>",
            references=self._references_html(),
            chart_json=chart_json,
        )
        if output_path:
            Path(output_path).write_text(html, encoding="utf-8")
        return html


def generate_design_code_report(
    result: Union[ExampleResult, Dict[str, Any]],
    standard: str,
    output_path: Optional[str] = None,
    **kwargs: Any,
) -> str:
    """Convenience wrapper for ``DesignCodeReport``."""
    report = DesignCodeReport(standard=standard, result=result, **kwargs)
    return report.generate(output_path=output_path)

_HTML_TEMPLATE = """<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\" />
  <title>{title}</title>
  <script src=\"https://cdn.plot.ly/plotly-2.27.0.min.js\"></script>
  <style>
    body {{ font-family: Arial, sans-serif; margin: 24px; color: #1f2937; background: #f6f8fb; }}
    h1 {{ color: #0f2744; margin-bottom: 4px; }}
    h2 {{ color: #1f4e79; margin-top: 28px; }}
    .meta, .panel, table {{ background: #fff; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.12); }}
    .meta, .panel {{ padding: 14px 16px; margin: 10px 0; }}
    table {{ border-collapse: collapse; width: 100%; margin-top: 8px; }}
    th, td {{ border: 1px solid #e5e7eb; padding: 8px 10px; font-size: 13px; text-align: left; }}
    th {{ background: #e9f0f8; }}
    .summary-box {{ display: grid; grid-template-columns: repeat(2, minmax(240px, 1fr)); gap: 8px; }}
    .status-pass {{ color: #1f7a3a; font-size: 1.05em; }}
    .status-fail {{ color: #c62828; font-size: 1.05em; }}
  </style>
</head>
<body>
  <h1>{title}</h1>
  <div class=\"meta\"><strong>Standard:</strong> {standard} | <strong>Company:</strong> {company} | <strong>Project:</strong> {project_ref} | <strong>Revision:</strong> {revision}</div>

  <h2>Methodology Summary</h2>
  <div class=\"panel\">{methodology}</div>

  <h2>Applicable Scope</h2>
  <div class=\"panel\">{scope}</div>

  <h2>S-N Curve Parameters</h2>
  {sn_table}

  <h2>Damage Calculation Methodology</h2>
  <div class=\"panel\">Palmgren-Miner linear accumulation is applied as D = Σ(ni/Ni), where ni is applied cycles and Ni is allowable cycles from the selected S-N curve.</div>

  <h2>Interactive S-N Chart</h2>
  <div class=\"panel\"><div id=\"sn-chart\" style=\"height:430px;\"></div></div>

  <h2>Damage Breakdown</h2>
  {damage_table}

  <h2>Summary</h2>
  {summary_box}

  <h2>Recommendations</h2>
  <div class=\"panel\">{recommendations}</div>

  <h2>References</h2>
  <div class=\"panel\">{references}</div>

  <script>
    Plotly.newPlot('sn-chart', {chart_json}.data, {chart_json}.layout, {{responsive: true}});
  </script>
</body>
</html>
"""
