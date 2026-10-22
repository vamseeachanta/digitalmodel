# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Build an offline structural module report from public inputs and live calculations.

Usage: PYTHONPATH=src python scripts/reporting/build_structural_module_report.py
Numerical results are never stored in the prose template. No solver is invoked.
"""

from __future__ import annotations

import argparse
import hashlib
import html
import json
import math
import re
from pathlib import Path
from typing import Any

from digitalmodel.structural.structural_analysis import (
    STEEL_S355,
    StressCalculator,
    StressState,
)

ROOT = Path(__file__).resolve().parents[2]
REPORT_DIR = ROOT / "docs/reports/modules/structural"
TEMPLATE = Path(__file__).with_name("structural_module_report.html")
UI = Path(__file__).with_name("structural_module_report.js")
STYLE_SOURCE = ROOT / "docs/reports/pipeline-ffs-assessment.html"


def calculate() -> dict:
    """Call the public API for every output and pressure sample."""
    source_file = REPORT_DIR / "public-cylinder.json"
    source = json.loads(source_file.read_text(encoding="utf-8"))
    raw, conversion = source["inputs"], source["conversions"]
    inputs = {
        "pressure_mpa": raw["pressure_psi"] * conversion["psi_to_pa"] * conversion["pa_to_mpa"],
        "radius_m": raw["diameter_in"] * conversion["inch_to_m"] / 2,
        "thickness_m": raw["thickness_in"] * conversion["inch_to_m"],
        "length_m": raw["length_in"] * conversion["inch_to_m"],
    }
    inputs["diameter_mm"] = 2 * inputs["radius_m"] * conversion["m_to_mm"]
    inputs["thickness_mm"] = inputs["thickness_m"] * conversion["m_to_mm"]
    inputs["length_mm"] = inputs["length_m"] * conversion["m_to_mm"]
    # This required constructor argument is not read by the selected stress methods.
    # No preset strength is used as a criterion for the public cylinder.
    calculator = StressCalculator(STEEL_S355)

    def at_pressure(pressure: float) -> dict:
        hoop = calculator.hoop_stress(pressure, inputs["radius_m"], inputs["thickness_m"])
        axial = calculator.longitudinal_stress(pressure, inputs["radius_m"], inputs["thickness_m"])
        state = StressState(sigma_x=axial, sigma_y=hoop)
        return {
            "pressure_mpa": pressure,
            "hoop_mpa": float(hoop),
            "axial_mpa": float(axial),
            "equivalent_mpa": float(state.von_mises()),
            "principals_mpa": [float(value) for value in state.principal_stresses()],
            "max_shear_mpa": float(state.max_shear()),
        }

    series = [at_pressure(inputs["pressure_mpa"] * index / 20) for index in range(21)]
    results = at_pressure(inputs["pressure_mpa"])
    reference_hoop = inputs["pressure_mpa"] * inputs["radius_m"] / inputs["thickness_m"]
    metrics = {
        "wall_radius_percent": 100 * inputs["thickness_m"] / inputs["radius_m"],
        "hoop_difference_percent": 100 * (results["hoop_mpa"] - reference_hoop) / reference_hoop,
    }
    return {
        "source_inputs": raw, "inputs": inputs, "results": results, "series": series,
        "metrics": metrics, "strength_limit_mpa": None,
        "source_input_digest": hashlib.sha256(source_file.read_bytes()).hexdigest(),
        "sources": {"pressure": source["source"], "conversions": conversion},
    }


def lookup(data: dict, key: str) -> Any:
    """Resolve a numeric field in the embedded calculation record."""
    value = data
    for part in key.split("."):
        value = value[int(part)] if isinstance(value, list) else value[part]
    return value


def number(data: dict, key: str, decimals: int, tag: str = "span") -> str:
    """Format a finite quantity and preserve its calculation-field binding."""
    value = lookup(data, key)
    if not math.isfinite(value):
        raise ValueError(f"Non-finite report quantity: {key}")
    text = f"{value:,.{decimals}f}".replace("-", "−")
    return f'<{tag} data-number="{key}" data-decimals="{decimals}">{text}</{tag}>'


def chart(data: dict) -> str:
    """Render an accessible offline SVG; all hover values originate in the API."""
    series = data["series"]
    max_pressure = float(data["inputs"]["pressure_mpa"])
    max_stress = float(data["results"]["hoop_mpa"])
    def x(value: float) -> float:
        return 85 + 620 * value / max_pressure

    def y(value: float) -> float:
        return 360 - 290 * value / max_stress
    parts = ['<svg viewBox="0 0 820 440" role="img" aria-label="Membrane stress against internal pressure">',
             '<path d="M85 60V360H710" fill="none" stroke="#183449"/>']
    for index in (0, 5, 10, 15, 20):
        key = f"series.{index}.pressure_mpa"
        position = x(series[index]["pressure_mpa"])
        parts.append(f'<text x="{position:.2f}" y="386" text-anchor="middle" font-size="13">'
                     f'{number(data, key, 1, "tspan")}</text>')
    for fraction in (0, 0.5, 1):
        value = max_stress * fraction
        parts.append(f'<text x="75" y="{y(value) + 4:.2f}" text-anchor="end" font-size="13">{value:.1f}</text>')
    components = (("hoop_mpa", "Hoop", "#137e8a"),
                  ("axial_mpa", "Axial", "#516573"),
                  ("equivalent_mpa", "Equivalent", "#9b6100"))
    for offset, (key, label, colour) in enumerate(components):
        points = " ".join(f'{x(row["pressure_mpa"]):.2f},{y(row[key]):.2f}' for row in series)
        parts.append(f'<polyline points="{points}" fill="none" stroke="{colour}" stroke-width="2"/>')
        parts.append(f'<text x="{100 + offset * 200}" y="32" fill="{colour}" font-size="14">{label} stress</text>')
        for index, row in enumerate(series):
            tooltip = (f"Public cylinder; {label.lower()} membrane stress; "
                       f"pressure {row['pressure_mpa']:.1f} MPa; demand {row[key]:.1f} MPa; "
                       "strength limit NOT_EVALUATED; strength is not evaluated.")
            parts.append(f'<circle tabindex="0" cx="{x(row["pressure_mpa"]):.2f}" cy="{y(row[key]):.2f}" '
                         f'r="4" fill="{colour}" data-point="{index}" data-component="{key}" '
                         f'aria-label="{html.escape(tooltip)}"><title>{html.escape(tooltip)}</title></circle>')
    parts.extend(['<text x="390" y="420" text-anchor="middle">Internal pressure (MPa)</text>',
                  '<text transform="translate(20 220) rotate(-90)" text-anchor="middle">Stress (MPa)</text>',
                  '</svg>'])
    return "\n".join(parts)


def build(output_dir: Path = REPORT_DIR) -> None:
    """Write the single-file report and its matching engineering change record."""
    data = calculate()
    changes = {
        "document": "structural-module-report", "revision": "1",
        "revisions": [{"revision": "1", "date": "2026-10-22", "changes": [
            "First issue establishes the closed-cylinder membrane stress example and its diameter assumption.",
            "Material strength, pressure acceptance and radial displacement remain not evaluated."
        ]}],
    }
    template = TEMPLATE.read_text(encoding="utf-8")
    match = re.search(r"<style>(.*?)</style>", STYLE_SOURCE.read_text(encoding="utf-8"), re.S)
    if match is None:
        raise ValueError("The house-style exemplar is missing its stylesheet")
    style = match.group(1)
    text = re.sub(r"\{\{(svg-)?number:([\w.]+):(\d+)\}\}",
                  lambda match: number(data, match[2], int(match[3]),
                                       "tspan" if match[1] else "span"), template)
    substitutions = {
        "STYLE": style,
        "CHART": chart(data),
        "DATA": json.dumps(data, ensure_ascii=False, allow_nan=False).replace("<", "\\u003c"),
        "CHANGES": json.dumps(changes, ensure_ascii=False, allow_nan=False),
        "UI": UI.read_text(encoding="utf-8"),
    }
    for key, value in substitutions.items():
        text = text.replace("{{" + key + "}}", value)
    if "{{" in text:
        raise ValueError("Unresolved report template field")
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    (output_dir / "structural-module-report.html").write_text(text, encoding="utf-8")
    (output_dir / "changes.json").write_text(json.dumps(changes, indent=2) + "\n", encoding="utf-8")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=REPORT_DIR)
    build(parser.parse_args().output_dir)
