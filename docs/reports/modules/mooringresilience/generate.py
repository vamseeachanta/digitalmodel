"""Build the self-contained mooring screening analysis without a solver.

Rebuild from the repository root:
    PYTHONPATH=src python docs/reports/modules/mooringresilience/generate.py
"""
from dataclasses import asdict
from html import escape
import json
from pathlib import Path
import re

import yaml

from digitalmodel.mooring_resilience import (
    Metocean, MooringConfig, ResilienceFactors, assess,
)

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]


def calculate():
    """Call the public interface with an explicitly synthetic fatigue curve."""
    cfg = MooringConfig(12, 15000, 22600, 5, 20, 60, 1000)
    # Demonstration inputs, not standard safety factors or a material curve.
    factors = ResilienceFactors(
        fos_intact=2, fos_damaged=2, fos_foundation=2, redistribution=2,
        design_life_years=10, annual_cycles=100, dyn_tension_per_Hs_kN=22.6,
        sn_a_D=1000, sn_m=1,
    )
    cases = []
    for height in (2, 3, 4, 5):
        result = asdict(assess(cfg, Metocean(height, 11), factors))
        result = {key: round(value, 1 if key.endswith("_kN") or key.endswith("_years") else 3)
                       if isinstance(value, float) else value
                  for key, value in result.items()}
        cases.append({"wave_height_m": height, "result": result})
    evidence = {}
    for name in ("fpso_mooring_full", "anchor_capacity"):
        folder = ROOT / "atlases" / name
        version = (folder / "default.txt").read_text(encoding="utf-8").strip()
        record = json.loads((folder / version / "validation.json").read_text(encoding="utf-8"))
        manifest = yaml.safe_load((folder / version / "manifest.yaml").read_text(encoding="utf-8"))
        evidence[name] = {
            "domains": {axis["name"]: [min(axis["grid"]), max(axis["grid"])]
                        for axis in manifest["axes"]},
            "version": version, "holdouts": record["n_holdout"],
            "error_percent": round(100 * record["max_rel_error"], 1),
            "threshold_percent": round(100 * record["threshold"], 1),
        }
    return {"configuration": asdict(cfg), "factors": asdict(factors),
            "period_s": 11, "cases": cases, "evidence": evidence}


ATLAS_LABELS = {"fpso_mooring_full": "Intact-tension atlas",
                "anchor_capacity": "Anchor-capacity atlas"}
DOMAIN_LABELS = {"Hs": "Significant wave height (m)", "Tp": "Peak period (s)",
                 "water_depth_m": "Water depth (m)", "diameter_m": "Anchor diameter (m)",
                 "length_m": "Skirt length (m)", "su_kpa": "Undrained strength (kPa)"}

INPUT_LABELS = {
    "n_lines": "Line count", "mbl_kN": "Minimum breaking load",
    "line_area_mm2": "Steel area", "anchor_diameter_m": "Anchor diameter",
    "anchor_length_m": "Anchor skirt length", "soil_su_kpa": "Undrained shear strength",
    "water_depth_m": "Water depth", "fos_intact": "Intact strength factor",
    "fos_damaged": "Damaged strength factor", "fos_foundation": "Foundation factor",
    "redistribution": "Damaged-to-intact tension multiplier",
    "design_life_years": "Specified duration", "annual_cycles": "Annual cycles",
    "dyn_tension_per_Hs_kN": "Dynamic tension per wave height",
    "sn_a_D": "Fatigue curve coefficient", "sn_m": "Fatigue curve exponent",
}

UNITS = {
    "n_lines": "count", "mbl_kN": "kN", "line_area_mm2": "mm²",
    "anchor_diameter_m": "m", "anchor_length_m": "m", "soil_su_kpa": "kPa",
    "water_depth_m": "m", "design_life_years": "years",
    "annual_cycles": "cycles/year", "dyn_tension_per_Hs_kN": "kN/m",
    "sn_a_D": "cycles·MPa for the chosen exponent", "sn_m": "dimensionless",
}
LABELS = {
    "t_intact_kN": ("Intact tension", "kN"),
    "t_damaged_kN": ("Damaged tension", "kN"),
    "util_intact": ("Intact utilization", "dimensionless"),
    "util_damaged": ("Damaged utilization", "dimensionless"),
    "foundation_capacity_kN": ("Anchor capacity", "kN"),
    "util_foundation": ("Foundation utilization", "dimensionless"),
    "fatigue_life_years": ("Synthetic fatigue life", "years"),
    "fatigue_margin": ("Synthetic fatigue margin", "dimensionless"),
    "governing_util": ("Governing utilization", "dimensionless"),
}


def input_table(data):
    rows = []
    for group in ("configuration", "factors"):
        for key, value in data[group].items():
            rows.append(f"<tr><td>{escape(INPUT_LABELS[key])}</td><td>{value:g}</td>"
                        f"<td>{UNITS.get(key, 'dimensionless')}</td>"
                        f"<td>{'Public test geometry [1]' if group == 'configuration' else 'Synthetic input [3]'}</td></tr>")
    return "<table><tr><th>Input</th><th>Value</th><th>Unit</th><th>Source</th></tr>" + "".join(rows) + "</table>"


def result_table(data):
    rows = []
    for key, (label, unit) in LABELS.items():
        digits = 1 if unit in ("kN", "years") else 3
        value = data["cases"][0]["result"][key]
        rows.append(f"<tr><td>{label}</td><td>{value:.{digits}f}</td><td>{unit}</td><td>Module call [3]</td></tr>")
    return "<table><tr><th>Output</th><th>Value</th><th>Unit</th><th>Source</th></tr>" + "".join(rows) + "</table>"


def render(data):
    """Inline the existing report stylesheet, report data and comments."""
    exemplar = (ROOT / "docs/api/cfd/turbulent-flat-plate-verification.html").read_text(encoding="utf-8")
    style = re.search(r"<style>(.*?)</style>", exemplar, re.S).group(1)
    brand = (ROOT / "docs/api/_assets/brand.css").read_text(encoding="utf-8")
    changes = (HERE / "changes.json").read_text(encoding="utf-8")
    template = (HERE / "report.template.html").read_text(encoding="utf-8")
    replacements = {
        "STYLE": re.sub(r"/\*.*?\*/", "", brand + style, flags=re.S),
        "DATA": json.dumps(data, ensure_ascii=False, allow_nan=False),
        "CHANGES": changes.strip(), "INPUTS": input_table(data),
        "RESULTS": result_table(data),
        "LIFE": f'{data["cases"][0]["result"]["fatigue_life_years"]:.1f}',
        "DURATION": f'{data["factors"]["design_life_years"]:g}',
        "DOMAINS": "".join(f"<li>{escape(ATLAS_LABELS[name])}: " + "; ".join(
            f"{escape(DOMAIN_LABELS[axis])} from {bounds[0]:g} to {bounds[1]:g}"
            for axis, bounds in evidence["domains"].items()) + "</li>"
            for name, evidence in data["evidence"].items()),
        "MARGIN": f'{data["cases"][0]["result"]["fatigue_margin"]:.3f}',
        "EVIDENCE": "".join(
            f'<tr><td>{escape(ATLAS_LABELS[name])}</td><td>{e["holdouts"]}</td>'
            f'<td>{e["error_percent"]:.1f}</td><td>{e["threshold_percent"]:.1f}</td><td>[2]</td></tr>'
            for name, e in data["evidence"].items()),
    }
    for key, value in replacements.items():
        template = template.replace("@@" + key + "@@", value)
    return "\n".join(line.rstrip() for line in template.splitlines()) + "\n"


def write_report(destination):
    """Save UTF-8 text with portable line endings."""
    Path(destination).write_text(render(calculate()), encoding="utf-8", newline="\n")


if __name__ == "__main__":
    write_report(HERE / "report.html")
