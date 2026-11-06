"""Offline tank-settlement intake, diagnostic geometry and fail-closed report.

Run this standalone entry point to avoid importing the general solver engine.
API 653 sixth-edition Annex B pointers and criteria remain unverified; no
code-derived acceptance values are calculated or accepted as free-form overrides.
"""
import argparse
from datetime import date
import html
import json
from pathlib import Path

import yaml

from digitalmodel.asset_integrity.applicability import Applicability
from digitalmodel.asset_integrity.assessment.ffs_decision import decide
from digitalmodel.asset_integrity.assessment.ffs_report import FFSReport
from digitalmodel.asset_integrity.tank_settlement import (
    GEOMETRY_SOURCES, finite, reduce_bottom_profile, reduce_edge_settlement,
    reduce_shell_survey,
)

CRITERIA_GAP = (
    "API 653, requested sixth edition (2024), Annex B: current-edition clause "
    "pointers, survey sufficiency, cosine-fit validity, applicability, acceptance "
    "equations and edge/bottom curves have not been verified. No allowable "
    "or Annex B compliance verdict is established."
)


def _require_keys(value, name, required, optional=()):
    """Reject malformed mappings before field access or keyword expansion."""
    if not isinstance(value, dict):
        raise ValueError(f"{name} must be a mapping")
    if not set(required).issubset(value) or set(value) - set(required) - set(optional):
        raise ValueError(f"{name} has missing required keys or unexpected keys")
    return value


def _tank_context(block):
    tank = _require_keys(block["tank"], "tank", (
        "diameter_m", "height_m", "fill_height_m", "course_thicknesses_mm", "material_grade"))
    for key in ("diameter_m", "height_m"):
        finite(tank[key], key, positive=True)
    fill = finite(tank["fill_height_m"], "fill_height_m", nonnegative=True)
    if fill > tank["height_m"]:
        raise ValueError("fill_height_m exceeds height_m")
    thicknesses = tank["course_thicknesses_mm"]
    if not isinstance(thicknesses, list) or not thicknesses:
        raise ValueError("course_thicknesses_mm must be a nonempty list")
    for value in thicknesses:
        finite(value, "course thickness", positive=True)
    if not isinstance(tank["material_grade"], str) or not tank["material_grade"].strip():
        raise ValueError("material_grade must be a nonempty description")
    return tank


def assess(block):
    """Retain inspection context and expose incomplete checks through shared FFS.

    All geometry is diagnostic. In particular, max(abs(residual)) is NOT the
    Annex B local out-of-plane deflection. Course thickness, grade and fill
    are retained for engineering follow-up, not used to infer missing limits.
    """
    _require_keys(block, "tank_settlement", ("tank", "shell_survey"),
                  ("edge", "bottom_profile", "component_id"))
    tank = _tank_context(block)
    shell = _require_keys(block["shell_survey"], "shell_survey", ("angles_deg", "elevations_mm"))
    geometry = {"shell": reduce_shell_survey(tank["diameter_m"], **shell),
                "edge": None, "bottom": None}
    flags = ["UNVERIFIED_ANNEX_B_CRITERIA", "SHELL_COVERAGE_UNVERIFIED"]
    notes = [CRITERIA_GAP, "Shell station count and spacing have not been verified against Annex B; "
             "fit conditioning is diagnostic and does not establish survey sufficiency."]
    for name, key, reducer, fields in (
        ("edge", "edge", reduce_edge_settlement,
         ("radial_extent_m", "shell_elevation_mm", "reference_elevation_mm")),
        ("bottom", "bottom_profile", reduce_bottom_profile, ("distances_m", "elevations_mm")),
    ):
        if key in block:
            geometry[name] = reducer(**_require_keys(block[key], key, fields))
        else:
            flags.append(f"MISSING_{name.upper()}_SURVEY")
            notes.append(f"No {name} survey supplied; missing measurements are not zero.")
    checks = {name: {"status": "NOT_EVALUATED", "allowable": None,
                     "reason": CRITERIA_GAP}
              for name in ("planar_tilt", "out_of_plane", "edge", "bottom")}
    # Shared decision adapter: zero tokens have no physical margin/life meaning;
    # raised applicability flags bypass all pressure-specific numeric bands.
    decision = decide(0, 1, 0, "tank", applicability=Applicability(False, flags, notes)).to_dict()
    for key in ("margin", "margin_allowable", "rsf", "rsf_a", "remaining_life_yr"):
        decision[key] = None
    decision["governing_criterion"] = "; ".join(notes)
    return {"schema_version": 1, "workflow_id": "api653-tank-settlement-screen",
            "basis": "diagnostic geometry only; input provenance is not verified",
            "tank": dict(tank), "shell_survey": dict(shell),
            "edge_survey": block.get("edge"), "bottom_profile": block.get("bottom_profile"),
            "geometry": geometry,
            "checks": checks, "decision": decision, "action": "ESCALATE-TO-LEVEL-3",
            "sources": GEOMETRY_SOURCES, "limitations": notes + [
                "Maximum residual is not a code-defined local out-of-plane deflection.",
                "No re-level, repair, acceptance, remaining-life or pressure rating is established.",
                "Review survey datum, spacing, repeatability, nozzles, roof and weld geometry; "
                "verify Annex B criteria before deciding whether Level 3 analysis is needed."]}


def _report(result, component_id):
    def display(value):
        if isinstance(value, float):
            return f"{value:.3g}"
        if isinstance(value, list):
            return ", ".join(display(v) for v in value)
        return value
    sections = {"Revision history": {"Revision 0": f"{date.today().isoformat()}: Initial diagnostic analysis; criteria unresolved."}}
    for name, geometry in result["geometry"].items():
        metrics = {}
        for key, value in (geometry or {"survey": "not supplied"}).items():
            if key in {"slope", "tilt_slope"}:
                key, value = "slope_mm_per_m", value * 1000
            if key == "r_squared" and value is not None:
                key, value = "explained_variation_percent", value * 100
            metrics[key] = display(value)
        sections[f"{name.title()} diagnostic geometry"] = metrics
    for name, check in result["checks"].items():
        sections[f"{name} acceptance check"] = check
    sections["Sources"] = {s["document"]: s["url"] for s in GEOMETRY_SOURCES}
    report = FFSReport.generate_screening_html(
        component_id, "Settlement screening analysis", result["decision"], sections,
        result["limitations"],
    )
    # Significant figures preserve small nonzero tilts in mm/m.
    slope = result["geometry"]["shell"]["tilt_slope"]
    report = report.replace("</body>", f"<p>Shell tilt: {html.escape(f'{slope * 1000:.3g}')} mm/m.</p></body>")
    return report


def run_workflow(cfg, output_dir):
    """Write a readback-friendly JSON result and offline HTML assessment."""
    if not isinstance(cfg, dict) or cfg.get("basename") != "tank_settlement_screen":
        raise ValueError("basename must be tank_settlement_screen")
    if "tank_settlement" not in cfg:
        raise ValueError("tank_settlement is required")
    block = cfg["tank_settlement"]
    result = assess(block)
    output = Path(output_dir)
    output.mkdir(parents=True, exist_ok=True)
    payload = json.dumps(result, indent=2, allow_nan=False) + "\n"
    (output / "settlement-summary.json").write_text(payload, encoding="utf-8")
    (output / "settlement-report.html").write_text(
        _report(result, block.get("component_id", "TANK")), encoding="utf-8")
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("input", type=Path)
    parser.add_argument("--output-dir", type=Path)
    args = parser.parse_args()
    with args.input.open(encoding="utf-8") as stream:
        cfg = yaml.safe_load(stream)
    run_workflow(cfg, args.output_dir or args.input.parent / "results")


if __name__ == "__main__":
    main()
