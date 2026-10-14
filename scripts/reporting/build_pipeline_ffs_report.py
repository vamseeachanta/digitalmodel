"""Build a deterministic synthetic assessment using the existing numerical methods.

Run with the package source on PYTHONPATH. No external process is launched.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from dataclasses import asdict
from decimal import Decimal
from html import escape
from pathlib import Path

import pandas as pd
import yaml

from digitalmodel.asset_integrity.applicability import merge, Applicability
from digitalmodel.asset_integrity.pipeline_defect_screen import assess
from digitalmodel.asset_integrity.assessment.ffs_coordinator import FFSComponent, assess_component
from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
from digitalmodel.asset_integrity.assessment.measurement_sufficiency import MeasurementSufficiency
from digitalmodel.asset_integrity.assessment.ffs_decision import (
    DecisionBands, decide, reduced_mawp, remaining_life,
)
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.spec import DocumentMeta, FigureBlock, ReportSpec, Section, TableBlock, TextBlock
from digitalmodel.units import Q_

TITLE = "Fitness-for-service assessment: two corroded pipeline sections, remaining strength and operating decision"
SECTION_TITLES = (
    "Executive Summary", "Introduction", "Design Data", "Analysis Methodology",
    "Detailed Results", "Key Findings", "Limitations", "Recommendations",
    "References", "Acronyms and Symbols",
)
WORKFLOW = Path("examples/workflows/pipeline-ffs-assessment")
ASSUMED = "assumed, based on previous experience"
CALCULATION_SOURCES = (
    "src/digitalmodel/asset_integrity/pipeline_defect_screen.py",
    "src/digitalmodel/asset_integrity/pipeline_defect_screen_inputs.py",
    "src/digitalmodel/asset_integrity/corroded_pipe.py",
    "src/digitalmodel/asset_integrity/rstreng_2d.py",
    "src/digitalmodel/asset_integrity/dnv_rp_f101.py",
    "src/digitalmodel/asset_integrity/circumferential_defect.py",
    "src/digitalmodel/asset_integrity/applicability.py",
    "src/digitalmodel/asset_integrity/assessment/ffs_coordinator.py",
    "src/digitalmodel/asset_integrity/assessment/ffs_decision.py",
    "src/digitalmodel/asset_integrity/assessment/measurement_sufficiency.py",
    "src/digitalmodel/asset_integrity/assessment/ffs_router.py",
    "src/digitalmodel/asset_integrity/assessment/grid_parser.py",
    "src/digitalmodel/asset_integrity/assessment/level1_screener.py",
    "src/digitalmodel/asset_integrity/assessment/level2_engine.py",
    "src/digitalmodel/asset_integrity/assessment/pitting.py",
    "src/digitalmodel/asset_integrity/composite_repair.py",
    "src/digitalmodel/units.py",
    "src/digitalmodel/codes.py",
    "src/digitalmodel/materials/grades.py",
    "src/digitalmodel/reporting/engine.py",
    "src/digitalmodel/reporting/assets/assessment_report.html.j2",
)
REFERENCES = (
    "ASME B31G, Manual for Determining the Remaining Strength of Corroded Pipelines, "
    "2012. Edition-matched qualification of the legacy validation evidence is not established.",
    "DNV-RP-F101, Corroded Pipelines, 2010-10. No clause title is asserted; "
    "the public archive identifies the document and edition only.",
    "API 579-1/ASME FFS-1, Fitness-For-Service, 2021; "
    "Part 5, Assessment of Local Metal Loss. The local-loss relation is simplified; "
    "this demonstration is not a complete qualification to the standard.",
    "ASME B31.8, Gas Transmission and Distribution Piping Systems, 2022; "
    "841, Steel Pipe. Edition qualification is required before application to an operating line.",
)


def digest(path):
    """Portable content pin; original checkout bytes are never rewritten."""
    return hashlib.sha256(path.read_bytes().replace(b"\r\n", b"\n")).hexdigest()


def load_yaml(path):
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def map_decision(verdict, status, life, interval, *, flags=False, supplemental=()):
    """Escalation precedes measurement gaps, de-rating and acceptance."""
    if flags:
        return "escalate"
    outcomes = {verdict, *supplemental}
    if status == "ESCALATE" or outcomes & {"ESCALATE", "REPAIR", "REPLACE"}:
        return "escalate"
    if status == "TAKE_MORE":
        return "take more measurements"
    if outcomes & {"DERATE", "RE_RATE"}:
        return "re-rate"
    if outcomes <= {"ACCEPT", "MONITOR"} and life >= interval and life > 0:
        return "accept"
    return "escalate"


def pressure_rating(pressure, pressure_margin):
    """Pressure ratio only: axial membrane demand has a separate restriction."""
    return float(reduced_mawp(Q_(pressure, "psi"))(pressure_margin, 1.0).value.to("psi").magnitude)


def evaluate(root):
    cfg = load_yaml(root / WORKFLOW / "input.yml")
    sources, inputs = [], []
    for item in cfg["sections"]:
        path = (root / item["path"]).resolve()
        if not path.is_relative_to(root.resolve()):
            raise ValueError("Input must remain inside the worktree")
        if digest(path) != item["sha256_lf"]:
            raise ValueError("Synthetic input digest mismatch")
        sources.append(dict(item))
        inputs.append(load_yaml(path))
    first = dict(inputs[0]["pipeline_defect_screen"])
    second = inputs[1]["ffs_assessment"]
    b = {**cfg["section_b_supplement"], **second["component"], "grid": second["grid"]}
    output = []
    for item, data in zip(cfg["sections"], (first, b)):
        data["component_id"] = item["name"]
        data["data_origin"] = "synthetic, for demonstration"
        component = FFSComponent(
            component_id=item["name"], design_code=second["component"]["design_code"],
            nominal_od_in=data["nominal_od_in"], nominal_wt_in=data["nominal_wt_in"],
            design_pressure_psi=data["design_pressure_psi"], smys_psi=data["smys_psi"],
            corrosion_rate_in_per_yr=cfg["corrosion_rate_in_per_yr"],
            rsf_a=second["component"]["rsf_a"],
        )
        output.append(evaluate_section(data, component, cfg))
    return serialise({"sources": sources, "sections": output})


def evaluate_section(data, component, cfg):
    strength = assess(data)
    frame = pd.DataFrame(data["grid"], index=data["axial_positions_in"])
    # Both grids bound local patches; degraded-cell fraction within a selected
    # window does not establish general loss around the pipe circumference.
    detailed = assess_component(component, frame, force_type="LML")
    reference_wall = component.nominal_wt_in - component.fca_in
    # This constructor argument is named t_min_in, but is used only as t_c by
    # the existing local-loss relation. Keep the pressure-design wall separate.
    local_loss = Level2Engine(
        "LML", component.nominal_od_in, component.nominal_wt_in,
        reference_wall, component.rsf_a,
    ).evaluate(frame)
    sufficiency = MeasurementSufficiency().evaluate(
        frame, nominal_wt_in=component.nominal_wt_in, assessment_type="LML",
        level1_result=detailed.level1, level2_result=local_loss,
    )
    # Invert the linear pressure-wall relation already evaluated by the
    # B31.8 implementation; no design coefficient is duplicated here.
    uncorroded_rating = component.design_pressure_psi * reference_wall / detailed.t_min_in
    local_rating = float(reduced_mawp(Q_(uncorroded_rating, "psi"))(
        local_loss["rsf"], component.rsf_a,
    ).value.to("psi").magnitude)
    app = merge(Applicability(**strength["applicability"]), local_loss["applicability"])
    rate = cfg["corrosion_rate_in_per_yr"]
    life_rows = []
    for factor in cfg["sensitivity_multipliers"]:
        current_rate = rate * factor
        life_rows.append([current_rate, remaining_life(
            Q_(detailed.t_measured_min_in, "inch"), Q_(detailed.t_min_in, "inch"),
            Q_(current_rate, "inch/year"),
        )])
    life = remaining_life(Q_(detailed.t_measured_min_in, "inch"),
                          Q_(detailed.t_min_in, "inch"), Q_(rate, "inch/year"))
    interval = life * cfg["interval_life_fraction"]
    minimum = min(min(row["margin"] for row in strength["methods"]),
                  local_rating / component.design_pressure_psi)
    pressure_margin = min(row["margin"] for row in strength["methods"] if row["allowable_pressure_psi"] is not None)
    # Current strength includes the corrected local-loss rating. The declared
    # point-minimum life rule separately retains the intervention requirement.
    decision = decide(minimum, 1.0, life, "pipeline", applicability=app,
                      screening_pass=minimum >= 1.0,
                      bands=DecisionBands(monitor_band=0, derate_floor=0, repair_life_yr=0))
    final = map_decision(decision.verdict.value, sufficiency.status, life, interval,
                         flags=not app.ok)
    rating = min(pressure_rating(data["design_pressure_psi"], pressure_margin),
                 local_rating)
    coordinator = detailed.to_dict()
    # The automatic repair helper lacks a repair-specific design basis here.
    # Its default repair-life interval must not be confused with assessed life.
    coordinator.pop("repair_recommendation", None)
    # Coordinator RSF/rating and decision use a different reference wall and
    # are diagnostic only. Do not mix them with the issued local-loss result.
    for field in ("rsf", "rsf_a", "folias_factor", "verdict", "rerated_pressure_psi",
                  "sufficiency_status", "passes", "code_reference"):
        coordinator.pop(field, None)
    return {
        "name": data["component_id"], "inputs": data, "component": asdict(component),
        "strength": strength, "coordinator": coordinator,
        "level1": detailed.level1, "level2": serialise(local_loss),
        "reference_wall_in": reference_wall,
        "uncorroded_rating_psi": uncorroded_rating, "local_loss_rating_psi": local_rating,
        "sufficiency": asdict(sufficiency), "applicability": app.to_dict(),
        "remaining_life_yr": life, "interval_yr": interval, "life_sensitivity": life_rows,
        "strength_decision": decision.to_dict(), "decision": final,
        "pressure_rating_psi": rating,
        "axial_limit_psi": strength["methods"][-1]["allowable_axial_stress_psi"],
    }


def serialise(value):
    if isinstance(value, Applicability):
        return value.to_dict()
    if isinstance(value, dict):
        return {key: serialise(val) for key, val in value.items()}
    if isinstance(value, (tuple, list)):
        return [serialise(val) for val in value]
    return value


def number(value, *, thickness=False):
    if value is None:
        return "not applicable"
    if not math.isfinite(float(value)):
        return "not established"
    if thickness:
        return f"{value:.3f}"
    return format(Decimal(f"{value:.4g}"), ",f")


def ratio(value):
    return f"{value * 100:.1f}%"


def text(value):
    return TextBlock(markdown=value)


def table(title, columns, rows):
    return TableBlock(title=title, columns=columns, rows=rows)


def method_name(value):
    return value.replace("_", " ")


def action_text(section):
    status = section["decision"]
    row, col = section["sufficiency"]["metrics"]["min_location"]
    axial = section["inputs"]["axial_positions_in"][row]
    location = f"around axial position {number(axial)} in, column {col + 1}"
    if status == "take more measurements":
        return (f"Add readings {location}, "
                "and extend coverage to bound the complete defect. Repeat the strength and thickness assessments. "
                f"Continued operation at {number(section['inputs']['design_pressure_psi'])} psi is not justified "
                "by this assessment while the local profile remains under-sampled; strength margin alone does not establish measurement adequacy.")
    if status == "re-rate":
        return (f"Reduce pressure to {number(section['pressure_rating_psi'])} psi or lower and maintain "
                f"axial membrane stress at or below {number(section['axial_limit_psi'])} psi. "
                "Confirm the operating basis and reassess at the stated interval.")
    if status == "escalate":
        return ("Obtain complete defect geometry, material and load evidence for a higher-level assessment; "
                "resolve the minimum-wall deficit and separate pressure and axial restrictions before operation is justified. "
                f"For that assessment, add readings {location}. "
                f"Continued operation at {number(section['inputs']['design_pressure_psi'])} psi is not justified by this assessment.")
    return f"Maintain the stated loads and repeat inspection within {number(section['interval_yr'])} yr."


def svg_figure(key, title, body, height, caption):
    svg = (f'<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 680 {height}" '
           f'role="img" aria-label="{escape(title)}"><rect width="680" height="{height}" fill="#f5f8fa"/>'
           f'<g font-family="sans-serif" font-size="13" fill="#183449">{body}</g></svg>')
    return FigureBlock(figure_id=key, title=title, svg=svg, caption=caption)


def figures(section, key):
    data = section["inputs"]
    grid, xs = data["grid"], data["axial_positions_in"]
    body = '<text x="24" y="24">Axial position (in) / circumferential reading column</text>'
    for row_index, row in enumerate(grid):
        y = 64 + row_index * 42
        body += f'<text x="32" y="{y + 23}">{number(xs[row_index])}</text>'
        for col, value in enumerate(row):
            x = 120 + col * 82
            colour = f'hsl(195 48% {42 + 50 * value / data["nominal_wt_in"]:.1f}%)'
            body += f'<rect x="{x}" y="{y}" width="78" height="38" fill="{colour}"/>'
            body += f'<text x="{x+12}" y="{y+24}">{number(value, thickness=True)}</text>'
    heat = svg_figure(key + "-grid", "Thickness grid (in)", body, 92 + len(grid)*42,
                      "Darker cells indicate lower remaining wall; each cell is labelled in inches. Columns follow the supplied order.")
    points = []
    body = '<text x="24" y="24">River-bottom remaining wall (in)</text>'
    for x, row in zip(xs, grid):
        px = 60 + 560 * (x-xs[0]) / (xs[-1]-xs[0])
        py = 215 - 160 * min(row) / data["nominal_wt_in"]
        points.append(f"{px:.1f},{py:.1f}")
        body += f'<text x="{px:.1f}" y="{py-12:.1f}" text-anchor="middle">{number(min(row), thickness=True)}</text>'
        body += f'<text x="{px:.1f}" y="245" text-anchor="middle">{number(x)}</text>'
    body += '<polyline points="' + ' '.join(points) + '" fill="none" stroke="#137e8a" stroke-width="3"/>'
    body += '<text x="275" y="274">Axial position (in)</text>'
    profile = svg_figure(key + "-profile", "River-bottom profile", body, 290,
                         "Minimum remaining wall at each axial station; the effective-area calculations use the corresponding maximum depth.")
    rows = section["strength"]["methods"]
    maximum = max(1.0, max(row["margin"] for row in rows))
    body = '<text x="24" y="24">Allowable-to-demand ratio (%)</text>'
    threshold = 230 + 350 / maximum
    body += f'<line x1="{threshold:.1f}" x2="{threshold:.1f}" y1="38" y2="292" stroke="#9b4b27" stroke-dasharray="5 3"/>'
    for i, row in enumerate(rows):
        y = 45 + i*40
        width = 350 * row["margin"] / maximum
        body += f'<text x="12" y="{y+18}">{escape(method_name(row["method"]))}</text>'
        body += f'<rect x="230" y="{y}" width="{width:.1f}" height="26" fill="#137e8a"/>'
        body += f'<text x="{240+width:.1f}" y="{y+18}">{ratio(row["margin"])}</text>'
    comparison = svg_figure(key + "-comparison", "Method comparison", body, 305,
                            "The dashed line is the 100.0% criterion. Pressure ratios and the separate axial ratio are dimensionless; they do not qualify combined loading.")
    return [heat, profile, comparison]


def method_table(section, title):
    rows = []
    for row in section["strength"]["methods"]:
        rows.append([method_name(row["method"]), number(row["capacity_pressure_psi"]),
                     number(row["allowable_pressure_psi"]), number(row["allowable_axial_stress_psi"]),
                     number(row["demand_psi"]), ratio(row["margin"]),
                     "within stated limits" if row["applicability"]["ok"] else "outside stated limits"])
    return table(title, ["Method", "Capacity (psi)", "Allowable pressure (psi)",
                         "Allowable axial stress (psi)", "Demand (psi)", "Ratio (%)", "Applicability"], rows)


def result_blocks(section, key):
    c, suff, local = section["coordinator"], section["sufficiency"], section["level2"]
    blocks = [text("# " + section["name"]), *figures(section, key), method_table(section, "Remaining strength")]
    blocks.append(table("Thickness and local-loss assessment", ["Quantity", "Result", "Criterion or basis"], [
        ["Minimum remaining wall (in)", number(c["t_measured_min_in"], thickness=True), "Synthetic grid"],
        ["Mean remaining wall (in)", number(c["t_measured_avg_in"], thickness=True), "Synthetic grid"],
        ["Required pressure wall (in)", number(c["t_min_in"], thickness=True), "Pressure design: Reference 4"],
        ["Point-minimum pressure-wall comparison", "minimum wall meets criterion" if section["level1"]["verdict"] == "ACCEPT" else "minimum wall below criterion", "Assumed intervention rule: minimum wall ≥ required pressure wall; not an API Level 1 criterion"],
        ["Reference wall away from flaw (in)", number(section["reference_wall_in"], thickness=True), "Full-wall background; future allowance deducted"],
        ["Level 2 remaining strength factor", number(local["rsf"]), "Below the allowable factor; reduced pressure rating evaluated (Reference 3)" if local["rsf"] < local["rsf_a"] else "At or above the allowable factor (Reference 3)"],
        ["Reduced local-loss rating (psi)", number(section["local_loss_rating_psi"]), "Reference 3; pressure rating before loss from Reference 4"],
        ["Assessment path", "Local metal loss", "Bounded patch; general-loss averaging is not applied"],
        ["Bulging factor", number(local["folias_factor"]), "Simplified local-loss relation"],
        ["Measurement sufficiency", "additional readings required" if suff["status"] == "TAKE_MORE" else suff["status"].lower(), "Retained independently of strength"],
        ["Valid readings / degraded cells", f"{suff['metrics']['n_readings']} / {suff['metrics']['profile_cells']}", "Local-profile adequacy; see Section 3.5"],
        ["Coefficient of variation", ratio(suff["metrics"]["cov"]), "Descriptive only; no general-loss averaging criterion applied"],
        ["Nominal re-inspection interval (yr)", number(section["interval_yr"]), "Assumed half-life; not an operating authorization"],
    ]))
    blocks.append(table("Declared-rate remaining-life sensitivity", ["Rate (in/yr)", "Remaining life (yr)"],
                        [[number(rate), number(life)] for rate, life in section["life_sensitivity"]]))
    blocks.append(text(f"Decision: **{section['decision']}**. {action_text(section)}"))
    return blocks


def design_blocks(result):
    blocks = [text("# 3.1 Site data\n\nNo site data are used. A generic onshore line in location Class 1, Division 1 is assumed for both sections; all inputs are synthetic, for demonstration."),
              text("# 3.2 Equipment data")]
    rows = []
    for s in result["sections"]:
        d = s["inputs"]
        values = [("Outside diameter (in)", number(d["nominal_od_in"])),
                  ("Nominal wall (in)", number(d["nominal_wt_in"], thickness=True)),
                  ("Strength class", "X52-equivalent; assumed from yield strength"),
                  ("Pressure design basis", "B31.8; assumed" if s is result["sections"][0] else "B31.8; supplied synthetic basis"),
                  ("Yield / tensile strengths (psi)", number(d["smys_psi"]) + " / " + number(d["smts_psi"])),
                  ("Design pressure (psi [MPa])", number(d["design_pressure_psi"]) + " [" + number(Q_(d["design_pressure_psi"], "psi").to("MPa").magnitude) + "]"),
                  ("Axial membrane stress (psi [MPa])", number(d["axial_stress_psi"]) + " [" + number(Q_(d["axial_stress_psi"], "psi").to("MPa").magnitude) + "]"),
                  ("Circumferential extent (in)", number(d["circumferential_width_in"])),
                  ("Axial positions (in)", ", ".join(number(x) for x in d["axial_positions_in"])),
                  ("Corrosion rate (in/yr)", number(s["component"]["corrosion_rate_in_per_yr"]))]
        rows.extend([[s["name"], key, val, "synthetic, for demonstration"] for key, val in values])
    blocks.append(table("Pipe geometry, loading and rate", ["Section", "Input", "Value", "Origin"], rows))
    blocks.extend([
        text("Section B tensile strength, axial stress and circumferential extent are assumed from Section A; "
             "its 1 in row spacing is assumed from the default unit index. These are not additional measured inputs. "
             "Both rates are assumed for this assessment; to be replaced by a rate from repeat inspection. "
             "Displayed values are rounded; calculations retain full precision."),
        text("# 3.3 Environmental data\n\nNot applicable. Internal pressure and separate tensile axial membrane stress only.\n\n"
             "# 3.4 Vessel data\n\nNot applicable."),
        text("# 3.5 Criteria\n\nAllowable-to-demand ratio ≥ 1 (100.0%) with no applicability flag is the current-strength criterion. "
             "The method applicability limits are applied without reproducing their licensed values (References 1–3). "
             "Local-profile coverage, reading count and measurement tolerance are evaluated using the existing assessment defaults, "
             "informed by Reference 3 and inspection practice; they are not represented as code mandates. "
             "A boundary minimum requires extended coverage. General-loss averaging is not applied to either bounded local patch. "
             "Remaining life is the linear time from the point minimum to the required pressure wall, zero when already below it. "
             "This is an assumed intervention rule using the pressure-design wall from Reference 4, not an API Level 1 assessment. "
             "Re-inspection at half that time is assumed, based on previous experience; zero means immediate action. "
             "Under-sampled data do not support operation, regardless of the calculated strength."),
        text("# 3.6 Method\n\nThe methods and precedence rules are stated in Section 4.\n\n"
             "# 3.7 Load cases\n\nTwo sections are assessed by six strength methods each. Each section also receives "
             "a point-minimum pressure-wall comparison, a simplified Level 2 local-loss assessment and three corrosion-rate cases: half, nominal and double. "
             "Pressure and axial membrane stress are evaluated separately; no combined-load qualification is implied."),
    ])
    return blocks


def methodology():
    return [text(value) for value in (
        "B31G uses maximum depth and the full axial window with a parabolic loss area; long flaws use the infinite-length branch. "
        "The relative-depth limit is applied before disposition. Method source: Reference 1.",
        "Modified B31G uses the same depth/window with a modified area factor and two-branch bulging correction. "
        "RSTRENG instead searches all axial subsegments using trapezoidal lost area and returns the lowest capacity; both retain the original-family depth limit. "
        "Method source: Reference 1; their numeric evidence is limited to regression anchors.",
        "RSTRENG-2D projects each axial row to its greatest depth before applying the same effective-area calculation. "
        "It equals RSTRENG by construction and does not establish two-dimensional interaction capacity. Method source: Reference 1.",
        "DNV-RP-F101 uses tensile strength, maximum depth and length correction for a single longitudinal defect. "
        "The allowable pressure applies modelling and operating factors to capacity. Method source: Reference 2; "
        "no unverified part or clause title is asserted.",
        "Circumferential net-section balance reduces axial membrane capacity in proportion to lost circumferential area. "
        "The extent comparison uses the supplied axial length and joint factors; exceeding that comparison does not invalidate the subsequent net-section balance. "
        "Through-wall loss raises an applicability flag. This is not a burst-pressure or combined-loading calculation. Method source: Reference 3.",
        "The point-minimum comparison uses the required pressure wall (Reference 4) as an assumed intervention criterion, not an API Level 1 criterion. "
        "Both sections are local metal loss: Section A is a single bounded patch, not general loss around the pipe. "
        "The Level 2 calculation uses the wall away from the flaw, after uniform loss and future allowance, "
        "as the reference wall in the existing simplified local-loss relation (Reference 3). "
        "The full-wall background establishes that reference for these synthetic grids. The reduced rating uses the uncorroded "
        "pressure rating obtained by inverting the linear pressure-wall calculation (Reference 4), then applies the remaining-strength reduction. "
        "The local-loss length equals the count of degraded axial rows times the index spacing, whereas the six-method maximum-depth calculations use the full window. "
        "This difference is retained explicitly. A shell parameter outside the supported range raises an applicability flag.",
        "The validation record reproduces the cited DNV worked-example capacities to better than 0.001%. "
        "That accuracy claim does not extend to all methods: the B31G published comparisons have coarser reported tolerances, "
        "and Modified B31G and RSTRENG are regression-anchored only. No published example values are reproduced here.",
        "Assumed decision precedence: **escalate**, then **take more measurements**, then **re-rate**, then **accept**. "
        "Any applicability flag, escalation, repair or replacement takes priority over inadequate sampling. "
        "Otherwise an under-sampled result requires more measurements; a de-rating retains both pressure and axial restrictions. "
        "Acceptance or monitoring gives **accept** only with "
        "positive remaining life at least as long as the assumed re-inspection interval. Other outcomes give **escalate**. "
        "The pressure reduction uses only pressure ratios and the corrected local-loss pressure rating, never an axial ratio. "
        "The reduced rating participates in the strength decision even when the remaining strength factor is below its allowable value.",
    )]


def section_material(result):
    summary, findings, recommendations = [], [], []
    detailed, appendix = [], []
    for index, s in enumerate(result["sections"]):
        row = min(s["strength"]["methods"], key=lambda r: r["margin"])
        summary.append(text(f"**{s['name']}: {s['decision']}.** {action_text(s)} "
                            "The decision will change if denser measurements resolve a different minimum, defect extent or loss pattern; "
                            "a repeat-inspection corrosion rate will change the remaining-life projection. "
                            f"{method_name(row['method'])} governs the six-method comparison: "
                            f"{number(row['allowable_pressure_psi'])} psi against {number(row['demand_psi'])} psi, "
                            f"a ratio of {ratio(row['margin'])} and margin of {ratio(row['margin'] - 1)}. "
                            f"Minimum wall is {number(s['coordinator']['t_measured_min_in'], thickness=True)} in. "
                            f"The reduced local-loss rating is {number(s['local_loss_rating_psi'])} psi."))
        suff = s["sufficiency"]["metrics"]
        sampling_basis = (f"The local profile contains {suff['profile_cells']} degraded cells. "
                          "The assumed point-minimum intervention rule takes precedence over sampling gaps. ")
        findings.append(text(f"{s['name']}: {method_name(row['method'])} governs the six-method comparison at "
                             f"{number(row['allowable_pressure_psi'])} psi versus {number(row['demand_psi'])} psi, "
                             f"a ratio of {ratio(row['margin'])} against 100.0%. "
                             f"The reduced local-loss rating is {number(s['local_loss_rating_psi'])} psi against "
                             f"{number(s['inputs']['design_pressure_psi'])} psi, a ratio of "
                             f"{ratio(s['local_loss_rating_psi'] / s['inputs']['design_pressure_psi'])}. "
                             "The lower pressure rating participates in the decision. "
                             + sampling_basis + f"The final decision is {s['decision']}; strength alone does not resolve measurement adequacy."))
        recommendations.append(text(s["name"] + ": **" + s["decision"] + "**. " + action_text(s) +
                                    " Source: the measurement-sufficiency criteria in Section 3.5 and the assessed thickness/strength results in Section 5."))
        detailed.extend(result_blocks(s, f"section-{index + 1}"))
        grid = s["inputs"]["grid"]
        appendix.append(table(s["name"] + " — complete thickness grid",
                              ["Axial position (in)"] + [f"Column {i+1} (in)" for i in range(len(grid[0]))],
                              [[number(x)] + [number(t, thickness=True) for t in row]
                               for x, row in zip(s["inputs"]["axial_positions_in"], grid)]))
        appendix.append(method_table(s, s["name"] + " — complete method rows"))
    return summary, findings, recommendations, detailed, appendix


def report_spec(root, result):
    summary, findings, recommendations, detailed, appendix = section_material(result)
    assumptions = load_yaml(root / WORKFLOW / "assumptions.yml")
    assumption_rows = [[a["parameter"], a["value"], a["units"], a.get("report_label", ASSUMED)]
                       for a in assumptions["assumptions"] if a.get("used_for_assessment", True)]
    appendix.append(table("Assumptions", ["Parameter", "Value or basis", "Units", "Status"], assumption_rows))
    blocks = [summary,
              [text("Two synthetic corroded pipeline sections are assessed to demonstrate the relationship between remaining strength, "
                    "measurement sufficiency and an operating decision. No measured or site-specific data are used. "
                    "The purpose is to identify the next engineering action; this demonstration does not establish fitness for an actual operating asset.")],
              design_blocks(result), methodology(), detailed, findings,
              [text("The grids are synthetic and bound single defect windows. Colony interaction, mill-tolerance correction, crack-like defects, "
                    "bending, external pressure and combined loading are not assessed. "
                    "Corrosion rates are declared assumptions rather than evidence from repeat inspection. "
                    "Remaining life refers only to the required pressure-wall criterion and does not predict the future crossing of every six-method capacity curve. "
                    "RSTRENG-2D equals RSTRENG by construction; Modified B31G and RSTRENG have no external validation beyond regression anchors. "
                    "The corrected Level 2 reference wall does not establish complete edition-matched qualification to Reference 3; "
                    "the bulging relation remains simplified. "
                    "A strength ratio above unity does not overcome deficient wall or inadequate measurements. "
                    "Section B spacing and supplementary loads and material strength require confirmation for any physical application.")],
              recommendations, [text("\n".join(f"{i}. {ref}" for i, ref in enumerate(REFERENCES, 1)))],
              [table("Acronyms and symbols", ["Symbol", "Meaning"], [
                  ["FFS", "Fitness-for-service"], ["SMYS / SMTS", "Specified minimum yield / tensile strength"],
                  ["RSF / RSFₐ", "Remaining / allowable remaining strength factor"], ["MAWP", "Maximum allowable working pressure"],
                  ["UT", "Ultrasonic thickness"], ["COV", "Coefficient of variation"], ["in / psi / MPa / yr", "inch / pounds per square inch / megapascal / year"],
                  ["F₁ / F₂", "Modelling / operational usage factor"], ["D / t / d / L", "Diameter / nominal wall / loss depth / axial length"],
              ])]]
    provenance = Provenance()
    for source in result["sources"]:
        provenance.add("synthetic", source["path"], digest="sha256-lf:" + source["sha256_lf"])
    return ReportSpec(
        document=DocumentMeta(number="B0000-RPT-001-00", revision="00", title=TITLE,
                              project="Synthetic assessment", client="Demonstration", date="2026-10-13"),
        sections=[Section(key=f"section-{i}", title=title, blocks=body)
                  for i, (title, body) in enumerate(zip(SECTION_TITLES, blocks), 1)],
        appendices=[Section(key="appendices", title="Appendices", blocks=appendix)],
        provenance=provenance,
    )


def build(root, output=None):
    root = Path(root).resolve()
    output = Path(output) if output is not None else root / "docs/reports"
    source_digests = {name: digest(root / name) for name in CALCULATION_SOURCES}
    result = evaluate(root)
    page = render_html(report_spec(root, result), assessment_presentation=True)
    html_path = output / "pipeline-ffs-assessment.html"
    manifest_path = output / "pipeline-ffs-assessment.manifest.json"
    manifest = {
        "schema_version": 1, "data_status": "synthetic, for demonstration",
        "intended_use": "worked example; no actual operating asset qualification",
        "excluded_outputs": ["Automatic composite-repair candidate advice; no repair-specific design basis is supplied"],
        "digest_convention": "Input SHA-256 after CRLF to LF normalisation; output SHA-256 over exact UTF-8 bytes",
        "assessment": result,
        "calculation_sources_sha256_lf": source_digests,
        "assumptions_sha256_lf": digest(root / WORKFLOW / "assumptions.yml"),
        "configuration_sha256_lf": digest(root / WORKFLOW / "input.yml"),
        "builder_sha256_lf": digest(root / "scripts/reporting/build_pipeline_ffs_report.py"),
        "report_sha256": hashlib.sha256(page.encode("utf-8")).hexdigest(),
        "figures": "inline vector figures; 680-unit width; no raster or external dependencies",
    }
    if source_digests != {name: digest(root / name) for name in CALCULATION_SOURCES}:
        raise ValueError("Calculation source changed during build")
    output.mkdir(parents=True, exist_ok=True)
    html_path.write_text(page, encoding="utf-8", newline="\n")
    manifest_path.write_text(json.dumps(manifest, ensure_ascii=False, indent=2, sort_keys=True,
                                        allow_nan=False) + "\n", encoding="utf-8", newline="\n")
    if hashlib.sha256(html_path.read_bytes()).hexdigest() != manifest["report_sha256"]:
        raise ValueError("Report readback mismatch")
    if json.loads(manifest_path.read_text(encoding="utf-8")) != manifest:
        raise ValueError("Manifest readback mismatch")
    return html_path, manifest_path


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    for artifact in build(Path(__file__).resolve().parents[2], args.output):
        print(artifact.name)
