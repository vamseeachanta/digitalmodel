"""Build a deterministic synthetic assessment using the held numerical methods.

Run with the package source on PYTHONPATH. No external process is launched.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
from dataclasses import asdict
from html import escape
from pathlib import Path

import pandas as pd
import yaml

from digitalmodel.asset_integrity.applicability import merge, Applicability
from digitalmodel.asset_integrity.pipeline_defect_screen import assess
from digitalmodel.asset_integrity.assessment.ffs_coordinator import FFSComponent, assess_component
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
    "2012; original and modified remaining-strength methods. The validation "
    "basis also identifies the 1991 Nonmandatory Appendix A worked example.",
    "DNV-RP-F101, Corroded Pipelines, October 2010; Section 8.2, single-defect capacity. "
    "Part B, Calibrated Safety Factor, supplies the method description. "
    "Equivalence to the 2021 edition is not established for this validation basis.",
    "API 579-1/ASME FFS-1, Fitness-For-Service, 2021; Part 4, General Metal Loss; "
    "Part 5, Local Metal Loss; Level 1 and Level 2 assessment. "
    "Circumferential extent is evaluated by a bounded net-section membrane calculation.",
    "ASME B31.8, Gas Transmission Piping, 2022; pressure design, minimum wall thickness. "
    "The 2022 pressure design basis is assumed; edition qualification is required "
    "before application to an operating line.",
)


def digest(path):
    """Portable content pin; original checkout bytes are never rewritten."""
    return hashlib.sha256(path.read_bytes().replace(b"\r\n", b"\n")).hexdigest()


def load_yaml(path):
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def map_decision(verdict, status, life, interval, *, flags=False, supplemental=()):
    """Applicability first; inadequate sampling overrides numerical strength."""
    if flags:
        return "escalate"
    if status == "TAKE_MORE":
        return "take more measurements"
    outcomes = {verdict, *supplemental}
    if status == "ESCALATE" or outcomes & {"ESCALATE", "REPAIR", "REPLACE"}:
        return "escalate"
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
    detailed = assess_component(component, frame)
    app = merge(Applicability(**strength["applicability"]), detailed.applicability)
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
    minimum = min(row["margin"] for row in strength["methods"])
    pressure_margin = min(row["margin"] for row in strength["methods"] if row["allowable_pressure_psi"] is not None)
    # Current strength has its own criterion; the coordinator retains wall/life
    # intervention. A finite life must not relabel an unchanged strength result.
    decision = decide(minimum, 1.0, life, "pipeline", applicability=app,
                      screening_pass=minimum >= 1.0,
                      bands=DecisionBands(monitor_band=0, derate_floor=0, repair_life_yr=0))
    final = map_decision(decision.verdict.value, detailed.sufficiency_status, life, interval,
                         flags=not app.ok, supplemental=(detailed.verdict,))
    rating = min(pressure_rating(data["design_pressure_psi"], pressure_margin),
                 detailed.rerated_pressure_psi)
    coordinator = detailed.to_dict()
    # The automatic repair helper lacks a repair-specific design basis here.
    # Its default repair-life interval must not be confused with assessed life.
    coordinator.pop("repair_recommendation", None)
    return {
        "name": data["component_id"], "inputs": data, "component": asdict(component),
        "strength": strength, "coordinator": coordinator,
        "level1": detailed.level1, "level2": serialise(detailed.level2),
        "sufficiency": asdict(detailed.sufficiency), "applicability": app.to_dict(),
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
    return f"{value:.3f}" if thickness else f"{value:.4g}"


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
    if status == "take more measurements":
        actions = []
        for action in section["sufficiency"]["actions"]:
            count = f"{action['count']} additional readings " if action["count"] is not None else "readings "
            actions.append(action["action"].replace("_", " ") + ": " + count + action["location"])
        tail = " Repeat the strength and thickness assessments after resolving the minimum and the complete defect extent."
        if section["remaining_life_yr"] == 0:
            tail += " Immediate engineering assessment is required; no operating extension is established at the stated pressure."
        return "; ".join(actions).capitalize() + "." + tail
    if status == "re-rate":
        return (f"Reduce pressure to {number(section['pressure_rating_psi'])} psi or lower and maintain "
                f"axial membrane stress at or below {number(section['axial_limit_psi'])} psi. "
                "Confirm the operating basis and reassess at the stated interval.")
    if status == "escalate":
        return ("Obtain complete defect geometry, material and load evidence for a higher-level assessment; "
                "resolve applicability and separate pressure and axial restrictions before operation is justified.")
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
    c, suff = section["coordinator"], section["sufficiency"]
    blocks = [text("# " + section["name"]), *figures(section, key), method_table(section, "Remaining strength")]
    blocks.append(table("Thickness and assessment levels", ["Quantity", "Result", "Criterion or basis"], [
        ["Minimum remaining wall (in)", number(c["t_measured_min_in"], thickness=True), "Synthetic grid"],
        ["Mean remaining wall (in)", number(c["t_measured_avg_in"], thickness=True), "Synthetic grid"],
        ["Required pressure wall (in)", number(c["t_min_in"], thickness=True), "Assumed pressure design factors"],
        ["Level 1", "minimum wall meets criterion" if section["level1"]["verdict"] == "ACCEPT" else "minimum wall below criterion", "Minimum wall ≥ required pressure wall"],
        ["Level 2 remaining strength factor", number(c["rsf"]), "≥ " + number(c["rsf_a"])],
        ["Assessment path", "General metal loss" if c["assessment_type"] == "GML" else "Local metal loss", "Automatic morphology classification"],
        ["Bulging factor", number(c["folias_factor"]), "Not used for general-loss averaging" if c["assessment_type"] == "GML" else "Simplified local-loss relation"],
        ["Measurement sufficiency", "additional readings required" if suff["status"] == "TAKE_MORE" else suff["status"].lower(), "Retained independently of strength"],
        ["Valid readings / degraded cells", f"{suff['metrics']['n_readings']} / {suff['metrics']['profile_cells']}", "Minimum 15 readings; local profile minimum 5 cells"],
        ["Coefficient of variation", ratio(suff["metrics"]["cov"]), "General-loss limit 10.0%"],
        ["Nominal re-inspection interval (yr)", number(section["interval_yr"]), "Assumed half-life; not an operating authorization"],
    ]))
    blocks.append(table("Declared-rate remaining-life sensitivity", ["Rate (in/yr)", "Remaining life (yr)"],
                        [[number(rate), number(life)] for rate, life in section["life_sensitivity"]]))
    blocks.append(text(f"Decision: **{section['decision']}**. {action_text(section)}"))
    if c["verdict"] in ("REPAIR", "REPLACE", "ESCALATE"):
        blocks.append(text("The supplemental thickness assessment calls for engineering intervention. "
                           "Additional measurements do not establish an operating extension or negate the wall deficiency."))
    return blocks


def design_blocks(result):
    blocks = [text("# 3.1 Site data\n\nNo site data are used. A generic onshore line is assumed; all inputs are synthetic, for demonstration."),
              text("# 3.2 Equipment data")]
    rows = []
    for s in result["sections"]:
        d = s["inputs"]
        values = [("Outside diameter (in)", number(d["nominal_od_in"])),
                  ("Nominal wall (in)", number(d["nominal_wt_in"], thickness=True)),
                  ("Strength class", "X52-equivalent; assumed from yield strength"),
                  ("Pressure design basis", "B31.8; assumed for Section A"),
                  ("Yield / tensile strengths (psi)", number(d["smys_psi"]) + " / " + number(d["smts_psi"])),
                  ("Design pressure (psi [MPa])", number(d["design_pressure_psi"]) + " [" + number(Q_(d["design_pressure_psi"], "psi").to("MPa").magnitude) + "]"),
                  ("Axial membrane stress (psi [MPa])", number(d["axial_stress_psi"]) + " [" + number(Q_(d["axial_stress_psi"], "psi").to("MPa").magnitude) + "]"),
                  ("Circumferential extent (in)", number(d["circumferential_width_in"])),
                  ("Axial positions (in)", ", ".join(number(x) for x in d["axial_positions_in"])),
                  ("Corrosion rate (in/yr)", "0.005")]
        rows.extend([[s["name"], key, val, "synthetic, for demonstration"] for key, val in values])
    blocks.append(table("Pipe geometry, loading and rate", ["Section", "Input", "Value", "Origin"], rows))
    blocks.extend([
        text("Section B tensile strength, axial stress, circumferential extent and factors are assumed from Section A; "
             "its 1 in row spacing is assumed from the default unit index. These are not additional measured inputs. "
             "The pressure design basis for Section A is assumed to match Section B. "
             "Both rates are assumed for this assessment; to be replaced by a rate from repeat inspection. "
             "Displayed values are rounded; calculations retain full precision."),
        text("# 3.3 Environmental data\n\nNot applicable. Internal pressure and separate tensile axial membrane stress only.\n\n"
             "# 3.4 Vessel data\n\nNot applicable."),
        text("# 3.5 Criteria\n\nAllowable-to-demand ratio ≥ 1 (100.0%) with no applicability flag is the current-strength criterion. "
             "Original-family depth loss is limited to 80.0%; the tensile-strength method to 85.0%; through-wall loss requires escalation. "
             "The measurement defaults are assumed, based on previous experience: at least 15 readings, general-loss coefficient of "
             "variation ≤ 10.0%, at least 5 degraded cells for a local profile, and 0.020 in measurement tolerance. "
             "A boundary minimum requires extended coverage. A remaining strength factor below 0.50 requires higher-level assessment. "
             "Remaining life is the linear time to the required pressure wall, zero when already below it. "
             "Re-inspection at half that time is assumed, based on previous experience; zero means immediate action. "
             "Under-sampled data do not support operation, regardless of the calculated strength."),
        text("# 3.6 Method\n\nThe methods and precedence rules are stated in Section 4.\n\n"
             "# 3.7 Load cases\n\nTwo sections are assessed by six strength methods each. Each section also receives "
             "Level 1 and Level 2 thickness assessments and three corrosion-rate cases: half, nominal and double. "
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
        "The allowable pressure multiplies capacity by the assumed modelling and operating factors; partial-safety-factor tables are not used. Method source: Reference 2.",
        "Circumferential net-section balance reduces axial membrane capacity in proportion to lost circumferential area. "
        "The extent comparison uses the supplied axial length and joint factors; exceeding that comparison does not invalidate the subsequent net-section balance. "
        "Through-wall loss raises an applicability flag. This is not a burst-pressure or combined-loading calculation. Method source: Reference 3.",
        "Level 1 compares minimum remaining wall with required pressure wall using the assumed pressure-design factors (Reference 4). "
        "Level 2 uses area-average wall for general loss or a simplified local-loss bulging relation (Reference 3). "
        "The local-loss length equals the count of degraded axial rows times the index spacing, whereas the six-method maximum-depth calculations use the full window. "
        "This difference is retained explicitly. The local-loss shell parameter is bounded at 20; a used value outside that range raises an applicability flag.",
        "The validation record reproduces the cited DNV worked-example capacities to better than 0.001%. "
        "That accuracy claim does not extend to all methods: the B31G published comparisons have coarser reported tolerances, "
        "and Modified B31G and RSTRENG are regression-anchored only. No published example values are reproduced here.",
        "Decision precedence: any applicability flag gives **escalate**. Otherwise an under-sampled result gives **take more measurements**, "
        "whatever the strength result. With sufficient data, escalation, repair or replacement from either assessment gives **escalate**; "
        "a de-rating gives **re-rate** with both pressure and axial restrictions retained. Acceptance or monitoring gives **accept** only with "
        "positive remaining life at least as long as the assumed re-inspection interval. Other outcomes give **escalate**. "
        "The pressure reduction uses only pressure ratios and the supplemental pressure rating, never an axial ratio.",
    )]


def section_material(result):
    summary, findings, recommendations = [], [], []
    detailed, appendix = [], []
    for index, s in enumerate(result["sections"]):
        summary.append(text(f"**{s['name']}: {s['decision']}.** {action_text(s)} "
                            "The decision will change if denser measurements resolve a different minimum, defect extent or loss pattern; "
                            "a repeat-inspection corrosion rate will change the remaining-life projection."))
        row = min(s["strength"]["methods"], key=lambda r: r["margin"])
        suff = s["sufficiency"]["metrics"]
        sampling_basis = (f"General-loss variation is {ratio(suff['cov'])} against the 10.0% limit. "
                          if s["coordinator"]["assessment_type"] == "GML" else
                          f"The local profile contains {suff['profile_cells']} degraded cells against the minimum of 5. ")
        findings.append(text(f"{s['name']}: {method_name(row['method'])} governs the current-demand comparison at "
                             f"{number(row['allowable_pressure_psi'])} psi versus {number(row['demand_psi'])} psi, "
                             f"a ratio of {ratio(row['margin'])} against 100.0%. "
                             + sampling_basis + f"The final decision is {s['decision']}; strength alone does not resolve measurement adequacy."))
        recommendations.append(text(s["name"] + ": " + action_text(s) +
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
                    "The Level 2 relations are simplified; edition-matched API 579 Part 5 qualification is not established. "
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
                              project="Synthetic assessment", client="Demonstration"),
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
