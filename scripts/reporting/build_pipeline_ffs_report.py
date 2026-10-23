# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

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
from digitalmodel.asset_integrity.corroded_pipe import (
    ASME_B318_CLASS1_DESIGN_FACTOR, ASME_B318_CLASS1_SAFETY_FACTOR,
)
from digitalmodel.asset_integrity.assessment.ffs_coordinator import FFSComponent, assess_component
from digitalmodel.asset_integrity.assessment.level2_engine import Level2Engine
from digitalmodel.asset_integrity.assessment import measurement_sufficiency
from digitalmodel.asset_integrity.assessment.measurement_sufficiency import MeasurementSufficiency
from digitalmodel.asset_integrity.assessment.ffs_decision import (
    DecisionBands, decide, reduced_mawp, remaining_life,
)
from digitalmodel.reporting.engine import render_html
from digitalmodel.reporting.provenance import Provenance
from digitalmodel.reporting.spec import DocumentMeta, FigureBlock, ReportSpec, RevisionRow, Section, TableBlock, TextBlock
from digitalmodel.units import Q_

TITLE = "Fitness-for-service assessment: two corroded pipeline sections, remaining strength and operating decision"
SECTION_TITLES = (
    "Executive Summary", "Introduction", "Design Data", "Analysis Methodology",
    "Detailed Results", "Key Findings", "Limitations", "Recommendations",
    "References", "Acronyms and Symbols",
)
WORKFLOW = Path("examples/workflows/pipeline-ffs-assessment")
ASSUMED = "assumed for this assessment"
CALCULATION_SOURCES = (
    "src/digitalmodel/asset_integrity/pipeline_defect_screen.py",
    "src/digitalmodel/asset_integrity/pipeline_defect_screen_inputs.py",
    "src/digitalmodel/asset_integrity/corroded_pipe.py",
    "src/digitalmodel/asset_integrity/rstreng_2d.py",
    "src/digitalmodel/asset_integrity/dnv_rp_f101.py",
    "src/digitalmodel/asset_integrity/circumferential_defect.py",
    "src/digitalmodel/asset_integrity/applicability.py",
    "src/digitalmodel/asset_integrity/input_validation.py",
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
REFERENCE_CAVEATS = (
    "Edition-matched qualification of the legacy validation evidence is not established.",
    "The numerical engine documents its constants against this edition and its successor. "
    "Edition-matched qualification of the DNV validation evidence is not established.",
    "The local-loss relation is simplified; "
    "this demonstration is not a complete qualification to the standard.",
    "Edition qualification is required before application to an operating line.",
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
    if outcomes <= {"ACCEPT", "MONITOR"} and life > 0:
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
    factors = {
        "safety_factor": ASME_B318_CLASS1_SAFETY_FACTOR,
        "usage_factor": ASME_B318_CLASS1_DESIGN_FACTOR,
        "axial_design_factor": ASME_B318_CLASS1_DESIGN_FACTOR,
    }
    b = {**factors, **cfg["section_b_supplement"], **second["component"], "grid": second["grid"]}
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


def _local_loss_projection(frame, component, reference_wall, uncorroded_rating):
    """Return a lambda-range domain and evaluator, preserving the received grid."""
    trial = frame.astype(float).copy()
    minimum = float(frame.min().min())
    mask = frame == minimum
    local = Level2Engine("LML", component.nominal_od_in, component.nominal_wt_in,
                         reference_wall, component.rsf_a)
    domain = local.local_loss_projection_domain(frame)

    def evaluate(wall):
        trial[mask] = wall
        result = local.evaluate(trial)
        if not result["applicability"].ok:
            raise ValueError("Lambda-range qualification disagrees with the engine")
        # At M=1, RSF=1 for every positive wall. Use its right-hand limit at
        # zero instead of the engine's 0/0 fallback (legacy Part 5 relation).
        right_hand_limit = wall == 0 and result["folias_factor"] == 1
        rsf = 1.0 if right_hand_limit else result["rsf"]
        rating = reduced_mawp(Q_(uncorroded_rating, "psi"))(
            rsf, component.rsf_a,
        ).value.to("psi").magnitude
        if not math.isfinite(rating):
            raise ValueError("Projected pressure is not finite")
        applicability = result["applicability"].to_dict()
        if wall == 0:
            applicability = Applicability(
                ok=False, flags=["ZERO_WALL_UNQUALIFIED"],
                notes=["A zero-wall mathematical endpoint does not qualify a perforated wall"],
            ).to_dict()
        return {"wall_in": wall, "pressure_psi": float(rating),
                "flaw_length_in": result["flaw_length_in"],
                "applicability": applicability,
                "endpoint_kind": "right-hand limit" if right_hand_limit else "evaluated"}
    return domain, evaluate


def local_loss_limit_wall(frame, component, reference_wall, uncorroded_rating):
    """Bracket a pressure crossing within lambda-range applicability only.

    Remaining-thickness ratio, minimum remaining wall and the other Part 5
    prerequisites are not checked here; full applicability is not established.

    Bisection requires opposite-sign endpoint residuals: NIST DLMF 3.8(iii),
    https://dlmf.nist.gov/3.8#iii (Bisection Method). Engineering coefficients
    remain in the existing API 579-1/ASME FFS-1 (2021) implementation.
    """
    target = component.design_pressure_psi
    if not all(math.isfinite(v) and v > 0 for v in
               (target, uncorroded_rating, component.rsf_a)):
        raise ValueError("Pressures and allowable RSF must be positive finite inputs")
    domain, evaluate = _local_loss_projection(frame, component, reference_wall, uncorroded_rating)
    evidence = {"domain_wall_in": domain, "target_pressure_psi": target,
                "qualification_scope": "lambda-range only",
                "initial_bracket": [], "final_bracket": [], "monotonic": False}

    def finish(status, wall, reason):
        return {**evidence, "status": status, "wall_in": wall, "reason": reason}

    if domain is None:
        return finish("INAPPLICABLE", None, "Current geometry has no lambda-range depletion interval")
    low, high = [evaluate(wall) for wall in domain]
    evidence["initial_bracket"] = [low, high]
    evidence["final_bracket"] = [low, high]
    # For fixed M>=1, d(RSF)/d(Rt)=M(M-1)/(M-1+Rt)^2 >= 0.
    # As wall increases, degraded rows can only disappear, also increasing RSF.
    # Check sampled ordering as well; tolerate only one pressure ULP of rounding.
    if low["pressure_psi"] > high["pressure_psi"]:
        return finish("NON_MONOTONIC", None, "Endpoint pressures decrease with wall")
    evidence["monotonic"] = True
    if high["pressure_psi"] < target:
        return finish("TARGET_UNREACHABLE", high["wall_in"], "Current rating is already below demand")
    if low["pressure_psi"] >= target:
        if not low["applicability"]["ok"]:
            return finish("INAPPLICABLE", None,
                          "Life is not established: no pressure crossing before the unqualified zero-wall endpoint")
        status = "BOUNDARY" if low["pressure_psi"] == target else "BOUNDED"
        return finish(status, low["wall_in"], "No failing pressure endpoint inside the lambda range; life is censored")
    return _bisect_local_loss(evaluate, evidence, finish, low, high, target)


def _bisect_local_loss(evaluate, evidence, finish, low, high, target):
    """Retain a lambda-range failing/passing bracket, checking every sample."""
    while math.nextafter(low["wall_in"], high["wall_in"]) < high["wall_in"]:
        middle = evaluate((low["wall_in"] + high["wall_in"]) / 2)
        if not (math.nextafter(low["pressure_psi"], -math.inf) <= middle["pressure_psi"]
                <= math.nextafter(high["pressure_psi"], math.inf)):
            evidence["monotonic"] = False
            return finish("NON_MONOTONIC", None, "Interior pressure violates bracket ordering")
        if middle["pressure_psi"] < target:
            low = middle
        else:
            high = middle
        evidence["final_bracket"] = [low, high]
    if low["flaw_length_in"] != high["flaw_length_in"]:
        return finish("BOUNDED", high["wall_in"], "Flaw-extent boundary censors the crossing; no continuous root is established")
    status = "BOUNDARY" if high["wall_in"] == evidence["domain_wall_in"][1] else "CROSSING"
    return finish(status, high["wall_in"], "Passing wall retained with adjacent failing wall")


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
    # Only the deficient point-minimum case changes projection route. The
    # engine relation is called directly; no standard equation is duplicated.
    life_limit = detailed.t_min_in
    life_basis = "point-minimum pressure wall"
    life_search = None
    if detailed.t_measured_min_in < detailed.t_min_in:
        life_search = local_loss_limit_wall(frame, component, reference_wall, uncorroded_rating)
        life_limit = life_search["wall_in"]
        if life_limit is None:
            raise ValueError(f"Local-loss life is not established: {life_search['status']}")
        life_basis = "fixed-geometry reduced local-loss rating; lambda-range applicability only"
        if life_search["status"] in {"BOUNDED", "BOUNDARY"}:
            life_basis += "; censored at the supported boundary"
    life_rows = []
    for factor in cfg["sensitivity_multipliers"]:
        current_rate = rate * factor
        life_rows.append([current_rate, remaining_life(
            Q_(detailed.t_measured_min_in, "inch"), Q_(life_limit, "inch"),
            Q_(current_rate, "inch/year"),
        )])
    life = remaining_life(Q_(detailed.t_measured_min_in, "inch"),
                          Q_(life_limit, "inch"), Q_(rate, "inch/year"))
    interval = life * cfg["interval_life_fraction"]
    minimum = min(min(row["margin"] for row in strength["methods"]),
                  local_rating / component.design_pressure_psi)
    pressure_margin = min(row["margin"] for row in strength["methods"] if row["allowable_pressure_psi"] is not None)
    # Current strength and the stated projection route remain separate from
    # the formal-evaluation referral returned by measurement sufficiency.
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
        "life_limit_wall_in": life_limit, "life_basis": life_basis, "life_search": life_search,
        "remaining_life_yr": life, "interval_yr": interval, "life_sensitivity": life_rows,
        "strength_decision": decision.to_dict(), "decision": final,
        "pressure_rating_psi": rating,
        "axial_limit_psi": strength["methods"][-1]["allowable_axial_stress_psi"],
    }


PRIVATE_FACTOR_FIELDS = frozenset({
    "rsf_a", "safety_factor", "usage_factor", "axial_design_factor",
    "modelling_factor", "operational_usage_factor", "total_usage_factor",
})


def public_assessment(result):
    """Keep numerical results, excluding method factors and quoted thresholds."""
    def project(value):
        if isinstance(value, dict):
            return {key: project(child) for key, child in value.items()
                    if key not in PRIVATE_FACTOR_FIELDS}
        if isinstance(value, list):
            return [project(child) for child in value]
        return value

    public = project(result)
    for section in public["sections"]:
        point = section["level1"]
        point["pressure_wall_comparison"] = (
            "MINIMUM_MEETS_PRESSURE_WALL" if point.pop("verdict") == "ACCEPT"
            else "MINIMUM_BELOW_PRESSURE_WALL"
        )
        local = section["level2"]
        local["strength_factor_comparison"] = (
            "AT_OR_ABOVE_ALLOWABLE" if local.pop("verdict") == "ACCEPT"
            else "BELOW_ALLOWABLE_REDUCED_RATING_EVALUATED"
        )
        local["pressure_criterion"] = (
            "RATING_ABOVE_DEMAND" if section["local_loss_rating_psi"] >= section["inputs"]["design_pressure_psi"]
            else "RATING_BELOW_DEMAND"
        )
        decision = section["strength_decision"]
        decision["minimum_capacity_to_demand"] = decision.pop("rsf")
        decision["governing_criterion"] = (
            f"Minimum capacity/demand={decision['margin']:.4g}; required ratio=1. "
            "Includes the reduced local-loss pressure rating and separate axial restriction."
        )
        if section["sufficiency"]["status"] == "ESCALATE":
            section["sufficiency"]["reasons"] = [
                "Remaining strength factor below the allowable factor and above "
                "the replacement floor; formal local-loss evaluation is required. "
                "Field data is adequate. Method basis: Reference 3."
            ]
    return public


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
        count = sum(action["count"] or 0 for action in section["sufficiency"]["actions"])
        return (f"Add readings to resolve at least {count} more degraded cells {location}, across and immediately around the flaw. "
                "Repeat the strength and thickness assessments. "
                "Interim operation is a decision for the maintainer of the pipeline record; "
                "this assessment does not assign an interim pressure while the local profile remains under-sampled.")
    if status == "re-rate":
        return (f"Reduce pressure to {number(section['pressure_rating_psi'])} psi or lower and maintain "
                f"axial membrane stress at or below {number(section['axial_limit_psi'])} psi. "
                "Confirm the operating basis and reassess at the stated interval.")
    if status == "escalate":
        return ("Complete the formal local-loss evaluation: the remaining strength factor is below "
                "the allowable factor and above the replacement floor. Field data is adequate; "
                "the referral is driven by strength, not a sampling gap. "
                f"The current reduced local-loss rating is {number(section['local_loss_rating_psi'])} psi "
                f"against {number(section['inputs']['design_pressure_psi'])} psi. "
                "Confirm the applicability, material and load basis before adopting that rating. "
                "Interim operation is a decision for the maintainer of the pipeline record.")
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
        ["Point-minimum pressure-wall comparison", "minimum wall meets criterion" if section["level1"]["verdict"] == "ACCEPT" else "minimum wall below criterion", "Diagnostic comparison only; not an intervention rule or an API Level 1 criterion"],
        ["Reference wall away from flaw (in)", number(section["reference_wall_in"], thickness=True), "Full-wall background; future allowance deducted"],
        ["Level 2 remaining strength factor", number(local["rsf"]), "Below the allowable factor; reduced pressure rating evaluated (Reference 3)" if local["rsf"] < local["rsf_a"] else "At or above the allowable factor (Reference 3)"],
        ["Reduced local-loss rating (psi)", number(section["local_loss_rating_psi"]), "Reference 3; pressure rating before loss from Reference 4"],
        ["Assessment path", "Local metal loss", "Bounded patch; general-loss averaging is not applied"],
        ["Bulging factor", number(local["folias_factor"]), "Simplified local-loss relation"],
        ["Local-loss flaw length (in)", number(local["flaw_length_in"]), "Degraded axial rows times row spacing"],
        ["Life projection basis", section["life_basis"], "Fixed conditions and limitations in Section 3.5"],
        ["Life-limit wall (in)", number(section["life_limit_wall_in"], thickness=True), "Projected threshold; not the current minimum"],
        ["Measurement sufficiency", "additional readings required" if suff["status"] == "TAKE_MORE" else suff["status"].lower(), "Strength-driven formal evaluation; field data adequate" if suff["status"] == "ESCALATE" else "Local-profile sampling criterion; see Section 3.5"],
        ["Valid readings / degraded cells", f"{suff['metrics']['n_readings']} / {suff['metrics']['profile_cells']}", "Local-profile adequacy; see Section 3.5"],
        ["Coefficient of variation", ratio(suff["metrics"]["cov"]), "Descriptive only; no general-loss averaging criterion applied"],
        ["Nominal re-inspection interval (yr)", number(section["interval_yr"]), "Assumed half-life; not an operating authorization"],
    ]))
    blocks.append(table("Declared-rate remaining-life sensitivity", ["Rate (in/yr)", "Remaining life (yr)"],
                        [[number(rate), number(life)] for rate, life in section["life_sensitivity"]]))
    blocks.append(text(f"Decision: **{section['decision']}**. {action_text(section)}"))
    return blocks


def design_blocks(result):
    blocks = [text("# 3.1 Site data\n\nNo site data are used. A generic onshore line in Location Class 1, Division 2 is assumed for both sections; all inputs are synthetic, for demonstration."),
              text("# 3.2 Equipment data")]
    rows = []
    for s in result["sections"]:
        d = s["inputs"]
        values = [("Outside diameter (in)", number(d["nominal_od_in"])),
                  ("Nominal wall (in)", number(d["nominal_wt_in"], thickness=True)),
                  ("Strength class", "X52-equivalent; assumed from yield strength"),
                  ("Pressure design basis", "B31.8 Location Class 1, Division 2; assumed"),
                  ("Yield / tensile strengths (psi)", number(d["smys_psi"]) + " / " + number(d["smts_psi"])),
                  ("Design pressure (psi [MPa])", number(d["design_pressure_psi"]) + " [" + number(Q_(d["design_pressure_psi"], "psi").to("MPa").magnitude) + "]"),
                  ("Axial membrane stress (psi [MPa])", number(d["axial_stress_psi"]) + " [" + number(Q_(d["axial_stress_psi"], "psi").to("MPa").magnitude) + "]"),
                  ("Circumferential extent (in)", number(d["circumferential_width_in"])),
                  ("Axial positions (in)", ", ".join(number(x) for x in d["axial_positions_in"])),
                  ("Corrosion rate (in/yr)", number(s["component"]["corrosion_rate_in_per_yr"]))]
        rows.extend([[s["name"], key, val, "synthetic, for demonstration"] for key, val in values])
    blocks.append(table("Pipe geometry, loading and rate", ["Section", "Input", "Value", "Origin"], rows))
    blocks.extend([
        text("Section B tensile strength, axial stress and circumferential extent are copied from Section A; "
             "its 1 in row spacing is an assessment assumption. These are not additional measured inputs. "
             "Both rates are assumed for this assessment; to be replaced by a rate from repeat inspection. "
             "Displayed values are rounded; calculations retain full precision."),
        text("# 3.3 Environmental data\n\nNot applicable. Internal pressure and separate tensile axial membrane stress only.\n\n"
             "# 3.4 Vessel data\n\nNot applicable."),
        text("# 3.5 Criteria\n\nAllowable-to-demand ratio ≥ 1 (100.0%) with no applicability flag is the current-strength criterion. "
             "The implemented method applicability checks are applied without reproducing their licensed values (References 1–3). "
             f"The assessment defaults require at least {MeasurementSufficiency().min_readings} valid readings and a "
             f"minimum of {MeasurementSufficiency().min_lml_profile_cells} degraded cells resolving a local flaw. "
             "These are configurable engineering criteria informed by inspection practice, not code mandates. "
             f"The replacement floor of RSF {measurement_sufficiency._SEVERE_RSF_FLOOR:.2f} is an "
             "assessment default of the measurement-sufficiency evaluation; it is configurable, not a code mandate. "
             "A boundary minimum requires extended coverage. General-loss averaging is not applied to either bounded local patch. "
             "The reported Level 2 remaining strength factor uses a simplified closed form evaluated at the point-minimum thickness; "
             "it does not establish a complete Part 5 Level 2 evaluation. "
             "Section A life is projected to the wall at which its reduced local-loss rating reaches design pressure. "
             "The flaw length, background wall, bulging factor, material and loads are held fixed; lateral growth is not modelled. "
             "The pressure projection checks lambda-range applicability only. "
             "Remaining-thickness ratio and minimum-wall limits are not checked; full Part 5 applicability is not established. "
             "A passing and failing pressure bracket is established within that range before searching. "
             "A positive supported boundary without a pressure crossing gives a censored life bound; "
             "an unqualified zero-wall endpoint or inapplicable geometry does not establish life. "
             "Other failure modes or six-method ratings may limit life earlier. "
             "Section B retains the point-minimum pressure-wall projection. Neither projection is operating authorization. "
             "Re-inspection at half the projected life is an assessment assumption; zero means immediate action. "
             "Interim operation is a decision for the maintainer of the pipeline record when measurements are inadequate."),
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
        "Method source: Reference 1; their numeric evidence is limited to previous calculation comparisons.",
        "RSTRENG-2D projects each axial row to its greatest depth before applying the same effective-area calculation. "
        "It equals RSTRENG by construction and does not establish two-dimensional interaction capacity. Method source: Reference 1.",
        "DNV-RP-F101 uses tensile strength, maximum depth and length correction for a single longitudinal defect. "
        "The allowable pressure applies modelling and operating factors to capacity. Method source: Reference 2.",
        "Circumferential net-section balance reduces axial membrane capacity in proportion to lost circumferential area. "
        "The extent comparison uses the supplied axial length and joint factors; exceeding that comparison does not invalidate the subsequent net-section balance. "
        "Through-wall loss raises an applicability flag. This is not a burst-pressure or combined-loading calculation. Method source: Reference 3.",
        "The point-minimum comparison uses the required pressure wall (Reference 4) as a diagnostic, not an intervention or API Level 1 criterion. "
        "Both sections are local metal loss: Section A is a single bounded patch, not general loss around the pipe. "
        "The Level 2 calculation uses the wall away from the flaw, after uniform loss and future allowance, "
        "as the reference wall in the simplified local-loss relation (Reference 3). "
        "The full-wall background establishes that reference for these synthetic grids. The reduced rating uses the uncorroded "
        "pressure rating obtained by inverting the linear pressure-wall calculation (Reference 4), then applies the remaining-strength reduction. "
        "The local-loss length equals the count of degraded axial rows times the index spacing, whereas the six-method maximum-depth calculations use the full window. "
        "This difference is retained explicitly. A shell parameter outside the supported range raises an applicability flag.",
        "The validation record reproduces the cited DNV worked-example capacities to better than 0.001%. "
        "That accuracy claim does not extend to all methods: the B31G published comparisons have coarser reported tolerances, "
        "and Modified B31G and RSTRENG are checked only against previous calculations. No published example values are reproduced here.",
        "Applicability flags and severe loss below the replacement floor require escalation regardless of sampling. "
        "Otherwise sampling adequacy is checked first; the strength-factor referral is evaluated only after sampling is adequate. "
        "An under-sampled result requires more measurements; a de-rating retains both pressure and axial restrictions. "
        "Acceptance or monitoring gives **accept** only with "
        "positive remaining life. Other outcomes give **escalate**. "
        "The pressure reduction uses only pressure ratios and the corrected local-loss pressure rating, never an axial ratio. "
        "The reduced rating participates in the strength decision even when the remaining strength factor is below its allowable value.",
    )]


def deferred_referral(section):
    """Describe the strength referral deferred by a sampling gap."""
    if (section["decision"] == "take more measurements"
            and section["level2"]["rsf"] < section["level2"]["rsf_a"]):
        return ("Its strength factor is also below the allowable factor, so after the added readings "
                "a formal local-loss evaluation follows unless the repeat assessment changes that. ")
    return ""


def section_material(result):
    summary, findings, recommendations = [], [], []
    detailed, appendix = [], []
    for index, s in enumerate(result["sections"]):
        row = min(s["strength"]["methods"], key=lambda r: r["margin"])
        pressure = s["inputs"]["design_pressure_psi"]
        governing_rating = min(s["local_loss_rating_psi"], row["allowable_pressure_psi"])
        governing_name = ("Reduced local-loss rating" if governing_rating == s["local_loss_rating_psi"]
                          else method_name(row["method"]))
        comparison = (
            f"{method_name(row['method'])} governs the separate six-method comparison at "
            f"{number(row['allowable_pressure_psi'])} psi. "
            if governing_name == "Reduced local-loss rating" else ""
        )
        summary.append(text(f"**{s['name']}: {s['decision']}.** {action_text(s)} {deferred_referral(s)}"
                            "A changed minimum, defect extent or loss pattern will require reassessment; "
                            "a repeat-inspection corrosion rate will change the life projection. "
                            f"{governing_name} governs at {number(governing_rating)} psi against "
                            f"{number(pressure)} psi, a ratio of {ratio(governing_rating / pressure)}. "
                            + comparison +
                            f"Minimum wall is {number(s['coordinator']['t_measured_min_in'], thickness=True)} in."))
        suff = s["sufficiency"]["metrics"]
        sampling_basis = (f"The local profile contains {suff['profile_cells']} degraded cells. " +
                          ("Field data is adequate; the strength-factor comparison requires formal evaluation. " if s["sufficiency"]["status"] == "ESCALATE" else f"At least {MeasurementSufficiency().min_lml_profile_cells} degraded cells are required by the assessment sampling criterion. "))
        adequacy_note = (
            "; strength alone does not resolve measurement adequacy"
            if s["sufficiency"]["status"] == "TAKE_MORE" else ""
        )
        findings.append(text(f"{s['name']}: {method_name(row['method'])} governs the six-method comparison at "
                             f"{number(row['allowable_pressure_psi'])} psi versus {number(row['demand_psi'])} psi, "
                             f"a ratio of {ratio(row['margin'])} against 100.0%. "
                             f"The reduced local-loss rating is {number(s['local_loss_rating_psi'])} psi against "
                             f"{number(s['inputs']['design_pressure_psi'])} psi, a ratio of "
                             f"{ratio(s['local_loss_rating_psi'] / s['inputs']['design_pressure_psi'])}. "
                             "The lower pressure rating participates in the decision. "
                             + sampling_basis + f"The final decision is {s['decision']}{adequacy_note}. "
                             + deferred_referral(s)))
        recommendations.append(text(s["name"] + ": **" + s["decision"] + "**. " + action_text(s) + " " + deferred_referral(s) +
                                    "Source: the measurement-sufficiency criteria in Section 3.5 and the assessed thickness/strength results in Section 5."))
        detailed.extend(result_blocks(s, f"section-{index + 1}"))
        grid = s["inputs"]["grid"]
        appendix.append(table(s["name"] + " — complete thickness grid",
                              ["Axial position (in)"] + [f"Column {i+1} (in)" for i in range(len(grid[0]))],
                              [[number(x)] + [number(t, thickness=True) for t in row]
                               for x, row in zip(s["inputs"]["axial_positions_in"], grid)]))
        appendix.append(method_table(s, s["name"] + " — complete method rows"))
    return summary, findings, recommendations, detailed, appendix


class AssessmentDocumentMeta(DocumentMeta):
    """Optional assessment cover label; shared reports have no implicit label."""

    assessment_label: str = ""


def report_spec(root, result):
    summary, findings, recommendations, detailed, appendix = section_material(result)
    assumptions = load_yaml(root / WORKFLOW / "assumptions.yml")
    references = [f"{source}. {caveat}" for source, caveat in
                  zip(assumptions["method_sources"], REFERENCE_CAVEATS, strict=True)]
    config = load_yaml(root / WORKFLOW / "input.yml")
    revision_history = [RevisionRow(**row) for row in config["revision_history"]]
    latest_revision = revision_history[-1]
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
                    "Remaining life uses the Section A fixed-geometry reduced-rating route and the Section B pressure-wall route; it does not predict the future crossing of every six-method capacity curve. "
                    "RSTRENG-2D equals RSTRENG by construction; Modified B31G and RSTRENG have no external validation beyond previous calculation comparisons. "
                    "The corrected Level 2 reference wall does not establish complete edition-matched qualification to Reference 3; "
                    "the bulging relation remains simplified. "
                    "A strength ratio above unity does not establish measurement adequacy or authorize operation. "
                    "Section B spacing and supplementary loads and material strength require confirmation for any physical application.")],
              recommendations, [text("\n".join(f"{i}. {ref}" for i, ref in enumerate(references, 1)))],
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
        document=AssessmentDocumentMeta(number=f"B0000-RPT-001-{latest_revision.rev}",
                              revision=latest_revision.rev, title=TITLE,
                              project="Synthetic assessment", client="Demonstration", date=latest_revision.date,
                              assessment_label="Synthetic example",
                              prepared_by="—", checked_by="—", approved_by="—",
                              revision_history=revision_history),
        sections=[Section(key=f"section-{i}", title=title, blocks=body)
                  for i, (title, body) in enumerate(zip(SECTION_TITLES, blocks), 1)],
        appendices=[Section(key="appendices", title="Appendices", blocks=appendix)],
        provenance=provenance,
    )


def build(root, output=None):
    """Publish the site pair and legacy copies, or only an explicit output folder."""
    root = Path(root).resolve()
    directories = ((Path(output),) if output is not None else
                   (root / "docs/api/ffs", root / "docs/reports"))
    source_digests = {name: digest(root / name) for name in CALCULATION_SOURCES}
    result = evaluate(root)
    page = render_html(report_spec(root, result), assessment_presentation=True)
    manifest = {
        "schema_version": 1, "data_status": "synthetic, for demonstration",
        "intended_use": "worked example; no actual operating asset qualification",
        "excluded_outputs": ["Automatic composite-repair candidate advice; no repair-specific design basis is supplied"],
        "digest_convention": "Input SHA-256 after CRLF to LF normalisation; output SHA-256 over exact UTF-8 bytes",
        "assessment": public_assessment(result),
        "calculation_sources_sha256_lf": source_digests,
        "assumptions_sha256_lf": digest(root / WORKFLOW / "assumptions.yml"),
        "configuration_sha256_lf": digest(root / WORKFLOW / "input.yml"),
        "builder_sha256_lf": digest(root / "scripts/reporting/build_pipeline_ffs_report.py"),
        "report_sha256": hashlib.sha256(page.encode("utf-8")).hexdigest(),
        "figures": "inline vector figures; 680-unit width; no raster or external dependencies",
    }
    if source_digests != {name: digest(root / name) for name in CALCULATION_SOURCES}:
        raise ValueError("Calculation source changed during build")
    manifest_text = json.dumps(manifest, ensure_ascii=False, indent=2, sort_keys=True,
                               allow_nan=False) + "\n"
    for directory in directories:
        directory.mkdir(parents=True, exist_ok=True)
        html_path = directory / "pipeline-ffs-assessment.html"
        manifest_path = directory / "pipeline-ffs-assessment.manifest.json"
        html_path.write_text(page, encoding="utf-8", newline="\n")
        manifest_path.write_text(manifest_text, encoding="utf-8", newline="\n")
        if hashlib.sha256(html_path.read_bytes()).hexdigest() != manifest["report_sha256"]:
            raise ValueError("Report readback mismatch")
        if json.loads(manifest_path.read_text(encoding="utf-8")) != manifest:
            raise ValueError("Manifest readback mismatch")
    return (directories[0] / "pipeline-ffs-assessment.html",
            directories[0] / "pipeline-ffs-assessment.manifest.json")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    for artifact in build(Path(__file__).resolve().parents[2], args.output):
        print(artifact.name)
