# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Weight-only bounds and static QA for declared S-lay layouts."""

import math
from pathlib import Path

import yaml


def stinger_layout_for_depth(basis, depth):
    """Match declared depth numerically; a present map must contain the depth."""
    if "stinger_by_depth" not in basis:
        return None
    layouts = basis["stinger_by_depth"]
    if not isinstance(layouts, dict):
        raise ValueError("Invalid stinger depth map")
    numeric = {}
    for key, layout in layouts.items():
        try:
            value = float(key)
        except (TypeError, ValueError) as error:
            raise ValueError("Invalid stinger depth key") from error
        if not math.isfinite(value) or value in numeric:
            raise ValueError("Nonfinite or duplicate stinger depth key")
        numeric[value] = layout
    layout = numeric.get(float(depth))
    if not layout:
        raise ValueError(f"No stinger layout declared for depth {depth} m")
    return layout


def end_a_support_reaction(end_a_x, support_x, forces):
    """Read contact resultant at the support nearest measured global End A X.

    The declared fixed, level supports and End A must use the same global
    frame. Ties use support order. Positive resultant denotes contact, as in
    the stinger lift-off readback; zero denotes loss of contact.
    """
    if len(support_x) == 0 or len(support_x) != len(forces):
        raise ValueError("Aligned nonempty support positions and forces required")
    if not all(math.isfinite(value) for value in [end_a_x, *support_x, *forces]):
        raise ValueError("Finite End A and support readbacks required")
    index = min(range(len(support_x)), key=lambda i: abs(support_x[i] - end_a_x))
    return forces[index]


def screen_case(campaign: Path, case_id: str) -> dict:
    """Separate length closure and tangent closure in a weight-only hand model.

    The approximation uses a smooth circular supported arc, horizontal
    touchdown, an inextensible empty pipe and frictionless supports. It cannot
    calculate bending, lift-off, free-end motion or native force residuals.
    """
    table = yaml.safe_load((campaign / "case-table.yml").read_text(encoding="utf-8"))
    basis = table["basis"]
    case = next(row for row in table["cases"] if row["id"] == case_id)
    pipe = basis["pipeline"]
    od = pipe["dimensions"]["outer_diameter"]
    wall = pipe["dimensions"]["wall_thickness"]
    mass = basis["steel_density_te_m3"] * math.pi / 4 * (od**2 - (od - 2 * wall)**2)
    for coating in [pipe["coatings"]["corrosion"], *pipe["coatings"]["weight"]]:
        outer = od + 2 * coating["thickness"]
        mass += coating["density"] * math.pi / 4 * (outer**2 - od**2)
        od = outer
    weight = (mass - basis["water_density_te_m3"] * math.pi / 4 * od**2) * basis["gravity_m_s2"]
    stinger = basis["equipment"]["stinger"]
    radius = stinger["radius"]
    layout = stinger_layout_for_depth(basis, case["depth_m"])
    arc = layout["length_m"] if layout else sum(section["length"] for section in stinger["sections"])
    folder = campaign / "models" / case_id / "includes"
    support_types = yaml.safe_load((folder / "13_supports.yml").read_text(encoding="utf-8"))["SupportTypes"]
    supports = yaml.safe_load((folder / "06_vessels.yml").read_text(encoding="utf-8"))["Vessels"][0]["Supports"]
    diameter = next(row["Diameter"] for row in support_types if row["Name"] == supports[-1]["SupportType"])
    axis_offset = stinger["rollers"][-1]["z_offset"] + diameter / 2 + od / 2
    origin = stinger["origin_position"]
    depth = case["depth_m"]
    tension = case["top_tension_kn"]
    length = sum(segment["length"] for segment in pipe["segments"])
    line = yaml.safe_load((folder / "07_lines.yml").read_text(encoding="utf-8"))["Lines"][0]
    connection_key = next(key for key in line if key.startswith("Connection, "))
    top, bottom = line[connection_key]
    span = top[1] - bottom[1]
    winch = yaml.safe_load((folder / "11_winches.yml").read_text(encoding="utf-8"))["Winches"][0]
    lead = winch["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"][0][1] - top[1]
    if origin[1:] != [0, 0] or abs(axis_offset) > 1e-9:
        raise ValueError("Weight-only screen requires the declared level pipe-axis arc")

    def geometry(supported_arc):
        theta = supported_arc / radius
        z = origin[2] - radius * (1 - math.cos(theta)) + axis_offset
        height = depth + z
        tip_tension = tension + weight * (z - top[3])
        horizontal = tip_tension - weight * height
        if min(height, horizontal) <= 0:
            raise ValueError("Screen requires positive suspended height and horizontal tension")
        a = horizontal / weight
        suspended = math.sqrt(height**2 + 2 * a * height)
        suspended_span = a * math.acosh(1 + height / a)
        projection = radius * math.sin(theta)
        taut = supported_arc + suspended + span - projection - suspended_span
        return {"height_m": height, "tip_tension_kn": tip_tension,
                "horizontal_tension_kn": horizontal, "suspended_length_m": suspended,
                "suspended_span_m": suspended_span, "supported_arc_m": supported_arc,
                "projected_arc_m": projection, "taut_length_m": taut,
                "excess_m": length - taut, "tip_angle_deg": math.degrees(theta),
                "departure_deg": math.degrees(math.acos(horizontal / tip_tension)),
                "taut_anchor_span_m": projection + suspended_span + length - supported_arc - suspended}

    current = geometry(arc)
    # Change only arc length at the declared radius; solve T_tip cos(theta) = H.
    lo, hi = 0.0, math.acos(1 - depth / radius)
    for _ in range(100):
        theta = (lo + hi) / 2
        drop = radius * (1 - math.cos(theta))
        residual = (tension - weight * drop) * math.cos(theta) - (tension - weight * depth)
        if residual > 0:
            lo = theta
        else:
            hi = theta
    angle_only = geometry(radius * (lo + hi) / 2)
    return {"case_id": case_id, "weight_kn_m": weight, "depth_m": depth,
            "top_tension_kn": tension, "pipe_length_m": length, "lead_m": lead,
            "length_tolerance_m": 0.1, "angle_tolerance_deg": 0.1,
            "tolerance_meaning": "arithmetic reproduction only; not physical model accuracy",
            "touchdown_assumption": "Seabed surface; omitting coated radius understates excess by 0.2-0.4 m.",
            "current": current,
            "length_only": {"anchor_span_m": current["taut_anchor_span_m"],
                            "excess_m": current["excess_m"] - (current["taut_anchor_span_m"] - span),
                            "angle_mismatch_deg": current["departure_deg"] - current["tip_angle_deg"]},
            "angle_only": angle_only,
            "combined": {"supported_arc_m": angle_only["supported_arc_m"],
                         "anchor_span_m": angle_only["taut_anchor_span_m"],
                         "excess_m": angle_only["excess_m"] - (angle_only["taut_anchor_span_m"] - span),
                         "angle_mismatch_deg": angle_only["departure_deg"] - angle_only["tip_angle_deg"],
                         "qualification": "Weight-only counterfactual; not the declared stiff design basis."}}


PENDING_TABLE_B_ROWS = ("touchdown_x", "sag_bend_strain", "stiff_closure")
PENDING_REFERENCE_REASON = "published reference pending, #2608"


def evaluate_static_acceptance(*, wire_length, end_a_reaction, last_support_force,
                               grounded_tension, overbend_strain, lift_off_arc,
                               departure_angle, reference, touchdown_x=None,
                               sag_bend_strain=None, diameter=0.762, radius=150):
    """Report static QA with separate pass, fail and NOT_EVALUATED counts.

    Touchdown, sag-bend and stiff closure await published references in #2608.
    Their readings and any supplied reference values do not affect acceptance.
    S1 bounds conformity; S2 design acceptance and convergence remain separate.
    """
    if len(grounded_tension) == 0:
        raise ValueError("Grounded tension readback is required")
    reference_values = [reference.get(name) for name in ("lift_off_arc", "departure_angle")]
    values = [wire_length, end_a_reaction, last_support_force, overbend_strain,
              diameter, radius, lift_off_arc, departure_angle,
              *grounded_tension, *reference_values]
    if not all(isinstance(value, (int, float)) and math.isfinite(value) for value in values):
        raise ValueError("Finite native readings and reference values required")
    if diameter <= 0 or radius <= 0:
        raise ValueError("Positive diameter and radius required")
    checks = {
        "wire_length": (5 <= wire_length <= 15, "Tensioner wire must remain between 5 and 15 m"),
        "end_a_reaction": (end_a_reaction > 0, "End A must remain in contact with its nearest support"),
        "stinger_conformity": (
            0 <= overbend_strain <= 1.10 * diameter / (2 * radius) and last_support_force == 0,
            "S1 requires strain conformity and zero last-support force"),
        "lift_off_arc": (abs(lift_off_arc - reference["lift_off_arc"]) <= 5 + 1e-12,
                         "lift_off_arc differs from Table B beyond its QA tolerance"),
        "departure_angle": (abs(departure_angle - reference["departure_angle"]) <= 3 + 1e-12,
                            "departure_angle differs from Table B beyond its QA tolerance"),
        "grounded_tension": (min(grounded_tension) >= 0,
                             "Grounded effective tension must be nonnegative"),
    }
    rows = {name: {"status": "PASS" if passed else "FAIL", "reason": reason}
            for name, (passed, reason) in checks.items()}
    rows.update({name: {"status": "NOT_EVALUATED", "reason": PENDING_REFERENCE_REASON}
                 for name in PENDING_TABLE_B_ROWS})
    counts = {name: sum(row["status"] == status for row in rows.values())
              for name, status in (("passed", "PASS"), ("failed", "FAIL"),
                                   ("not_evaluated", "NOT_EVALUATED"))}
    status = ("rejected" if counts["failed"] else
              f"accepted with {counts['not_evaluated']} rows not evaluated")
    return {"status": status, "fully_accepted": False, "counts": counts, "rows": rows}


def assert_static_acceptance(**readings):
    """Reject failed evaluated rows and return an explicit partial QA report."""
    result = evaluate_static_acceptance(**readings)
    failed = [row["reason"] for row in result["rows"].values() if row["status"] == "FAIL"]
    if failed:
        raise ValueError("; ".join(failed))
    return result
