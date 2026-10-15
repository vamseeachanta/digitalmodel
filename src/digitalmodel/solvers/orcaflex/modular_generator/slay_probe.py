# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Prepare one anchor-span diagnostic variation without native execution."""

from copy import deepcopy
import hashlib
import json
import math
from pathlib import Path

import yaml

from . import _NoAliasDumper
from .writers.basefile import build_variation_document


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
    arc = sum(section["length"] for section in stinger["sections"])
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
    lead = winch["Connection, ConnectionX, ConnectionY, ConnectionZ, ConnectionzRelativeTo"][0][1] - origin[0]
    if origin[1:] != [0, 0] or abs(axis_offset) > 1e-9:
        raise ValueError("Probe screen requires the declared level pipe-axis arc")

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
    return {"case_id": case_id, "weight_kn_m": weight, "depth_m": depth,
            "top_tension_kn": tension, "pipe_length_m": length, "lead_m": lead,
            "length_tolerance_m": 0.1, "angle_tolerance_deg": 0.1,
            "current": current,
            "length_only": {"anchor_span_m": current["taut_anchor_span_m"],
                            "excess_m": current["excess_m"] - (current["taut_anchor_span_m"] - span),
                            "angle_mismatch_deg": current["departure_deg"] - current["tip_angle_deg"]},
            "angle_only": geometry(radius * (lo + hi) / 2)}


def prepare_anchor_span_probe(campaign: Path, output: Path) -> dict:
    """Write one text-only variation; preserve every base input and load form.

    Only End B x changes. The manifest binds base bytes and names the native
    readings needed to distinguish length closure from bending/contact effects.
    Neither a queue entry nor a native execution is produced.
    """
    case_id = "d0-t0"
    screen = screen_case(campaign, case_id)
    model = campaign / "models" / case_id
    lines = yaml.safe_load((model / "includes/07_lines.yml").read_text(encoding="utf-8"))
    overrides = deepcopy(lines)
    line = overrides["Lines"][0]
    key = next(key for key in line if key.startswith("Connection, "))
    top, bottom = line[key]
    initial_span = top[1] - bottom[1]
    target_span = screen["current"]["taut_anchor_span_m"]
    bottom[1] = top[1] - target_span
    relative = "probes/d0-t0-anchor-span.yml"
    document = build_variation_document(model / "master.yml", overrides, campaign / relative)
    path = output / relative
    manifest_path = path.with_suffix(".json")
    if path.exists() or manifest_path.exists():
        raise ValueError("Use a new output directory; existing probe files are preserved")
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(yaml.dump(document, Dumper=_NoAliasDumper, sort_keys=False,
                              allow_unicode=True), encoding="utf-8")
    manifest = {"schema": "slay-anchor-span-probe-1", "production_case": False,
                "base_case": case_id, "native_status": "not_run",
                "case_table_sha256": hashlib.sha256((campaign / "case-table.yml").read_bytes()).hexdigest(),
                "base_files": {p.relative_to(campaign).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
                               for p in sorted(model.rglob("*.yml"))},
                "probe_file": relative, "probe_sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                "initial_span_m": initial_span, "target_span_m": target_span,
                "screen": screen,
                "required_readings": ["end_a_displacement", "wire_direction_and_length",
                                      "active_roller_contacts", "departure_slope", "force_residuals",
                                      "tip_contact_force_and_curvature", "maximum_overbend_strain",
                                      "grounded_effective_tension"],
                "interpretation": "Continue anchor span from initial to target with loads and supports fixed. "
                                  "Convergence with a positive wire and the intended force/contact state "
                                  "would show length closure is sufficient for this configuration. "
                                  "Failure alone cannot identify the tip angle: torsional restraint, "
                                  "anchor embedment and bending/contact residuals remain unresolved."}
    manifest_path.write_text(json.dumps(manifest, indent=2, allow_nan=False) + "\n", encoding="utf-8")
    return manifest
