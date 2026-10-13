# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent checks of the reference jacket transcription and report."""

from collections import Counter
import math
from pathlib import Path

import pytest
import yaml

FORM = Path(__file__).parents[1] / "fixtures/cathodic_protection/workflow_inputs/reference_jacket_60m_members.yml"


def schedule():
    """Read the sole authoritative transcribed schedule."""
    return yaml.safe_load(FORM.read_text())["structural_model"]


def test_reference_schedule_preserves_all_joints_members_and_properties():
    model = schedule()
    assert len(model["joints"]) == 30
    assert len(model["members"]) == 63
    assert len(model["property_sets"]) == 9
    assert len(model["section_classes"]) == 7
    assert {m["id"] for m in model["members"]} == set(range(1, 64))
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    counts = Counter(m["section_class"] for m in model["members"])
    for member in model["members"]:
        a, b = member["joint_ids"]
        assert member["true_length_m"] == pytest.approx(math.dist(joints[a], joints[b]))
        assert member["count"] == 1
        assert str(member["id"]) in member["locator"]
    for section in model["section_classes"]:
        members = [m for m in model["members"] if m["section_class"] == section["section_class"]]
        assert section["count"] == counts[section["section_class"]]
        assert section["total_true_length_m"] == pytest.approx(sum(m["true_length_m"] for m in members))
    assert model["total_true_length_m"] == pytest.approx(sum(m["true_length_m"] for m in model["members"]))


def test_reference_mass_excludes_transition_piece_and_piles():
    model = schedule()
    props = {p["id"]: p for p in model["property_sets"]}
    total = 0
    for member in model["members"]:
        p = props[member["property_set_id"]]
        d, t = p["diameter_m"], p["wall_thickness_m"]
        total += math.pi / 4 * (d ** 2 - (d - 2 * t) ** 2) * member["true_length_m"] * p["density_kg_m3"]
    assert model["steel_mass_kg"] == pytest.approx(total)
    assert model["excluded_concentrated_mass_kg"] == 450000
    comparison = next(c for c in model["comparisons"] if c["quantity"] == "Tubular jacket steel mass")
    assert comparison["printed"] == 1471000
    assert comparison["model"] == pytest.approx(total)
    assert comparison["difference_percent"] == pytest.approx(100 * (total / 1471000 - 1))


def test_reference_comparisons_recompute_every_percentage_and_section_match():
    model = schedule()
    for row in model["comparisons"]:
        assert row["difference_percent"] == pytest.approx(100 * (row["model"] / row["printed"] - 1))
        assert row["locator"]
    sections = yaml.safe_load(FORM.read_text())["member_sections"]
    assert len(sections) == 7
    for row in sections:
        assert row["count"] > 0
        assert row["true_length_m"] > 0
        assert any(s["diameter_m"] == row["diameter_m"] and s["wall_thickness_m"] * 1000 == pytest.approx(row["wall_thickness_mm"]) for s in model["section_classes"])


def test_reference_geometry_checks_derive_from_coordinates():
    model = schedule()
    form = yaml.safe_load(FORM.read_text())
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    assert set(joints) == set(range(1, 31))
    assert {p["id"] for p in model["property_sets"]} == set(range(1, 10))
    expected = {"Leg count": len([j for j in joints.values() if j[2] == min(q[2] for q in joints.values())]),
                "Water depth": -min(j[2] for j in joints.values()),
                "Height above mudline": max(j[2] for j in joints.values()) - min(j[2] for j in joints.values())}
    for level, ids in (("Top", (1, 2, 3)), ("Mudline", (16, 17, 18))):
        for a, b in ((ids[0], ids[1]), (ids[1], ids[2]), (ids[2], ids[0])):
            expected[f"{level} triangle side {a}-{b}"] = math.dist(joints[a][:2], joints[b][:2])
        expected[f"{level} y-projection span"] = max(joints[i][1] for i in ids) - min(joints[i][1] for i in ids)
    heights = [joints[i][2] - joints[16][2] for i in (13, 10, 7, 4, 1)]
    expected.update({f"Frame node height {i+1}": v for i, v in enumerate(heights)})
    expected.update({f"Bay height {i+1}": heights[i+1] - heights[i] for i in range(4)})
    for a, b in ((1, 13), (2, 14), (3, 15)):
        dz = joints[a][2] - joints[b][2]
        expected[f"Leg {a} inclination in 3D"] = math.degrees(math.atan(math.dist(joints[a][:2], joints[b][:2]) / dz))
        expected[f"Leg {a} inclination in y projection"] = math.degrees(math.atan(abs(joints[a][1] - joints[b][1]) / dz))
    for section in form["member_sections"]:
        cls = next(c for c in model["section_classes"] if c["diameter_m"] == section["diameter_m"] and c["wall_thickness_m"] * 1000 == pytest.approx(section["wall_thickness_mm"]))
        expected[section["description"] + " diameter"] = cls["diameter_m"]
        expected[section["description"] + " wall thickness"] = cls["wall_thickness_m"] * 1000
        assert section["count"] == cls["count"]
        assert section["true_length_m"] == cls["total_true_length_m"]
    for row in model["comparisons"]:
        if row["quantity"] != "Tubular jacket steel mass":
            assert row["model"] == pytest.approx(expected[row["quantity"]])


def test_reference_classes_and_licence_are_independently_bound():
    import hashlib

    model = schedule()
    props = {p["id"]: p for p in model["property_sets"]}
    for member in model["members"]:
        assert member["section_class"] == min(member["property_set_id"], 7)
        assert set(member["joint_ids"]) <= set(range(1, 31))
    for cls in model["section_classes"]:
        members = [m for m in model["members"] if m["property_set_id"] in cls["property_set_ids"]]
        mass = sum(math.pi * (props[m["property_set_id"]]["diameter_m"] * props[m["property_set_id"]]["wall_thickness_m"] - props[m["property_set_id"]]["wall_thickness_m"] ** 2) * m["true_length_m"] * props[m["property_set_id"]]["density_kg_m3"] for m in members)
        assert cls["steel_mass_kg"] == pytest.approx(mass)
    for row in model["joints"] + model["members"] + model["property_sets"]:
        assert model["filename"] in row["locator"]
        assert model["revision"][:8] in row["locator"]
    licence = FORM.with_name(model["licence_copy"])
    record = next(r for r in model["source_files"] if r["filename"] == "LICENSE")
    assert licence.stat().st_size == record["size_bytes"]
    assert hashlib.sha256(licence.read_bytes()).hexdigest() == record["sha256"]


def test_reference_drawing_projects_every_member_on_one_scale():
    from xml.etree import ElementTree as ET
    from digitalmodel.cathodic_protection.report_reference import reference_figure

    model = schedule()
    figure = reference_figure(model)
    root = ET.fromstring(figure.svg)
    scale = float(root.attrib["data-scale"])
    lines = {int(n.attrib["data-member"].split("-")[-1]): n for n in root.iter() if "data-member" in n.attrib}
    assert len(lines) == len(model["members"])
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    props = {p["id"]: p for p in model["property_sets"]}
    min_y = min(joints[i][1] - props[m["property_set_id"]]["diameter_m"] / 2 for m in model["members"] for i in m["joint_ids"])
    top = max(joints[i][2] + props[m["property_set_id"]]["diameter_m"] / 2 for m in model["members"] for i in m["joint_ids"])
    for m in model["members"]:
        a, b = (joints[i] for i in m["joint_ids"])
        line = lines[m["id"]]
        assert float(line.attrib["x1"]) == pytest.approx(55 + (a[1] - min_y) * scale)
        assert float(line.attrib["x2"]) == pytest.approx(55 + (b[1] - min_y) * scale)
        assert float(line.attrib["y1"]) == pytest.approx(35 + (top - a[2]) * scale)
        assert float(line.attrib["y2"]) == pytest.approx(35 + (top - b[2]) * scale)
        assert abs(float(line.attrib["x2"]) - float(line.attrib["x1"])) == pytest.approx(abs(b[1] - a[1]) * scale)
        assert abs(float(line.attrib["y2"]) - float(line.attrib["y1"])) == pytest.approx(abs(b[2] - a[2]) * scale)
    text = " ".join(root.itertext())
    for value in ("Apache License 2.0", "CC BY 4.0", "Mroczek", "da77a393", "[1]", "[2]"):
        assert value in text
        assert value in figure.caption
    assert any("data-scale-bar-m" in n.attrib for n in root.iter())
    assert "y–z" in figure.caption
    levels = {n.attrib["data-level"]: float(n.attrib["data-elevation-m"]) for n in root.iter() if "data-level" in n.attrib}
    assert levels == {"waterline": 0, "mudline": -60}


def test_reference_drawing_bands_bar_and_dimension_labels():
    import re
    from xml.etree import ElementTree as ET
    from digitalmodel.cathodic_protection.report_reference import reference_figure

    model = schedule()
    root = ET.fromstring(reference_figure(model).svg)
    scale = float(root.attrib["data-scale"])
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    props = {p["id"]: p for p in model["property_sets"]}
    top = max(joints[i][2] + props[m["property_set_id"]]["diameter_m"] / 2 for m in model["members"] for i in m["joint_ids"])
    for group in (n for n in root.iter() if "data-zone" in n.attrib):
        lower, upper = float(group.attrib["data-lower-m"]), float(group.attrib["data-upper-m"])
        rect = next(n for n in group if n.tag.endswith("rect"))
        assert float(rect.attrib["y"]) == pytest.approx(35 + (top - upper) * scale)
        assert float(rect.attrib["height"]) == pytest.approx((upper - lower) * scale)
        assert (lower, upper) in ((-60, 0), (0, 14.1))
    bar = next(n for n in root.iter() if "data-scale-bar-m" in n.attrib)
    length = float(re.search(r" h ([^ ]+)", bar.attrib["d"])[1])
    assert length == pytest.approx(float(bar.attrib["data-scale-bar-m"]) * scale, abs=0.001)
    text = " ".join(root.itertext())
    zs = [j[2] for j in joints.values()]
    upper = [j[1] for j in joints.values() if j[2] == max(zs)]
    lower = [j[1] for j in joints.values() if j[2] == min(zs)]
    assert f"Axis height above mudline: {max(zs)-min(zs):.1f} m" in text
    assert f"water depth: {-min(zs):.1f} m" in text
    assert f"Projected top span: {max(upper)-min(upper):.3f} m" in text
    assert f"mudline span: {max(lower)-min(lower):.3f} m" in text


def test_reference_zone_areas_match_independent_axis_intersections():
    from digitalmodel.cathodic_protection.report_reference import reference_zone_rows

    model = schedule()
    form = yaml.safe_load(FORM.read_text())
    rows = reference_zone_rows(model, form["initial_pile_estimate"])
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    props = {p["id"]: p for p in model["property_sets"]}
    intervals = {"Water-column steel, undivided": (-60, 0),
                 "Above-water steel, undivided": (0, 14.1)}
    assigned = 0
    for row in rows:
        if row["zone"] == "Buried piles, initial estimate":
            assert row["count"] == 3
            assert row["length_m"] == 3 * 65
            assert row["area_m2"] == pytest.approx(3 * math.pi * 3 * 65)
            continue
        lower, upper = intervals[row["zone"]]
        length, count = 0, 0
        for m in model["members"]:
            if m["section_class"] != row["section_class"]:
                continue
            a, b = (joints[i] for i in m["joint_ids"])
            low, high = sorted((a[2], b[2]))
            overlap = max(0, min(upper, high) - max(lower, low))
            piece = math.dist(a, b) * overlap / (high - low)
            length += piece
            count += piece > 0
        assert row["count"] == count
        assert row["length_m"] == pytest.approx(length)
        assert row["area_m2"] == pytest.approx(math.pi * row["diameter_m"] * length)
        assert row["diameter_m"] == props[row["section_class"]]["diameter_m"]
        assigned += row["length_m"]
    assert assigned == pytest.approx(model["total_true_length_m"])


def test_reference_zone_derivation_prints_formula_totals_and_limits():
    from digitalmodel.cathodic_protection.report_reference import reference_zone_blocks
    from digitalmodel.reporting.spec import TableBlock, TextBlock

    form = yaml.safe_load(FORM.read_text())
    blocks = reference_zone_blocks(schedule(), form["initial_pile_estimate"])
    text = " ".join(b.markdown for b in blocks if isinstance(b, TextBlock))
    assert "π × outside diameter × axis length in the elevation interval" in text
    assert "Splash separation is not evaluated" in text
    assert "initial pile estimate" in text
    assert "974.549 m" in text
    assert "outside the reported intervals: 0.000 m" in text
    table = next(b for b in blocks if isinstance(b, TableBlock))
    assert len(table.rows) == 15
    assert table.columns[-1] == "Locator"


def test_reference_inputs_anode_mass_and_mass_only_arrangement():
    from digitalmodel.cathodic_protection import _kernels as kernel
    from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
    from digitalmodel.cathodic_protection.report_reference import reference_arrangements

    form = yaml.safe_load(FORM.read_text())
    anode = form["inputs"]["anode"]
    mass = math.pi * anode["radius_m"] ** 2 * anode["length_m"] * kernel.ANODE_DENSITY_ALZNI
    assert anode["individual_anode_mass_kg"] == pytest.approx(mass)
    assert anode["length_m"] == 1
    assert anode["radius_m"] == 0.05
    cfg = run_cathodic_protection(form)
    result = cfg["results"]
    req, ver = result["anode_requirements"], result["current_output_verification"]
    needed = result["current_demand_A"]["total_mean_A"] * kernel.HOURS_PER_YEAR * result["design_life_years"] / (req["electrochemical_capacity_Ah_kg"] * req["utilization_factor"])
    assert ver["anode_count_source"] == "mass"
    assert ver["anode_count"] == math.ceil(needed / mass)
    arrangements = reference_arrangements(cfg)
    assert arrangements["stated"]["current_output_verification"] == ver
    if result["status"]["result"] == "PASS":
        assert arrangements["rechecked"] is None
    else:
        assert arrangements["rechecked"]["status"]["result"] == "PASS"


def test_reference_current_areas_match_independent_depth_splits():
    form = yaml.safe_load(FORM.read_text())
    model = schedule()
    joints = {j["id"]: j["coordinates_m"] for j in model["joints"]}
    props = {p["id"]: p for p in model["property_sets"]}
    for row in form["inputs"]["structure"]["zones"]:
        if row["zone"] == "buried":
            area = 3 * math.pi * 3 * 65
        else:
            lower, upper = row["axis_interval_m"]
            area = 0
            for member in model["members"]:
                a, b = (joints[i] for i in member["joint_ids"])
                low, high = sorted((a[2], b[2]))
                piece = math.dist(a, b) * max(0, min(high, upper) - max(low, lower)) / (high - low)
                area += math.pi * props[member["property_set_id"]]["diameter_m"] * piece
        assert row["area_m2"] == pytest.approx(area)


def test_reference_case_retains_the_repository_held_cp_basis():
    from digitalmodel.cathodic_protection._edition import DEFAULT_EDITION

    cfg = yaml.safe_load(FORM.read_text())["inputs"]
    held = yaml.safe_load(FORM.with_name("jacket.yml").read_text())["inputs"]
    assert cfg["environment"] == held["environment"]
    assert cfg["design_data"]["design_life"] == held["design_data"]["design_life"]
    assert cfg["design_data"]["edition"] == DEFAULT_EDITION
    for name in ("material", "type", "length_m", "radius_m"):
        assert cfg["anode"][name] == held["anode"][name]
    assert "utilization_factor" not in cfg["anode"]
    assert all(z["coating_category"] == held["structure"]["zones"][0]["coating_category"] for z in cfg["structure"]["zones"])


def test_reference_checks_show_every_demand_capacity_margin_and_verdict():
    from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
    from digitalmodel.cathodic_protection.report_reference import reference_arrangements, reference_checks
    from digitalmodel.cathodic_protection import _kernels as kernel

    cfg = run_cathodic_protection(yaml.safe_load(FORM.read_text()))
    arrangements = reference_arrangements(cfg)
    table = reference_checks(cfg, arrangements)
    result = cfg["results"]
    ver, req = result["current_output_verification"], result["anode_requirements"]
    expected = {
        "Initial current": (result["current_demand_A"]["total_initial_A"], ver["total_anode_current_output_initial_A"], "A"),
        "Final current": (result["current_demand_A"]["total_final_A"], ver["total_anode_current_output_final_A"], "A"),
        "Mass": (result["current_demand_A"]["total_mean_A"] * kernel.HOURS_PER_YEAR * result["design_life_years"] / (req["electrochemical_capacity_Ah_kg"] * req["utilization_factor"]), ver["anode_count"] * cfg["inputs"]["anode"]["individual_anode_mass_kg"], "kg")}
    assert table.columns == ["Arrangement", "Check", "Demand", "Capacity", "Margin", "Result", "Unit", "Locator"]
    assert len(table.rows) == 3
    for row in table.rows:
        demand, capacity, unit = expected[row[1]]
        decimals = 0 if unit == "kg" else 3
        assert row[2] == f"{demand:.{decimals}f}"
        assert row[3] == f"{capacity:.{decimals}f}"
        assert float(row[4].removesuffix("%")) == pytest.approx(100 * (capacity / demand - 1), abs=0.051)
        assert row[5] == ("PASS" if capacity >= demand else "FAIL")
        assert row[6] == unit
        assert row[7].startswith("[3]")


def test_reference_reduced_arrangement_rechecks_without_mutating_inputs():
    from copy import deepcopy
    from digitalmodel.cathodic_protection.engine_adapter import run_cathodic_protection
    from digitalmodel.cathodic_protection.report_reference import reference_arrangements, reference_checks

    cfg = run_cathodic_protection(yaml.safe_load(FORM.read_text()))
    cfg["inputs"]["anode"]["count"] = cfg["results"]["current_output_verification"]["count_by_mass"] - 1
    cfg = run_cathodic_protection(cfg)
    before = deepcopy(cfg)
    arrangements = reference_arrangements(cfg)
    assert cfg == before
    stated, revised = arrangements["stated"], arrangements["rechecked"]
    assert stated["status"]["result"] == "FAIL"
    assert revised["status"]["result"] == "PASS"
    assert revised["current_output_verification"]["anode_count"] == stated["current_output_verification"]["recommended_anode_count"]
    assert all(revised["status"]["checks"].values())
    assert revised["anode_requirements"]["individual_mass_kg"] == stated["anode_requirements"]["individual_mass_kg"]
    for name in ("equivalent_radius_initial_m", "equivalent_radius_final_m", "anode_resistance_initial_ohm", "anode_resistance_final_ohm"):
        assert revised["current_output_verification"][name] == stated["current_output_verification"][name]
    table = reference_checks(cfg, arrangements)
    assert len(table.rows) == 6
    for name, result in (("Stated arrangement", stated), ("Rechecked arrangement", revised)):
        rows = [r for r in table.rows if r[0] == name]
        count = result["current_output_verification"]["anode_count"]
        for row in rows:
            if row[1] == "Mass":
                capacity = count * cfg["inputs"]["anode"]["individual_anode_mass_kg"]
                assert row[3] == f"{capacity:.0f}"
                assert row[5] == ("PASS" if result["status"]["checks"]["mass"] else "FAIL")
            else:
                phase = row[1].split()[0].lower()
                capacity = result["current_output_verification"][f"total_anode_current_output_{phase}_A"]
                assert row[3] == f"{capacity:.3f}"
                assert row[5] == ("PASS" if result["status"]["checks"][f"{phase}_current_output"] else "FAIL")
