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
