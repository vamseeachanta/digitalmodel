# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free checks of emitted element sizes, including every section join."""

import copy
import hashlib
import json
import math
from pathlib import Path

import pytest

from scripts.riser_campaign import frozen_scope as frozen

CAMPAIGN = Path(__file__).resolve().parents[2] / "examples/riser-campaign"
COUNT_TABLE = "LineType, Length, NumberOfSegments"


def count_sections(line):
    """Diagnose the old target-only mesh using nearest integer counts."""
    if COUNT_TABLE in line:
        return line[COUNT_TABLE]
    return [[kind, length, max(1, round(length / target))]
            for kind, length, target in line["LineType, Length, TargetSegmentLength"]]


def emitted_sizes(sections):
    """Expand actual lengths/counts independently of the generator's grading."""
    sizes = []
    for _, length, count in sections:
        assert math.isfinite(length) and length > 0
        assert type(count) is int and count > 0
        sizes.extend([length / count] * count)
    return sizes


def assert_adjacent_limit(sections):
    sizes = emitted_sizes(sections)
    assert all(max(a / b, b / a) <= 2.0 for a, b in zip(sizes, sizes[1:]))


def scopes():
    return json.loads((CAMPAIGN / "manifest.json").read_text(encoding="utf-8"))["frozen_tranche"]["rows"]


@pytest.mark.parametrize("scope", scopes(), ids=lambda row: row["case_id"])
def test_all_frozen_native_elements_are_graded(scope):
    geometry = frozen.prepare_geometry(scope)
    load = next(row for row in frozen.expand_loads({"rows": [scope]}) if row["group"] == "Q0")
    model = frozen.native_model(scope, load, geometry)
    sections = count_sections(model["Lines"][0])
    assert_adjacent_limit(sections)
    assert COUNT_TABLE in model["Lines"][0]
    assert math.fsum(row[1] for row in sections) == pytest.approx(geometry["installed_length_m"], abs=1e-9)
    sizes = emitted_sizes(sections)
    assert max(sizes) <= 3.0
    assert max(sizes) > 2.5  # Keep coarse interiors as well as local refinement.
    assert sizes[0] <= 0.5

    hotspots = [0, *[row["suspended_length_m"] for row in geometry["offsets"]]]
    if scope["configuration"] == "SLWR":
        hotspots += [geometry["buoyancy_start_m"], geometry["buoyancy_end_m"]]
        for row in geometry["offsets"]:
            shape = row["shape"]
            hotspots += [b[0] for a, b, c in zip(shape, shape[1:], shape[2:])
                         if (b[2] - a[2]) * (c[2] - b[2]) < 0]
    station = 0.0
    buoyed_length = 0.0
    for kind, length, count in sections:
        end = station + length
        if any(min(end, s + 10) - max(station, s - 10) > 1e-9 for s in hotspots):
            assert length / count <= 0.5
        if kind == "buoyed-pipe":
            if buoyed_length == 0:
                assert station == pytest.approx(geometry["buoyancy_start_m"], abs=1e-9)
            buoyed_length += length
            last_buoyed_end = end
        station = end
    if scope["configuration"] == "SLWR":
        assert buoyed_length == pytest.approx(geometry["buoyancy_length_m"], abs=1e-9)
        assert last_buoyed_end == pytest.approx(geometry["buoyancy_end_m"], abs=1e-9)
    else:
        assert buoyed_length == 0
    assert frozen.native_model(scope, load, geometry) == model


@pytest.mark.parametrize("coarse,fine,ratio", [
    (["pipe", 10.085364085611673, 3], ["pipe", 20.0, 40], 6.723576057074449),
    (["pipe", 10.025711952248457, 3], ["pipe", 4.574288047751452, 9], 6.575260574490966),
])
def test_original_discontinuity_is_rejected_in_both_directions(coarse, fine, ratio):
    # Reproduced from the reviewed generator, without a native solver.
    assert (coarse[1] / coarse[2]) / (fine[1] / fine[2]) == pytest.approx(ratio)
    for sections in ([coarse, fine], [fine, coarse]):
        with pytest.raises(AssertionError):
            assert_adjacent_limit(sections)


@pytest.mark.parametrize("lengths", [
    [0.44603845041444146, 2.9989380095565417, 0.44603845041444146],
    [0.013, 12.0000000001, 0.4999999999],
    [0.5, 6.0000000001, 0.5],
    [0.5, 0.0001, 0.5],
    [0.4999999999, 3.0000000001, 0.013],
])
def test_awkward_lengths_and_material_joins_are_preserved(lengths):
    targets = [["pipe", lengths[0], 0.5], ["buoyed-pipe", lengths[1], 3.0],
               ["pipe", lengths[2], 0.5]]
    original = copy.deepcopy(targets)
    graded = frozen._grade_mesh(targets)
    assert targets == original
    assert_adjacent_limit(graded)
    station = 0.0
    joins = []
    for index, (kind, length, _) in enumerate(graded):
        if index and kind != graded[index - 1][0]:
            joins.append(station)
        station += length
    assert joins == pytest.approx([lengths[0], lengths[0] + lengths[1]], abs=1e-12)
    assert station == pytest.approx(math.fsum(lengths), abs=1e-12)
    assert frozen._grade_mesh(targets) == graded


def test_abrupt_mutation_of_emitted_counts_is_rejected():
    scope = scopes()[0]
    geometry = frozen.prepare_geometry(scope)
    load = frozen.expand_loads({"rows": [scope]})[0]
    sections = copy.deepcopy(frozen.native_model(scope, load, geometry)["Lines"][0][COUNT_TABLE])
    sections[0][2] *= 10
    with pytest.raises(AssertionError):
        assert_adjacent_limit(sections)


@pytest.mark.parametrize("length,target", [
    (0, 0.5), (-1, 0.5), (math.nan, 0.5), (math.inf, 0.5),
    (1, 0), (1, -0.5), (1, math.nan), (1, math.inf),
])
def test_invalid_mesh_intervals_fail_closed(length, target):
    with pytest.raises(ValueError, match="finite and positive"):
        frozen._grade_mesh([["pipe", length, target]])


def test_review_packet_binds_sources_and_exact_frozen_coverage():
    root = CAMPAIGN.parents[1]
    packet = json.loads((root / "docs/verification/riser-mesh-review.json").read_text(encoding="utf-8"))
    assert packet["status"] == "SOLVER_FREE_ONLY"
    assert packet["adjacent_element_ratio_limit"] == 2.0
    assert packet["native_qualification"] == "NOT_RUN"
    assert packet["case_order"] == [scope["case_id"] for scope in scopes()]
    expected = frozen.expand_loads({"rows": scopes()})
    assert [item["id"] for item in packet["inputs"]] == [item["id"] for item in expected]
    assert len({item["path"] for item in packet["inputs"]}) == 144
    assert {entry["path"] for entry in packet["source_files"]} >= {
        "scripts/riser_campaign/frozen_scope.py", "scripts/riser_campaign/generate_inputs.py",
        "scripts/riser_campaign/audit_mesh.py", "tests/riser_campaign/test_mesh.py",
        "tests/riser_campaign/test_frozen_scope.py", "examples/riser-campaign/README.md", ".gitattributes",
    }
    for entry in packet["source_files"]:
        payload = (root / entry["path"]).read_bytes()
        assert hashlib.sha256(payload).hexdigest() == entry["sha256"], entry["path"]
        assert len(payload) == entry["bytes"]
    audits = packet["mesh_audits"]
    assert [row["case_id"] for row in audits] == packet["case_order"]
    assert all(row["unrounded_adjacent_ratio_pass"] for row in audits)
    assert all(row["maximum_adjacent_ratio_bound"] <= 2.0 for row in audits)
    assert all(row["physical_model_matches"] == 12 for row in audits)


@pytest.mark.parametrize("sections", [
    [], [["pipe", 1.0, 0]], [["pipe", 1.0, True]], [["pipe", 1.0, 2.5]],
    [["pipe", math.nan, 1]], [["pipe", math.inf, 1]], [["pipe", -1.0, 1]],
    [["pipe", 20.0, 40], ["pipe", 10.085364085611673, 3]],
    [["pipe", 10.085364085611673, 3], ["pipe", 20.0, 40]],
])
def test_packet_audit_rejects_invalid_or_abrupt_mesh(sections):
    from scripts.riser_campaign.audit_mesh import mesh_statistics
    with pytest.raises(ValueError):
        mesh_statistics(sections)


def test_packet_audit_refuses_changed_raw_bytes(tmp_path):
    from scripts.riser_campaign.audit_mesh import _verified
    payload = b"original input\n"
    path = tmp_path / "input.yml"
    path.write_bytes(payload)
    entry = {"path": path.name, "sha256": hashlib.sha256(payload).hexdigest(), "bytes": len(payload)}
    assert _verified(tmp_path, entry) == payload
    path.write_bytes(b"modified input\n")
    with pytest.raises(ValueError, match="packet digest mismatch"):
        _verified(tmp_path, entry)
