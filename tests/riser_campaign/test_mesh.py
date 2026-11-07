# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Solver-free checks of emitted element sizes, including every section join."""

import copy
import hashlib
import json
import math
from pathlib import Path

import pytest
import yaml

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


def assert_named_hydrodynamic_axes(model):
    """With torsion disabled, the named normal coefficient serves x and y."""
    assert all(line["IncludeTorsion"] == "No" for line in model["Lines"])
    for line_type in model["LineTypes"]:
        assert "Cd" not in line_type and "Ca" not in line_type
        drag = {"x": line_type["Cdn"], "y": line_type["Cdn"], "z": line_type["Cdz"]}
        added_mass = {"x": line_type["Can"], "y": line_type["Can"], "z": line_type["Caz"]}
        assert drag == {"x": 1.2, "y": 1.2, "z": 0.008}, line_type["Name"]
        assert added_mass == {"x": 1.0, "y": 1.0, "z": 0.0}, line_type["Name"]


@pytest.mark.parametrize("scope", scopes(), ids=lambda row: row["case_id"])
def test_serialized_coefficients_use_named_normal_and_axial_fields(scope):
    from scripts.riser_campaign.generate_inputs import _native_bytes, reviewed_geometries

    tranche = json.loads((CAMPAIGN / "manifest.json").read_bytes())["frozen_tranche"]
    geometry = reviewed_geometries(tranche, frozen.source_payloads())[scopes().index(scope)]
    load = frozen.expand_loads({"rows": [scope]})[0]
    serialized = _native_bytes(frozen.native_model(scope, load, geometry), "")
    assert_named_hydrodynamic_axes(yaml.safe_load(serialized))


@pytest.mark.parametrize("name", ["pipe", "buoyed-pipe"])
def test_old_positional_coefficients_fail_axis_regression(name):
    model = {"Lines": [{"IncludeTorsion": "No"}], "LineTypes": [
        {"Name": name, "Cd": [1.2, 0.008], "Ca": [1.0, 0]}]}
    with pytest.raises(AssertionError):
        assert_named_hydrodynamic_axes(model)


@pytest.mark.parametrize("field,value", [
    ("Cd", [1.2, 0.008, 0.008]), ("Ca", [1.0, 0.0, 0.0]),
    ("Cd", [1.3, 0.008]), ("Ca", [1.1, 0]), ("Cdn", 1.2),
])
def test_axis_audit_rejects_an_unexpected_reference(field, value):
    from scripts.riser_campaign.audit_mesh import _correct_coefficient_axes

    model = {"Lines": [{"IncludeTorsion": "No"}], "LineTypes": [
        {"Name": "pipe", "Cd": [1.2, 0.008], "Ca": [1.0, 0]}]}
    model["LineTypes"][0][field] = value
    with pytest.raises(ValueError, match="known positional defect"):
        _correct_coefficient_axes(model)


def test_axis_audit_preserves_every_other_field():
    from scripts.riser_campaign.audit_mesh import _correct_coefficient_axes

    before = {"Lines": [{"IncludeTorsion": "No", COUNT_TABLE: [["pipe", 10.0, 20]]}],
              "LineTypes": [{"Name": "pipe", "Cd": [1.2, 0.008], "Ca": [1.0, 0], "EA": 123.0}]}
    after = copy.deepcopy(before)
    _correct_coefficient_axes(after)
    assert_named_hydrodynamic_axes(after)
    assert before["Lines"] == after["Lines"]
    for key in ("Name", "EA"):
        assert before["LineTypes"][0][key] == after["LineTypes"][0][key]


def test_axis_audit_rejects_a_torsion_change():
    from scripts.riser_campaign.audit_mesh import _correct_coefficient_axes

    with pytest.raises(ValueError, match="torsion disabled"):
        _correct_coefficient_axes({"Lines": [{"IncludeTorsion": "Yes"}], "LineTypes": [
            {"Name": "pipe", "Cd": [1.2, 0.008], "Ca": [1.0, 0]}]})


@pytest.mark.parametrize("horizontal,vertical", [(3.0, 4.0), (3.0, -4.0), (5.0, 0.0)])
def test_inextensible_straight_line_dependency_is_rejected(horizontal, vertical):
    from digitalmodel.marine_ops.marine_analysis.catenary.solver import CatenaryInput, CatenarySolver

    # A weighted inextensible line at its chord length has no finite sagging
    # catenary solution. Retain the dependency's explicit singular-case guard.
    with pytest.raises(ValueError, match="must exceed straight distance for an inextensible catenary"):
        CatenarySolver().solve(CatenaryInput(
            length=5.0, horizontal_span=horizontal, vertical_span=vertical,
            weight_per_length=100.0, ea_stiffness=math.inf))


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
    correction = packet["historical_corrections"]["coefficient_axis_correction"]
    assert correction["counted_mesh"] == correction["all_other_model_fields"] == "IDENTICAL"
    assert correction["include_torsion"] == "No"
    assert packet["case_order"] == [scope["case_id"] for scope in scopes()]
    expected = frozen.expand_loads({"rows": scopes()})
    assert [item["id"] for item in packet["inputs"]] == [item["id"] for item in expected]
    assert len({item["path"] for item in packet["inputs"]}) == 144
    assert {entry["path"] for entry in packet["source_files"]} >= {
        "scripts/riser_campaign/frozen_scope.py", "scripts/riser_campaign/generate_inputs.py",
        "scripts/riser_campaign/audit_mesh.py", "tests/riser_campaign/test_mesh.py",
        "tests/riser_campaign/test_frozen_scope.py", "examples/riser-campaign/README.md", ".gitattributes",
    }
    # Shared dependencies remain evidence for the full native-handoff audit.
    # CI checks their behavior through the regenerated input digests below.
    owned_prefixes = ("examples/riser-campaign/", "scripts/riser_campaign/", "tests/riser_campaign/")
    for entry in packet["source_files"]:
        if not entry["path"].startswith(owned_prefixes):
            continue
        payload = (root / entry["path"]).read_bytes()
        assert hashlib.sha256(payload).hexdigest() == entry["sha256"], entry["path"]
        assert len(payload) == entry["bytes"], entry["path"]
    from scripts.riser_campaign.generate_inputs import _json_bytes
    runtime_payload = _json_bytes(packet["producing_runtime"]["versions"])
    assert packet["producing_runtime"]["record"]["sha256"] == hashlib.sha256(runtime_payload).hexdigest()
    assert packet["producing_runtime"]["record"]["bytes"] == len(runtime_payload)
    assert "examples/riser-campaign/reviewed-geometry.json" in {entry["path"] for entry in packet["source_files"]}
    audits = packet["mesh_audits"]
    assert [row["case_id"] for row in audits] == packet["case_order"]
    assert all(row["unrounded_adjacent_ratio_pass"] for row in audits)
    assert all(row["maximum_adjacent_ratio_bound"] <= 2.0 for row in audits)
    assert all(row["physical_model_matches"] == 12 for row in audits)


def test_review_packet_treats_shared_source_bytes_as_handoff_evidence(monkeypatch):
    root = CAMPAIGN.parents[1]
    packet = json.loads((root / "docs/verification/riser-mesh-review.json").read_bytes())
    shared = {root / entry["path"] for entry in packet["source_files"]
              if not entry["path"].startswith(("examples/riser-campaign/",
                                             "scripts/riser_campaign/", "tests/riser_campaign/"))}
    assert shared
    read_bytes = Path.read_bytes

    def refuse_shared_read(path):
        if path in shared:
            raise AssertionError("CI must check shared behavior through regeneration, not source bytes")
        return read_bytes(path)

    monkeypatch.setattr(Path, "read_bytes", refuse_shared_read)
    test_review_packet_binds_sources_and_exact_frozen_coverage()


def test_review_packet_still_rejects_changed_campaign_source_bytes(monkeypatch):
    source = CAMPAIGN.parents[1] / "scripts/riser_campaign/generate_inputs.py"
    read_bytes = Path.read_bytes

    def changed_source(path):
        payload = read_bytes(path)
        return payload + b"# changed campaign source\n" if path == source else payload

    monkeypatch.setattr(Path, "read_bytes", changed_source)
    with pytest.raises(AssertionError, match="scripts/riser_campaign/generate_inputs.py"):
        test_review_packet_binds_sources_and_exact_frozen_coverage()


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


def test_regeneration_matches_every_reviewed_input_digest(tmp_path):
    from scripts.riser_campaign.generate_inputs import generate

    packet = json.loads((CAMPAIGN.parents[1] / "docs/verification/riser-mesh-review.json").read_bytes())
    index = generate(CAMPAIGN / "manifest.json", tmp_path / "regenerated")
    actual = [entry for case in index["frozen_tranche"]["cases"] for entry in case["inputs"]]
    assert len(actual) == len(packet["inputs"]) == 144
    for generated, reviewed in zip(actual, packet["inputs"]):
        for key in ("id", "path", "sha256", "bytes"):
            assert generated[key] == reviewed[key], (generated["id"], key)
        assert_named_hydrodynamic_axes(yaml.safe_load((tmp_path / "regenerated" / generated["path"]).read_bytes()))


def test_frozen_handoff_does_not_resolve_platform_dependent_geometry(tmp_path, monkeypatch):
    from scripts.riser_campaign.generate_inputs import generate

    def unexpected_solve(*args, **kwargs):
        raise AssertionError("frozen handoff must use digest-bound reviewed geometry")

    monkeypatch.setattr(frozen, "prepare_geometry", unexpected_solve)
    index = generate(CAMPAIGN / "manifest.json", tmp_path / "retained")
    packet = json.loads((CAMPAIGN.parents[1] / "docs/verification/riser-mesh-review.json").read_bytes())
    assert [entry["sha256"] for case in index["frozen_tranche"]["cases"] for entry in case["inputs"]] == [
        entry["sha256"] for entry in packet["inputs"]]


def test_runtime_is_recorded_and_digest_bound(tmp_path):
    from scripts.riser_campaign.audit_mesh import _verified
    from scripts.riser_campaign.generate_inputs import generate, producing_runtime

    index = generate(CAMPAIGN / "manifest.json", tmp_path / "runtime")
    runtime = index["producing_runtime"]
    assert json.loads(_verified(tmp_path / "runtime", runtime)) == producing_runtime()
    assert set(producing_runtime()["libraries"]) >= {"numpy", "scipy", "PyYAML", "pydantic", "jsonschema"}
    (tmp_path / "runtime" / runtime["path"]).write_bytes(b"{}\n")
    with pytest.raises(ValueError, match="packet digest mismatch"):
        _verified(tmp_path / "runtime", runtime)


def test_native_serialization_uses_fixed_float_spelling_and_lf(monkeypatch):
    import os
    from scripts.riser_campaign.generate_inputs import _native_bytes

    monkeypatch.setattr(os, "linesep", "\r\n")
    from digitalmodel.solvers.orcaflex.yaml_utils import OrcaFlexDumper
    monkeypatch.setitem(OrcaFlexDumper.yaml_representers, float,
                        lambda dumper, value: dumper.represent_scalar("tag:yaml.org,2002:float", "1,0"))
    tree = {"values": [0.0, -0.0, 1.0, 1e-8, 1e20, 0.12345678901234566]}
    expected = (b"# numerical fixture\nvalues:\n- 0.0\n- -0.0\n- 1.0\n"
                b"- 1.0e-08\n- 1.0e+20\n- 0.12345678901234566\n")
    assert _native_bytes(tree, "# numerical fixture\n") == expected


def test_reviewed_geometry_refuses_changed_basis():
    from scripts.riser_campaign.generate_inputs import reviewed_geometries

    manifest = json.loads((CAMPAIGN / "manifest.json").read_bytes())
    changed = copy.deepcopy(manifest["frozen_tranche"])
    changed["rows"][0]["water_depth_m"] = "1501"
    with pytest.raises(ValueError, match="reviewed geometry basis"):
        reviewed_geometries(changed, frozen.source_payloads())


def test_reviewed_geometry_refuses_modified_snapshot(tmp_path, monkeypatch):
    from scripts.riser_campaign import generate_inputs as writer

    manifest = json.loads((CAMPAIGN / "manifest.json").read_bytes())
    payload = json.loads((CAMPAIGN / writer.REVIEWED_GEOMETRY).read_bytes())
    payload["geometries"][0]["installed_length_m"] += 1
    (tmp_path / writer.REVIEWED_GEOMETRY).write_text(json.dumps(payload), encoding="utf-8")
    monkeypatch.setattr(writer, "CAMPAIGN", tmp_path)
    with pytest.raises(ValueError, match="snapshot digest mismatch"):
        writer.reviewed_geometries(manifest["frozen_tranche"], frozen.source_payloads())


@pytest.mark.parametrize("reference_table", [COUNT_TABLE, "LineType, Length, TargetSegmentLength"])
@pytest.mark.parametrize("mutation", ["none", "mesh", "mass"])
def test_frozen_audit_accepts_unchanged_counted_reference(reference_table, mutation):
    from scripts.riser_campaign.audit_mesh import _compare_frozen_models

    original = {"Lines": [{reference_table: [["pipe", 10.0, 2]], "IncludeTorsion": "No"}],
                "LineTypes": [{"Name": "pipe", "MassPerUnitLength": 1}]}
    candidate = copy.deepcopy(original)
    candidate["Lines"][0][COUNT_TABLE] = candidate["Lines"][0].pop(reference_table)
    if mutation == "mesh":
        candidate["Lines"][0][COUNT_TABLE][0][2] = 3
    elif mutation == "mass":
        candidate["LineTypes"][0]["MassPerUnitLength"] = 2
    if mutation == "mass" or (mutation == "mesh" and reference_table == COUNT_TABLE):
        with pytest.raises(ValueError, match="changed"):
            _compare_frozen_models(original, candidate, False)
    else:
        before, after = _compare_frozen_models(original, candidate, False)
        assert before == [["pipe", 10.0, 2]]
        assert after == [["pipe", 10.0, 3 if mutation == "mesh" else 2]]


@pytest.mark.parametrize("mutation", ["none", "mesh", "axial_drag", "mass", "torsion"])
def test_frozen_audit_preserves_strict_coefficient_correction(mutation):
    from scripts.riser_campaign.audit_mesh import _compare_frozen_models

    original = {"Lines": [{COUNT_TABLE: [["pipe", 10.0, 2]], "IncludeTorsion": "No"}],
                "LineTypes": [{"Cd": [1.2, 0.008], "Ca": [1.0, 0], "MassPerUnitLength": 1}]}
    candidate = copy.deepcopy(original)
    line_type = candidate["LineTypes"][0]
    del line_type["Cd"], line_type["Ca"]
    line_type.update(Cdn=1.2, Cdz=0.008, Can=1.0, Caz=0)
    if mutation == "mesh":
        candidate["Lines"][0][COUNT_TABLE][0][2] = 3
    elif mutation == "axial_drag":
        line_type["Cdz"] = 0
    elif mutation == "mass":
        line_type["MassPerUnitLength"] = 2
    elif mutation == "torsion":
        candidate["Lines"][0]["IncludeTorsion"] = "Yes"
    if mutation == "none":
        assert _compare_frozen_models(original, candidate, True) == (
            [["pipe", 10.0, 2]], [["pipe", 10.0, 2]])
    else:
        with pytest.raises(ValueError, match="changed"):
            _compare_frozen_models(original, candidate, True)


HISTORY_REVISION = "d81ccf84ddc12a2a3cc45340537bec47a7807cd5"
PACKET_PATH = "docs/verification/riser-mesh-review.json"
REISSUE_REVISION = "bb123d57e5a51bfc15020eeee3674cd5103043e4"


def previous_packet(tmp_path):
    """Retrieve the exact retained packet without changing the checkout."""
    path = tmp_path / "previous-packet.json"
    payload = (Path(__file__).parent / "fixtures/prior-packet.json").read_bytes()
    path.write_bytes(payload)
    return path


def test_audit_history_preserves_records_and_binds_committed_bytes(tmp_path):
    from scripts.riser_campaign.audit_mesh import _history

    path = previous_packet(tmp_path)
    old = json.loads(path.read_bytes())
    history, source = _history(path, HISTORY_REVISION)
    assert history == old["historical_corrections"]
    assert source == [{"path": PACKET_PATH, "revision": HISTORY_REVISION,
                       "sha256": hashlib.sha256(path.read_bytes()).hexdigest(),
                       "bytes": len(path.read_bytes()), "records": history}]


@pytest.mark.parametrize("mutation", ["record", "whitespace", "revision"])
def test_audit_history_rejects_uncommitted_or_wrong_revision(tmp_path, mutation):
    from scripts.riser_campaign.audit_mesh import _history

    path = previous_packet(tmp_path)
    revision = HISTORY_REVISION
    if mutation == "revision":
        revision = "0b83ecb072ca59f410b14872d426f9e5699ed41e"
    elif mutation == "whitespace":
        path.write_bytes(path.read_bytes() + b"\n")
    else:
        old = json.loads(path.read_bytes())
        old["historical_corrections"]["coefficient_axis_correction"]["counted_mesh"] = "changed"
        path.write_bytes(json.dumps(old).encode("utf-8"))
    with pytest.raises(ValueError, match="history packet digest mismatch"):
        _history(path, revision)


def test_audit_history_imports_original_top_level_records(tmp_path):
    from scripts.riser_campaign.audit_mesh import _history

    revision = "0b83ecb072ca59f410b14872d426f9e5699ed41e"
    path = tmp_path / "original-packet.json"
    path.write_bytes((Path(__file__).parent / "fixtures/original-packet.json").read_bytes())
    old = json.loads(path.read_bytes())
    history, source = _history(path, revision)
    assert history == {"recorded_at_revision": revision,
                       "reference_revision": old["reference_revision"],
                       **{key: old[key] for key in (
                           "coefficient_axis_correction", "original_coefficient_axis_correction")}}
    assert source[-1]["revision"] == revision


@pytest.mark.parametrize("options", [
    {"history_from": Path("previous.json")}, {"history_revision": HISTORY_REVISION},
])
def test_audit_rejects_incomplete_or_ambiguous_history_options(tmp_path, options):
    from scripts.riser_campaign.audit_mesh import audit

    with pytest.raises(ValueError, match="history"):
        audit(tmp_path / "reference", tmp_path / "candidate", HISTORY_REVISION, **options)


@pytest.fixture(scope="module")
def audit_outputs(tmp_path_factory):
    """Retain reviewed bytes independently of branch-only Git history."""
    from scripts.riser_campaign.audit_mesh import reference_fixture
    from scripts.riser_campaign.generate_inputs import generate

    root = tmp_path_factory.mktemp("audit-outputs")
    reference, candidate = root / "reference", root / "candidate"
    for relative, payload in reference_fixture().items():
        target = reference / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(payload)
    generate(CAMPAIGN / "manifest.json", candidate)
    return reference, candidate


def test_audit_reproduces_committed_packet_with_verified_history(tmp_path, audit_outputs):
    """Reproduce the packet from digest-pinned reference inputs and history."""
    from scripts.riser_campaign.audit_mesh import audit

    committed = json.loads((CAMPAIGN.parents[1] / PACKET_PATH).read_bytes())
    reference, candidate = audit_outputs
    packet = audit(reference, candidate, HISTORY_REVISION,
                   history_from=Path(__file__).parent / "fixtures/reissue-packet.json",
                   history_revision=REISSUE_REVISION)
    # Runtime records also affect the containing index digests. All remaining
    # fields, including source bindings, history and input digests, must match.
    for value in (packet, committed):
        for key in ("audit_runtime", "producing_runtime", "reference_index", "candidate_index"):
            value.pop(key)
    assert packet == committed


@pytest.mark.parametrize("mutation", ["order", "numeric_spelling", "value", "list_order", "type"])
def test_serialization_audit_compares_original_models_and_records_digests(tmp_path, mutation):
    from scripts.riser_campaign.audit_mesh import _compare_original_inputs

    reference, candidate = tmp_path / "reference", tmp_path / "candidate"
    reference.mkdir()
    candidate.mkdir()
    model = {"General": {"UnitsSystem": "SI", "Duration": 1}, "Loads": [1, 2]}
    changed = copy.deepcopy(model)
    if mutation == "value":
        changed["General"]["Duration"] = 20
    elif mutation == "numeric_spelling":
        changed["General"]["Duration"] = 1.0
    elif mutation == "list_order":
        changed["Loads"].reverse()
    elif mutation == "type":
        changed["General"]["Duration"] = True
    for root, value, order in ((reference, model, False), (candidate, changed, True)):
        (root / "input.yml").write_text(yaml.safe_dump(value, sort_keys=order), encoding="utf-8")
        (root / "geometry.json").write_text("{}\n", encoding="utf-8")

    def entry(root, path):
        payload = (root / path).read_bytes()
        return {"path": path, "sha256": hashlib.sha256(payload).hexdigest(), "bytes": len(payload)}

    before = [{"case_id": "synthetic", "geometry": entry(reference, "geometry.json"),
               "inputs": [entry(reference, "input.yml")]}]
    after = [{"case_id": "synthetic", "geometry": entry(candidate, "geometry.json"),
              "inputs": [entry(candidate, "input.yml")]}]
    changes = []
    if mutation in ("order", "numeric_spelling"):
        assert _compare_original_inputs(reference, candidate, before, after,
                                        serialization_change=changes) == 1
        assert changes == [{"path": "input.yml", "old_sha256": before[0]["inputs"][0]["sha256"],
                            "new_sha256": after[0]["inputs"][0]["sha256"],
                            "classification": ("KEY_ORDER_ONLY" if mutation == "order"
                                               else "SERIALIZATION_ONLY"), "changed_field_paths": []}]
        with pytest.raises(ValueError, match="index changed"):
            _compare_original_inputs(reference, candidate, before, after)
    else:
        field = "$.General.Duration" if mutation in ("value", "type") else "$.Loads[0]"
        with pytest.raises(ValueError, match="value change") as error:
            _compare_original_inputs(reference, candidate, before, after, serialization_change=changes)
        assert field in str(error.value)
        assert changes[0]["classification"] == "VALUE_CHANGE"
        assert field in changes[0]["changed_field_paths"]


@pytest.mark.parametrize("filename, revision", [
    ("prior-packet.json", HISTORY_REVISION), ("reissue-packet.json", REISSUE_REVISION),
    ("original-packet.json", "0b83ecb072ca59f410b14872d426f9e5699ed41e"),
])
def test_history_works_without_retained_git_objects(tmp_path, monkeypatch, filename, revision):
    import subprocess
    from scripts.riser_campaign import audit_mesh

    def missing_git(*args, **kwargs):
        return subprocess.CompletedProcess(args[0], 128, stdout=b"", stderr=b"missing object")

    monkeypatch.setattr(audit_mesh.subprocess, "run", missing_git)
    path = Path(__file__).parent / "fixtures" / filename
    history, sources = audit_mesh._history(path, revision)
    assert history["coefficient_axis_correction"]["counted_mesh"] == "IDENTICAL"
    assert sources[-1]["revision"] == revision
    if revision == REISSUE_REVISION:
        previous = json.loads(path.read_bytes())["history_source"]
        assert {key: sources[0][key] for key in previous} == previous
        assert len(sources) == 2
    with pytest.raises(ValueError, match="unavailable.*fixture"):
        audit_mesh._history(path, "0" * 40)


def test_history_reissues_retain_every_predecessor_and_new_corrections(tmp_path, monkeypatch):
    import subprocess
    from scripts.riser_campaign import audit_mesh

    prior = previous_packet(tmp_path)
    history, sources = audit_mesh._history(prior, HISTORY_REVISION)
    packet = {"reference_revision": HISTORY_REVISION, "historical_corrections": history,
              "history": sources, "coefficient_axis_correction": {"new": "record"}}
    payload = json.dumps(packet).encode("utf-8")
    prior.write_bytes(payload)
    revision = "1" * 40

    def git_packet(args, **kwargs):
        output = (revision + "\n") if args[1] == "rev-parse" else payload
        return subprocess.CompletedProcess(args, 0, stdout=output, stderr="")

    monkeypatch.setattr(audit_mesh.subprocess, "run", git_packet)
    retained, appended = audit_mesh._history(prior, revision)
    assert retained == history
    assert appended[:-1] == sources
    assert appended[-1]["records"]["coefficient_axis_correction"] == {"new": "record"}
    assert appended[-1]["sha256"] == hashlib.sha256(payload).hexdigest()


@pytest.mark.parametrize("tranche", ["original", "frozen"])
@pytest.mark.parametrize("mutation", ["order", "value"])
def test_serialization_mode_audits_both_tranches(tmp_path, audit_outputs, tranche, mutation):
    import shutil
    from scripts.riser_campaign.audit_mesh import audit

    reference, current = audit_outputs
    candidate = tmp_path / "candidate"
    shutil.copytree(current, candidate)
    index = json.loads((candidate / "index.json").read_bytes())
    cases = index["cases"] if tranche == "original" else index["frozen_tranche"]["cases"]
    entry = cases[0]["inputs"][0]
    target = candidate / entry["path"]
    model = yaml.safe_load(target.read_bytes())
    if mutation == "value":
        model["General"]["UnitsSystem"] = "US"
    if mutation == "order":
        lines = target.read_bytes().splitlines(keepends=True)
        starts = [i for i, line in enumerate(lines)
                  if line and line[:1] not in b" #\r\n-\t" and b":" in line]
        blocks = [b"".join(lines[a:b]) for a, b in zip(starts, [*starts[1:], len(lines)])]
        payload = b"".join(lines[:starts[0]]) + b"".join(reversed(blocks))
    else:
        payload = yaml.safe_dump(model, sort_keys=True).encode("utf-8")
    target.write_bytes(payload)
    entry.update(sha256=hashlib.sha256(payload).hexdigest(), bytes=len(payload))
    (candidate / "index.json").write_text(json.dumps(index), encoding="utf-8")
    options = {"history_from": previous_packet(tmp_path), "history_revision": HISTORY_REVISION,
               "serialization_change": True}
    if mutation == "value":
        with pytest.raises(ValueError, match=r"value change.*General.UnitsSystem"):
            audit(reference, candidate, HISTORY_REVISION, **options)
    else:
        packet = audit(reference, candidate, HISTORY_REVISION, **options)
        records = packet["serialization_change"]["inputs"]
        assert len(records) == 432
        changed = next(row for row in records if row["path"] == entry["path"])
        assert changed["classification"] == "KEY_ORDER_ONLY"
        assert changed["old_sha256"] != changed["new_sha256"]
        assert changed["changed_field_paths"] == []
        assert packet["history"][-1]["revision"] == HISTORY_REVISION


def test_audit_reproduction_without_branch_objects(tmp_path, audit_outputs, monkeypatch):
    import subprocess
    from scripts.riser_campaign import audit_mesh

    def missing_git(args, **kwargs):
        return subprocess.CompletedProcess(args, 128, stdout=b"", stderr=b"missing object")

    monkeypatch.setattr(audit_mesh.subprocess, "run", missing_git)
    reference, candidate = audit_outputs
    packet = audit_mesh.audit(reference, candidate, HISTORY_REVISION,
                              history_from=Path(__file__).parent / "fixtures/reissue-packet.json",
                              history_revision=REISSUE_REVISION)
    assert len(packet["inputs"]) == 144
    assert packet["preservation"]["original_input_byte_matches"] == 288


def test_history_fixture_tampering_is_rejected(tmp_path, monkeypatch):
    import subprocess
    from scripts.riser_campaign import audit_mesh

    fixture = tmp_path / "fixtures"
    fixture.mkdir()
    (fixture / "prior-packet.json").write_bytes(previous_packet(tmp_path).read_bytes() + b"\n")
    monkeypatch.setattr(audit_mesh, "FIXTURES", fixture)
    monkeypatch.setattr(audit_mesh.subprocess, "run", lambda args, **kwargs:
                        subprocess.CompletedProcess(args, 128, stdout=b"", stderr=b"missing object"))
    with pytest.raises(ValueError, match="fixture digest mismatch"):
        audit_mesh._history(previous_packet(tmp_path), HISTORY_REVISION)


@pytest.mark.parametrize("serialization_change", [False, True])
def test_history_can_accompany_declared_coefficient_corrections(tmp_path, audit_outputs,
                                                               serialization_change):
    import shutil
    from scripts.riser_campaign.audit_mesh import audit

    reviewed, candidate = audit_outputs
    reference = tmp_path / "reference"
    shutil.copytree(reviewed, reference)
    index = json.loads((reference / "index.json").read_bytes())
    for cases in (index["cases"], index["frozen_tranche"]["cases"]):
        for case in cases:
            for entry in case["inputs"]:
                target = reference / entry["path"]
                model = yaml.safe_load(target.read_bytes())
                for line_type in model["LineTypes"]:
                    line_type["Cd"] = [line_type.pop("Cdn"), line_type.pop("Cdz")]
                    line_type["Ca"] = [line_type.pop("Can"), line_type.pop("Caz")]
                payload = yaml.safe_dump(model, sort_keys=False).encode("utf-8")
                target.write_bytes(payload)
                entry.update(sha256=hashlib.sha256(payload).hexdigest(), bytes=len(payload))
    (reference / "index.json").write_text(json.dumps(index), encoding="utf-8")
    packet = audit(reference, candidate, HISTORY_REVISION, coefficient_axes=True,
                   original_coefficient_axes=True, history_from=previous_packet(tmp_path),
                   history_revision=HISTORY_REVISION, serialization_change=serialization_change)
    assert packet["history"][-1]["revision"] == HISTORY_REVISION
    assert packet["historical_corrections"]["coefficient_axis_correction"]["counted_mesh"] == "IDENTICAL"
    assert packet["coefficient_axis_correction"]["all_other_model_fields"] == "IDENTICAL"
    assert packet["original_coefficient_axis_correction"]["inputs"] == 288
    if serialization_change:
        assert len(packet["serialization_change"]["inputs"]) == 432
        assert packet["serialization_change"]["declared_corrections"] == {
            "coefficient_axes": True, "original_coefficient_axes": True}


def test_reference_fixture_tampering_is_rejected(tmp_path, monkeypatch):
    from scripts.riser_campaign import audit_mesh

    (tmp_path / "reviewed-reference.tar.xz").write_bytes(b"altered archive")
    monkeypatch.setattr(audit_mesh, "FIXTURES", tmp_path)
    with pytest.raises(ValueError, match="fixture digest mismatch"):
        audit_mesh.reference_fixture()


@pytest.mark.parametrize("mutation", ["sha256", "bytes", "omission", "duplicate", "empty"])
def test_reference_without_raw_snapshots_requires_exact_retained_source_pins(tmp_path, mutation):
    from scripts.riser_campaign.audit_mesh import _verify_reference_sources, reference_fixture

    retained = reference_fixture()
    assert not any(name.startswith("source/") for name in retained)
    sources = json.loads(retained["index.json"])["frozen_tranche"]["code_sources"]
    for relative, payload in retained.items():
        target = tmp_path / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_bytes(payload)
    _verify_reference_sources(tmp_path, sources, HISTORY_REVISION)
    changed = copy.deepcopy(sources)
    if mutation == "omission":
        changed.pop()
    elif mutation == "duplicate":
        changed.append(copy.deepcopy(changed[0]))
    elif mutation == "empty":
        changed = []
    else:
        changed[0][mutation] = "0" * 64 if mutation == "sha256" else changed[0][mutation] + 1
    with pytest.raises(ValueError, match="retained source pin"):
        _verify_reference_sources(tmp_path, changed, HISTORY_REVISION)
