# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Offline library observations; no native loadability or response claim."""

from copy import deepcopy
from hashlib import sha256
from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex.format_converter.single_to_spec import (
    SingleToSpecConverter,
)
from digitalmodel.solvers.orcaflex.format_converter.spec_to_single import (
    SpecToSingleConverter,
)
from digitalmodel.solvers.orcaflex.semantic_diff.taxonomy import (
    MISSING,
    DiffCategory,
    is_semantically_equivalent,
)
from digitalmodel.solvers.orcaflex.yaml_utils import orcaflex_load

CASE = "a01_catenary_riser"
LIBRARY = (
    Path(__file__).resolve().parents[4]
    / "docs/domains/orcaflex/library/tier2_fast"
    / CASE
)
LINE_SECTIONS = "LineType, Length, TargetSegmentLength"


def _required_file(path: Path) -> Path:
    assert path.is_file(), f"{CASE}: required library file missing: {path.name}"
    return path


@pytest.fixture
def library_inputs():
    """Require existing inputs and check their bytes after every consumer."""
    paths = (
        _required_file(LIBRARY / "monolithic" / "A01 Catenary riser.yml"),
        _required_file(LIBRARY / "spec.yml"),
    )
    before = {path: sha256(path.read_bytes()).hexdigest() for path in paths}
    yield paths
    for path, digest in before.items():
        assert sha256(path.read_bytes()).hexdigest() == digest, (
            f"{CASE}: library input changed: {path.name}"
        )


@pytest.fixture
def original(library_inputs):
    data, _ = orcaflex_load(library_inputs[0])
    return data


@pytest.fixture
def generated(library_inputs, tmp_path):
    target = tmp_path / "generated.yml"
    report = SpecToSingleConverter().convert(library_inputs[1], target)
    assert report.success, f"{CASE}/forward: {report.warnings}"
    data, _ = orcaflex_load(_required_file(target))
    return data


def test_missing_library_input_fails_instead_of_skipping(tmp_path, library_inputs):
    """A required input cannot disappear behind a data-dependent skip."""
    source = tmp_path / "spec.yml"
    source.write_bytes(library_inputs[1].read_bytes())
    assert _required_file(source) == source
    source.rename(tmp_path / "omitted.yml")
    with pytest.raises(AssertionError, match="required library file"):
        _required_file(source)


def test_library_input_observations(original, library_inputs):
    """Pin recorded SI inputs, independently of converter calculations."""
    spec, _ = orcaflex_load(library_inputs[1])
    assert set(original) == {
        "General", "VariableData", "Environment", "VesselTypes",
        "LineTypes", "Vessels", "Lines", "Groups",
    }, CASE
    assert original["General"]["UnitsSystem"] == "SI", CASE
    env = original["Environment"]
    assert env["SeabedOriginDepth"] == 100, f"{CASE}/Environment/depth (m)"
    assert "WaterDepth" not in env, f"{CASE}/Environment/depth alias"
    assert env["WaveTrains"][0]["WaveHeight"] == 6, f"{CASE}/wave height (m)"
    assert env["WaveTrains"][0]["WavePeriod"] == 7, f"{CASE}/wave period (s)"
    assert env["RefCurrentSpeed"] == 0.7, f"{CASE}/current speed (m/s)"
    assert env["RefCurrentDirection"] == 180, f"{CASE}/current direction (deg)"
    assert original["General"]["StageDuration"] == [7, 35], CASE
    assert original["General"]["ImplicitConstantTimeStep"] == 0.1, CASE
    lengths = [row[1] for row in original["Lines"][0][LINE_SECTIONS]]
    assert lengths == [90, 20, 20], f"{CASE}/Lines/section lengths (m)"
    assert sum(lengths) == 130, f"{CASE}/Lines/total length (m)"
    assert spec["environment"]["water"]["depth"] == 100, CASE
    assert spec["environment"]["waves"]["height"] == 6, CASE
    assert spec["environment"]["waves"]["period"] == 7, CASE
    assert [s["length"] for s in spec["riser"]["lines"][0]["sections"]] == lengths


def test_forward_generation_preserves_direct_invariants(generated):
    """Read generated YAML back; compare against independent SI observations."""
    assert generated["General"]["UnitsSystem"] == "SI", f"{CASE}/General/units"
    env = generated["Environment"]
    assert env["SeabedOriginDepth"] == 100, f"{CASE}/Environment/depth (m)"
    assert env["WaveTrains"][0]["WaveHeight"] == 6, f"{CASE}/wave height (m)"
    assert env["WaveTrains"][0]["WavePeriod"] == 7, f"{CASE}/wave period (s)"
    lengths = [row[1] for row in generated["Lines"][0][LINE_SECTIONS]]
    assert lengths == [90, 20, 20], f"{CASE}/Lines/section lengths (m)"
    assert sum(lengths) == 130, f"{CASE}/Lines/total length (m)"


def test_reverse_extraction_reports_exact_boundary(library_inputs, tmp_path):
    """Pin best-effort extraction, including its missing depth field."""
    target = tmp_path / "extracted.yml"
    report = SingleToSpecConverter().convert(library_inputs[0], target)
    assert report.success, f"{CASE}/reverse: {report.warnings}"
    assert report.is_best_effort, CASE
    assert report.confidence == 0.88, f"{CASE}/reverse/confidence"
    assert set(report.unmapped_sections) == {
        "VesselTypes", "LineTypes", "Vessels", "Lines", "Groups",
    }, CASE
    assert set(report.expected_gaps) == {"VesselTypes", "Vessels", "Groups"}, CASE
    assert set(report.actionable_gaps) == {"LineTypes", "Lines"}, CASE
    assert set(report.unmapped_sections) == (
        set(report.expected_gaps) | set(report.actionable_gaps)
    ), CASE
    spec, _ = orcaflex_load(_required_file(target))
    assert spec["environment"]["waves"]["height"] == 6, CASE
    assert spec["environment"]["waves"]["period"] == 7, CASE
    assert spec["simulation"]["stages"] == [7, 35], CASE
    assert spec["simulation"]["time_step"] == 0.1, CASE
    assert "depth" not in spec["environment"]["water"], (
        f"{CASE}/reverse: missing WaterDepth is a known gap, not a default"
    )


def _diff_details(diffs):
    return "\n".join(
        f"{CASE}/{d.section}/{d.key}: {d.category.value}; "
        f"before={d.strict_value!r}; after={d.generated_value!r}"
        for d in diffs
    )


@pytest.mark.parametrize("section", ["General", "Environment", "Groups"])
def test_section_comparisons_retain_classified_evidence(original, generated, section):
    """Compare whole sections without declaring an overall policy match."""
    assert section in generated, f"{CASE}/{section}: required section missing"
    before, after = original[section], generated[section]
    _, diffs = is_semantically_equivalent({section: before}, {section: after})
    details = _diff_details(diffs)
    assert is_semantically_equivalent({section: before}, {section: before}) == (
        True, []
    ), details
    for diff in diffs:
        assert diff.section == section, details
        assert diff.key in before or diff.key in after, details
        assert diff.strict_value == before.get(diff.key, MISSING), details
        assert diff.generated_value == after.get(diff.key, MISSING), details
        assert isinstance(diff.category, DiffCategory), details
    by_key = {diff.key: diff for diff in diffs}
    for key in before.keys() & after.keys():
        if (
            type(before[key]) in (str, int, float, bool, type(None))
            and type(before[key]) is type(after[key])
            and before[key] == after[key]
        ):
            assert key not in by_key, details
    if section == "General":
        assert "DefaultViewSize" in before and "DefaultViewSize" not in after, details
        assert "DefaultViewSize" in by_key, details
        omitted = by_key["DefaultViewSize"]
        assert omitted.category == DiffCategory.INTENTIONAL_OMISSION, details
        assert omitted.generated_value is MISSING and omitted.omission_reason, details
        for key, diff in by_key.items():
            if key.startswith(("DefaultView", "DefaultShaded")) and key != "DefaultViewSize":
                assert diff.category != DiffCategory.INTENTIONAL_OMISSION, details
        if "DefaultViewAzimuth" not in after:
            assert "DefaultViewAzimuth" in by_key, details
            assert by_key["DefaultViewAzimuth"].category == (
                DiffCategory.LOADABILITY_SIGNIFICANT
            ), details
    if section == "Environment":
        for key in ("SeabedOriginDepth", "MultipleCurrentDataCanBeDefined"):
            assert key in by_key, details
            assert by_key[key].category == DiffCategory.NORMALIZATION_ONLY, details
    if section == "Groups" and before["Structure"] != after.get("Structure", MISSING):
        assert "Structure" in by_key, details
        assert by_key["Structure"].category == (
            DiffCategory.REFERENCE_RESOLUTION_SIGNIFICANT
        ), details


def test_changed_depth_is_significant(generated):
    """A one-metre change cannot be classified as a harmless normalization."""
    changed = deepcopy(generated)
    assert changed["Environment"]["SeabedOriginDepth"] == 100, CASE
    changed["Environment"]["SeabedOriginDepth"] = 101
    equivalent, diffs = is_semantically_equivalent(generated, changed)
    assert not equivalent, _diff_details(diffs)
    assert [(d.section, d.key, d.strict_value, d.generated_value, d.category)
            for d in diffs] == [
        ("Environment", "SeabedOriginDepth", 100, 101,
         DiffCategory.PHYSICS_SIGNIFICANT),
    ], _diff_details(diffs)


@pytest.mark.parametrize(
    "section, category",
    [("Environment", DiffCategory.LOADABILITY_SIGNIFICANT),
     ("Groups", DiffCategory.REFERENCE_RESOLUTION_SIGNIFICANT)],
)
def test_whole_section_loss_blocks_policy_match(generated, section, category):
    """Section removal remains significant even for organization data."""
    changed = deepcopy(generated)
    removed = changed.pop(section)
    equivalent, diffs = is_semantically_equivalent(generated, changed)
    assert not equivalent, _diff_details(diffs)
    assert len(diffs) == 1, _diff_details(diffs)
    diff = diffs[0]
    assert (diff.section, diff.key, diff.category) == (section, "", category), (
        _diff_details(diffs)
    )
    assert diff.strict_value == removed, _diff_details(diffs)
    assert diff.generated_value is MISSING, _diff_details(diffs)


def test_unknown_boolean_to_number_is_significant(generated):
    """Unknown flags retain type-sensitive comparison beside real sections."""
    before = deepcopy(generated)
    before["Unknown"] = {"Flag": True}
    after = deepcopy(before)
    after["Unknown"]["Flag"] = 1
    equivalent, diffs = is_semantically_equivalent(before, after)
    assert not equivalent, _diff_details(diffs)
    assert [(d.section, d.key, d.category) for d in diffs] == [
        ("Unknown", "Flag", DiffCategory.PHYSICS_SIGNIFICANT),
    ], _diff_details(diffs)
    assert diffs[0].strict_value is True, _diff_details(diffs)
    assert type(diffs[0].generated_value) is int, _diff_details(diffs)
