"""Tests for the BaseFile variation-model writer.

Covers TDD rows 9-13 of
``workspace-hub/docs/plans/2026-09-11-issue-3843-generator-consolidation.md``.

OrcaFlex composes a variation model two ways, with different semantics.
``BaseFile:`` clears all existing model data and then loads the named file, and
accepts either a binary ``.dat`` or a text ``.yml``.  ``IncludeFile:`` merges
incrementally from whatever state the model is in, and is text only.  The writer
under test emits the first form.

Row 13 is the load-bearing assertion.  Its comparator is
``template_generator.TemplateGenerator._generate_reference``, an independent
in-tree implementation of the same vendor-documented format, captured as goldens
under ``goldens/basefile/``.  Read ``goldens/basefile/PROVENANCE.md`` before
relying on that assertion: the two implementations overlap on the ``BaseFile``
key only, so the goldens constrain that key and nothing else.  The override
sections have comparator class ``none``.

DEFERRED — plan row 14.  Row 14 asserts the collision check introduced by
workspace-hub#3838 against this writer's output.  That check lives on an
unmerged PR branch and is absent from ``main``, so it is not implemented here.
Implement row 14 once digitalmodel#2099 merges.
"""
from __future__ import annotations

from pathlib import Path

import pytest
import yaml

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    _OBJECT_SECTIONS,
)
from digitalmodel.solvers.orcaflex.modular_generator.writers.basefile import (
    build_variation_document,
    object_section_order,
    write_variation_model,
)

# tests/solvers/orcaflex/modular_generator/ -> repository root
REPO_ROOT = Path(__file__).resolve().parents[4]
GOLDENS = Path(__file__).resolve().parent / "goldens" / "basefile"


def _load(path: Path):
    return yaml.safe_load(path.read_text(encoding="utf-8"))


def _emitted_key_order(text: str) -> list[str]:
    """Top-level mapping keys, in the order they appear in the emitted text."""
    return [
        line.split(":", 1)[0]
        for line in text.splitlines()
        if line
        and not line[0].isspace()
        and ":" in line
        # A block-sequence item of a top-level key also starts at column 0.
        and not line.startswith(("#", "-", "%"))
    ]


# --------------------------------------------------------------------------
# Row 9 — writer emits BaseFile plus overrides, path relative to the output
# --------------------------------------------------------------------------

def test_row09_emits_basefile_key_and_override_sections(tmp_path):
    base = tmp_path / "base" / "model.yml"
    base.parent.mkdir(parents=True)
    base.write_text("General:\n  UnitsSystem: SI\n", encoding="utf-8")
    out = tmp_path / "cases" / "case_a.yml"

    overrides = {"LineTypes": [{"Name": "Chain_84mm_R4", "OD": 0.084}]}
    doc = build_variation_document(base, overrides, out)

    assert "BaseFile" in doc
    assert doc["LineTypes"] == [{"Name": "Chain_84mm_R4", "OD": 0.084}]
    # A variation model composed with BaseFile carries no IncludeFile key.
    assert "IncludeFile" not in doc


def test_row09_basefile_path_is_relative_to_the_output_file(tmp_path):
    base = tmp_path / "base" / "model.yml"
    base.parent.mkdir(parents=True)
    base.write_text("General: {}\n", encoding="utf-8")
    out = tmp_path / "cases" / "case_a.yml"

    doc = build_variation_document(base, {}, out)

    assert doc["BaseFile"] == "../base/model.yml"
    assert not Path(doc["BaseFile"]).is_absolute()
    # POSIX separators regardless of host platform, matching the comparator.
    assert "\\" not in doc["BaseFile"]


def test_row09_base_below_the_output_directory_needs_no_traversal(tmp_path):
    base = tmp_path / "base" / "model.yml"
    base.parent.mkdir(parents=True)
    base.write_text("General: {}\n", encoding="utf-8")
    out = tmp_path / "case_a.yml"

    assert build_variation_document(base, {}, out)["BaseFile"] == "base/model.yml"


def test_row09_write_variation_model_round_trips(tmp_path):
    base = tmp_path / "base" / "model.dat"
    base.parent.mkdir(parents=True)
    base.write_text("stub\n", encoding="utf-8")
    out = tmp_path / "cases" / "case_a.yml"

    written = write_variation_model(base, {"Vessels": [{"Name": "FPSO"}]}, out)

    assert written == out
    assert out.exists()
    assert _load(out) == build_variation_document(base, {"Vessels": [{"Name": "FPSO"}]}, out)


# --------------------------------------------------------------------------
# Row 10 — a .dat base is emitted unchanged, because BaseFile accepts binary
# --------------------------------------------------------------------------

@pytest.mark.parametrize("suffix", [".dat", ".yml", ".yaml", ".DAT"])
def test_row10_base_extension_is_emitted_unchanged(tmp_path, suffix):
    base = tmp_path / "base" / f"model{suffix}"
    base.parent.mkdir(parents=True, exist_ok=True)
    base.write_text("stub\n", encoding="utf-8")
    out = tmp_path / "cases" / "case_a.yml"

    doc = build_variation_document(base, {}, out)

    assert doc["BaseFile"] == f"../base/model{suffix}"
    assert doc["BaseFile"].endswith(suffix)


def test_row10_dat_base_is_not_rejected(tmp_path):
    """A binary base is a valid BaseFile target and must not raise."""
    base = tmp_path / "base" / "model.dat"
    base.parent.mkdir(parents=True)
    base.write_bytes(b"\x00\x01\x02not-utf8\xff")
    out = tmp_path / "case_a.yml"

    doc = build_variation_document(base, {"Lines": [{"Name": "L1"}]}, out)

    assert doc["BaseFile"] == "base/model.dat"


# --------------------------------------------------------------------------
# Row 11 — reference-before-use ordering
# --------------------------------------------------------------------------

def test_row11_line_types_precede_lines_regardless_of_input_order(tmp_path):
    base = tmp_path / "model.yml"
    base.write_text("General: {}\n", encoding="utf-8")
    out = tmp_path / "case_a.yml"

    # Deliberately inverted: Lines first, the referenced LineTypes last.
    overrides = {
        "Lines": [{"Name": "Mooring_L1", "LineType": "Chain_84mm_R4"}],
        "Vessels": [{"Name": "FPSO"}],
        "LineTypes": [{"Name": "Chain_84mm_R4", "OD": 0.084}],
    }
    text = yaml.dump(build_variation_document(base, overrides, out), sort_keys=False)
    order = _emitted_key_order(text)

    assert order[0] == "BaseFile"
    assert order.index("LineTypes") < order.index("Lines")
    assert order.index("VesselTypes" if "VesselTypes" in order else "Vessels") < order.index("Lines")


def test_row11_buoys_precede_lines(tmp_path):
    """A line connected to a 6D buoy must find that buoy already declared.

    Orcina's documented rule is that a referenced object appears before any
    reference to it, and that vessels and 3D/6D buoys appear before lines,
    links, winches and shapes.  The generator already honours this: the
    committed
    ``docs/domains/orcaflex/templates/mooring_systems/calm_buoy/master.yml``
    loads ``08_buoys.yml`` before ``07_lines.yml``, carrying the comment
    "Buoys must be before Lines since lines connect to buoys", and
    ``BuilderRegistry`` registers BuoysBuilder at order 80 against LinesBuilder
    at 90.

    ``post_validator._OBJECT_SECTIONS`` puts ``6DBuoys`` AFTER ``Lines``.  It is
    a membership set, not a dependency order, and ordering by it emits a
    forward reference.  This test fails against that order by construction.
    """
    base = tmp_path / "model.yml"
    base.write_text("General: {}\n", encoding="utf-8")
    out = tmp_path / "case_a.yml"

    overrides = {
        "Lines": [{"Name": "Mooring_L1", "EndBConnection": "Buoy_A"}],
        "6DBuoys": [{"Name": "Buoy_A"}],
        "3DBuoys": [{"Name": "Buoy_B"}],
        "Shapes": [{"Name": "Seabed_Ramp"}],
        "Vessels": [{"Name": "FPSO"}],
        "Links": [{"Name": "Link_1"}],
        "Winches": [{"Name": "Winch_1"}],
    }
    text = yaml.dump(build_variation_document(base, overrides, out), sort_keys=False)
    order = _emitted_key_order(text)

    # Referenced before referencing.
    assert order.index("6DBuoys") < order.index("Lines")
    assert order.index("3DBuoys") < order.index("Lines")
    assert order.index("Vessels") < order.index("Lines")
    assert order.index("Shapes") < order.index("Lines")
    # Links and winches reference lines, so they follow.
    assert order.index("Lines") < order.index("Links")
    assert order.index("Lines") < order.index("Winches")


def test_row11_emitted_order_follows_the_builder_registry(tmp_path):
    base = tmp_path / "model.yml"
    base.write_text("General: {}\n", encoding="utf-8")
    out = tmp_path / "case_a.yml"

    # Every object section, supplied in exactly reversed canonical order.
    canonical = object_section_order()
    overrides = {name: [{"Name": f"{name}_1"}] for name in reversed(canonical)}
    text = yaml.dump(build_variation_document(base, overrides, out), sort_keys=False)
    order = _emitted_key_order(text)

    assert order[0] == "BaseFile"
    assert order[1:] == list(canonical)


def test_object_section_order_covers_every_object_section():
    """The derived order is total over `_OBJECT_SECTIONS`.

    A section the registry's builders do not claim must still be placed
    deterministically, not dropped and not appended arbitrarily.
    """
    canonical = object_section_order()
    assert set(_OBJECT_SECTIONS) <= set(canonical)
    assert len(canonical) == len(set(canonical))


def test_object_section_order_matches_the_registry_for_claimed_sections():
    """The order of claimed sections is the registry's order, not a local table.

    BuoysBuilder is registered at 80 and LinesBuilder at 90, so buoys precede
    lines here for the same reason they do in a generated master.yml.
    """
    from digitalmodel.solvers.orcaflex.modular_generator.builders.registry import (
        BuilderRegistry,
    )

    canonical = object_section_order()

    # section -> the registered order of the earliest builder claiming it.
    anchors: dict[str, int] = {}
    for _output_file, builder_cls in BuilderRegistry.get_ordered_builders():
        for section in getattr(builder_cls, "_sections", ()):
            anchors.setdefault(section, builder_cls._order)

    assert anchors, "no builder declares _sections; the derivation is inert"
    assert anchors["6DBuoys"] < anchors["Lines"]

    # Two claimed sections sort by their registered order, never against it.
    claimed = [s for s in canonical if s in anchors]
    for earlier, later in zip(claimed, claimed[1:]):
        assert anchors[earlier] <= anchors[later], (
            f"{earlier} (order {anchors[earlier]}) emitted before "
            f"{later} (order {anchors[later]})"
        )


def test_row11_non_object_sections_are_not_dropped(tmp_path):
    """General and Environment are not object sections and must still be emitted.

    The plan's pseudocode iterates `_OBJECT_SECTIONS` only, which would discard
    every other override silently.  The writer emits non-object sections ahead of
    the object sections instead, in the order supplied.
    """
    base = tmp_path / "model.yml"
    base.write_text("General: {}\n", encoding="utf-8")
    out = tmp_path / "case_a.yml"

    overrides = {
        "Lines": [{"Name": "L1"}],
        "General": {"StageDuration": [8.0, 100.0]},
        "Environment": {"WaterDepth": 200.0},
    }
    doc = build_variation_document(base, overrides, out)
    order = _emitted_key_order(yaml.dump(doc, sort_keys=False))

    assert doc["General"] == {"StageDuration": [8.0, 100.0]}
    assert doc["Environment"] == {"WaterDepth": 200.0}
    assert order == ["BaseFile", "General", "Environment", "Lines"]


# --------------------------------------------------------------------------
# Row 12 — no YAML anchors, because OrcFxAPI's parser rejects them
# --------------------------------------------------------------------------

def _shared_structure_overrides() -> dict:
    """Overrides in which one object is referenced twice by identity.

    PyYAML's default Dumper emits an anchor and an alias for a repeated object
    identity.  OrcFxAPI's YAML parser rejects both.
    """
    vertices = [[0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [1.0, 1.0, 0.0]]
    drag_area = {"X": 12.5, "Y": 12.5, "Z": 30.0}
    return {
        "6DBuoys": [
            {"Name": "Buoy_A", "Vertices": vertices, "DragArea": drag_area},
            {"Name": "Buoy_B", "Vertices": vertices, "DragArea": drag_area},
        ],
        "LineTypes": [
            {"Name": "Chain_A", "DragArea": drag_area},
            {"Name": "Chain_B", "DragArea": drag_area},
        ],
    }


def test_row12_control_default_dumper_does_alias():
    """Guard against a vacuous row-12 assertion.

    If this fails, the fixture no longer contains shared structure and the
    anchor-free assertion below would pass for the wrong reason.
    """
    text = yaml.dump(_shared_structure_overrides(), sort_keys=False)
    assert "&id" in text and "*id" in text


def test_row12_no_yaml_anchors_in_written_output(tmp_path):
    base = tmp_path / "model.yml"
    base.write_text("General: {}\n", encoding="utf-8")
    out = tmp_path / "case_a.yml"

    write_variation_model(base, _shared_structure_overrides(), out)
    text = out.read_text(encoding="utf-8")

    assert "&id" not in text
    assert "*id" not in text
    # The de-aliased document still parses, and still carries both copies.
    parsed = yaml.safe_load(text)
    assert parsed["6DBuoys"][0]["Vertices"] == parsed["6DBuoys"][1]["Vertices"]
    assert len(parsed["LineTypes"]) == 2


# --------------------------------------------------------------------------
# Row 13 — the load-bearing assertion against the committed goldens
# --------------------------------------------------------------------------

def _golden_cases():
    manifest = _load(GOLDENS / "manifest.yml")
    return [(case["case_id"], case) for case in manifest["cases"]]


def test_goldens_declare_their_comparator_class():
    manifest = _load(GOLDENS / "manifest.yml")
    assert manifest["comparator_class"] == "cross-solver"
    assert manifest["covers"] == ["BaseFile"]
    assert (GOLDENS / "PROVENANCE.md").exists()


@pytest.mark.parametrize("case_id,case", _golden_cases(), ids=lambda v: v if isinstance(v, str) else "")
def test_row13_writer_basefile_matches_committed_golden(case_id, case):
    """The writer's BaseFile reference matches the independent implementation.

    Comparator class `cross-solver`.  The goldens were captured from
    `template_generator._generate_reference` at commit
    b2b462d53c0829414bb29ae0ba4cb1ba93f93b20; see PROVENANCE.md.  They constrain
    the BaseFile key only — `_generate_reference` emits no inline override
    sections, so the writer's override half has no comparator.
    """
    base = REPO_ROOT / case["base_file"]
    out = REPO_ROOT / case["output_file"]
    golden = _load(REPO_ROOT / case["golden"])

    doc = build_variation_document(base, {}, out)

    assert doc["BaseFile"] == golden["BaseFile"]
    assert doc["BaseFile"] == case["captured_base_file_value"]


def test_row13_golden_reproduces_a_committed_case_file():
    """The calm_buoy golden agrees with the case file already in the tree.

    `docs/domains/orcaflex/templates/.../cases/case_deep_water.yml` was authored
    independently of this capture.  Agreement with it is a third data point on
    the BaseFile reference, not merely agreement with the capture.
    """
    committed = _load(
        REPO_ROOT
        / "docs/domains/orcaflex/templates/mooring_systems/calm_buoy_hybrid"
        / "cases/case_deep_water.yml"
    )
    golden = _load(GOLDENS / "cases" / "calm_buoy_deep_water.yml")
    assert golden["BaseFile"] == committed["BaseFile"]
