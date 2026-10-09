"""Tests for the OrcaFlex collection-key collision check.

An OrcaFlex text data file encodes an object collection two ways, with
different semantics:

- a name-keyed **mapping** (``LineTypes:`` then ``MyType:`` then fields)
  PATCHES the collection;
- a **list** of ``- Name:`` entries REPLACES the whole collection, so any
  object not listed is deleted.

The modular generator emits list style, which is correct for authoring but
safe only while each collection is written by exactly one includefile.  Two
includefiles emitting the same collection key in list style means the later
one silently deletes everything the earlier one created.  The check under
test fails closed on that condition.

Test-pair dependency, do not delete one without the other:
``test_calm_buoy_template_passes`` (real committed template set, no
regression) is meaningless unless ``test_two_list_style_emitters_collide``
proves the check fires at all.  A check that never raises passes the
template test trivially.
"""

from __future__ import annotations

from pathlib import Path

import pytest

from digitalmodel.solvers.orcaflex.modular_generator.post_validator import (
    CollectionCollisionError,
    check_collection_collisions,
)

CALM_BUOY_TEMPLATE = (
    Path(__file__).resolve().parents[4]
    / "docs"
    / "domains"
    / "orcaflex"
    / "templates"
    / "mooring_systems"
    / "calm_buoy"
    / "master.yml"
)


# ----------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------


def _write_model(root: Path, master_entries: list[str], files: dict[str, str]) -> Path:
    """Write a master file plus its includes under *root*.

    Args:
        root: Directory to build the model in.
        master_entries: Include paths, relative to *root*, in composition order.
        files: Mapping of relative path -> file body.

    Returns:
        Path to the written ``master.yml``.
    """
    for rel_path, body in files.items():
        target = root / rel_path
        target.parent.mkdir(parents=True, exist_ok=True)
        target.write_text(body, encoding="utf-8")

    master = root / "master.yml"
    master.write_text(
        "%YAML 1.1\n---\n"
        + "".join(f"- includefile: {entry}\n" for entry in master_entries),
        encoding="utf-8",
    )
    return master


_LIST_STYLE_LINE_TYPES_A = """\
LineTypes:
  - Name: Chain_84mm_R4
    Category: General
    OD: 0.084
"""

_LIST_STYLE_LINE_TYPES_B = """\
LineTypes:
  - Name: Wire_90mm
    Category: General
    OD: 0.090
"""

_MAPPING_STYLE_LINE_TYPES = """\
LineTypes:
  Chain_84mm_R4:
    OD: 0.084
"""

_MAPPING_STYLE_LINE_TYPES_B = """\
LineTypes:
  Wire_90mm:
    OD: 0.090
"""


# ----------------------------------------------------------------------
# Test 1 — the check fires (paired with test 6)
# ----------------------------------------------------------------------


def test_two_list_style_emitters_collide(tmp_path: Path) -> None:
    """Two includefiles emitting LineTypes in list style must fail closed.

    Paired with ``test_calm_buoy_template_passes`` — that test is
    meaningless without this one.
    """
    master = _write_model(
        tmp_path,
        ["includes/05_line_types.yml", "includes/09_more_line_types.yml"],
        {
            "includes/05_line_types.yml": _LIST_STYLE_LINE_TYPES_A,
            "includes/09_more_line_types.yml": _LIST_STYLE_LINE_TYPES_B,
        },
    )

    with pytest.raises(CollectionCollisionError) as exc_info:
        check_collection_collisions(master)

    message = str(exc_info.value)
    assert "LineTypes" in message
    assert "includes/05_line_types.yml" in message
    assert "includes/09_more_line_types.yml" in message


# ----------------------------------------------------------------------
# Test 2 — one list, one mapping
# ----------------------------------------------------------------------


def test_list_plus_mapping_passes(tmp_path: Path) -> None:
    """A mapping patches the collection, so it does not collide with a list."""
    master = _write_model(
        tmp_path,
        ["includes/05_line_types.yml", "includes/10_patch.yml"],
        {
            "includes/05_line_types.yml": _LIST_STYLE_LINE_TYPES_A,
            "includes/10_patch.yml": _MAPPING_STYLE_LINE_TYPES,
        },
    )

    result = check_collection_collisions(master)

    assert result.collisions == []
    styles = [style for _, style in result.emitters["LineTypes"]]
    assert styles == ["list", "mapping"]


# ----------------------------------------------------------------------
# Test 3 — two mappings
# ----------------------------------------------------------------------


def test_two_mapping_style_emitters_pass(tmp_path: Path) -> None:
    """Two mapping-style emitters both patch; neither replaces."""
    master = _write_model(
        tmp_path,
        ["includes/a.yml", "includes/b.yml"],
        {
            "includes/a.yml": _MAPPING_STYLE_LINE_TYPES,
            "includes/b.yml": _MAPPING_STYLE_LINE_TYPES_B,
        },
    )

    result = check_collection_collisions(master)

    assert result.collisions == []


# ----------------------------------------------------------------------
# Test 4 — one collection per includefile
# ----------------------------------------------------------------------


def test_one_collection_per_includefile_passes(tmp_path: Path) -> None:
    """The shape the generator emits must remain valid."""
    master = _write_model(
        tmp_path,
        [
            "includes/05_line_types.yml",
            "includes/06_vessels.yml",
            "includes/07_lines.yml",
            "includes/08_buoys.yml",
        ],
        {
            "includes/05_line_types.yml": _LIST_STYLE_LINE_TYPES_A,
            "includes/06_vessels.yml": "Vessels:\n  - Name: FPSO\n",
            "includes/07_lines.yml": "Lines:\n  - Name: Mooring1\n",
            "includes/08_buoys.yml": "6DBuoys:\n  - Name: CALM Buoy\n",
        },
    )

    result = check_collection_collisions(master)

    assert result.collisions == []
    assert result.unrecognised == []
    assert len(result.composed_files) == 4


# ----------------------------------------------------------------------
# Test 5 — collision outside the composition
# ----------------------------------------------------------------------


def test_collision_in_unreferenced_file_passes(tmp_path: Path) -> None:
    """Only files the master actually composes are in scope."""
    master = _write_model(
        tmp_path,
        ["includes/05_line_types.yml"],
        {
            "includes/05_line_types.yml": _LIST_STYLE_LINE_TYPES_A,
            # Present in includes/ but never referenced by the master.
            "includes/99_orphan_line_types.yml": _LIST_STYLE_LINE_TYPES_B,
        },
    )

    result = check_collection_collisions(master)

    assert result.collisions == []
    composed = {path.name for path in result.composed_files}
    assert "99_orphan_line_types.yml" not in composed


# ----------------------------------------------------------------------
# Test 6 — the real committed template set (paired with test 1)
# ----------------------------------------------------------------------


def test_calm_buoy_template_passes() -> None:
    """The committed CALM-buoy template must pass unchanged.

    Paired with ``test_two_list_style_emitters_collide``, which proves the
    check fires; without it this test passes trivially.
    """
    assert CALM_BUOY_TEMPLATE.is_file(), f"template missing: {CALM_BUOY_TEMPLATE}"

    result = check_collection_collisions(CALM_BUOY_TEMPLATE)

    assert result.collisions == []
    assert result.unrecognised == [], (
        "every top-level key in the committed template must be recognised; "
        f"unrecognised: {result.unrecognised}"
    )
    assert len(result.composed_files) == 8


# ----------------------------------------------------------------------
# Test 7 — unrecognised top-level key
# ----------------------------------------------------------------------


def test_unrecognised_top_level_key_is_surfaced(tmp_path: Path) -> None:
    """An unknown top-level key is reported, never silently ignored."""
    master = _write_model(
        tmp_path,
        ["includes/05_line_types.yml", "includes/50_unknown.yml"],
        {
            "includes/05_line_types.yml": _LIST_STYLE_LINE_TYPES_A,
            "includes/50_unknown.yml": "NotARealOrcaFlexSection:\n  - Name: X\n",
        },
    )

    result = check_collection_collisions(master)

    assert result.collisions == []
    assert ("includes/50_unknown.yml", "NotARealOrcaFlexSection") in result.unrecognised
    assert any(
        warning.category == "unrecognised"
        and "NotARealOrcaFlexSection" in warning.message
        for warning in result.warnings
    )
