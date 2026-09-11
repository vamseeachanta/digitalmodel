"""Tests for the name-keyed list merge.

The behaviour under test is ported from
``digitalmodel.solvers.orcaflex.template_generator.TemplateGenerator._merge_object_lists``
(read at ``template_generator.py:273-323``). The assertions therefore encode the
existing contract, including the ``has_names`` fallback and the deep copying of
every retained and appended entry.

Cases 5 to 8 exist because an implementation that indexes ``item["Name"]``
unconditionally raises ``KeyError`` on input the ported function handles, and an
implementation that omits ``deepcopy`` aliases base entries into the result.
"""

from __future__ import annotations

from digitalmodel.solvers.orcaflex.modular_generator.merge import merge_named_lists


def test_override_replaces_same_named_entry_wholesale():
    """Row 1: a same-named override entry is present with no residue of the base entry."""
    base = [{"Name": "LineA", "Length": 100.0, "OuterDiameter": 0.5}]
    override = [{"Name": "LineA", "Length": 250.0}]

    result = merge_named_lists(base, override)

    assert result == [{"Name": "LineA", "Length": 250.0}]
    assert len(result) == 1
    assert result[0]["Length"] == 250.0


def test_base_key_absent_from_override_is_gone():
    """Row 2: replacement, not deep merge — the base-only key does not survive."""
    base = [{"Name": "LineA", "Length": 100.0, "OuterDiameter": 0.5}]
    override = [{"Name": "LineA", "Length": 250.0}]

    result = merge_named_lists(base, override)

    assert "OuterDiameter" not in result[0]


def test_unknown_name_appends_at_end():
    """Row 3: a name absent from the base appends last; base order is unchanged."""
    base = [{"Name": "LineA", "Length": 100.0}, {"Name": "LineB", "Length": 200.0}]
    override = [{"Name": "LineC", "Length": 300.0}]

    result = merge_named_lists(base, override)

    assert [item["Name"] for item in result] == ["LineA", "LineB", "LineC"]
    assert result[2] == {"Name": "LineC", "Length": 300.0}


def test_base_order_preserved_under_override():
    """Row 4: an overridden entry keeps the position its base entry held."""
    base = [
        {"Name": "LineA", "Length": 100.0},
        {"Name": "LineB", "Length": 200.0},
        {"Name": "LineC", "Length": 300.0},
    ]
    override = [{"Name": "LineB", "Length": 999.0}]

    result = merge_named_lists(base, override)

    assert [item["Name"] for item in result] == ["LineA", "LineB", "LineC"]
    assert result[1]["Length"] == 999.0
    assert result[0]["Length"] == 100.0
    assert result[2]["Length"] == 300.0


def test_item_without_name_falls_back_to_wholesale_replacement():
    """Row 5: a list carrying an unnamed item falls back to list replacement, no raise."""
    # Unnamed item in the override.
    base = [{"Name": "LineA", "Length": 100.0}]
    override = [{"NoNameKey": 1}]

    result = merge_named_lists(base, override)

    assert result == [{"NoNameKey": 1}]

    # Unnamed item in the base.
    base = [{"Length": 100.0}]
    override = [{"Name": "LineA", "Length": 250.0}]

    result = merge_named_lists(base, override)

    assert result == [{"Name": "LineA", "Length": 250.0}]

    # A non-dict item also disables name keying.
    base = [1.0, 2.0, 3.0]
    override = [4.0]

    result = merge_named_lists(base, override)

    assert result == [4.0]


def test_empty_override_with_non_name_base_returns_base_copy():
    """Row 6: an empty override over an unnamed base returns a copy of the base."""
    base = [{"Length": 100.0}, {"Length": 200.0}]
    override: list = []

    result = merge_named_lists(base, override)

    assert result == [{"Length": 100.0}, {"Length": 200.0}]
    assert result is not base
    assert result[0] is not base[0]


def test_result_does_not_alias_the_base():
    """Row 7: mutating the merged result leaves the base untouched, on both paths."""
    # Name-keyed path — retained base entries are copied.
    base = [
        {"Name": "LineA", "Sections": [{"Length": 100.0}]},
        {"Name": "LineB", "Sections": [{"Length": 200.0}]},
    ]
    override = [{"Name": "LineB", "Sections": [{"Length": 999.0}]}]

    result = merge_named_lists(base, override)
    result[0]["Sections"][0]["Length"] = -1.0
    result[0]["Injected"] = True

    assert base[0]["Sections"][0]["Length"] == 100.0
    assert "Injected" not in base[0]

    # Appended override entries are copied too.
    override_entry = {"Name": "LineC", "Sections": [{"Length": 300.0}]}
    result = merge_named_lists(base, [override_entry])
    result[-1]["Sections"][0]["Length"] = -1.0

    assert override_entry["Sections"][0]["Length"] == 300.0

    # Fallback path — the returned base copy is independent as well.
    unnamed_base = [{"Sections": [{"Length": 100.0}]}]
    result = merge_named_lists(unnamed_base, [])
    result[0]["Sections"][0]["Length"] = -1.0

    assert unnamed_base[0]["Sections"][0]["Length"] == 100.0

    # Fallback path — the returned override copy is independent as well.
    unnamed_override = [{"Sections": [{"Length": 300.0}]}]
    result = merge_named_lists([{"Name": "LineA"}], unnamed_override)
    result[0]["Sections"][0]["Length"] = -1.0

    assert unnamed_override[0]["Sections"][0]["Length"] == 300.0


def test_falsy_entries_are_skipped():
    """Row 8: falsy entries in either list are skipped without raising."""
    base = [{"Name": "LineA", "Length": 100.0}, None, {}, {"Name": "LineB", "Length": 200.0}]
    override = [None, {}, {"Name": "LineB", "Length": 999.0}]

    result = merge_named_lists(base, override)

    assert [item["Name"] for item in result] == ["LineA", "LineB"]
    assert result[1]["Length"] == 999.0

    # Falsy-only lists do not raise and contribute nothing.
    assert merge_named_lists([None, {}], [None]) == []
    assert merge_named_lists([], []) == []
