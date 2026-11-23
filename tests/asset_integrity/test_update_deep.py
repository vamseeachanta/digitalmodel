"""
Tests for asset_integrity.common.update_deep module.

Covers: AttributeDict attribute/key access, update_deep_dictionary
behaviour across a range of input types and nesting depths.
"""

import pytest


# ---------------------------------------------------------------------------
# AttributeDict
# ---------------------------------------------------------------------------


class TestAttributeDict:
    """Tests for AttributeDict — dict subclass with attribute access."""

    def _make(self, data=None):
        from digitalmodel.asset_integrity.common.update_deep import AttributeDict
        return AttributeDict(data or {})

    def test_key_access_matches_init_values(self):
        ad = self._make({"a": 1, "b": 2})
        assert ad["a"] == 1
        assert ad["b"] == 2

    def test_attribute_access_matches_key_access(self):
        ad = self._make({"x": 10})
        assert ad.x == ad["x"] == 10

    def test_setting_attribute_updates_dict(self):
        ad = self._make()
        ad.new_key = "hello"
        assert ad["new_key"] == "hello"

    def test_setting_key_updates_attribute(self):
        ad = self._make()
        ad["another"] = 99
        assert ad.another == 99

    def test_empty_init_is_empty_dict(self):
        from digitalmodel.asset_integrity.common.update_deep import AttributeDict
        ad = AttributeDict()
        assert len(ad) == 0

    def test_nested_values_accessible_as_keys(self):
        ad = self._make({"nested": {"inner": 5}})
        assert ad["nested"]["inner"] == 5

    def test_numeric_values_preserved(self):
        ad = self._make({"pi": 3.14159, "zero": 0, "neg": -7})
        assert ad.pi == pytest.approx(3.14159)
        assert ad.zero == 0
        assert ad.neg == -7

    def test_contains_check_works(self):
        ad = self._make({"present": True})
        assert "present" in ad
        assert "missing" not in ad

    def test_iteration_yields_keys(self):
        ad = self._make({"a": 1, "b": 2, "c": 3})
        keys = set(ad)
        assert keys == {"a", "b", "c"}

    def test_list_value_accessible(self):
        ad = self._make({"items": [1, 2, 3]})
        assert ad.items == [1, 2, 3]

    def test_none_value_preserved(self):
        ad = self._make({"x": None})
        assert ad.x is None

    def test_overwrite_existing_key_via_attribute(self):
        ad = self._make({"score": 5})
        ad.score = 10
        assert ad["score"] == 10


# ---------------------------------------------------------------------------
# update_deep_dictionary
# ---------------------------------------------------------------------------


class TestUpdateDeepDictionary:
    """Tests for the recursive deep-merge function."""

    def _call(self, base, update):
        from digitalmodel.asset_integrity.common.update_deep import (
            update_deep_dictionary,
        )
        return update_deep_dictionary(base, update)

    def test_flat_update_adds_new_key(self):
        result = self._call({"a": 1}, {"b": 2})
        assert result["b"] == 2
        assert result["a"] == 1

    def test_flat_update_overwrites_existing_key(self):
        result = self._call({"a": 1, "b": 2}, {"b": 99})
        assert result["b"] == 99
        assert result["a"] == 1

    def test_nested_merge_preserves_untouched_keys(self):
        base = {"top": {"keep": "yes", "change": "old"}}
        update = {"top": {"change": "new"}}
        result = self._call(base, update)
        assert result["top"]["keep"] == "yes"
        assert result["top"]["change"] == "new"

    def test_deeply_nested_three_levels(self):
        base = {"a": {"b": {"c": 1, "d": 2}}}
        update = {"a": {"b": {"c": 9}}}
        result = self._call(base, update)
        assert result["a"]["b"]["c"] == 9
        assert result["a"]["b"]["d"] == 2

    def test_non_mapping_base_replaced_with_update(self):
        result = self._call(123, {"x": 1})
        assert result == {"x": 1}

    def test_empty_update_returns_base_unchanged(self):
        base = {"a": 1, "b": 2}
        result = self._call(base, {})
        assert result == {"a": 1, "b": 2}

    def test_empty_base_gets_all_update_keys(self):
        result = self._call({}, {"x": 10, "y": 20})
        assert result == {"x": 10, "y": 20}

    def test_list_value_overwritten_not_merged(self):
        base = {"items": [1, 2, 3]}
        update = {"items": [4, 5]}
        result = self._call(base, update)
        assert result["items"] == [4, 5]

    def test_none_value_in_update_sets_key_to_none(self):
        result = self._call({"a": 5}, {"a": None})
        assert result["a"] is None

    def test_string_value_overwrite(self):
        result = self._call({"name": "old"}, {"name": "new"})
        assert result["name"] == "new"

    def test_returns_same_dict_object(self):
        base = {"a": 1}
        result = self._call(base, {"b": 2})
        assert result is base

    def test_multiple_top_level_keys_merged(self):
        base = {"x": 1, "y": {"p": 1, "q": 2}, "z": 3}
        update = {"y": {"q": 99}, "w": 4}
        result = self._call(base, update)
        assert result["x"] == 1
        assert result["y"]["p"] == 1
        assert result["y"]["q"] == 99
        assert result["z"] == 3
        assert result["w"] == 4

    def test_numeric_zero_base_replaced(self):
        result = self._call(0, {"a": 1})
        assert result == {"a": 1}
