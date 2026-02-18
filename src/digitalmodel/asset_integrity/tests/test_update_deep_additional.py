from digitalmodel.asset_integrity.common.update_deep import (
    AttributeDict,
    update_deep_dictionary,
)


def test_attribute_dict_attribute_and_key_access_are_equivalent() -> None:
    data = AttributeDict({"value": 10})
    data.extra = 20
    assert data.value == 10
    assert data["value"] == 10
    assert data.extra == 20
    assert data["extra"] == 20


def test_update_deep_replaces_non_mapping_base() -> None:
    result = update_deep_dictionary(123, {"a": 1, "b": 2})
    assert result == {"a": 1, "b": 2}


def test_update_deep_nested_merge_and_override() -> None:
    base = {"a": {"b": 1, "c": 2}, "x": 1}
    update = {"a": {"b": 9}, "y": 2}
    result = update_deep_dictionary(base, update)
    assert result["a"]["b"] == 9
    assert result["a"]["c"] == 2
    assert result["x"] == 1
    assert result["y"] == 2

