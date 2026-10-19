"""Custom SafeLoader boundary contracts, using synthetic YAML grammar examples.

PyYAML 6.0.2 SafeLoader constructs standard YAML values, rejects Python tags,
and get_single_data rejects multiple documents. Duplicate keys and anchors are
additionally refused by the catalogue's existing contract.
https://github.com/yaml/pyyaml/blob/6.0.2/lib/yaml/__init__.py
"""
import pytest
import yaml

from digitalmodel.bha_database.loader import BhaDatabaseError, read_yaml


@pytest.mark.parametrize("raw,expected", [
    (b"name: alpha\nitems: [left, right]\n", {
        "name": "alpha", "items": ["left", "right"],
    }),
    (b"", None),
])
def test_standard_yaml_preserves_raw_bytes(tmp_path, raw, expected):
    """Expected mappings/lists follow the literal key and sequence entries."""
    path = tmp_path / "catalogue.yaml"
    path.write_bytes(raw)
    actual_raw, actual_data = read_yaml(path)
    assert actual_raw == raw
    assert actual_data == expected


@pytest.mark.parametrize("text,error", [
    ("name: alpha\nname: beta\n", BhaDatabaseError),
    ("first: &shared []\nsecond: *shared\n", BhaDatabaseError),
    ("!!python/object/apply:builtins.list []\n", yaml.constructor.ConstructorError),
    ("---\nname: alpha\n---\nname: beta\n", yaml.composer.ComposerError),
])
def test_unsupported_yaml_is_refused(tmp_path, text, error):
    path = tmp_path / "catalogue.yaml"
    path.write_text(text, encoding="utf-8")
    with pytest.raises(error):
        read_yaml(path)
