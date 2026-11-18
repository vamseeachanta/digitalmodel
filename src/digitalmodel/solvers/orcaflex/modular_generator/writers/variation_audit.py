# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Audit property-only artifacts and supplied Standard-model snapshots.

Source: Orcina, OrcaFlex online help, Variation models, Text data format and
Connections (accessed 2026-10-22):
https://www.orcina.com/webhelp/OrcaFlex/Content/html/Variationmodels.htm

No solver is imported or called. Only externally resolved Standard exports can
supply native evidence; dictionary merges and synthetic snapshots cannot.
"""

from collections.abc import Mapping
from pathlib import Path
from typing import Any

import yaml

from ...yaml_utils import OrcaFlexLoader
from .basefile import _COMPOSITION_DIRECTIVES, validate_variation_document


class _ArtifactLoader(OrcaFlexLoader):
    """Reject duplicates and aliases before property-only syntax validation."""

    def compose_node(self, parent, index):
        if self.check_event(yaml.AliasEvent):
            raise ValueError("Variation artifact must not contain YAML aliases")
        return super().compose_node(parent, index)

    def construct_mapping(self, node, deep=False):
        self.flatten_mapping(node)
        result = {}
        for key_node, value_node in node.value:
            key = self.construct_object(key_node, deep=deep)
            try:
                if key in result:
                    raise ValueError(
                        "Variation artifact contains a duplicate mapping key"
                    )
                result[key] = self.construct_object(value_node, deep=deep)
            except TypeError:
                raise ValueError(
                    "Variation artifact contains an invalid mapping key"
                ) from None
        return result


def load_variation_document(path: str | Path) -> dict[str, Any]:
    """Load one artifact, rejecting ambiguity rather than silently repairing it.

    Native blank scalars remain empty text and explicit null remains None.
    Error messages omit source content. No referenced model is opened.
    """
    try:
        with Path(path).open(encoding="utf-8-sig") as handle:
            document = yaml.load(handle, Loader=_ArtifactLoader)
    except yaml.YAMLError:
        raise ValueError("Invalid variation YAML") from None
    validate_variation_document(document)
    return document


def _at_path(document: Any, path: tuple[str | int, ...]) -> Any:
    value = document
    try:
        for part in path:
            if isinstance(value, Mapping) and isinstance(part, str):
                value = value[part]
            elif isinstance(value, list) and type(part) is int and part >= 0:
                value = value[part]
            else:
                raise ValueError("Selection must address an existing data item")
    except (KeyError, IndexError):
        raise ValueError("Selection must address an existing data item") from None
    return value


def _contains_mapping(value: Any) -> bool:
    return isinstance(value, Mapping) or (
        isinstance(value, list) and any(_contains_mapping(item) for item in value)
    )


def _equal(left: Any, right: Any) -> bool:
    """Exact, type-sensitive equality, including every table row and object."""
    if type(left) is not type(right):
        return False
    if isinstance(left, Mapping):
        return left.keys() == right.keys() and all(
            _equal(left[key], right[key]) for key in left
        )
    if isinstance(left, list):
        return len(left) == len(right) and all(
            _equal(a, b) for a, b in zip(left, right)
        )
    return bool(left == right)


def validate_resolved_variation(
    reference: Mapping[str, Any],
    resolved: Mapping[str, Any],
    expected_changes: Mapping[tuple[str | int, ...], Any],
) -> None:
    """Require selected values and equality of every unselected snapshot item.

    Supply independent Standard exports of the master and the variation after
    converting it to Standard in memory. This function cannot authenticate
    that provenance. Paths use mapping keys and Python list indices; they are
    not OrcaFlex indexed-data expressions. Expected values are required inputs.
    Selections address existing scalar/table data only, never names or whole
    object collections. Equality is exact with no engineering tolerance.
    Inputs are not modified. A mismatch raises ValueError without data values.
    """
    for document in (reference, resolved):
        if not isinstance(document, Mapping) or not document:
            raise ValueError("Expected a nonempty resolved Standard snapshot")
        if any(
            isinstance(key, str) and key.casefold() in _COMPOSITION_DIRECTIVES
            for key in document
        ):
            raise ValueError("Snapshot must be resolved to a Standard model")
    if not isinstance(expected_changes, Mapping):
        raise ValueError("Expected changes must map data-item paths to values")

    selected = set(expected_changes)
    for path, expected in expected_changes.items():
        if not isinstance(path, tuple) or not path or path[-1] == "Name":
            raise ValueError("Selection must address a data item, never an object name")
        original = _at_path(reference, path)
        actual = _at_path(resolved, path)
        if any(_contains_mapping(value) for value in (original, actual, expected)):
            raise ValueError(
                "Selection must not mask object collections or property mappings"
            )
        if any(path[:length] in selected for length in range(1, len(path))):
            raise ValueError("Selected paths must not overlap")
        if not _equal(actual, expected):
            raise ValueError("Selected data item does not match its expected value")

    def unchanged(left: Any, right: Any, path: tuple = ()) -> bool:
        if path in selected:
            return True
        if type(left) is not type(right):
            return False
        if isinstance(left, Mapping):
            return left.keys() == right.keys() and all(
                unchanged(left[key], right[key], (*path, key)) for key in left
            )
        if isinstance(left, list):
            return len(left) == len(right) and all(
                unchanged(a, b, (*path, index))
                for index, (a, b) in enumerate(zip(left, right))
            )
        return _equal(left, right)

    if not unchanged(reference, resolved):
        raise ValueError("Unselected resolved-model data changed")
