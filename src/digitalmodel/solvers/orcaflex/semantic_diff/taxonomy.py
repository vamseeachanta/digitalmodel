# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Conservative classification of differences in already-parsed YAML mappings.

The registry is a synthetic policy example, not a solver schema or validator.
See docs/domains/orcaflex/SEMANTIC_DIFF_COMPARISON_POLICY.md for its boundary.
"""

from __future__ import annotations

import math
from collections.abc import Mapping
from dataclasses import dataclass
from enum import Enum, StrEnum
from types import MappingProxyType
from typing import Any


class DiffCategory(StrEnum):
    """Describe why a YAML difference matters to the comparison policy."""

    PHYSICS_SIGNIFICANT = "physics-significant"
    LOADABILITY_SIGNIFICANT = "loadability-significant"
    REFERENCE_RESOLUTION_SIGNIFICANT = "reference-resolution-significant"
    NORMALIZATION_ONLY = "normalization-only"
    UI_COSMETIC_ONLY = "ui-cosmetic-only"
    INTENTIONAL_OMISSION = "intentional-omission"


class _Missing(Enum):
    ABSENT = "missing"


# An absent key is distinct from an explicit YAML null (None).
MISSING = _Missing.ABSENT


@dataclass(frozen=True)
class Rule:
    """Declare one scoped property policy and its optional omission reason."""

    category: DiffCategory
    scalar_kind: str | None = None
    omission_reason: str | None = None


# Synthetic examples only; no inference from property names or prefixes.
# Basis and input values: SEMANTIC_DIFF_COMPARISON_POLICY.md, Examples.
REGISTRY: Mapping[str, Mapping[str, Rule]] = MappingProxyType({
    "General": MappingProxyType({
        "DefaultViewAzimuth": Rule(DiffCategory.UI_COSMETIC_ONLY, "number"),
        "DefaultViewSize": Rule(
            DiffCategory.UI_COSMETIC_ONLY,
            "number",
            "Synthetic comparison deliberately excludes the saved view size.",
        ),
    }),
    "Environment": MappingProxyType({
        "WaterDepth": Rule(DiffCategory.PHYSICS_SIGNIFICANT, "number"),
        "SeabedOriginDepth": Rule(DiffCategory.PHYSICS_SIGNIFICANT, "number"),
        "Density": Rule(DiffCategory.PHYSICS_SIGNIFICANT, "number"),
        "MultipleCurrentDataCanBeDefined": Rule(
            DiffCategory.PHYSICS_SIGNIFICANT, "boolean"
        ),
    }),
    "Groups": MappingProxyType({
        "Structure": Rule(DiffCategory.REFERENCE_RESOLUTION_SIGNIFICANT),
    }),
})

_NONBLOCKING = frozenset({
    DiffCategory.NORMALIZATION_ONLY,
    DiffCategory.UI_COSMETIC_ONLY,
    DiffCategory.INTENTIONAL_OMISSION,
})
_BOOLEAN_ENCODINGS = {"Yes": True, "No": False}


@dataclass(frozen=True)
class Diff:
    """Retain a property difference, including explicit omission evidence."""

    section: str
    key: str
    strict_value: Any
    generated_value: Any
    category: DiffCategory
    omission_reason: str | None = None


def _identical(left: Any, right: Any) -> bool:
    """Compare parsed values without Python's boolean/numeric type coercion."""
    if type(left) is not type(right):
        return False
    if isinstance(left, dict):
        left_keys = {(type(key), key) for key in left}
        right_keys = {(type(key), key) for key in right}
        return left_keys == right_keys and all(
            _identical(value, right[key]) for key, value in left.items()
        )
    if isinstance(left, list):
        return len(left) == len(right) and all(
            _identical(a, b) for a, b in zip(left, right)
        )
    return bool(left == right)


def _valid_scalar(value: Any, kind: str) -> bool:
    if kind == "number":
        return type(value) is int or (type(value) is float and math.isfinite(value))
    if kind == "boolean":
        return type(value) is bool or (
            type(value) is str and value in _BOOLEAN_ENCODINGS
        )
    raise ValueError(f"Unsupported scalar policy: {kind}")


def _normalized(value: Any, kind: str) -> Any:
    if kind == "boolean" and type(value) is str:
        return _BOOLEAN_ENCODINGS[value]
    return value


def classify_diff(
    section: str, key: str, strict_value: Any, generated_value: Any
) -> DiffCategory:
    """Classify an actual difference; use MISSING for an absent property.

    Unknown properties remain physics-significant, meaning potentially relevant,
    not demonstrated to change results. Missing registered scalars and invalid
    scalar types are loadability risks, not evidence of an actual load failure.
    Only an explicit, directional registry entry permits intentional omission.

    Raises:
        ValueError: The values are identical, so there is no diff to classify.
    """
    if _identical(strict_value, generated_value):
        raise ValueError("Classification requires different values")
    rule = REGISTRY.get(section, {}).get(key)
    if rule is None:
        return DiffCategory.PHYSICS_SIGNIFICANT
    # Reference changes cannot be hidden by normalization or omission policies.
    if rule.category == DiffCategory.REFERENCE_RESOLUTION_SIGNIFICANT:
        return rule.category
    if strict_value is MISSING or generated_value is MISSING:
        if (
            generated_value is MISSING
            and rule.omission_reason
            and rule.scalar_kind
            and _valid_scalar(strict_value, rule.scalar_kind)
        ):
            return DiffCategory.INTENTIONAL_OMISSION
        return DiffCategory.LOADABILITY_SIGNIFICANT
    if rule.scalar_kind:
        if not all(
            _valid_scalar(value, rule.scalar_kind)
            for value in (strict_value, generated_value)
        ):
            return DiffCategory.LOADABILITY_SIGNIFICANT
        if _normalized(strict_value, rule.scalar_kind) == _normalized(
            generated_value, rule.scalar_kind
        ):
            return DiffCategory.NORMALIZATION_ONLY
    return rule.category


def is_semantically_equivalent(
    strict_yaml: dict[str, Any], generated_yaml: dict[str, Any]
) -> tuple[bool, list[Diff]]:
    """Return a bounded policy match and all categorized property differences.

    This legacy-shaped API name does not establish physical equivalence,
    loadability, or reference validity. No schema or reference graph is checked.
    Inputs are parsed section mappings; nested properties and lists stay atomic.
    Whole-section loss/addition/shape changes are always significant.
    """
    diffs: list[Diff] = []
    for section in sorted(strict_yaml.keys() | generated_yaml.keys()):
        before = strict_yaml.get(section, MISSING)
        after = generated_yaml.get(section, MISSING)
        if _identical(before, after):
            continue
        if not isinstance(before, dict) or not isinstance(after, dict):
            category = (
                DiffCategory.REFERENCE_RESOLUTION_SIGNIFICANT
                if section == "Groups"
                else DiffCategory.LOADABILITY_SIGNIFICANT
            )
            diffs.append(Diff(section, "", before, after, category))
            continue
        for key in sorted(before.keys() | after.keys()):
            strict_value = before.get(key, MISSING)
            generated_value = after.get(key, MISSING)
            if _identical(strict_value, generated_value):
                continue
            category = classify_diff(section, key, strict_value, generated_value)
            rule = REGISTRY.get(section, {}).get(key)
            reason = (
                rule.omission_reason
                if category == DiffCategory.INTENTIONAL_OMISSION and rule
                else None
            )
            diffs.append(Diff(
                section, key, strict_value, generated_value, category, reason
            ))
    return all(diff.category in _NONBLOCKING for diff in diffs), diffs
