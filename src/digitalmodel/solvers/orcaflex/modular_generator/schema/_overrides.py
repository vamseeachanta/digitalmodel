"""Dotted-path override helper for ProjectInputSpec with Pydantic re-validation.

Dotted paths target the canonical ``model_dump()`` shape; use integer segments
to index into list-valued fields (e.g. ``pipeline.segments.0.length``).
"""
from __future__ import annotations

from typing import Any

from .root import ProjectInputSpec


def _set_nested_safe(d: dict, path: str, value: Any) -> None:
    keys = path.split(".")
    cursor: Any = d
    for i, k in enumerate(keys):
        is_terminal = i == len(keys) - 1
        if isinstance(cursor, list):
            try:
                idx = int(k)
            except ValueError:
                raise KeyError(
                    f"non-integer segment {k!r} for list at dotted path {path!r}"
                )
            if not 0 <= idx < len(cursor):
                raise IndexError(
                    f"list index {idx} out of range at dotted path {path!r}"
                )
            if is_terminal:
                cursor[idx] = value
            else:
                cursor = cursor[idx]
        elif isinstance(cursor, dict):
            if k not in cursor:
                raise KeyError(
                    f"path segment {k!r} not found in dotted path {path!r}"
                )
            if is_terminal:
                cursor[k] = value
            else:
                cursor = cursor[k]
        else:
            raise TypeError(
                f"cannot traverse non-container at segment {k!r} in dotted path {path!r}"
            )


def apply_dotted_override(
    spec: ProjectInputSpec, dotted: str, value: Any
) -> ProjectInputSpec:
    d = spec.model_dump()
    _set_nested_safe(d, dotted, value)
    return ProjectInputSpec.model_validate(d)
