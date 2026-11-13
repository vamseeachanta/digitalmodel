"""Dotted-path override helper for ProjectInputSpec with Pydantic re-validation.

Dotted paths target the canonical ``model_dump()`` shape; use integer segments
to index into list-valued fields (e.g. ``pipeline.segments.0.length``).

Overrides and campaign spec files keep unset-field provenance: only fields
that were authored (or edited) are dumped, so schema defaults do not become
authored values on re-validation. Renderers that overlay only authored typed
fields, such as the riser-plus-generic route, depend on this.
"""
from __future__ import annotations

from copy import deepcopy
from typing import Any

from pydantic import BaseModel, ValidationError

from .root import ProjectInputSpec


def _mark_edited_ancestors(value: Any) -> bool:
    """Mark a field set when any value below it is set; return whether set.

    In-place attribute edits (``spec.environment.water.depth = x``) record the
    edit on the nested model only. Without this, ``exclude_unset`` would drop
    an edit made below a field whose parent was never authored.
    """
    if isinstance(value, BaseModel):
        for name in type(value).model_fields:
            if _mark_edited_ancestors(getattr(value, name, None)):
                value.__pydantic_fields_set__.add(name)
        return bool(value.model_fields_set)
    if isinstance(value, (list, tuple)):
        return any([_mark_edited_ancestors(item) for item in value])
    if isinstance(value, dict):
        return any([_mark_edited_ancestors(item) for item in value.values()])
    return False


def dump_authored(spec: ProjectInputSpec, **kwargs: Any) -> dict:
    """Dump authored and edited fields only; the input spec is not mutated."""
    copy = spec.model_copy(deep=True)
    _mark_edited_ancestors(copy)
    return copy.model_dump(exclude_unset=True, **kwargs)


def _resolves(d: Any, path: str) -> bool:
    for key in path.split("."):
        if isinstance(d, dict) and key in d:
            d = d[key]
        elif isinstance(d, list) and key.isdigit() and int(key) < len(d):
            d = d[int(key)]
        else:
            return False
    return True


def _materialize_path(partial: dict, spec: ProjectInputSpec, path: str) -> None:
    """Copy unset containers on ``path`` from the full dump into ``partial``.

    Only the containers on the override path are copied, so a sweep of a
    default-valued field still resolves; paths absent from the full dump are
    left for ``_set_nested_safe`` to report.
    """
    if _resolves(partial, path):
        return
    full: Any = spec.model_dump()
    for key in path.split("."):
        if isinstance(partial, dict) and isinstance(full, dict):
            if key not in full:
                return
            if key not in partial:
                partial[key] = deepcopy(full[key])
            partial, full = partial[key], full[key]
        elif isinstance(partial, list) and isinstance(full, list):
            try:
                index = int(key)
            except ValueError:
                return
            if not 0 <= index < min(len(partial), len(full)):
                return
            partial, full = partial[index], full[index]
        else:
            return


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
    d = dump_authored(spec)
    _materialize_path(d, spec, dotted)
    _set_nested_safe(d, dotted, value)
    try:
        return ProjectInputSpec.model_validate(d)
    except ValidationError as exc:
        # Surface the offending sweep parameter so multi-sweep campaign debugging
        # points at the dotted path + value, not just an opaque Pydantic trace
        # (#535). Original ValidationError stays accessible via __cause__.
        raise ValueError(
            f"Sweep parameter {dotted!r} produced invalid spec for value {value!r}"
        ) from exc
