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
from pydantic.fields import FieldInfo

from .root import ProjectInputSpec


def _differs_from_default(field: FieldInfo, value: Any, model: BaseModel) -> bool:
    """Whether an unset container no longer equals its declared default."""
    try:
        default = field.get_default(
            call_default_factory=True, validated_data=dict(model.__dict__)
        )
    except TypeError:  # pydantic < 2.10 takes no validated_data argument
        default = field.get_default(call_default_factory=True)
    return _plain(value) != _plain(default)


def _plain(value: Any) -> Any:
    """Comparable form: models as dumps, tuples as lists."""
    if isinstance(value, BaseModel):
        return value.model_dump()
    if isinstance(value, (list, tuple)):
        return [_plain(item) for item in value]
    if isinstance(value, dict):
        return {key: _plain(item) for key, item in value.items()}
    return value


def _mark_edited_ancestors(value: Any) -> bool:
    """Mark a field set when it holds an edit; return whether ``value`` is set.

    Two kinds of edit record no assignment on the field that holds them, so
    ``exclude_unset`` would drop them:

    - an attribute edit below an unset parent
      (``spec.environment.water.depth = x`` records the edit on ``water``);
    - an in-place edit inside a default list or dictionary
      (``spec.simulation.stages[1] = 30``, ``properties["Cd"] = ...``).

    An unset field is marked when anything below it is set, or when it is a
    container that no longer equals its declared default. Unedited defaults
    stay unset. No validation is run.
    """
    if isinstance(value, BaseModel):
        for name, field in type(value).model_fields.items():
            item = getattr(value, name, None)
            below = _mark_edited_ancestors(item)
            if name in value.model_fields_set:
                continue
            if below or (
                isinstance(item, (list, tuple, dict))
                and _differs_from_default(field, item, value)
            ):
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
