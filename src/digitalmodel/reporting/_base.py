#!/usr/bin/env python3
"""Reporting block library — base types.

ABOUTME: Marker base model and the report-block protocol for the reusable
reporting block library (``digitalmodel.reporting``). Generalized from the
diffraction report-builder pattern (#1018).
"""

from __future__ import annotations

from typing import Callable, Protocol, runtime_checkable

from pydantic import BaseModel, ConfigDict


class ReportDataModel(BaseModel):
    """Marker base for report envelope (data) models.

    Subclass this for any pydantic model that serves as the data envelope
    passed to report block builders. ``arbitrary_types_allowed`` is enabled so
    envelopes may carry numpy arrays or other non-pydantic field types without
    extra per-model configuration.

    This is a *marker* base only — it intentionally adds no fields, validators
    or serialization behaviour beyond the permissive ``model_config``.
    """

    model_config = ConfigDict(arbitrary_types_allowed=True)


@runtime_checkable
class ReportBlock(Protocol):
    """Structural protocol for a report block.

    Any object exposing ``render(self, data, **kwargs) -> str`` satisfies it.
    Because it is ``runtime_checkable`` you may use
    ``isinstance(obj, ReportBlock)`` to detect block-shaped objects at runtime.
    """

    def render(self, data, **kwargs) -> str:  # pragma: no cover - protocol stub
        ...


# Diffraction-style builders are plain functions ``(data, **kwargs) -> str``;
# the backbone must accept either a ReportBlock or a plain callable, so we
# expose this alias as the canonical "builder is just a function" type.
ReportBlockFn = Callable[..., str]
