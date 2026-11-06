# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Strict external JSON loader; no default path and no repository fallback."""

from __future__ import annotations

import json
from decimal import Decimal
from pathlib import Path

from digitalmodel.citations import Citation

from .costing import CostBasis, Rate, _validate_basis


def _unique_object(pairs: list[tuple]) -> dict:
    result = {}
    for key, value in pairs:
        if key in result:
            raise ValueError("Duplicate JSON field")
        result[key] = value
    return result


def load_cost_basis(path: str | Path) -> CostBasis:
    """Load an explicitly configured external basis, never a vendored table.

    Citation pages are resolved at calculation time, not trusted just because
    loading succeeded. Errors omit the supplied path and document contents.
    Public synthetic examples shall be copied to external scratch before loading.
    """
    try:
        resolved = Path(path).resolve()
        public_root = Path(__file__).resolve().parents[3]
        if resolved.is_relative_to(public_root):
            raise ValueError("Cost basis must be external to the public repository")
        text = resolved.read_text(encoding="utf-8")
    except (OSError, RuntimeError, UnicodeError):
        raise ValueError("External cost basis is unavailable") from None
    try:
        data = json.loads(text, parse_float=Decimal, object_pairs_hook=_unique_object)
        expected = {"schema_version", "currency", "revision", "data_kind", "rates"}
        if not isinstance(data, dict) or set(data) != expected:
            raise ValueError("Unexpected cost-basis fields")
        if type(data["schema_version"]) is not int or data["schema_version"] != 1:
            raise ValueError("Unsupported cost-basis schema version")
        if not isinstance(data["rates"], list):
            raise ValueError("rates must be a list")
        rates = []
        for row in data["rates"]:
            if not isinstance(row, dict) or set(row) != {
                "component",
                "unit",
                "value",
                "citation",
            }:
                raise ValueError("Unexpected rate fields")
            rates.append(
                Rate(
                    row["component"],
                    row["unit"],
                    row["value"],
                    Citation(**row["citation"]),
                )
            )
        basis = CostBasis(
            data["currency"], data["revision"], data["data_kind"], tuple(rates)
        )
        _validate_basis(basis)
        return basis
    except (ValueError, TypeError, KeyError):
        raise ValueError(
            "Invalid external cost basis; check schema, units and citations"
        ) from None
