# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Synthetic offline CMEMS contract exercise; no provider data or connectivity.

This internal schema is not a Copernicus Marine product response. The adapter
accepts an already decoded mapping and performs no file, network or cache I/O.
"""

from collections.abc import Mapping
from copy import deepcopy
from datetime import datetime, timezone
from math import isfinite
import re
from typing import Any


_PROVENANCE = {
    "origin": "synthetic",
    "schema_version": "cmems-synthetic-v1",
    "purpose": "offline-contract-exercise",
    "third_party_data": False,
    "units": {"significant_wave_height": "m", "peak_period": "s"},
}
_RECORD_FIELDS = {"timestamp", "significant_wave_height", "peak_period"}
_TIMESTAMP = re.compile(
    r"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(?:\.\d{1,6})?"
    r"(?:Z|[+-](?:[01]\d|2[0-3]):[0-5]\d)",
    re.ASCII,
)


def _fields(value: object, expected: set[str], message: str) -> Mapping[str, Any]:
    """Reject missing and extra fields without reporting caller contents."""
    if not isinstance(value, Mapping) or set(value) != expected:
        raise ValueError(message)
    return value


def _timestamp(value: object) -> datetime:
    """Require an ISO timestamp with seconds and an explicit UTC offset."""
    if not isinstance(value, str) or not _TIMESTAMP.fullmatch(value):
        raise ValueError("invalid timestamp")
    try:
        return datetime.fromisoformat(value).astimezone(timezone.utc)
    except (ValueError, OverflowError):
        raise ValueError("invalid timestamp") from None


def _wave_value(value: object, *, period: bool) -> float | None:
    """Preserve nulls; accept finite JSON numbers in metres or seconds only."""
    if value is None:
        return None
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise ValueError("invalid wave value")
    try:
        result = float(value)
    except OverflowError:
        raise ValueError("invalid wave value") from None
    if not isfinite(result) or result < 0 or (period and result == 0):
        raise ValueError("invalid wave value")
    return result


class OfflineCMEMSFixture:
    """Validate synthetic wave samples for the existing scatter reducer.

    Validation completes before any records are exposed. Only the documented
    synthetic provenance and fields are accepted; provider metadata, URLs and
    credential fields have no place in this schema. No mean-to-peak period or
    unit conversion is performed. Declaring synthetic origin does not establish
    rights to real data: provider-derived inputs must never use this adapter.
    """

    def __init__(self, payload: Mapping[str, Any]) -> None:
        """Snapshot an already decoded synthetic fixture.

        Args:
            payload: Mapping with exact provenance and a list of wave records.

        Raises:
            ValueError: If fields, provenance, units or samples are invalid.
        """
        payload = _fields(payload, {"provenance", "records"}, "invalid fixture fields")
        provenance = _fields(
            payload["provenance"], set(_PROVENANCE), "invalid synthetic provenance"
        )
        if provenance != _PROVENANCE or provenance["third_party_data"] is not False:
            raise ValueError("invalid synthetic provenance")
        samples = payload["records"]
        if not isinstance(samples, list):
            raise ValueError("invalid records list")
        records: list[dict[str, Any]] = []
        for sample in samples:
            sample = _fields(sample, _RECORD_FIELDS, "invalid record fields")
            records.append(
                {
                    "timestamp": _timestamp(sample["timestamp"]),
                    "wave_height": _wave_value(
                        sample["significant_wave_height"], period=False
                    ),
                    "wave_period": _wave_value(sample["peak_period"], period=True),
                    "synthetic": True,
                    "provenance": deepcopy(dict(provenance)),
                }
            )
        self._records = records

    def records(self) -> list[dict[str, Any]]:
        """Return independent normalized records with per-record provenance."""
        return deepcopy(self._records)
