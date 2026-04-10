"""Shared fixtures for drilling-riser adapter tests."""

from __future__ import annotations

import csv
from pathlib import Path

import pytest

_CSV_PATH = (
    Path(__file__).resolve().parents[3]
    / "worldenergydata"
    / "data"
    / "modules"
    / "vessel_fleet"
    / "curated"
    / "drilling_riser_components.csv"
)


@pytest.fixture()
def csv_path() -> Path:
    """Return the absolute path to the drilling-riser components CSV."""
    assert _CSV_PATH.exists(), f"CSV not found: {_CSV_PATH}"
    return _CSV_PATH


@pytest.fixture()
def all_csv_records(csv_path: Path) -> list[dict[str, str]]:
    """Load all drilling-riser component records as raw CSV dicts."""
    with csv_path.open(newline="", encoding="utf-8") as handle:
        records = list(csv.DictReader(handle))
    assert len(records) == 36, f"Expected 36 records, got {len(records)}"
    return records


@pytest.fixture()
def raw_csv_rows(all_csv_records: list[dict[str, str]]) -> list[dict[str, str]]:
    """Backward-compatible alias for raw string-valued CSV rows."""
    return all_csv_records
