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
def raw_csv_rows(csv_path: Path) -> list[dict[str, str]]:
    """Load every row from the CSV as raw string-valued dicts."""
    with open(csv_path, newline="", encoding="utf-8") as fh:
        return list(csv.DictReader(fh))
