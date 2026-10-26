# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Explicit synthetic data for material API tests, never an engineering basis."""
from pathlib import Path

import pytest

from digitalmodel.materials.catalog import load_catalog, use_catalog


@pytest.fixture(autouse=True)
def synthetic_catalog():
    catalog = load_catalog(Path(__file__).parent / "fixtures" / "catalog.json")
    with use_catalog(catalog, allow_synthetic=True):
        yield catalog
