# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Assert engineering values through the existing catalogue persistence API."""

import pandas as pd
import pytest

from digitalmodel.data_systems.data.catalog import CatalogEntry, DataCatalog


@pytest.mark.parametrize("format_name", ["csv", "json"])
def test_catalogue_round_trip_preserves_integrated_force(tmp_path, format_name):
    """OpenStax University Physics Vol. 1 (2016), §14.1, Eq. 14.3: p=F/A.

    Chosen uniform panel pressures are 20000 and 50000 Pa, with corresponding
    areas 3 and 2 m². For parallel panels the independent hand sum is
    F = 20000*3 + 50000*2 = 160000 N. No trapezoidal or spatial interpolation
    is involved: each pressure is constant over its associated panel.
    """
    table = pd.DataFrame({"pressure_pa": [20000, 50000], "area_m2": [3, 2]})
    payload = tmp_path / f"panels.{format_name}"
    if format_name == "csv":
        table.to_csv(payload, index=False)
    else:
        table.to_json(payload, orient="records")
    catalog_file = tmp_path / "catalog.yaml"
    catalog = DataCatalog(catalog_file, base_path=tmp_path)
    catalog.add_entry(
        CatalogEntry(
            name="panels",
            file=payload.name,
            format=format_name,
            version="1.0.0",
            description="Synthetic constant-pressure panels in SI units",
            columns=["pressure_pa", "area_m2"],
        )
    )
    catalog.save()
    restored = DataCatalog.load_catalog(catalog_file, base_path=tmp_path)
    loaded = pd.DataFrame(restored.load("panels"))
    assert loaded["pressure_pa"].tolist() == [20000, 50000]
    assert loaded["area_m2"].tolist() == [3, 2]
    assert (loaded["pressure_pa"] * loaded["area_m2"]).sum() == 160000
