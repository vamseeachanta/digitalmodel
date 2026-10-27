# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Run the synthetic data checks described in docs/domains/metocean/data-values.md.

Inputs are deliberately chosen arithmetic fixtures, not measured properties.
The output directory must be supplied explicitly; all generated files go there.
"""

import argparse
import math
from pathlib import Path

import numpy as np
import pandas as pd

from digitalmodel.data.hydrostatic import read_pressure_csv, write_hydrostatic_csv
from digitalmodel.data_models.pipe_section import PipeSection
from digitalmodel.data_systems.data.catalog import CatalogEntry, DataCatalog
from digitalmodel.hydrodynamics.well_datasets.shear_flow_loader import (
    vorticity_from_velocity,
)


def run_example(output_dir: Path) -> dict[str, float]:
    """Return answers after pressure, geometry and catalogue round trips."""
    output_dir.mkdir(parents=True, exist_ok=True)
    pressure_file = output_dir / "pressure.csv"
    # OpenStax University Physics Vol. 1 (2016), §14.1, Eq. 14.9.
    # Synthetic rho=1000, g=10, h=(0,2,5) gives p=(0,20000,50000) Pa.
    write_hydrostatic_csv(pressure_file, [0, 2, 5], density_kg_m3=1000, gravity_m_s2=10)
    pressures = read_pressure_csv(pressure_file)

    # OpenStax Precalculus 2e (2021), §5.1: subtract pi*r² for radii 2 and 1.
    section_file = output_dir / "section.json"
    section_file.write_text(PipeSection(4, 2).to_json(), encoding="utf-8")
    section = PipeSection.from_json(section_file.read_text(encoding="utf-8"))

    # OpenStax University Physics Vol. 1 (2016), §14.1, Eq. 14.3: F=p*A.
    # Chosen parallel panels: 20000*3 + 50000*2 = 160000 N.
    panels = pd.DataFrame(
        {
            "pressure_pa": [row["pressure_pa"] for row in pressures[1:]],
            "area_m2": [3, 2],
        }
    )
    panels.to_csv(output_dir / "panels.csv", index=False)
    catalog_file = output_dir / "catalog.yaml"
    catalog = DataCatalog(catalog_file, base_path=output_dir)
    catalog.add_entry(
        CatalogEntry(
            name="panels",
            file="panels.csv",
            format="csv",
            version="1.0.0",
            description="Synthetic constant-pressure parallel panels in SI units",
        )
    )
    catalog.save()
    loaded = DataCatalog.load_catalog(catalog_file, base_path=output_dir).load("panels")
    force = float((loaded["pressure_pa"] * loaded["area_m2"]).sum())

    # OpenStax Calculus Vol. 3 (2016), §6.5, Eq. 6.17: dv/dx-du/dy.
    # Chosen u=3*y+2*x+7, v=5*x-4*y-1 gives omega_z=5-3=2 s^-1.
    x, y = np.meshgrid(np.arange(4) * 0.5, np.arange(3) * 2)
    velocity = np.stack((3 * y + 2 * x + 7, 5 * x - 4 * y - 1))
    vorticity = vorticity_from_velocity(velocity, dx_m=0.5, dy_m=2)

    # Exact arithmetic fixtures; no design acceptance limits are applied.
    assert [row["pressure_pa"] for row in pressures] == [0, 20000, 50000]
    assert section.area_m2 == 3 * math.pi
    assert force == 160000
    np.testing.assert_array_equal(vorticity, np.full((1, 3, 4), 2.0))
    return {
        "deepest_pressure_pa": pressures[-1]["pressure_pa"],
        "pipe_area_m2": section.area_m2,
        "panel_force_n": force,
        "vorticity_per_s": float(vorticity[0, 0, 0]),
    }


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("output_dir", type=Path)
    answers = run_example(parser.parse_args().output_dir)
    for quantity, value in answers.items():
        print(f"{quantity}: {value:.1f}")
