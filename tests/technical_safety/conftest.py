"""Synthetic criteria helpers; no current standard limits are asserted."""

import pytest

from digitalmodel.foam_system.sizing import Citation
from digitalmodel.technical_safety.criteria import CitedValue, EditionCriteria


@pytest.fixture
def project_criteria():
    def make(edition="test-A", energy=14.0, jet=250.0, pool=100.0, reduced=60.0):
        citation = Citation("Synthetic screening basis", edition, "arithmetic fixture")
        values = {
            "supply_vessel_collision_energy": CitedValue(
                value=energy, unit="MJ", citation=citation
            ),
            "riser_collision_energy": CitedValue(
                value=energy, unit="MJ", citation=citation
            ),
            "jet_peak": CitedValue(value=jet, unit="kW/m2", citation=citation),
            "jet_average": CitedValue(value=200.0, unit="kW/m2", citation=citation),
            "jet_water_average": CitedValue(
                value=120.0, unit="kW/m2", citation=citation
            ),
            "pool_fuel_controlled_peak": CitedValue(
                value=pool, unit="kW/m2", citation=citation
            ),
            "pool_fuel_controlled_average": CitedValue(
                value=pool, unit="kW/m2", citation=citation
            ),
            "pool_fuel_controlled_water_average": CitedValue(
                value=reduced, unit="kW/m2", citation=citation
            ),
            "pool_ventilation_controlled_peak": CitedValue(
                value=180.0, unit="kW/m2", citation=citation
            ),
        }
        return EditionCriteria(
            standard=citation.standard, edition=edition, status="project", values=values
        )

    return make
