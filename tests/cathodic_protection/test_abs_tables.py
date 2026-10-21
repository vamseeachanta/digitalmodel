# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Metadata-aware checks of supported ABS lookups against existing CSVs.

Source: ABS Guidance Notes on Cathodic Protection of Ships, December 2017,
Section 2 Tables 1, 3, 4, 5 and Section 3 Tables 2, 4. The existing dataset
PROVENANCE.md records the transcription basis. No new table values are added.
"""

import csv
from pathlib import Path

import pytest

from digitalmodel.cathodic_protection import abs_ships_tables as tbl
from digitalmodel.citations import Citation, CitedValue
from digitalmodel.citations.table_verification import verify_table_value

DATASETS = (
    Path(__file__).resolve().parents[1] / "fixtures" / "test_vectors"
    / "cathodic_protection" / "datasets" / "abs-gn-ships" / "2017-12"
)
CITATION_FIXTURES = Path(__file__).resolve().parents[1] / "citations" / "fixtures"


def _rows(filename):
    with (DATASETS / filename).open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle))


def _verify(actual, value, units, section, note):
    expected = CitedValue(
        value=float(value), units=units,
        citation=Citation(
            code_id="abs-gn-ships", publisher="ABS", revision="2017-12",
            section=section,
            wiki_path="wikis/engineering-standards/wiki/standards/abs-gn-ships-cathodic-protection-2017.md",
            note=note,
        ),
    )
    verify_table_value(actual, expected, repo_root=CITATION_FIXTURES)


@pytest.mark.parametrize("row", [r for r in _rows("section-2-table-4.csv") if r["durability"] != "all"])
def test_coating(row):
    key = row["durability"]
    low, high = tbl.coating_breakdown_range(key)
    _verify(low, row["low_percent"], "%/yr", "Section 2, Table 4", f"{key} durability lower rate")
    _verify(high, row["high_percent"], "%/yr", "Section 2, Table 4", f"{key} durability upper rate")


@pytest.mark.parametrize("row", _rows("section-2-table-5.csv"))
def test_average_density(row):
    periods = {"<=18": "up_to_18_months", "19-36": "19_to_36_months", "37-60": "37_to_60_months"}
    period = periods[row["docking_period_months"]]
    low, high = tbl.average_coated_current_density(period)
    _verify(low, row["current_low_mA_m2"], "mA/m2", "Section 2, Table 5", f"{period} lower bound")
    _verify(high, row["current_high_mA_m2"], "mA/m2", "Section 2, Table 5", f"{period} upper bound")


SITUATIONS = {
    "V <= 1 m/s without tidal influence": "up_to_1_m_s_no_tide",
    "V <= 1 m/s with tidal influence": "up_to_1_m_s_with_tide",
    "1 < V < 10 m/s": "1_to_10_m_s",
    "V >= 10 m/s": "at_least_10_m_s",
    "Vessels in ice": "ice",
}


@pytest.mark.parametrize("row", [r for r in _rows("section-2-table-3.csv") if r["situation"] in SITUATIONS])
@pytest.mark.parametrize("condition", ("bare", "coated"))
def test_design_density(row, condition):
    situation = SITUATIONS[row["situation"]]
    low, high = tbl.design_current_density(situation, condition)
    note = f"{situation} {condition} steel"
    _verify(low, row[f"{condition}_low_mA_m2"], "mA/m2", "Section 2, Table 3", f"{note} lower bound")
    _verify(high, row[f"{condition}_high_mA_m2"], "mA/m2", "Section 2, Table 3", f"{note} upper bound")


@pytest.mark.parametrize("row", [r for r in _rows("section-3-table-4.csv") if r["alloy"] in {"A1", "A2", "A3", "A4"}])
def test_aluminium(row):
    key = row["alloy"]
    potential, capacity = tbl.aluminium_properties(key)
    _verify(potential, row["closed_circuit_potential_V"], "V", "Section 3, Table 4", f"{key} closed-circuit potential")
    _verify(capacity, row["capacity_Ah_kg"], "Ah/kg", "Section 3, Table 4", f"{key} practical capacity")


@pytest.mark.parametrize("row", _rows("section-3-table-2.csv"))
def test_zinc(row):
    key = row["alloy"]
    # Select the lower end of the existing CSV temperature range, not a limit
    # inferred from the implementation (ABS December 2017, Section 3 Table 2).
    temperature = float(row["temperature_C"].split("-")[0])
    potential, capacity = tbl.zinc_properties(key, temperature_c=temperature)
    _verify(potential, row["closed_circuit_potential_V"], "V", "Section 3, Table 2", f"{key} closed-circuit potential")
    _verify(capacity, row["capacity_Ah_kg"], "Ah/kg", "Section 3, Table 2", f"{key} practical capacity")


@pytest.mark.parametrize(("anaerobic", "row"), zip((False, True), _rows("section-2-table-1.csv")[:2], strict=True))
def test_protection(row, anaerobic):
    _verify(tbl.protection_potential(anaerobic), row["minimum_negative_potential_V"],
            "V", "Section 2, Table 1", "steel protection potential")
