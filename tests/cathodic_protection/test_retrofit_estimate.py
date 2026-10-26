# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Independent checks of the published-jacket retrofit estimate.

Published inputs: Xu (2023), Study on Implementation and Optimization of
Cathodic Protection for Offshore Oil Platform Jacket, Engineering Advances
3(3), pp. 241–245, Tables 1–3 and p. 244, Section 5.
Assumed inputs: the declared basis in the published-jacket fixture, based on
previous experience. No expected count is obtained from the implementation.
"""

import copy
import csv
import io
import math
from pathlib import Path

import pytest
import yaml

from digitalmodel.cathodic_protection.retrofit_estimate import (
    estimate_published_jacket_retrofit,
    retrofit_estimate_csv,
)

INPUT = Path(__file__).parents[1] / "fixtures/cathodic_protection/workflow_inputs/published_jacket_84m.yml"
CHART = Path(__file__).parents[2] / "docs/domains/cathodic_protection/reports/published-jacket-84m/retrofit-estimate.csv"


@pytest.fixture
def design():
    """Return the existing published areas, design densities and assumed basis."""
    return yaml.safe_load(INPUT.read_text(encoding="utf-8"))


def estimate(design, order="initial_final_mean"):
    """Select the explicitly stated phase-order assumption for this estimate."""
    return estimate_published_jacket_retrofit(design, phase_order=order)


def hand_outputs():
    """Existing section-periphery model: B401 (2021), Sec. 7.8, Table 10-7.

    Xu (2023), p. 242 Section 2: length 2.020 m, nominal mass 323.4 kg,
    resistivity 0.22 ohm.m and driving voltage 0.25 V. Assumed net mass,
    utilisation 0.90, based on previous experience; depletion keeps length
    and similar section shape. The published section is interpreted as a
    symmetric trapezoid: widths 240 and 270 mm, height 270 mm (p. 242). The kernel's long stand-off formula
    is R = rho/(2*pi*L) * (ln(4*L/r)-1); output = voltage/R.
    """
    perimeter = (240 + 270 + 2 * math.hypot(270, (270 - 240) / 2)) / 1000
    radius = perimeter / (2 * math.pi)
    return tuple(
        0.25 / (0.22 / (2 * math.pi * 2.020) * (math.log(4 * 2.020 / r) - 1))
        for r in (radius, radius * math.sqrt(1 - 0.90))
    )


def test_upper_zone_hand_calculation_estimate(design):
    """Xu (2023), p. 242 Tables 1–2; p. 244 Table 3, stated phase order.

    The estimate uses the explicit initial/final/mean assumption, rather than
    the paper's printed initial/maintain/late headings. Upper area 3726.8 m2:
    initial estimate = (170-150)/1000 * 3726.8 = 74.536 A;
    mean estimate = (110-70)/1000 * 3726.8 = 149.072 A;
    final signed estimate = (80-100)/1000 * 3726.8 = -74.536 A.
    No negative current is credited to an added-anode estimate.
    Mass estimate = 149.072*25*8760/(2000*0.90) kg;
    count estimate = ceil(mass/323.4) = 57, mass governed.
    Life, capacity, utilisation and net mass are the declared assumed basis;
    8760 h/year is the existing kernel conversion, not a source table value.
    """
    zone = estimate(design)["zones"]["upper-bare-seawater"]
    assert zone["extra_current_A_estimate"] == pytest.approx(
        {"initial": 74.536, "mean": 149.072, "final": -74.536}
    )
    assert zone["sizing_current_A_estimate"]["final"] == 0
    mass = 149.072 * 25 * 8760 / (2000 * 0.90)
    assert zone["anodes_estimate"]["net_mass_kg_estimate"] == pytest.approx(mass)
    assert zone["anodes_estimate"]["count_by_mass_estimate"] == math.ceil(mass / 323.4) == 57
    initial, _ = hand_outputs()
    assert zone["anodes_estimate"]["count_by_initial_current_estimate"] == math.ceil(74.536 / initial)
    assert zone["anodes_estimate"]["count_by_final_current_estimate"] == 0
    assert zone["anodes_estimate"]["governing_check_estimate"] == "mass"


def test_design_baseline_unchanged_on_declared_assumed_basis(design):
    """Xu p. 242 Tables 1–2 and Section 2; mass rule on assumed basis.

    Mean = 652.9*.020 + 3726.8*.070 + 15737*.060 + 10814.8*.020 + 9*2
    = 1452.450 A; ceil(1452.450*25*8760/(2000*.90*323.4)) = 547.
    Installed count 569 is published at p. 242 Section 2, not a sized value.
    """
    original = copy.deepcopy(design)
    result = estimate(design)
    baseline = result["design_on_declared_assumed_basis"]
    assert baseline["count_by_mass"] == math.ceil(1452.450 * 25 * 8760 / (2000 * .90 * 323.4)) == 547
    initial, final = hand_outputs()
    assert baseline["count_by_initial_current"] == math.ceil(2688.285 / initial) == 537
    assert baseline["count_by_final_current"] == math.ceil(1892.052 / final) == 529
    assert baseline["required_anodes"] == 547
    assert baseline["installed_anodes"] == 569
    assert baseline["governing_check"] == "mass"
    assert design == original


@pytest.mark.parametrize("order", ["initial_final_mean", "initial_mean_final"])
def test_total_is_sized_from_unrounded_pooled_demand_estimate(design, order):
    """Xu Tables 1–3 differences; independent geometry and mass arithmetic."""
    result = estimate(design, order)
    mean_delta, final_delta = (621.182, -231.906) if order == "initial_final_mean" else (194.638, 194.638)
    total = result["total_estimate"]
    assert total["extra_current_A_estimate"] == pytest.approx(
        {"initial": 389.276, "mean": mean_delta, "final": final_delta}
    )
    initial, final = hand_outputs()
    checks = {
        "mass": math.ceil(mean_delta * 25 * 8760 / (2000 * .90 * 323.4)),
        "initial": math.ceil(389.276 / initial),
        "final": math.ceil(max(final_delta, 0) / final),
    }
    counts = total["anodes_estimate"]
    for check, count in checks.items():
        suffix = "mass" if check == "mass" else f"{check}_current"
        assert counts[f"count_by_{suffix}_estimate"] == count
    assert counts["additional_anodes_estimate"] == max(checks.values())
    assert counts["governing_check_estimate"] == max(checks, key=checks.get)


def test_only_deep_zone_receives_pooled_anodes_estimate(design):
    """Xu (2023), p. 244 Section 5: both deep levels lie in the lower zone.

    An estimate contribution is not an installation allocation. The initial
    lower-zone demand count estimate coincides with a retired quantity; the
    calculation does not reuse that quantity as a retrofit total estimate.
    """
    result = estimate(design)
    total = result["total_estimate"]["anodes_estimate"]["additional_anodes_estimate"]
    assert total == math.ceil(621.182 * 25 * 8760 / (2000 * .90 * 323.4))
    for zone_id, zone in result["zones"].items():
        assert zone["assigned_anodes_estimate"] == (total if zone_id == "lower-bare-seawater" else 0)
    assert result["placement_estimate"]["depths_m_estimate"] == [70.5, 84.1]
    assert result["placement_estimate"]["per_level_allocation_estimate"] is None
    assert result["placement_estimate"]["existing_anodes_reallocated"] is False
    assert result["zones"]["lower-bare-seawater"]["anodes_estimate"]["count_by_initial_current_estimate"] == math.ceil(314.740 / hand_outputs()[0])


def test_unaffected_zones_and_connected_allowance_have_no_shortfall_estimate(design):
    """Table 3 changes bare seawater only; Table 2 well allowance stays 9*2 A."""
    result = estimate(design)
    for name in ("coated-seawater", "buried-steel"):
        zone = result["zones"][name]
        assert zone["extra_current_A_estimate"] == {"initial": 0, "mean": 0, "final": 0}
        assert zone["anodes_estimate"]["additional_anodes_estimate"] == 0
    assert result["unchanged_connected_allowance_A"] == 18


def test_phase_mapping_is_explicit_and_source_headings_retained(design):
    """Xu p. 244 Table 3 headings are initial, maintain, late, in that order."""
    with pytest.raises(TypeError):
        estimate_published_jacket_retrofit(design)
    with pytest.raises(ValueError, match="phase_order"):
        estimate(design, "unknown")
    assumed = estimate(design)
    paper = estimate(design, "initial_mean_final")
    assert assumed["phase_order"] == "initial_final_mean"
    assert "initial / maintain / late" in assumed["phase_order_basis"]
    assert "assumed" in assumed["phase_order_basis"]
    assert paper["zones"]["upper-bare-seawater"]["subtropical_density_mA_m2"] == {"initial": 170, "mean": 80, "final": 110}


def test_estimate_labels_and_unevaluated_items_are_explicit(design):
    result = estimate(design)
    assert result["status"] == "ESTIMATE"
    assert "estimate on stated assumptions, not an acceptance of an installed design" in result["basis"]
    assert set(result["not_evaluated"]) == {
        "per-level allocation", "electrical continuity", "measured depletion",
        "the date the retrofit is needed by", "confirmation after installation",
    }
    for row in [*result["zones"].values(), result["total_estimate"], result["placement_estimate"]]:
        assert "estimate" in row["label"].lower()
    for sentence in result["assumptions"]:
        assert "estimate" in sentence.lower()
    for header in csv.DictReader(io.StringIO(retrofit_estimate_csv(result))).fieldnames:
        if "anodes" in header or "extra" in header:
            assert "estimate" in header


def test_chart_csv_matches_every_output_value(design):
    """Rebuilding the chart preserves labels and numerical values within tolerance."""
    result = estimate(design)
    rendered = retrofit_estimate_csv(result)
    rows = list(csv.DictReader(io.StringIO(rendered)))
    stored = list(csv.DictReader(io.StringIO(CHART.read_text(encoding="utf-8"))))
    assert len(stored) == len(rows)
    for actual, expected in zip(rows, stored):
        for key, value in actual.items():
            if key.endswith(("_A", "_A_estimate", "_m2")):
                assert float(value) == pytest.approx(float(expected[key]), rel=1e-12, abs=1e-12)
            else:
                assert value == expected[key]
    assert len(rows) == len(result["zones"])
    for row in rows:
        zone = result["zones"][row["zone"]]
        assert "estimate" in row["label"].lower()
        assert row["phase_order"] == result["phase_order"]
        assert row["source"] == result["source"]
        assert row["declared_assumed_basis"] == result["declared_assumed_basis"]
        assert "assumed, based on previous experience" in row["declared_assumed_basis"]
        assert float(row["area_m2"]) == zone["area_m2"]
        for phase in ("initial", "mean", "final"):
            assert float(row[f"design_{phase}_A"]) == zone["design_current_A"][phase]
            assert float(row[f"subtropical_{phase}_A_estimate"]) == zone["subtropical_current_A_estimate"][phase]
            assert float(row[f"extra_{phase}_A_estimate"]) == zone["extra_current_A_estimate"][phase]
        assert int(row["demand_anodes_estimate"]) == zone["anodes_estimate"]["additional_anodes_estimate"]
        assert int(row["assigned_anodes_estimate"]) == zone["assigned_anodes_estimate"]
        assert row["governing_check_estimate"] == zone["anodes_estimate"]["governing_check_estimate"]


@pytest.mark.parametrize("section", ["missing", "2021 mm × (240+270) mm × 270 mm"])
def test_unsupported_section_stops_the_estimate(design, section):
    """The source section is required; no mass-radius fallback is permitted."""
    design["inputs"]["structure"]["verification_evidence"]["published_anode_dimensions"]["expression"] = section
    with pytest.raises(ValueError, match="section|length"):
        estimate(design)


def test_inconsistent_radius_stops_the_estimate(design):
    """Do not silently replace an explicitly supplied, different geometry."""
    design["inputs"]["anode"]["radius_m"] = math.sqrt(323.4 / (math.pi * 2.020 * 2750))
    with pytest.raises(ValueError, match="radius"):
        estimate(design)


def test_changed_assumptions_do_not_keep_the_declared_basis_label(design):
    """A different life cannot be labelled as the declared assumed basis."""
    design["inputs"]["design_data"]["design_life"] *= 2
    with pytest.raises(ValueError, match="declared assumed basis"):
        estimate(design)


@pytest.mark.parametrize("zone_index", range(4))
@pytest.mark.parametrize("field", ["area_m2", "initial", "mean", "final"])
def test_changed_zone_inputs_cannot_retain_published_attribution(design, zone_index, field):
    """All Table 1 areas and Table 2 phase densities belong to the fixed case."""
    zone = design["inputs"]["structure"]["zones"][zone_index]
    values = zone if field == "area_m2" else zone["effective_current_density_A_m2"]
    values[field] *= 2
    with pytest.raises(ValueError, match="published case"):
        estimate(design)


@pytest.mark.parametrize("section", [
    "4040 mm × (240+270) mm × 270 mm",
    "2020 mm × (480+270) mm × 270 mm",
    "2020 mm × (240+540) mm × 270 mm",
    "2020 mm × (240+270) mm × 540 mm",
])
def test_numeric_unpublished_sections_stop_before_calculation(design, section, monkeypatch):
    """Matching caller length cannot qualify unpublished numeric geometry."""
    design["inputs"]["structure"]["verification_evidence"]["published_anode_dimensions"]["expression"] = section
    if section.startswith("4040"):
        design["inputs"]["anode"]["length_m"] = 4.04

    def unexpected_calculation(*args, **kwargs):
        pytest.fail("Unpublished geometry reached the calculation adapter")

    monkeypatch.setattr(
        "digitalmodel.cathodic_protection.retrofit_estimate.run_cathodic_protection",
        unexpected_calculation,
    )
    with pytest.raises(ValueError, match="published case|section"):
        estimate(design)


@pytest.mark.parametrize("section, field, value", [
    ("design_data", "design_life", 26.0),
    ("design_data", "edition", "2010"),
    ("design_data", "structure_type", "monopile"),
    ("environment", "seawater_temperature_C", 20.0),
    ("environment", "seawater_resistivity_ohm_m", 0.44),
    ("environment", "driving_voltage_V", 0.5),
    ("anode", "material", "zinc"),
    ("anode", "individual_anode_mass_kg", 323.4001),
    ("anode", "count", 569.5),
    ("anode", "utilization_factor", 0.8),
    ("anode", "density_kg_m3", 2600.0),
])
def test_changed_scalar_basis_cannot_retain_published_attribution(design, section, field, value):
    """Unrounded supplied values and unsupported overrides must be checked."""
    design["inputs"][section][field] = value
    with pytest.raises(ValueError, match="published case|declared assumed basis"):
        estimate(design)


@pytest.mark.parametrize("field, value", [("count", 18), ("current_A_each", 4.0)])
def test_changed_connected_allowance_cannot_retain_published_attribution(design, field, value):
    """Table 2 supplies nine connected items at 2 A each, not just a total."""
    design["inputs"]["structure"]["connected_current_allowances"][0][field] = value
    with pytest.raises(ValueError, match="published case"):
        estimate(design)


@pytest.mark.parametrize("field", ["zones", "connected_current_allowances"])
def test_duplicate_case_rows_stop_the_estimate(design, field):
    rows = design["inputs"]["structure"][field]
    rows.append(copy.deepcopy(rows[0]))
    with pytest.raises(ValueError, match="published case"):
        estimate(design)


@pytest.mark.parametrize("value", [None, True, "0.22", float("nan"), float("inf"), -float("inf")])
def test_invalid_resistivity_stops_with_a_basis_error(design, value):
    design["inputs"]["environment"]["seawater_resistivity_ohm_m"] = value
    with pytest.raises(ValueError, match="published case"):
        estimate(design)


@pytest.mark.parametrize("section, field", [
    ("design_data", "design_life"),
    ("design_data", "edition"),
    ("environment", "seawater_resistivity_ohm_m"),
    ("anode", "individual_anode_mass_kg"),
    ("anode", "count"),
])
def test_missing_basis_does_not_use_adapter_defaults(design, section, field):
    del design["inputs"][section][field]
    with pytest.raises(ValueError, match="published case"):
        estimate(design)


@pytest.mark.parametrize("field", ["components", "anode_families", "riser_base_assessment"])
def test_other_routes_cannot_retain_fixed_case_attribution(design, field):
    design["inputs"][field] = []
    with pytest.raises(ValueError, match="published case"):
        estimate(design)


def test_explicit_declared_defaults_and_reordered_zones_remain_supported(design):
    """Explicit defaults and input ordering do not change the physical basis."""
    design["inputs"]["anode"].update(utilization_factor=0.90, density_kg_m3=2750.0)
    design["inputs"]["structure"]["zones"].reverse()
    original = copy.deepcopy(design)
    result = estimate(design)
    assert result["total_estimate"]["anodes_estimate"]["additional_anodes_estimate"] == 234
    assert design == original
