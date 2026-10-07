"""Edition crosswalk: one B401 jacket and one F103 flowline across editions (#2208).

Documented differences (see docs/plans/2026-09-26-issue-2208-cp-editions.md):

- B401 2005 / 2010 / 2017 / 2021 design current densities are identical;
  only the table labels and citation revisions differ.
- Paint coating category IV exists in 2021 only.
- Zn closed circuit potential in seawater is -1.00 V up to 2017 and
  -1.030 V in 2021 (Table 8-6).
- F103 non-buried mean current density at 60 °C is 0.060 A/m2 in 2010
  (Table 5-1) and 0.075 A/m2 in 2019 (Table 6-2).
- F103 FBE constant ``a`` is 0.010 in 2010 (Table A.1) and 0.030 in 2019
  (Table A-1).
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import pytest

from digitalmodel.cathodic_protection import b401_tables, f103_tables
from digitalmodel.cathodic_protection._edition import Edition
from digitalmodel.cathodic_protection.b401_tables import (
    AnodeEnvironment,
    AnodeMaterial,
    DepthBand,
    PaintCategory,
)
from digitalmodel.cathodic_protection.dnv_rp_f103 import (
    BraceletDesignInput,
    design_bracelet_cp,
)
from digitalmodel.cathodic_protection.f103_tables import Exposure, LinepipeCoating
from digitalmodel.cathodic_protection.marine_structure_cp import (
    ClimateRegion,
    ExposureZone,
    StructuralZone,
    marine_structure_current_demand,
)
from digitalmodel.citations import validate_citation

CITATION_FIXTURES = Path(__file__).resolve().parents[1] / "citations" / "fixtures"
B401_EDITIONS: list[Edition] = ["2005", "2010", "2017", "2021"]
B401_LABELS = {
    "2005": ("2011", "Table 10-"),
    "2010": ("2011", "Table 10-"),
    "2017": ("2017-06", "Table A-"),
    "2021": ("2021-05", "Table 8-"),
}


def _jacket_zones() -> list[StructuralZone]:
    return [
        StructuralZone(
            zone_name="submerged_legs",
            exposure_zone=ExposureZone.SUBMERGED,
            surface_area_m2=2000.0,
            depth_m=45.0,
            coating_breakdown_factor=0.05,
        ),
        StructuralZone(
            zone_name="buried_piles",
            exposure_zone=ExposureZone.BURIED_MUDLINE,
            surface_area_m2=500.0,
            coating_breakdown_factor=1.0,
        ),
    ]


def _flowline(**overrides: Any) -> BraceletDesignInput:
    base: dict[str, Any] = dict(
        outer_diameter_m=0.3239,
        wall_thickness_m=0.0127,
        length_m=10000.0,
        linepipe_coating=LinepipeCoating.FBE,
        exposure=Exposure.NON_BURIED,
        fluid_temperature_c=60.0,
        design_life_years=25.0,
        seawater_resistivity_ohm_m=0.30,
        bracelet_net_mass_kg=40.0,
        bracelet_length_m=0.30,
        bracelet_thickness_m=0.04,
    )
    base.update(overrides)
    return BraceletDesignInput(**base)


# ---------------------------------------------------------------------------
# B401 jacket
# ---------------------------------------------------------------------------


@pytest.fixture(scope="module")
def jacket_results():
    return {
        edition: marine_structure_current_demand(
            _jacket_zones(),
            ClimateRegion.TEMPERATE,
            edition=edition,
            anode_length_m=2.0,
        )
        for edition in B401_EDITIONS
    }


def test_b401_current_demand_identical_across_editions(jacket_results):
    reference = jacket_results["2010"]
    for edition, result in jacket_results.items():
        assert result.total_initial_current_A == pytest.approx(reference.total_initial_current_A)
        assert result.total_mean_current_A == pytest.approx(reference.total_mean_current_A)
        assert result.total_final_current_A == pytest.approx(reference.total_final_current_A)
        assert result.total_anode_mass_kg == pytest.approx(reference.total_anode_mass_kg)
        assert result.number_of_anodes == reference.number_of_anodes
        assert result.edition_used == edition


def test_b401_citations_follow_the_edition(jacket_results):
    for edition, result in jacket_results.items():
        revision, prefix = B401_LABELS[edition]
        assert f"dnv-rp-b401 {revision} {prefix}1" in result.citations
        assert f"dnv-rp-b401 {revision} {prefix}2" in result.citations
        assert any(label.startswith(f"dnv-rp-b401 {revision} {prefix}6 with ") for label in result.citations)
        assert all(label.startswith(f"dnv-rp-b401 {revision} ") for label in result.citations)


@pytest.mark.parametrize("edition", B401_EDITIONS)
def test_b401_zinc_seawater_potential_changes_in_2021(edition):
    potential = b401_tables.anode_closed_circuit_potential(
        AnodeMaterial.ZINC, AnodeEnvironment.SEAWATER, edition
    )
    voltage = b401_tables.design_driving_voltage(AnodeMaterial.ZINC, edition)
    if edition == "2021":
        assert potential.value == -1.030
        assert voltage.value == pytest.approx(0.23)
    else:
        assert potential.value == -1.00
        assert voltage.value == pytest.approx(0.20)


@pytest.mark.parametrize("edition", B401_EDITIONS)
def test_b401_category_iv_only_in_2021(edition):
    if edition == "2021":
        a, b = b401_tables.coating_breakdown_constants(PaintCategory.IV, DepthBand.M0_30, edition)
        assert (a.value, b.value) == (0.02, 0.008)
        assert a.citation.section == "Table 8-4"
    else:
        with pytest.raises(ValueError, match="category IV"):
            b401_tables.coating_breakdown_constants(PaintCategory.IV, DepthBand.M0_30, edition)
    a, b = b401_tables.coating_breakdown_constants(PaintCategory.III, DepthBand.M0_30, edition)
    assert (a.value, b.value) == (0.02, 0.012)


def test_b401_2021_hot_anode_changes_driving_voltage_and_capacity():
    """A 70 °C Al anode in sediments uses the 80 °C row of Table 8-6."""
    hot = marine_structure_current_demand(
        _jacket_zones(),
        ClimateRegion.TEMPERATE,
        edition="2021",
        anode_length_m=2.0,
        anode_surface_temperature_c=70.0,
    )
    ambient = marine_structure_current_demand(
        _jacket_zones(), ClimateRegion.TEMPERATE, edition="2021", anode_length_m=2.0
    )
    assert hot.total_mean_current_A == pytest.approx(ambient.total_mean_current_A)
    # Al seawater at 70 °C -> row 80: -1.000 V, so 0.20 V instead of 0.25 V
    assert hot.anode_current_output_initial_A < ambient.anode_current_output_initial_A
    with pytest.raises(ValueError, match="Use edition '2021'"):
        marine_structure_current_demand(
            _jacket_zones(),
            ClimateRegion.TEMPERATE,
            edition="2017",
            anode_length_m=2.0,
            anode_surface_temperature_c=70.0,
        )


# ---------------------------------------------------------------------------
# F103 flowline
# ---------------------------------------------------------------------------


def test_f103_flowline_60c_fbe_differs_between_2010_and_2019():
    r10 = design_bracelet_cp(_flowline(), edition="2010")
    r19 = design_bracelet_cp(_flowline(), edition="2019")
    r21 = design_bracelet_cp(_flowline(), edition="2021")  # amended print alias

    assert r10.mean_current_density_A_m2 == 0.060  # Table 5-1, >50-80 °C
    assert r19.mean_current_density_A_m2 == 0.075  # Table 6-2, >50-80 °C
    assert r10.f_cf_linepipe == pytest.approx(0.010 + 0.0003 * 25.0)  # Table A.1 FBE
    assert r19.f_cf_linepipe == pytest.approx(0.030 + 0.0010 * 25.0)  # Table A-1 FBE, no concrete
    assert r19.number_of_anodes > r10.number_of_anodes
    assert r21.model_dump() == r19.model_dump() | {"edition_used": "2019"}

    assert r10.citations == [
        "dnv-rp-f103 2010 Table 5-1",
        "dnv-rp-f103 2010 Table A.1",
        "dnv-rp-b401 2011 Table 10-8",
        "dnv-rp-b401 2011 Table 10-6",
        "dnv-rp-b401 2011 Table 10-6 with Sec. 5 (structure-to-electrolyte potential criteria)",
    ]
    assert r19.citations == [
        "dnv-rp-f103 2019-09 Table 6-2",
        "dnv-rp-f103 2019-09 Table A-1",
        "dnv-rp-f103 2019-09 [6.4.2] (anode utilisation factor)",
        "dnv-rp-f103 2019-09 Table 6-3",
        "dnv-rp-f103 2019-09 Table 6-3 with [6.7.11] (design protective potential)",
    ]
    assert r10.standard == "DNV-RP-F103 (October 2010)"
    assert r19.standard == "DNVGL-RP-F103 (September 2019, amended May 2021)"
    assert r10.provenance == "verified-2010-tables"
    assert r19.provenance == "verified-2019-tables"


def test_f103_2019_concrete_weight_coating_selects_the_fbe_row():
    with_concrete = design_bracelet_cp(_flowline(concrete_weight_coating=True), edition="2019")
    without = design_bracelet_cp(_flowline(), edition="2019")
    assert with_concrete.f_cf_linepipe == pytest.approx(0.030 + 0.0003 * 25.0)
    assert without.f_cf_linepipe == pytest.approx(0.030 + 0.0010 * 25.0)
    # The 2010 table is not split: the flag changes nothing.
    assert (
        design_bracelet_cp(_flowline(concrete_weight_coating=True), edition="2010").f_cf_linepipe
        == design_bracelet_cp(_flowline(), edition="2010").f_cf_linepipe
    )


def test_f103_2019_buried_hot_anode_uses_table_6_3_row():
    buried = _flowline(exposure=Exposure.BURIED, anode_surface_temperature_c=60.0)
    r19 = design_bracelet_cp(buried, edition="2019")
    assert r19.mean_current_density_A_m2 == 0.040  # Table 6-2 buried, >50-80 °C
    assert r19.anode_capacity_Ah_kg == 680.0  # Table 6-3 Al sediment, 60 °C row
    assert r19.driving_voltage_V == pytest.approx(0.20)  # -0.80 - (-1.000)
    with pytest.raises(ValueError, match="Use edition '2019' \\(Table 6-3\\)"):
        design_bracelet_cp(buried, edition="2010")


def test_f103_2016_aliases_to_2019_with_a_warning():
    with pytest.warns(UserWarning, match="July 2016 print is not on file"):
        r16 = design_bracelet_cp(_flowline(), edition="2016")
    r19 = design_bracelet_cp(_flowline(), edition="2019")
    assert r16.model_dump() == r19.model_dump()
    assert r16.standard == "DNVGL-RP-F103 (September 2019, amended May 2021)"


# ---------------------------------------------------------------------------
# Citations resolve against the vendored fixture pages
# ---------------------------------------------------------------------------


@pytest.mark.parametrize("edition", ["2017", "2021"])
def test_b401_edition_citations_resolve(edition):
    for cited in (
        b401_tables.design_current_density(
            b401_tables.Climate.TEMPERATE, DepthBand.M0_30, b401_tables.DesignPhase.MEAN, edition
        ),
        b401_tables.anode_capacity(AnodeMaterial.ALUMINIUM, AnodeEnvironment.SEAWATER, edition),
    ):
        validate_citation(cited.citation, repo_root=CITATION_FIXTURES)


def test_f103_2019_citations_resolve():
    for cited in (
        f103_tables.mean_current_density(Exposure.BURIED, 90.0, "2019"),
        f103_tables.linepipe_coating_constants(LinepipeCoating.FBE, "2019")[0],
        f103_tables.anode_capacity(AnodeMaterial.ZINC, AnodeEnvironment.SEDIMENT, "2019"),
        f103_tables.bracelet_utilisation_factor("2019"),
    ):
        validate_citation(cited.citation, repo_root=CITATION_FIXTURES)
