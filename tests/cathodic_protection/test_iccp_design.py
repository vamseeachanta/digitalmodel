"""Tests for impressed current cathodic protection (ICCP) system design."""

import math

import pytest

from digitalmodel.cathodic_protection._experimental import ExperimentalModelError
from digitalmodel.cathodic_protection._provisional_galvanic import ProvisionalValue
from digitalmodel.cathodic_protection.iccp_design import (
    DEFAULT_UTILISATION_FACTOR,
    ICCP_ANODE_RECORDS,
    AnodeBedType,
    AnodeMaterial,
    IccpEnvironment,
    RectifierSizingInput,
    anode_bed_design,
    cable_sizing,
    deep_well_resistance,
    horizontal_column_resistance,
    iccp_anode_life,
    multiple_vertical_anode_resistance,
    rectifier_sizing,
    single_vertical_anode_resistance,
)


def test_rectifier_sizing_pipeline():
    """Typical pipeline ICCP: 5 A current, 2 ohm ground bed."""
    input_params = RectifierSizingInput(
        total_current_A=5.0,
        ground_bed_resistance_ohm=2.0,
        structure_coating_resistance_ohm=1.0,
        cable_resistance_ohm=0.5,
        back_emf_V=2.0,
        safety_factor=1.25,
    )
    result = rectifier_sizing(input_params)

    # V = (5 * (2+1+0.5) + 2) * 1.25 = (17.5 + 2) * 1.25 = 24.375 V
    assert result.dc_voltage_V == pytest.approx(24.375, rel=0.01)
    assert result.dc_current_A == 5.0
    assert result.power_W > 0
    # Should recommend at least 24V rating
    assert result.recommended_rating_V >= 24
    assert result.recommended_rating_A >= 5


def test_rectifier_sizing_higher_current():
    """Large structure with 50 A demand."""
    input_params = RectifierSizingInput(
        total_current_A=50.0,
        ground_bed_resistance_ohm=0.5,
        structure_coating_resistance_ohm=0.3,
        cable_resistance_ohm=0.2,
        back_emf_V=2.0,
    )
    result = rectifier_sizing(input_params)

    assert result.dc_current_A == 50.0
    assert result.recommended_rating_A >= 50
    assert result.power_W > 1000  # should be >1 kW for 50 A
    assert result.exceeds_standard_range is False
    assert result.units_required == 1


def test_rectifier_sizing_flags_exceeding_standard_range():
    """300 A into a 5 ohm bed: V = (300*(5+1+0) + 2) * 1.25 = 2252.5 V.

    Largest standard unit is 120 V / 200 A, so the voltage needs
    ceil(2252.5 / 120) = 19 units (current alone would need 2). The old
    code silently reported 120 V / 200 A (issue #2209).
    """
    input_params = RectifierSizingInput(
        total_current_A=300.0,
        ground_bed_resistance_ohm=5.0,
        structure_coating_resistance_ohm=1.0,
        cable_resistance_ohm=0.0,
        back_emf_V=2.0,
        safety_factor=1.25,
    )
    result = rectifier_sizing(input_params)

    assert result.dc_voltage_V == pytest.approx(2252.5, rel=1e-6)
    assert result.exceeds_standard_range is True
    assert result.units_required >= 19
    assert result.units_required == 19
    # Per-unit ratings stay at the largest standard size.
    assert result.recommended_rating_V == 120.0
    assert result.recommended_rating_A == 200.0


# ---- Anode ground-bed resistance (issue #2247) --------------------------------
#
# Hand values are the worked examples of DoD TSEWG Electrical Technical
# Paper 16 (March 2017), s.5, converted to SI (ohm-cm -> /100, feet -> *0.3048).

FT = 0.3048


def test_multiple_vertical_anode_resistance_tp16_example_9_anodes():
    """TP-16: rho = 4500 ohm-cm, L = 5 ft, d = 0.167 ft, S = 10 ft, N = 9 -> 3.26 ohm."""
    r = multiple_vertical_anode_resistance(45.0, 5 * FT, 0.167 * FT, 9, 10 * FT)
    assert r == pytest.approx(3.26, rel=5e-3)


@pytest.mark.parametrize(
    "n, expected",
    [(4, 28.6), (6, 21.2), (8, 17.0), (80, 2.6)],
)
def test_multiple_vertical_anode_resistance_tp16_table(n, expected):
    """TP-16: rho = 30 000 ohm-cm, L = 5.5 ft, d = 0.83 ft, S = 10 ft."""
    r = multiple_vertical_anode_resistance(300.0, 5.5 * FT, 0.83 * FT, n, 10 * FT)
    assert r == pytest.approx(expected, rel=1e-2)


@pytest.mark.parametrize(
    "length_ft, expected",
    [(100, 7.2), (150, 5.1), (200, 4.0), (250, 3.3), (300, 2.8), (400, 2.2)],
)
def test_deep_well_resistance_tp16_table(length_ft, expected):
    """TP-16 deep well: rho = 22 700 ohm-cm, backfill column d = 0.67 ft."""
    r = deep_well_resistance(227.0, length_ft * FT, 0.67 * FT)
    assert r == pytest.approx(expected, abs=0.06)


def test_single_vertical_anode_resistance_hand_value():
    """rho/(2 pi L)(ln(8L/d) - 1): 50/(2 pi 1.5) * (ln 160 - 1) = 5.30516 * 4.07517 = 21.6195."""
    assert single_vertical_anode_resistance(50.0, 1.5, 0.075) == pytest.approx(21.6195, rel=1e-5)
    assert multiple_vertical_anode_resistance(50.0, 1.5, 0.075, 1, 3.0) == pytest.approx(
        21.6195, rel=1e-5
    )


def test_horizontal_column_resistance_hand_value():
    """rho/(2 pi L)[ln(4L/d) + ln(L/h) - 2 + 2h/L], rho = 86, L = 10, d = 0.254, h = 3.048:

    ln(157.480) = 5.05930; ln(3.28084) = 1.18814; 2h/L = 0.6096;
    sum = 4.85704; 86/(2 pi 10) = 1.368732 -> R = 6.64799 ohm.
    """
    r = horizontal_column_resistance(86.0, 10.0, 0.254, 3.048)
    assert r == pytest.approx(6.64799, rel=1e-5)


def test_bed_resistance_decreases_with_anode_count_and_spacing():
    by_n = [multiple_vertical_anode_resistance(50.0, 1.5, 0.1, n, 5.0) for n in (2, 4, 8, 16)]
    assert all(b < a for a, b in zip(by_n, by_n[1:]))
    by_s = [multiple_vertical_anode_resistance(50.0, 1.5, 0.1, 6, s) for s in (2.0, 5.0, 10.0, 50.0)]
    assert all(b < a for a, b in zip(by_s, by_s[1:]))


def test_anode_bed_deep_well_uses_column_formula_without_uncited_factor():
    result = anode_bed_design(
        total_current_A=10.0,
        soil_resistivity_ohm_m=50.0,
        bed_type=AnodeBedType.DEEP_WELL,
        anode_material=AnodeMaterial.HIGH_SILICON_CAST_IRON,
        backfill_column_length_m=30.0,
        backfill_column_diameter_m=0.25,
    )
    expected = 50.0 / (2 * math.pi * 30.0) * (math.log(8 * 30.0 / 0.25) - 1.0)
    assert result.bed_resistance_ohm == pytest.approx(expected, abs=1e-4)
    assert result.bed_type == "deep_well"
    assert result.estimated_life_years is None


def test_anode_bed_missing_inputs_raise():
    with pytest.raises(ValueError, match="backfill_column_length_m"):
        anode_bed_design(10.0, 50.0, bed_type=AnodeBedType.DEEP_WELL)
    with pytest.raises(ValueError, match="burial_depth_m"):
        anode_bed_design(10.0, 50.0, bed_type=AnodeBedType.SHALLOW_HORIZONTAL)
    with pytest.raises(ValueError, match="DISTRIBUTED"):
        anode_bed_design(10.0, 50.0, bed_type=AnodeBedType.DISTRIBUTED)
    with pytest.raises(ValueError, match="anode_spacing_m"):
        anode_bed_design(100.0, 50.0, bed_type=AnodeBedType.SHALLOW_VERTICAL)
    with pytest.raises(ValueError, match="max_current_density_A_m2"):
        anode_bed_design(10.0, 50.0, anode_material=AnodeMaterial.SCRAP_STEEL)
    with pytest.raises(ValueError, match="must be positive"):
        anode_bed_design(-1.0, 50.0)


def test_anode_bed_count_from_current_density_limit():
    """HSCI in soil: J_max = 30 A/m2 (GCP 04-200-R1). Area = pi*0.075*1.5 = 0.35343 m2;

    100 A / (0.35343 * 30) = 9.43 -> 10 anodes.
    """
    result = anode_bed_design(100.0, 50.0, anode_spacing_m=5.0)
    assert result.number_of_anodes == 10
    assert result.max_current_density_A_m2 == 30.0
    assert result.anode_current_density_A_m2 <= 30.0
    assert result.bed_resistance_ohm == pytest.approx(
        multiple_vertical_anode_resistance(50.0, 1.5, 0.075, 10, 5.0), abs=1e-4
    )


def test_anode_bed_mmo_fewer_anodes():
    """MMO (50 A/m2 in carbonaceous backfill) needs no more anodes than HSCI (30 A/m2)."""
    hsci = anode_bed_design(100.0, 50.0, anode_spacing_m=5.0)
    mmo = anode_bed_design(
        100.0, 50.0, anode_material=AnodeMaterial.MIXED_METAL_OXIDE, anode_spacing_m=5.0
    )
    assert mmo.number_of_anodes <= hsci.number_of_anodes


def test_anode_bed_geometry_independent_of_experimental_flag():
    kwargs = dict(anode_spacing_m=5.0, anode_material=AnodeMaterial.HIGH_SILICON_CAST_IRON)
    plain = anode_bed_design(10.0, 50.0, **kwargs)
    exp = anode_bed_design(10.0, 50.0, experimental=True, **kwargs)
    assert plain.estimated_life_years is None
    assert exp.estimated_life_years is not None
    assert plain.number_of_anodes == exp.number_of_anodes
    assert plain.bed_resistance_ohm == exp.bed_resistance_ohm


# ---- Anode life (provisional literature data, experimental only) --------------

LB = 0.45359237


def test_anode_life_requires_experimental():
    with pytest.raises(ExperimentalModelError):
        iccp_anode_life(1.0, 1, AnodeMaterial.HIGH_SILICON_CAST_IRON, anode_mass_kg=10.0)


def test_anode_life_tp16_worked_example():
    """TP-16: L = N W u / (S I) = 8 * 31 lb * 0.85 / (1.0 lb/A-yr * 0.7 A) = 301.1 yr.

    (TP-16 rounds this to "300 years".)
    """
    life = iccp_anode_life(
        0.7,
        8,
        AnodeMaterial.HIGH_SILICON_CAST_IRON,
        anode_mass_kg=31 * LB,
        utilisation_factor=0.85,
        consumption_rate_kg_A_yr=1.0 * LB,
        experimental=True,
    )
    assert life == pytest.approx(8 * 31 * 0.85 / 0.7, rel=1e-12)


def test_anode_life_hsci_from_cited_density_hand_value():
    """HSCI in soil, cited values: rho = 7000 kg/m3, C = 0.30 kg/(A yr), u = 0.85.

    m = 7000 * pi * 0.0375**2 * 1.5 = 46.388 kg; life = 4 * 46.388 * 0.85 / (0.30 * 10) = 52.57 yr.
    """
    m = 7000.0 * math.pi * 0.0375**2 * 1.5
    life = iccp_anode_life(
        10.0,
        4,
        AnodeMaterial.HIGH_SILICON_CAST_IRON,
        anode_length_m=1.5,
        anode_diameter_m=0.075,
        experimental=True,
    )
    assert life == pytest.approx(4 * m * 0.85 / (0.30 * 10.0), rel=1e-12)
    assert life == pytest.approx(52.57, abs=0.01)


def test_anode_life_linear_in_mass_inverse_in_current():
    base = iccp_anode_life(
        5.0, 3, AnodeMaterial.GRAPHITE, anode_mass_kg=20.0, experimental=True
    )
    double_mass = iccp_anode_life(
        5.0, 3, AnodeMaterial.GRAPHITE, anode_mass_kg=40.0, experimental=True
    )
    double_current = iccp_anode_life(
        10.0, 3, AnodeMaterial.GRAPHITE, anode_mass_kg=20.0, experimental=True
    )
    assert double_mass == pytest.approx(2.0 * base, rel=1e-12)
    assert double_current == pytest.approx(0.5 * base, rel=1e-12)


def test_anode_life_coating_wear_hand_value():
    """Pt/Nb: life = N w A / (C I), w = 0.1 kg/m2 (user input), A = pi*0.025*1.0,

    C = 1e-5 kg/(A yr) (USNA EN380 Table 7.1), I = 10 A -> 0.1*0.0785398/1e-4 = 78.54 yr.
    """
    life = iccp_anode_life(
        10.0,
        1,
        AnodeMaterial.PLATINIZED_NIOBIUM,
        IccpEnvironment.SEAWATER,
        anode_length_m=1.0,
        anode_diameter_m=0.025,
        coating_loading_kg_m2=0.1,
        experimental=True,
    )
    assert life == pytest.approx(0.1 * math.pi * 0.025 / 1e-4, rel=1e-12)


def test_anode_life_mmo_pt_no_longer_use_cast_iron_density():
    """Reviewer probe (B9): Pt/Ti life was 397 608 yr from a cast-iron mass."""
    with pytest.raises(ValueError, match="coating_loading_kg_m2"):
        iccp_anode_life(
            10.0,
            1,
            AnodeMaterial.PLATINIZED_TITANIUM,
            anode_length_m=1.0,
            anode_diameter_m=0.025,
            experimental=True,
        )


def test_anode_life_missing_mass_for_material_without_density():
    with pytest.raises(ValueError, match="anode_mass_kg"):
        iccp_anode_life(
            1.0,
            1,
            AnodeMaterial.SCRAP_STEEL,
            anode_length_m=1.0,
            anode_diameter_m=0.1,
            experimental=True,
        )


def test_scrap_steel_rate_consistent_with_faraday():
    """TP-16 ~20 lb/A-yr vs Faraday Fe -> Fe2+: 55.85/(2 F) g/C * 3.15576e7 s = 9.133 kg/(A yr)."""
    faraday = 55.85 / (2 * 96485.33212) * 3.15576e7 / 1000.0
    cited = ICCP_ANODE_RECORDS[AnodeMaterial.SCRAP_STEEL].consumption_rate[
        IccpEnvironment.SOIL
    ].value
    assert cited == pytest.approx(faraday, rel=0.01)


def test_every_iccp_default_is_provisional_and_sourced():
    values = [DEFAULT_UTILISATION_FACTOR]
    for material in AnodeMaterial:
        rec = ICCP_ANODE_RECORDS[material]
        assert set(rec.consumption_rate) == set(IccpEnvironment)
        values.extend(rec.consumption_rate.values())
        values.extend(rec.max_current_density.values())
        if rec.density is not None:
            values.append(rec.density)
    for v in values:
        assert isinstance(v, ProvisionalValue)
        assert v.provisional is True
        assert v.source.strip()
        assert v.pending_standard.strip()
        assert v.value > 0


def test_provisional_value_requires_source():
    with pytest.raises(ValueError, match="source"):
        ProvisionalValue(value=1.0, units="-", source="  ")


def test_cable_sizing_standard():
    """Cable sizing for 10 A, 500 m one-way, max 2 V drop."""
    result = cable_sizing(
        current_A=10.0,
        cable_length_m=500.0,
        max_voltage_drop_V=2.0,
    )
    assert result["min_area_mm2"] > 0
    assert result["selected_area_mm2"] >= result["min_area_mm2"]
    assert result["voltage_drop_V"] <= 2.0
    # For 10 A, 500 m: min area = 0.0175 * 10 * 1000 / 2 = 87.5 mm²
    assert result["selected_area_mm2"] >= 87.5


def test_cable_sizing_temperature_correction():
    """Higher temperature increases resistivity → larger cable."""
    cool = cable_sizing(current_A=10.0, cable_length_m=100.0, temperature_c=20.0)
    hot = cable_sizing(current_A=10.0, cable_length_m=100.0, temperature_c=60.0)
    assert hot["min_area_mm2"] > cool["min_area_mm2"]
