"""Tests for coating breakdown factor calculations.

Expected values are hand-derived from the cited tables (issue #2207):

- DNV-RP-F103 Table A.1 (printed x100): FBE a=1, b=0.03 -> 0.01, 0.0003;
  3LPE / 3LPP a=0.1, b=0.003 -> 0.001, 0.00003; coal tar / asphalt enamel
  a=0.3, b=0.01 -> 0.003, 0.0001; polychloroprene a=0.1, b=0.01 -> 0.001,
  0.0001.
- DNV-RP-B401 Table 10-4: Cat I a=0.10, b=0.10 (0-30 m) / 0.05 (>30 m);
  Cat II a=0.05, b=0.025 / 0.015; Cat III a=0.02, b=0.012 / 0.008.
"""

import pytest

from digitalmodel.cathodic_protection.coating import (
    COATING_CONSTANTS,
    CoatingCategory,
    coating_breakdown_factors,
    coating_constants,
    coating_life_estimate,
    effective_bare_area_coated,
)

B401 = "2010"  # edition whose tables the wiki holds; avoids the None warning
F103 = "2010"


def test_fbe_breakdown_factors_25yr():
    """FBE (F103 Table A.1): a=0.01, b=0.0003/yr over 25 years."""
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.FBE,
        design_life_years=25.0,
        edition=B401,
        f103_edition=F103,
    )
    assert result.initial_factor == pytest.approx(0.01, abs=1e-6)
    # Final: 0.01 + 0.0003 * 25 = 0.0175
    assert result.final_factor == pytest.approx(0.0175, abs=1e-6)
    # Mean: 0.01 + 0.0003 * 12.5 = 0.01375
    assert result.mean_factor == pytest.approx(0.01375, abs=1e-6)


def test_linepipe_coating_result_carries_f103_citation():
    """Linepipe coating constants cite DNV-RP-F103 Table A.1 (revision 2010)."""
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.FBE,
        design_life_years=25.0,
        edition=B401,
        f103_edition=F103,
    )

    assert result.citation is not None
    assert result.citation.code_id == "dnv-rp-f103"
    assert result.citation.publisher == "DNV"
    assert result.citation.revision == "2010"
    assert result.citation.section == "Table A.1"
    assert result.standard == "DNV-RP-F103 (October 2010)"
    assert result.f103_edition_used == "2010"
    assert result.citations == ["dnv-rp-f103 2010 Table A.1"]


def test_paint_coating_result_carries_b401_citation():
    """Paint category III constants cite DNV-RP-B401 Table 10-4 (revision 2011)."""
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.PAINT_III,
        design_life_years=25.0,
        edition=B401,
    )

    assert result.citation is not None
    assert result.citation.code_id == "dnv-rp-b401"
    assert result.citation.revision == "2011"
    assert result.citation.section == "Table 10-4"
    assert result.standard == "DNV-RP-B401 (October 2010)"
    assert result.f103_edition_used is None
    assert result.citations == ["dnv-rp-b401 2011 Table 10-4"]


def test_paint_iii_25yr_shallow_matches_review_hand_calc():
    """Cat III, 0-30 m, 25 yr: f_cf = 0.02 + 0.012 * 25 = 0.32 (review B3)."""
    result = coating_breakdown_factors(
        CoatingCategory.PAINT_III, design_life_years=25.0, depth_m=20.0, edition=B401
    )
    assert result.initial_factor == pytest.approx(0.02, abs=1e-9)
    # Mean: 0.02 + 0.012 * 12.5 = 0.17
    assert result.mean_factor == pytest.approx(0.17, abs=1e-9)
    assert result.final_factor == pytest.approx(0.32, abs=1e-9)


def test_paint_iii_depth_row_changes_b():
    """Cat III b is 0.012 for 0-30 m and 0.008 for >30 m (Table 10-4 rows)."""
    shallow = coating_breakdown_factors(
        CoatingCategory.PAINT_III, design_life_years=25.0, depth_m=30.0, edition=B401
    )
    deep = coating_breakdown_factors(
        CoatingCategory.PAINT_III, design_life_years=25.0, depth_m=30.01, edition=B401
    )
    assert shallow.final_factor == pytest.approx(0.02 + 0.012 * 25.0, abs=1e-9)
    # Final: 0.02 + 0.008 * 25 = 0.22
    assert deep.final_factor == pytest.approx(0.22, abs=1e-9)


@pytest.mark.parametrize(
    ("category", "a", "b"),
    [
        (CoatingCategory.PAINT_I, 0.10, 0.10),
        (CoatingCategory.PAINT_II, 0.05, 0.025),
        (CoatingCategory.PAINT_III, 0.02, 0.012),
    ],
)
def test_paint_categories_shallow_constants(category, a, b):
    """Table 10-4 a and b (0-30 m row) per paint category."""
    c = coating_constants(category, depth_m=0.0, edition=B401)
    assert (c.a, c.b) == (a, b)


def test_3lpe_coating_low_degradation():
    """3LPE (F103 Table A.1): a=0.001, b=0.00003/yr."""
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.THREE_LAYER_PE,
        design_life_years=40.0,
        edition=B401,
        f103_edition=F103,
    )
    assert result.initial_factor == pytest.approx(0.001, abs=1e-9)
    # Final: 0.001 + 0.00003 * 40 = 0.0022
    assert result.final_factor == pytest.approx(0.0022, abs=1e-9)
    assert result.final_factor < 0.10  # 3LPE should stay below 10% at 40 yr


def test_coal_tar_enamel_constants():
    """Coal tar enamel (F103 Table A.1): a=0.003, b=0.0001/yr.

    Table A.1 gives glass-fibre-reinforced enamels a lower initial breakdown
    than FBE (0.003 vs 0.01), the reverse of the former uncited constants.
    """
    result = coating_breakdown_factors(
        coating_type=CoatingCategory.COAL_TAR_ENAMEL,
        design_life_years=20.0,
        edition=B401,
        f103_edition=F103,
    )
    assert result.initial_factor == pytest.approx(0.003, abs=1e-9)
    # Final: 0.003 + 0.0001 * 20 = 0.005
    assert result.final_factor == pytest.approx(0.005, abs=1e-9)
    assert result.initial_factor < COATING_CONSTANTS[CoatingCategory.FBE][0]


def test_temperature_has_no_effect_on_breakdown():
    """No cited temperature correction exists; ``temperature_c`` is inert."""
    normal = coating_breakdown_factors(
        coating_type=CoatingCategory.FBE,
        design_life_years=25.0,
        temperature_c=20.0,
        edition=B401,
        f103_edition=F103,
    )
    elevated = coating_breakdown_factors(
        coating_type=CoatingCategory.FBE,
        design_life_years=25.0,
        temperature_c=40.0,
        edition=B401,
        f103_edition=F103,
    )
    assert elevated.final_factor == pytest.approx(normal.final_factor, abs=1e-12)


def test_depth_has_no_effect_on_linepipe_coatings():
    """Table A.1 has no depth row; only paint categories change with depth."""
    shallow = coating_breakdown_factors(
        CoatingCategory.FBE, design_life_years=25.0, depth_m=0.0,
        edition=B401, f103_edition=F103,
    )
    deep = coating_breakdown_factors(
        CoatingCategory.FBE, design_life_years=25.0, depth_m=500.0,
        edition=B401, f103_edition=F103,
    )
    assert deep.final_factor == pytest.approx(shallow.final_factor, abs=1e-12)


def test_coating_life_estimate_fbe():
    """FBE reaches 50 % breakdown at (0.50 - 0.01) / 0.0003 = 1633.3 years."""
    result = coating_life_estimate(
        coating_type=CoatingCategory.FBE,
        threshold_factor=0.50,
        f103_edition=F103,
    )
    assert result.estimated_life_years == pytest.approx(1633.333, abs=0.01)
    assert result.time_to_threshold_years is not None
    assert result.citations == ["dnv-rp-f103 2010 Table A.1"]


def test_coating_life_estimate_paint_i():
    """Cat I, 0-30 m: (0.50 - 0.10) / 0.10 = 4.0 years to 50 % breakdown."""
    result = coating_life_estimate(
        CoatingCategory.PAINT_I, threshold_factor=0.50, depth_m=10.0, edition=B401
    )
    assert result.time_to_threshold_years == pytest.approx(4.0, abs=1e-9)


def test_bare_surface_area_increases_with_time():
    """Effective bare area should increase over time."""
    area_5yr = effective_bare_area_coated(
        total_surface_area_m2=1000.0,
        coating_type=CoatingCategory.FBE,
        elapsed_years=5.0,
        f103_edition=F103,
    )
    area_20yr = effective_bare_area_coated(
        total_surface_area_m2=1000.0,
        coating_type=CoatingCategory.FBE,
        elapsed_years=20.0,
        f103_edition=F103,
    )
    assert area_20yr > area_5yr
    # At 5 yr: 1000 * (0.01 + 0.0003 * 5) = 1000 * 0.0115 = 11.5 m²
    assert area_5yr == pytest.approx(11.5, abs=1e-6)


def test_bare_steel_is_unity_without_citation():
    """NONE: f_c = 1.0 at every time, no table to cite."""
    result = coating_breakdown_factors(CoatingCategory.NONE, 25.0, edition=B401)
    assert (result.initial_factor, result.mean_factor, result.final_factor) == (
        1.0, 1.0, 1.0,
    )
    assert result.citation is None
    assert result.citations == []
    assert COATING_CONSTANTS[CoatingCategory.NONE] == (1.0, 0.0)


@pytest.mark.parametrize(
    "category", [CoatingCategory.POLYURETHANE, CoatingCategory.CONCRETE_WEIGHT]
)
def test_uncited_categories_raise(category):
    """No Table 10-4 / Table A.1 row: the functions refuse to invent numbers."""
    assert category not in COATING_CONSTANTS
    with pytest.raises(ValueError, match="DNV-RP-B401 Table 10-4"):
        coating_breakdown_factors(category, 25.0, edition=B401)
    with pytest.raises(ValueError, match="DNV-RP-F103 Table A.1"):
        coating_life_estimate(category, edition=B401)
    with pytest.raises(ValueError, match=category.name):
        effective_bare_area_coated(100.0, category, 5.0, edition=B401)


def test_coating_constants_snapshot_matches_tables():
    """COATING_CONSTANTS holds the Table A.1 / Table 10-4 (0-30 m) values."""
    assert COATING_CONSTANTS[CoatingCategory.FBE] == (0.01, 0.0003)
    assert COATING_CONSTANTS[CoatingCategory.THREE_LAYER_PE] == (0.001, 0.00003)
    assert COATING_CONSTANTS[CoatingCategory.THREE_LAYER_PP] == (0.001, 0.00003)
    assert COATING_CONSTANTS[CoatingCategory.COAL_TAR_ENAMEL] == (0.003, 0.0001)
    assert COATING_CONSTANTS[CoatingCategory.ASPHALT_ENAMEL] == (0.003, 0.0001)
    assert COATING_CONSTANTS[CoatingCategory.NEOPRENE] == (0.001, 0.0001)
    assert COATING_CONSTANTS[CoatingCategory.PAINT_I] == (0.10, 0.10)
    assert COATING_CONSTANTS[CoatingCategory.PAINT_II] == (0.05, 0.025)
    assert COATING_CONSTANTS[CoatingCategory.PAINT_III] == (0.02, 0.012)
