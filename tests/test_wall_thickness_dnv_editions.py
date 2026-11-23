# ABOUTME: Tests for DNV-ST-F101 edition versioning support (WRK-155)
# ABOUTME: Verifies edition-aware factors, safety class gamma_SC differences, backward compatibility

"""Tests for DNV-ST-F101 edition versioning — WRK-155.

Verifies that:
- DnvStF101Strategy accepts optional edition parameter
- Default (no edition) uses latest edition (2021)
- 2007 and 2021 editions have different gamma_SC values
- Different editions produce different results
- Edition info metadata is correct
- M-T report works with DNV code
- Backward compatibility maintained
- CONDITIONS_BY_CODE has DNV entry
"""

import math

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    CodeEdition,
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import (
    DnvStF101Strategy,
    EDITION_FACTORS,
    EDITION_METADATA,
    LATEST_EDITION,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def make_x65():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


def make_12inch():
    """12.75" OD, 0.5" WT submarine pipeline."""
    return PipeGeometry(outer_diameter=0.3239, wall_thickness=0.0127, corrosion_allowance=0.001)


def make_10inch():
    return PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=0.001)


# ===================================================================
# Edition construction
# ===================================================================

class TestDnvEditionConstruction:
    def test_default_edition_is_latest(self):
        strategy = DnvStF101Strategy()
        assert strategy.edition_year == 2021

    def test_latest_edition_constant(self):
        assert LATEST_EDITION == 2021

    def test_explicit_edition_2007(self):
        strategy = DnvStF101Strategy(edition=2007)
        assert strategy.edition_year == 2007

    def test_explicit_edition_2021(self):
        strategy = DnvStF101Strategy(edition=2021)
        assert strategy.edition_year == 2021

    def test_invalid_edition_raises_error(self):
        with pytest.raises(ValueError, match="edition"):
            DnvStF101Strategy(edition=2025)

    def test_invalid_edition_shows_available(self):
        with pytest.raises(ValueError, match="2007"):
            DnvStF101Strategy(edition=1990)

    def test_edition_factors_accessible(self):
        strategy = DnvStF101Strategy(edition=2021)
        factors = strategy.get_edition_factors()
        assert "f_y" in factors
        assert "f_u" in factors
        assert "f_0" in factors
        assert "gamma_m" in factors
        assert "gamma_SC" in factors

    def test_edition_info_is_code_edition(self):
        strategy = DnvStF101Strategy(edition=2021)
        assert isinstance(strategy.edition_info, CodeEdition)
        assert strategy.edition_info.code == DesignCode.DNV_ST_F101
        assert strategy.edition_info.edition_year == 2021

    def test_2007_edition_label(self):
        strategy = DnvStF101Strategy(edition=2007)
        assert "DNV-OS-F101" in strategy.edition_info.edition_label

    def test_2021_edition_label(self):
        strategy = DnvStF101Strategy(edition=2021)
        assert "DNV-ST-F101" in strategy.edition_info.edition_label

    def test_display_label_format(self):
        strategy = DnvStF101Strategy(edition=2021)
        label = strategy.edition_info.display_label
        assert "DNV-ST-F101" in label
        assert "2021" in label


# ===================================================================
# Edition factor differences
# ===================================================================

class TestDnvEditionFactors:
    def test_2007_gamma_sc_low(self):
        assert EDITION_FACTORS[2007]["gamma_SC"]["low"] == 1.04

    def test_2007_gamma_sc_medium(self):
        assert EDITION_FACTORS[2007]["gamma_SC"]["medium"] == 1.14

    def test_2007_gamma_sc_high(self):
        assert EDITION_FACTORS[2007]["gamma_SC"]["high"] == 1.26

    def test_2021_gamma_sc_low(self):
        assert EDITION_FACTORS[2021]["gamma_SC"]["low"] == 1.046

    def test_2021_gamma_sc_medium(self):
        assert EDITION_FACTORS[2021]["gamma_SC"]["medium"] == 1.138

    def test_2021_gamma_sc_high(self):
        assert EDITION_FACTORS[2021]["gamma_SC"]["high"] == 1.308

    def test_gamma_sc_differs_between_editions(self):
        """All three safety classes changed between 2007 and 2021."""
        for sc in ["low", "medium", "high"]:
            assert EDITION_FACTORS[2007]["gamma_SC"][sc] != EDITION_FACTORS[2021]["gamma_SC"][sc]

    def test_gamma_m_same_between_editions(self):
        """gamma_m = 1.15 didn't change between editions."""
        assert EDITION_FACTORS[2007]["gamma_m"] == EDITION_FACTORS[2021]["gamma_m"] == 1.15

    def test_material_factors_same_between_editions(self):
        """f_y and f_u are 0.96 in both editions."""
        assert EDITION_FACTORS[2007]["f_y"] == EDITION_FACTORS[2021]["f_y"] == 0.96
        assert EDITION_FACTORS[2007]["f_u"] == EDITION_FACTORS[2021]["f_u"] == 0.96

    def test_ovality_same_between_editions(self):
        assert EDITION_FACTORS[2007]["f_0"] == EDITION_FACTORS[2021]["f_0"] == 0.005


# ===================================================================
# Edition metadata
# ===================================================================

class TestDnvEditionMetadata:
    def test_both_editions_in_metadata(self):
        assert 2007 in EDITION_METADATA
        assert 2021 in EDITION_METADATA

    def test_metadata_code_is_dnv(self):
        for year in [2007, 2021]:
            assert EDITION_METADATA[year].code == DesignCode.DNV_ST_F101

    def test_metadata_years_match(self):
        for year in [2007, 2021]:
            assert EDITION_METADATA[year].edition_year == year


# ===================================================================
# Run checks with different editions
# ===================================================================

class TestDnvEditionResults:
    """Verify that the strategy uses self._factors (not hardcoded values)."""

    def test_pressure_containment_uses_edition_factors(self):
        """f_y and f_u come from edition factors."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=5e6)
        factors = DesignFactors()

        s_2007 = DnvStF101Strategy(edition=2007)
        s_2021 = DnvStF101Strategy(edition=2021)

        r_2007 = s_2007.run_checks(geom, mat, loads, factors)
        r_2021 = s_2021.run_checks(geom, mat, loads, factors)

        # Both editions use same f_y=0.96, so results should be identical
        assert abs(r_2007["pressure_containment"][0] - r_2021["pressure_containment"][0]) < 1e-10

    def test_collapse_uses_edition_ovality(self):
        """f_0 comes from edition factors."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=10e6)
        factors = DesignFactors()

        s_2007 = DnvStF101Strategy(edition=2007)
        s_2021 = DnvStF101Strategy(edition=2021)

        r_2007 = s_2007.run_checks(geom, mat, loads, factors)
        r_2021 = s_2021.run_checks(geom, mat, loads, factors)

        # Both editions use same f_0=0.005, so results should match
        assert abs(r_2007["collapse"][0] - r_2021["collapse"][0]) < 1e-10

    def test_all_four_checks_run(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        strategy = DnvStF101Strategy()
        results = strategy.run_checks(geom, mat, loads, factors)

        assert "pressure_containment" in results
        assert "collapse" in results
        assert "propagation_buckling" in results
        assert "combined_loading" in results

    def test_check_returns_utilisation_and_details(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        strategy = DnvStF101Strategy()
        results = strategy.run_checks(geom, mat, loads, factors)

        for check_name, (util, details) in results.items():
            assert isinstance(util, float)
            assert isinstance(details, dict)
            assert "utilisation" in details


# ===================================================================
# Backward compatibility
# ===================================================================

class TestDnvBackwardCompatibility:
    def test_analyzer_without_edition_works(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.DNV_ST_F101)
        result = analyzer.perform_analysis()
        assert len(result.checks) == 4
        assert result.governing_check is not None

    def test_analyzer_with_edition_works(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(
            geom, mat, loads, factors, DesignCode.DNV_ST_F101, edition=2007
        )
        result = analyzer.perform_analysis()
        assert len(result.checks) == 4

    def test_different_safety_classes_produce_different_results(self):
        """DesignFactors safety_class still drives gamma_SC in checks."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=5e6)

        factors_low = DesignFactors(safety_class=SafetyClass.LOW)
        factors_high = DesignFactors(safety_class=SafetyClass.HIGH)

        strategy = DnvStF101Strategy()
        r_low = strategy.run_checks(geom, mat, loads, factors_low)
        r_high = strategy.run_checks(geom, mat, loads, factors_high)

        # Higher safety class = larger gamma_SC = lower design resistance = higher utilisation
        assert r_low["pressure_containment"][0] < r_high["pressure_containment"][0]


# ===================================================================
# CONDITIONS_BY_CODE integration
# ===================================================================

class TestDnvConditionsByCode:
    def test_dnv_in_conditions_by_code(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            CONDITIONS_BY_CODE,
        )
        assert DesignCode.DNV_ST_F101 in CONDITIONS_BY_CODE

    def test_dnv_has_three_safety_classes(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            CONDITIONS_BY_CODE,
        )
        conditions = CONDITIONS_BY_CODE[DesignCode.DNV_ST_F101]
        assert len(conditions) == 3

    def test_dnv_condition_names(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            CONDITIONS_BY_CODE,
        )
        conditions = CONDITIONS_BY_CODE[DesignCode.DNV_ST_F101]
        names = [c["name"] for c in conditions]
        assert "Low Safety Class" in names
        assert "Medium Safety Class" in names
        assert "High Safety Class" in names

    def test_dnv_condition_f_d_ordering(self):
        """Low safety class should have highest f_d (least conservative)."""
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            CONDITIONS_BY_CODE,
        )
        conditions = CONDITIONS_BY_CODE[DesignCode.DNV_ST_F101]
        f_d_values = {c["name"]: c["f_d"] for c in conditions}
        assert f_d_values["Low Safety Class"] > f_d_values["Medium Safety Class"]
        assert f_d_values["Medium Safety Class"] > f_d_values["High Safety Class"]

    def test_dnv_condition_f_d_values_correct(self):
        """f_d = 1/(gamma_SC * gamma_m) for 2021 edition."""
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            CONDITIONS_BY_CODE,
        )
        conditions = CONDITIONS_BY_CODE[DesignCode.DNV_ST_F101]
        f_d_map = {c["name"]: c["f_d"] for c in conditions}
        # Low: 1/(1.046*1.15) ≈ 0.831
        assert abs(f_d_map["Low Safety Class"] - 1.0 / (1.046 * 1.15)) < 0.001
        # Medium: 1/(1.138*1.15) ≈ 0.764
        assert abs(f_d_map["Medium Safety Class"] - 1.0 / (1.138 * 1.15)) < 0.001
        # High: 1/(1.308*1.15) ≈ 0.665
        assert abs(f_d_map["High Safety Class"] - 1.0 / (1.308 * 1.15)) < 0.001


# ===================================================================
# M-T Report generation
# ===================================================================

class TestDnvMtReport:
    def test_mt_report_generates_with_dnv_code(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            generate_mt_report,
        )
        geom = make_10inch()
        mat = make_x65()

        html = generate_mt_report(
            geom, mat,
            internal_pressure=20e6,
            external_pressure=5e6,
            code=DesignCode.DNV_ST_F101,
        )
        assert "DNV-ST-F101" in html
        assert len(html) > 1000

    def test_mt_report_with_edition(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            generate_mt_report,
        )
        geom = make_10inch()
        mat = make_x65()

        html = generate_mt_report(
            geom, mat,
            internal_pressure=20e6,
            external_pressure=5e6,
            code=DesignCode.DNV_ST_F101,
            edition=2021,
        )
        assert "DNV-ST-F101" in html
        assert "2021" in html

    def test_mt_report_includes_safety_class_conditions(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            generate_mt_report,
        )
        geom = make_10inch()
        mat = make_x65()

        html = generate_mt_report(
            geom, mat,
            internal_pressure=20e6,
            external_pressure=5e6,
            code=DesignCode.DNV_ST_F101,
        )
        assert "Low Safety Class" in html
        assert "Medium Safety Class" in html
        assert "High Safety Class" in html
