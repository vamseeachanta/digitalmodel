# ABOUTME: Tests for design code edition/versioning support (WRK-145)
# ABOUTME: Verifies CodeEdition metadata, edition-aware factors, backward compatibility

"""Tests for design code edition/versioning — WRK-145.

Verifies that:
- CodeEdition dataclass stores edition metadata
- Strategy classes accept optional edition parameter
- Different editions produce different results where factors changed
- Default (no edition) uses latest edition
- Report header includes edition information
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
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import (
    ApiRp1111Strategy,
)


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def make_x65():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


def make_10inch():
    return PipeGeometry(outer_diameter=0.27305, wall_thickness=0.0214, corrosion_allowance=0.001)


# ===================================================================
# CodeEdition dataclass
# ===================================================================

class TestCodeEdition:
    def test_code_edition_construction(self):
        ed = CodeEdition(
            code=DesignCode.API_RP_1111,
            edition_year=2015,
            edition_label="4th Edition",
        )
        assert ed.code == DesignCode.API_RP_1111
        assert ed.edition_year == 2015
        assert ed.edition_label == "4th Edition"

    def test_code_edition_display_label(self):
        ed = CodeEdition(
            code=DesignCode.API_RP_1111,
            edition_year=2015,
            edition_label="4th Edition",
        )
        assert "API-RP-1111" in ed.display_label
        assert "2015" in ed.display_label
        assert "4th Edition" in ed.display_label

    def test_code_edition_equality(self):
        ed1 = CodeEdition(DesignCode.API_RP_1111, 2015, "4th Edition")
        ed2 = CodeEdition(DesignCode.API_RP_1111, 2015, "4th Edition")
        assert ed1 == ed2

    def test_code_edition_different_years_not_equal(self):
        ed1 = CodeEdition(DesignCode.API_RP_1111, 1999, "3rd Edition")
        ed2 = CodeEdition(DesignCode.API_RP_1111, 2015, "4th Edition")
        assert ed1 != ed2


# ===================================================================
# Edition-aware strategy construction
# ===================================================================

class TestApiRp1111Editions:
    def test_default_edition_is_latest(self):
        strategy = ApiRp1111Strategy()
        assert strategy.edition_year == 2015

    def test_explicit_edition_1999(self):
        strategy = ApiRp1111Strategy(edition=1999)
        assert strategy.edition_year == 1999

    def test_explicit_edition_2015(self):
        strategy = ApiRp1111Strategy(edition=2015)
        assert strategy.edition_year == 2015

    def test_invalid_edition_raises_error(self):
        with pytest.raises(ValueError, match="edition"):
            ApiRp1111Strategy(edition=2025)

    def test_edition_factors_accessible(self):
        strategy = ApiRp1111Strategy(edition=2015)
        factors = strategy.get_edition_factors()
        assert "f_d" in factors
        assert "f_c" in factors
        assert "f_p" in factors

    def test_1999_and_2015_have_different_propagation_factor(self):
        """API RP 1111 3rd Ed (1999) had tighter propagation factor than 4th Ed."""
        s_old = ApiRp1111Strategy(edition=1999)
        s_new = ApiRp1111Strategy(edition=2015)
        assert s_old.get_edition_factors()["f_p"] != s_new.get_edition_factors()["f_p"]
        # 3rd Ed f_p=0.72 (tighter), 4th Ed f_p=0.80 (relaxed)
        assert s_old.get_edition_factors()["f_p"] < s_new.get_edition_factors()["f_p"]


# ===================================================================
# Different editions produce different results
# ===================================================================

class TestEditionResultDifferences:
    def test_propagation_utilisation_differs_between_editions(self):
        """3rd Ed has tighter propagation factor (0.72) → higher utilisation."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(external_pressure=5e6)
        factors = DesignFactors()

        s_1999 = ApiRp1111Strategy(edition=1999)
        s_2015 = ApiRp1111Strategy(edition=2015)

        results_1999 = s_1999.run_checks(geom, mat, loads, factors)
        results_2015 = s_2015.run_checks(geom, mat, loads, factors)

        util_1999 = results_1999["propagation"][0]
        util_2015 = results_2015["propagation"][0]

        # Different editions should produce different propagation utilisation
        assert util_1999 != util_2015
        # 3rd Ed is more conservative (f_p=0.72 vs 0.80) → higher utilisation
        assert util_1999 > util_2015

    def test_burst_utilisation_same_between_editions(self):
        """Burst factor f_d=0.72 didn't change between editions."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=25e6, external_pressure=10e6)
        factors = DesignFactors()

        s_1999 = ApiRp1111Strategy(edition=1999)
        s_2015 = ApiRp1111Strategy(edition=2015)

        results_1999 = s_1999.run_checks(geom, mat, loads, factors)
        results_2015 = s_2015.run_checks(geom, mat, loads, factors)

        util_1999 = results_1999["burst"][0]
        util_2015 = results_2015["burst"][0]

        assert abs(util_1999 - util_2015) < 1e-10


# ===================================================================
# Backward compatibility
# ===================================================================

class TestEditionBackwardCompatibility:
    def test_analyzer_without_edition_uses_latest(self):
        """Existing code without edition parameter still works."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(geom, mat, loads, factors, DesignCode.API_RP_1111)
        result = analyzer.perform_analysis()

        assert len(result.checks) == 3
        assert result.governing_check is not None

    def test_analyzer_with_edition_produces_result(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(
            geom, mat, loads, factors, DesignCode.API_RP_1111, edition=1999
        )
        result = analyzer.perform_analysis()

        assert len(result.checks) == 3
        assert result.governing_check is not None

    def test_analyzer_edition_stored(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(
            geom, mat, loads, factors, DesignCode.API_RP_1111, edition=2015
        )
        assert analyzer.edition == 2015

    def test_analyzer_no_edition_stored_as_none(self):
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(
            geom, mat, loads, factors, DesignCode.API_RP_1111
        )
        assert analyzer.edition is None

    def test_dnv_code_ignores_edition_gracefully(self):
        """DNV strategy doesn't have edition support yet — should work fine."""
        geom = make_10inch()
        mat = make_x65()
        loads = DesignLoads(internal_pressure=20e6, external_pressure=5e6)
        factors = DesignFactors()

        analyzer = WallThicknessAnalyzer(
            geom, mat, loads, factors, DesignCode.DNV_ST_F101
        )
        result = analyzer.perform_analysis()
        assert len(result.checks) == 4


# ===================================================================
# Report edition display
# ===================================================================

class TestReportEditionDisplay:
    def test_mt_report_includes_edition_in_header(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            generate_mt_report,
        )

        geom = make_10inch()
        mat = make_x65()

        html = generate_mt_report(
            geom, mat,
            internal_pressure=20e6,
            external_pressure=5e6,
            code=DesignCode.API_RP_1111,
            edition=2015,
        )

        assert "4th Edition" in html
        assert "2015" in html

    def test_mt_report_without_edition_still_works(self):
        from digitalmodel.structural.analysis.wall_thickness_mt_report import (
            generate_mt_report,
        )

        geom = make_10inch()
        mat = make_x65()

        html = generate_mt_report(
            geom, mat,
            internal_pressure=20e6,
            external_pressure=5e6,
            code=DesignCode.API_RP_1111,
        )

        assert "API-RP-1111" in html
