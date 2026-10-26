# ABOUTME: Skeleton tests for API RP 1111 wall thickness code strategy
# ABOUTME: Covers burst, collapse, propagation checks and edition-aware factors

import math
import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_codes.base import CodeStrategy


OD = 0.2731    # 10.75" pipe
WT = 0.0127    # 0.5"
SMYS = 358.5e6  # X52
SMTS = 455.1e6


def _geo(**kw):
    defaults = dict(outer_diameter=OD, wall_thickness=WT)
    defaults.update(kw)
    return PipeGeometry(**defaults)


def _mat(**kw):
    return PipeMaterial(grade="X52", smys=SMYS, smts=SMTS, **kw)


def _loads(**kw):
    return DesignLoads(**kw)


class TestApiRp1111Registration:
    def test_code_in_registry(self):
        import digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111  # noqa: F401
        assert DesignCode.API_RP_1111 in CODE_REGISTRY

    def test_strategy_satisfies_protocol(self):
        cls = CODE_REGISTRY[DesignCode.API_RP_1111]
        inst = cls()
        assert isinstance(inst, CodeStrategy)
        assert inst.code_name == "API-RP-1111"
        for name in ["burst", "collapse", "propagation"]:
            assert name in inst.check_names

    def test_default_edition_is_2015(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy, LATEST_EDITION
        strategy = ApiRp1111Strategy()
        assert strategy.edition_year == LATEST_EDITION == 2015

    def test_1999_edition_selection(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
        strategy = ApiRp1111Strategy(edition=1999)
        assert strategy.edition_year == 1999
        factors = strategy.get_edition_factors()
        assert factors["f_p"] == pytest.approx(0.72, abs=1e-4)

    def test_invalid_edition_raises(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
        with pytest.raises(ValueError, match="Unknown edition"):
            ApiRp1111Strategy(edition=2000)


class TestApiRp1111Burst:
    def _strategy(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
        return ApiRp1111Strategy()

    def test_burst_10mpa_thin_wall(self):
        strategy = self._strategy()
        D = OD
        t = WT
        # d_over_t > 15 for thin pipe
        assert D / t > 15
        # p_b = 0.90 * (SMYS + SMTS) * t / (D - t)
        p_b_expected = 0.90 * (SMYS + SMTS) * t / (D - t)
        p_b_design_expected = 0.72 * p_b_expected

        results = strategy.run_checks(_geo(), _mat(), _loads(internal_pressure=10e6), DesignFactors())
        util, details = results["burst"]
        assert details["p_b"] == pytest.approx(p_b_expected, rel=1e-4)
        assert details["p_b_design"] == pytest.approx(p_b_design_expected, rel=1e-4)
        assert util == pytest.approx(10e6 / p_b_design_expected, rel=1e-4)

    def test_burst_zero_pressure_zero_util(self):
        strategy = self._strategy()
        results = strategy.run_checks(_geo(), _mat(), _loads(), DesignFactors())
        util, _ = results["burst"]
        assert util == pytest.approx(0.0, abs=1e-9)

    def test_burst_utilisation_increases_with_pressure(self):
        strategy = self._strategy()
        r1 = strategy.run_checks(_geo(), _mat(), _loads(internal_pressure=5e6), DesignFactors())
        r2 = strategy.run_checks(_geo(), _mat(), _loads(internal_pressure=20e6), DesignFactors())
        assert r2["burst"][0] > r1["burst"][0]

    def test_2015_vs_1999_propagation_factor(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
        s1999 = ApiRp1111Strategy(edition=1999)
        s2015 = ApiRp1111Strategy(edition=2015)
        # 2015 has f_p=0.80 (more relaxed), 1999 has f_p=0.72 (more conservative)
        f1999 = s1999.get_edition_factors()
        f2015 = s2015.get_edition_factors()
        assert f1999["f_p"] < f2015["f_p"]
        # More conservative factors -> higher utilisation for same demand
        r1999 = s1999.run_checks(_geo(), _mat(), _loads(external_pressure=3e6), DesignFactors())
        r2015 = s2015.run_checks(_geo(), _mat(), _loads(external_pressure=3e6), DesignFactors())
        assert r1999["propagation"][0] > r2015["propagation"][0]


class TestApiRp1111Collapse:
    def _strategy(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
        return ApiRp1111Strategy()

    def test_collapse_no_external_zero_util(self):
        strategy = self._strategy()
        results = strategy.run_checks(_geo(), _mat(), _loads(), DesignFactors())
        util, _ = results["collapse"]
        assert util == pytest.approx(0.0, abs=1e-9)

    def test_collapse_has_details(self):
        strategy = self._strategy()
        results = strategy.run_checks(_geo(), _mat(), _loads(external_pressure=2e6), DesignFactors())
        _, details = results["collapse"]
        for key in ["p_el", "p_y", "p_c", "f_c", "p_c_design", "utilisation"]:
            assert key in details

    def test_collapse_thicker_wall_lower_util(self):
        strategy = self._strategy()
        ext_p = 5e6
        geo_thin = _geo(wall_thickness=0.010)
        geo_thick = _geo(wall_thickness=0.025)
        u_thin, _ = strategy.run_checks(geo_thin, _mat(), _loads(external_pressure=ext_p), DesignFactors())["collapse"]
        u_thick, _ = strategy.run_checks(geo_thick, _mat(), _loads(external_pressure=ext_p), DesignFactors())["collapse"]
        assert u_thick < u_thin


class TestApiRp1111PlasticCapacity:
    def _strategy(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import ApiRp1111Strategy
        return ApiRp1111Strategy()

    def test_plastic_moment_positive(self):
        strategy = self._strategy()
        M_p = strategy.compute_plastic_moment(_geo(), _mat())
        assert M_p > 0

    def test_plastic_tension_positive(self):
        strategy = self._strategy()
        S_p = strategy.compute_plastic_tension(_geo(), _mat())
        assert S_p > 0


class TestApiRp1111CombinedLoading:
    @pytest.mark.parametrize("edition", [1999, 2015])
    @pytest.mark.parametrize(
        "thickness, moment, tension, internal_mpa, external_mpa",
        [
            pytest.param(0.01905, 0.0, 0.0, 5.0, 0.5, id="current-pressure-only"),
            pytest.param(0.01905, 1.0e9, 0.0, 5.0, 0.5, id="current-extreme-moment"),
            pytest.param(0.01905, 100.0, 500000.0, 10.0, 1.0, id="current-mixed"),
            pytest.param(0.01905, 0.0, 0.0, 0.0, 0.0, id="unloaded"),
            pytest.param(0.030, 100.0, 500000.0, 10.0, 1.0, id="thick-wall"),
            pytest.param(0.01905, -100.0, -500000.0, 10.0, 1.0, id="signed-loads"),
            pytest.param(0.01905, 100.0, 500000.0, 1.0, 10.0, id="external-pressure"),
            pytest.param(0.01905, 0.0, 1.0e9, 5.0, 0.5, id="extreme-tension"),
        ],
    )
    def test_matches_existing_function(
        self, edition, thickness, moment, tension, internal_mpa, external_mpa
    ):
        """Regression parity with the existing function, not an independent validation."""
        from digitalmodel.subsea.pipeline.pipeline_pressure import (
            PipeMaterial as PipelineMaterial,
            api_combined_loading_check,
        )

        # The first three cases reuse the current standalone combined-check inputs.
        geometry = PipeGeometry(
            outer_diameter=0.3239,
            wall_thickness=thickness,
            corrosion_allowance=0.001,
            fabrication_tolerance=0.125,
        )
        material = PipeMaterial(grade="X65", smys=448.0e6, smts=531.0e6)
        loads = DesignLoads(
            internal_pressure=internal_mpa * 1.0e6,
            external_pressure=external_mpa * 1.0e6,
            bending_moment=moment,
            effective_tension=tension,
        )
        reference = api_combined_loading_check(
            D=0.3239,
            t=thickness,
            mat=PipelineMaterial(name="X65", SMYS=448.0, SMTS=531.0),
            M_Sd=moment,
            S_Sd=tension,
            p_i=internal_mpa,
            p_e=external_mpa,
        )
        strategy = CODE_REGISTRY[DesignCode.API_RP_1111](edition=edition)

        results = strategy.run_checks(geometry, material, loads, DesignFactors())

        assert "combined_loading" in strategy.check_names
        assert set(results) == set(strategy.check_names)
        utilisation, details = results["combined_loading"]
        assert utilisation == pytest.approx(reference["utilization"], rel=1e-12)
        assert (utilisation <= 1.0) == reference["pass"]
        assert details["utilisation"] == utilisation
        assert details["M_p"] == pytest.approx(reference["M_p"], rel=1e-12)
        assert details["S_p"] == pytest.approx(reference["S_p"], rel=1e-12)
        assert details["M_p"] == pytest.approx(
            strategy.compute_plastic_moment(geometry, material), rel=1e-12
        )
        assert details["S_p"] == pytest.approx(
            strategy.compute_plastic_tension(geometry, material), rel=1e-12
        )

    @pytest.mark.parametrize(
        "loads", [DesignLoads(bending_moment=1.0e9), DesignLoads(effective_tension=1.0e9)]
    )
    def test_combined_check_governs_analysis(self, loads):
        result = WallThicknessAnalyzer(
            _geo(), _mat(), loads, DesignFactors(), code=DesignCode.API_RP_1111
        ).perform_analysis()

        assert result.governing_check == "combined_loading"
        assert result.max_utilisation == result.checks["combined_loading"]
        assert result.is_safe is False
        for name in ("burst", "collapse", "propagation"):
            assert result.checks[name] == 0.0
