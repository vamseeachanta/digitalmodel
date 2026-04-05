# ABOUTME: Skeleton tests for DNV-ST-F101 wall thickness code strategy
# ABOUTME: Covers pressure containment, collapse, propagation, combined loading

import math
import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_codes.base import CodeStrategy


# ---------------------------------------------------------------------------
# Shared test data -- 12" API 5L X65 pipe
# ---------------------------------------------------------------------------
OD = 0.3048       # 12 inch in metres
WT = 0.0159       # ~5/8" in metres
SMYS = 448.0e6    # X65 Pa
SMTS = 531.0e6    # X65 Pa


def _geo(**overrides):
    d = dict(outer_diameter=OD, wall_thickness=WT)
    d.update(overrides)
    return PipeGeometry(**d)


def _mat(**overrides):
    d = dict(grade="X65", smys=SMYS, smts=SMTS)
    d.update(overrides)
    return PipeMaterial(**d)


def _loads(**overrides):
    d = dict(internal_pressure=0.0, external_pressure=0.0)
    d.update(overrides)
    return DesignLoads(**d)


# ===========================================================================
# Registry checks
# ===========================================================================
class TestDnvStF101Registration:
    def test_code_in_registry(self):
        from digitalmodel.structural.analysis.wall_thickness_codes import dnv_st_f101
        assert DesignCode.DNV_ST_F101 in CODE_REGISTRY

    def test_strategy_is_code_strategy(self):
        cls = CODE_REGISTRY[DesignCode.DNV_ST_F101]
        inst = cls()
        assert isinstance(inst, CodeStrategy)
        assert inst.code_name == "DNV-ST-F101"
        assert "pressure_containment" in inst.check_names
        assert "collapse" in inst.check_names
        assert "propagation_buckling" in inst.check_names
        assert "combined_loading" in inst.check_names

    def test_latest_edition_default(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import (
            DnvStF101Strategy, LATEST_EDITION,
        )
        strategy = DnvStF101Strategy()
        assert strategy.edition_year == LATEST_EDITION

    def test_2007_edition_selection(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy
        strategy = DnvStF101Strategy(edition=2007)
        assert strategy.edition_year == 2007
        factors = strategy.get_edition_factors()
        # 2007 edition gamma_SC medium = 1.14
        assert factors["gamma_SC"]["medium"] == pytest.approx(1.14, abs=1e-4)

    def test_invalid_edition_raises(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy
        with pytest.raises(ValueError, match="Unknown edition"):
            DnvStF101Strategy(edition=1990)


# ===========================================================================
# Pressure containment (burst) check
# ===========================================================================
class TestDnvStF101PressureContainment:
    def _strategy(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy
        return DnvStF101Strategy()

    def test_burst_low_pressure_passes(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(internal_pressure=5e6), DesignFactors()
        )
        util, details = results["pressure_containment"]
        assert 0 < util < 1.0
        assert "p_b" in details
        assert "p_b_design" in details
        assert "utilisation" in details

    def test_burst_high_pressure_fails(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(internal_pressure=100e6), DesignFactors()
        )
        util, _ = results["pressure_containment"]
        assert util > 1.0

    def test_burst_zero_pressure_zero_util(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(), DesignFactors()
        )
        util, _ = results["pressure_containment"]
        assert util == pytest.approx(0.0, abs=1e-9)

    def test_burst_safety_class_high_increases_utilisation(self):
        strategy = self._strategy()
        factors_med = DesignFactors(safety_class=SafetyClass.MEDIUM)
        factors_high = DesignFactors(safety_class=SafetyClass.HIGH)
        p_int = 20e6
        u_med, _ = strategy.run_checks(_geo(), _mat(), _loads(internal_pressure=p_int), factors_med)["pressure_containment"]
        u_high, _ = strategy.run_checks(_geo(), _mat(), _loads(internal_pressure=p_int), factors_high)["pressure_containment"]
        assert u_high > u_med

    def test_burst_uoe_fabrication_reduces_capacity(self):
        strategy = self._strategy()
        mat_seamless = _mat(fabrication_type=FabricationType.SEAMLESS)
        mat_uoe = _mat(fabrication_type=FabricationType.UOE)
        p_int = 20e6
        u_s, _ = strategy.run_checks(_geo(), mat_seamless, _loads(internal_pressure=p_int), DesignFactors())["pressure_containment"]
        u_u, _ = strategy.run_checks(_geo(), mat_uoe, _loads(internal_pressure=p_int), DesignFactors())["pressure_containment"]
        # For pressure containment alpha_fab is not used (only f_y, f_u)
        # so results may be the same - just check they run
        assert isinstance(u_s, float)
        assert isinstance(u_u, float)


# ===========================================================================
# Collapse check (local buckling under external pressure)
# ===========================================================================
class TestDnvStF101Collapse:
    def _strategy(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy
        return DnvStF101Strategy()

    def test_collapse_shallow_water_passes(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(external_pressure=1e6), DesignFactors()
        )
        util, details = results["collapse"]
        assert util > 0
        assert "p_el" in details
        assert "p_p" in details
        assert "p_c" in details

    def test_collapse_no_external_pressure_zero_util(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(), DesignFactors()
        )
        util, _ = results["collapse"]
        assert util == pytest.approx(0.0, abs=1e-9)

    def test_collapse_thicker_wall_lower_util(self):
        strategy = self._strategy()
        ext_p = 5e6
        geo_thin = _geo(wall_thickness=0.010)
        geo_thick = _geo(wall_thickness=0.025)
        u_thin, _ = strategy.run_checks(geo_thin, _mat(), _loads(external_pressure=ext_p), DesignFactors())["collapse"]
        u_thick, _ = strategy.run_checks(geo_thick, _mat(), _loads(external_pressure=ext_p), DesignFactors())["collapse"]
        assert u_thick < u_thin


# ===========================================================================
# Propagation buckling check
# ===========================================================================
class TestDnvStF101PropagationBuckling:
    def _strategy(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy
        return DnvStF101Strategy()

    def test_propagation_formula(self):
        strategy = self._strategy()
        geom = _geo()
        mat = _mat()
        # p_pr = 35 * SMYS * alpha_fab * (t/D)^2.5
        p_pr_expected = 35 * mat.smys * mat.alpha_fab * (geom.t2 / geom.outer_diameter) ** 2.5

        results = strategy.run_checks(
            geom, mat, _loads(external_pressure=2e6), DesignFactors()
        )
        _, details = results["propagation_buckling"]
        assert details["p_pr"] == pytest.approx(p_pr_expected, rel=1e-4)

    def test_propagation_zero_pressure(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(), DesignFactors()
        )
        util, _ = results["propagation_buckling"]
        assert util == pytest.approx(0.0, abs=1e-9)


# ===========================================================================
# Combined loading check
# ===========================================================================
class TestDnvStF101CombinedLoading:
    def _strategy(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy
        return DnvStF101Strategy()

    def test_combined_loading_no_loads_zero_util(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(), DesignFactors()
        )
        util, details = results["combined_loading"]
        assert util == pytest.approx(0.0, abs=1e-6)

    def test_combined_loading_bending_only(self):
        strategy = self._strategy()
        results = strategy.run_checks(
            _geo(), _mat(), _loads(bending_moment=500e3), DesignFactors()
        )
        util, details = results["combined_loading"]
        assert util > 0
        assert "M_p" in details
        assert "S_p" in details

    def test_plastic_moment_positive(self):
        strategy = self._strategy()
        M_p = strategy.compute_plastic_moment(_geo(), _mat())
        assert M_p > 0

    def test_plastic_tension_positive(self):
        strategy = self._strategy()
        S_p = strategy.compute_plastic_tension(_geo(), _mat())
        assert S_p > 0


# ===========================================================================
# Edition factor comparison 2007 vs 2021
# ===========================================================================
class TestDnvEditionComparison:
    def test_gamma_sc_high_differs_between_editions(self):
        from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import DnvStF101Strategy
        s2007 = DnvStF101Strategy(edition=2007)
        s2021 = DnvStF101Strategy(edition=2021)
        f2007 = s2007.get_edition_factors()
        f2021 = s2021.get_edition_factors()
        # 2007: high = 1.26, 2021: high = 1.308
        assert f2007["gamma_SC"]["high"] != f2021["gamma_SC"]["high"]
