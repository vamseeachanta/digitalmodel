# ABOUTME: Skeleton tests for WallThicknessAnalyzer and core data classes
# ABOUTME: Tests PipeGeometry, PipeMaterial, DesignLoads, DesignFactors, WallThicknessAnalyzer

import math
import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    CodeEdition,
    DesignCode,
    DesignFactors,
    DesignLoads,
    FabricationType,
    PipeGeometry,
    PipeMaterial,
    SafetyClass,
    WallThicknessAnalyzer,
    WallThicknessResult,
)


# ---------------------------------------------------------------------------
# PipeGeometry tests
# ---------------------------------------------------------------------------
class TestPipeGeometry:
    def test_basic_creation(self):
        g = PipeGeometry(outer_diameter=0.3048, wall_thickness=0.0127)
        assert g.outer_diameter == 0.3048
        assert g.wall_thickness == 0.0127

    def test_t1_property(self):
        g = PipeGeometry(
            outer_diameter=0.3048,
            wall_thickness=0.0127,
            corrosion_allowance=0.003,
            fabrication_tolerance=0.125,
        )
        expected_t1 = (0.0127 - 0.003) * (1 - 0.125)
        assert g.t1 == pytest.approx(expected_t1, rel=1e-6)

    def test_t2_property(self):
        g = PipeGeometry(outer_diameter=0.3048, wall_thickness=0.0127, corrosion_allowance=0.003)
        assert g.t2 == pytest.approx(0.0127 - 0.003, rel=1e-6)

    def test_inner_diameter_property(self):
        g = PipeGeometry(outer_diameter=0.3048, wall_thickness=0.0127)
        assert g.inner_diameter == pytest.approx(0.3048 - 2 * 0.0127, rel=1e-6)

    def test_d_over_t_property(self):
        g = PipeGeometry(outer_diameter=0.3048, wall_thickness=0.0127)
        assert g.d_over_t == pytest.approx(0.3048 / 0.0127, rel=1e-6)

    def test_invalid_negative_od_raises(self):
        with pytest.raises(ValueError, match="Outer diameter must be positive"):
            PipeGeometry(outer_diameter=-0.1, wall_thickness=0.01)

    def test_invalid_zero_wt_raises(self):
        with pytest.raises(ValueError, match="Wall thickness must be positive"):
            PipeGeometry(outer_diameter=0.3048, wall_thickness=0.0)

    def test_invalid_wt_too_large_raises(self):
        with pytest.raises(ValueError, match="less than half"):
            PipeGeometry(outer_diameter=0.3048, wall_thickness=0.16)

    def test_invalid_negative_ca_raises(self):
        with pytest.raises(ValueError, match="Corrosion allowance"):
            PipeGeometry(outer_diameter=0.3048, wall_thickness=0.02, corrosion_allowance=-0.001)


# ---------------------------------------------------------------------------
# PipeMaterial tests
# ---------------------------------------------------------------------------
class TestPipeMaterial:
    def test_basic_creation_x65(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6)
        assert m.grade == "X65"
        assert m.smys == 448e6
        assert m.smts == 531e6
        assert m.youngs_modulus == pytest.approx(207e9, rel=1e-6)

    def test_alpha_fab_seamless(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6, fabrication_type=FabricationType.SEAMLESS)
        assert m.alpha_fab == pytest.approx(1.00, abs=1e-4)

    def test_alpha_fab_uoe(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6, fabrication_type=FabricationType.UOE)
        assert m.alpha_fab == pytest.approx(0.93, abs=1e-4)

    def test_alpha_fab_uo(self):
        m = PipeMaterial(grade="X65", smys=448e6, smts=531e6, fabrication_type=FabricationType.UO)
        assert m.alpha_fab == pytest.approx(0.85, abs=1e-4)

    def test_smts_lt_smys_raises(self):
        with pytest.raises(ValueError, match="SMTS must be >= SMYS"):
            PipeMaterial(grade="X52", smys=448e6, smts=300e6)

    def test_negative_smys_raises(self):
        with pytest.raises(ValueError, match="SMYS must be positive"):
            PipeMaterial(grade="X52", smys=-10e6, smts=448e6)


# ---------------------------------------------------------------------------
# DesignLoads tests
# ---------------------------------------------------------------------------
class TestDesignLoads:
    def test_defaults_zero(self):
        l = DesignLoads()
        assert l.internal_pressure == 0.0
        assert l.external_pressure == 0.0
        assert l.bending_moment == 0.0
        assert l.effective_tension == 0.0

    def test_net_internal_pressure_positive(self):
        l = DesignLoads(internal_pressure=10e6, external_pressure=2e6)
        assert l.net_internal_pressure == pytest.approx(8e6, rel=1e-6)

    def test_net_internal_pressure_clamped_at_zero(self):
        l = DesignLoads(internal_pressure=1e6, external_pressure=5e6)
        assert l.net_internal_pressure == 0.0


# ---------------------------------------------------------------------------
# DesignFactors tests
# ---------------------------------------------------------------------------
class TestDesignFactors:
    def test_medium_safety_class_defaults(self):
        f = DesignFactors()
        assert f.safety_class == SafetyClass.MEDIUM
        assert f.gamma_sc == pytest.approx(1.138, rel=1e-3)
        assert f.gamma_m == pytest.approx(1.15, rel=1e-3)
        assert f.gamma_inc == pytest.approx(1.0, rel=1e-6)

    def test_low_safety_class(self):
        f = DesignFactors(safety_class=SafetyClass.LOW)
        assert f.gamma_sc == pytest.approx(1.046, rel=1e-3)

    def test_high_safety_class(self):
        f = DesignFactors(safety_class=SafetyClass.HIGH)
        assert f.gamma_sc == pytest.approx(1.308, rel=1e-3)


# ---------------------------------------------------------------------------
# CodeEdition tests
# ---------------------------------------------------------------------------
class TestCodeEdition:
    def test_display_label(self):
        ce = CodeEdition(DesignCode.DNV_ST_F101, 2021, "DNV-ST-F101")
        label = ce.display_label
        assert "DNV-ST-F101" in label
        assert "2021" in label


# ---------------------------------------------------------------------------
# WallThicknessResult tests
# ---------------------------------------------------------------------------
class TestWallThicknessResult:
    def test_defaults(self):
        r = WallThicknessResult()
        assert r.is_safe is True
        assert r.max_utilisation == 0.0
        assert r.governing_check is None
        assert r.checks == {}


# ---------------------------------------------------------------------------
# WallThicknessAnalyzer tests
# ---------------------------------------------------------------------------
class TestWallThicknessAnalyzer:
    def _make(self, **load_kwargs):
        geo = PipeGeometry(outer_diameter=0.3048, wall_thickness=0.0159)
        mat = PipeMaterial(grade="X65", smys=448e6, smts=531e6)
        loads = DesignLoads(**load_kwargs)
        factors = DesignFactors()
        return WallThicknessAnalyzer(geo, mat, loads, factors)

    def test_can_instantiate(self):
        analyzer = self._make()
        assert analyzer is not None
        assert analyzer.code == DesignCode.DNV_ST_F101

    def test_pressure_containment_runs(self):
        analyzer = self._make(internal_pressure=15e6)
        util, details = analyzer.check_pressure_containment()
        assert isinstance(util, float)
        assert util > 0
        assert "utilisation" in details

    def test_collapse_runs(self):
        analyzer = self._make(external_pressure=5e6)
        util, details = analyzer.check_collapse()
        assert isinstance(util, float)
        assert util > 0

    def test_combined_loading_no_loads(self):
        analyzer = self._make()
        util, details = analyzer.check_combined_loading()
        assert util == pytest.approx(0.0, abs=1e-6)

    def test_code_api_rp_1111_selection(self):
        geo = PipeGeometry(outer_diameter=0.3048, wall_thickness=0.0159)
        mat = PipeMaterial(grade="X65", smys=448e6, smts=531e6)
        loads = DesignLoads(internal_pressure=10e6)
        factors = DesignFactors()
        analyzer = WallThicknessAnalyzer(geo, mat, loads, factors, code=DesignCode.API_RP_1111)
        assert analyzer.code == DesignCode.API_RP_1111
