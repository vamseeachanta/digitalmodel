# ABOUTME: Self-contained tests for ASME B31.4 wall thickness code strategy
# ABOUTME: Covers burst (Barlow), collapse, propagation, and registry checks

import math

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
)
from digitalmodel.structural.analysis.wall_thickness_codes import CODE_REGISTRY
from digitalmodel.structural.analysis.wall_thickness_codes.base import CodeStrategy


# ---------------------------------------------------------------------------
# Shared fixture data -- 12" API 5L X52 pipe
# ---------------------------------------------------------------------------
OUTER_DIAMETER = 0.3048  # 12 inch
WALL_THICKNESS = 0.0127  # 0.5 inch
SMYS = 358.5e6  # X52 Pa
SMTS = 455.1e6  # X52 Pa


def _make_geometry(**overrides):
    defaults = dict(outer_diameter=OUTER_DIAMETER, wall_thickness=WALL_THICKNESS)
    defaults.update(overrides)
    return PipeGeometry(**defaults)


def _make_material(**overrides):
    defaults = dict(grade="X52", smys=SMYS, smts=SMTS)
    defaults.update(overrides)
    return PipeMaterial(**defaults)


def _make_loads(**overrides):
    defaults = dict(internal_pressure=0.0, external_pressure=0.0)
    defaults.update(overrides)
    return DesignLoads(**defaults)


# ===========================================================================
# Burst check -- ASME B31.4 S403.2.1
# ===========================================================================
class TestAsmeB314Burst:
    """ASME B31.4 S403.2.1 burst pressure check."""

    def test_12inch_x52_burst_utilisation(self):
        """Standard 12" X52 pipe at 10 MPa internal pressure.

        P_d = 2 * SMYS * t * F * E * T / D
            = 2 * 358.5e6 * 0.0127 * 0.72 * 1.0 * 1.0 / 0.3048
            = 21.497 MPa (approx)
        Utilisation = 10.0 / 21.497 = 0.4652 (approx)
        """
        from digitalmodel.structural.analysis.wall_thickness_codes.asme_b31_4 import (
            AsmeB314Strategy,
        )

        strategy = AsmeB314Strategy()
        geom = _make_geometry()
        mat = _make_material()
        loads = _make_loads(internal_pressure=10e6)
        factors = DesignFactors()

        results = strategy.run_checks(geom, mat, loads, factors)
        util, details = results["burst"]

        # Expected: P_d = 2 * 358.5e6 * 0.0127 * 0.72 / 0.3048
        p_d_expected = 2 * SMYS * WALL_THICKNESS * 0.72 * 1.0 * 1.0 / OUTER_DIAMETER
        util_expected = 10e6 / p_d_expected

        assert util == pytest.approx(util_expected, abs=1e-4)
        assert details["p_d"] == pytest.approx(p_d_expected, abs=1e0)

    def test_burst_with_corrosion_allowance(self):
        """Corrosion allowance reduces effective thickness -> higher utilisation."""
        from digitalmodel.structural.analysis.wall_thickness_codes.asme_b31_4 import (
            AsmeB314Strategy,
        )

        strategy = AsmeB314Strategy()
        ca = 0.003  # 3 mm corrosion allowance
        geom = _make_geometry(corrosion_allowance=ca)
        mat = _make_material()
        loads = _make_loads(internal_pressure=10e6)
        factors = DesignFactors()

        results = strategy.run_checks(geom, mat, loads, factors)
        util_with_ca, details = results["burst"]

        # Effective thickness reduced -> utilisation should be higher
        t_eff = WALL_THICKNESS - ca
        p_d_ca = 2 * SMYS * t_eff * 0.72 * 1.0 * 1.0 / OUTER_DIAMETER
        util_expected = 10e6 / p_d_ca

        assert util_with_ca == pytest.approx(util_expected, abs=1e-4)
        assert util_with_ca > 0.465  # Higher than without CA

    def test_zero_effective_thickness_raises(self):
        """Corrosion allowance equal to wall thickness -> ValueError or inf."""
        from digitalmodel.structural.analysis.wall_thickness_codes.asme_b31_4 import (
            AsmeB314Strategy,
        )

        strategy = AsmeB314Strategy()
        # corrosion_allowance = wall_thickness -> effective t = 0
        geom = _make_geometry(corrosion_allowance=WALL_THICKNESS)
        mat = _make_material()
        loads = _make_loads(internal_pressure=10e6)
        factors = DesignFactors()

        results = strategy.run_checks(geom, mat, loads, factors)
        util, _ = results["burst"]
        assert util == float("inf")


# ===========================================================================
# Collapse check -- ASME B31.4 S403.2.2
# ===========================================================================
class TestAsmeB314Collapse:
    """ASME B31.4 S403.2.2 collapse pressure check."""

    def test_12inch_x52_collapse(self):
        """Standard pipe at 5 MPa external pressure -> utilisation < 1.0."""
        from digitalmodel.structural.analysis.wall_thickness_codes.asme_b31_4 import (
            AsmeB314Strategy,
        )

        strategy = AsmeB314Strategy()
        geom = _make_geometry()
        mat = _make_material()
        loads = _make_loads(external_pressure=5e6)
        factors = DesignFactors()

        results = strategy.run_checks(geom, mat, loads, factors)
        util, details = results["collapse"]

        assert util < 1.0
        assert util > 0.0
        assert "p_el" in details
        assert "p_y" in details
        assert "p_c" in details


# ===========================================================================
# Propagation check -- ASME B31.4 S403.2.3
# ===========================================================================
class TestAsmeB314Propagation:
    """ASME B31.4 S403.2.3 propagation buckling check."""

    def test_12inch_x52_propagation(self):
        """Standard pipe at 5 MPa external pressure."""
        from digitalmodel.structural.analysis.wall_thickness_codes.asme_b31_4 import (
            AsmeB314Strategy,
        )

        strategy = AsmeB314Strategy()
        geom = _make_geometry()
        mat = _make_material()
        loads = _make_loads(external_pressure=5e6)
        factors = DesignFactors()

        results = strategy.run_checks(geom, mat, loads, factors)
        util, details = results["propagation"]

        # Expected: p_pr = 24 * SMYS * (t/D)^2.4
        p_pr_expected = 24 * SMYS * (WALL_THICKNESS / OUTER_DIAMETER) ** 2.4
        f_p = 0.80
        p_pr_design = f_p * p_pr_expected
        util_expected = 5e6 / p_pr_design

        assert util == pytest.approx(util_expected, abs=1e-4)
        assert details["p_pr"] == pytest.approx(p_pr_expected, abs=1e0)


# ===========================================================================
# Registry checks
# ===========================================================================
class TestAsmeB314Registration:
    """Verify ASME B31.4 registers in CODE_REGISTRY."""

    def test_code_in_registry(self):
        assert DesignCode.ASME_B31_4 in CODE_REGISTRY

    def test_strategy_is_code_strategy(self):
        cls = CODE_REGISTRY[DesignCode.ASME_B31_4]
        instance = cls()
        assert isinstance(instance, CodeStrategy)
        assert instance.code_name == "ASME-B31.4"
        assert "burst" in instance.check_names
        assert "collapse" in instance.check_names
        assert "propagation" in instance.check_names
