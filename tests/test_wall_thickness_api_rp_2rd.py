# ABOUTME: Tests for API RP 2RD riser wall thickness design code strategy
# ABOUTME: Burst, collapse, hoop stress checks and plastic capacity verification

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
from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_2rd import (
    ApiRp2rdStrategy,
)


@pytest.fixture
def geometry():
    return PipeGeometry(
        outer_diameter=0.27305,
        wall_thickness=0.02144,
        corrosion_allowance=0.001,
    )


@pytest.fixture
def material():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


@pytest.fixture
def loads():
    return DesignLoads(
        internal_pressure=20e6,
        external_pressure=10e6,
    )


@pytest.fixture
def factors():
    return DesignFactors()


@pytest.fixture
def strategy():
    return ApiRp2rdStrategy()


def test_registry_contains_api_rp_2rd():
    """API RP 2RD strategy is auto-registered in CODE_REGISTRY."""
    assert DesignCode.API_RP_2RD in CODE_REGISTRY
    assert CODE_REGISTRY[DesignCode.API_RP_2RD] is ApiRp2rdStrategy


def test_burst_utilisation_reasonable(strategy, geometry, material, loads, factors):
    """Burst utilisation is positive and < 1.0 for standard riser pipe."""
    results = strategy.run_checks(geometry, material, loads, factors)
    util, details = results["burst"]
    assert 0 < util < 1.0
    assert details["f_d"] == pytest.approx(2.0 / 3.0, abs=0.001)
    assert details["p_b"] > 0


def test_collapse_utilisation_reasonable(strategy, geometry, material, loads, factors):
    """Collapse utilisation is positive and < 1.0 for standard riser pipe."""
    results = strategy.run_checks(geometry, material, loads, factors)
    util, details = results["collapse"]
    assert 0 < util < 1.0
    assert details["f_c"] == pytest.approx(2.0 / 3.0, abs=0.001)


def test_hoop_check_present(strategy, geometry, material, loads, factors):
    """Hoop stress check is present in API RP 2RD results."""
    results = strategy.run_checks(geometry, material, loads, factors)
    assert "hoop" in results
    util, details = results["hoop"]
    assert util > 0
    assert details["sigma_allow"] == pytest.approx(0.6 * material.smys)


def test_hoop_utilisation_reasonable(strategy, geometry, material, loads, factors):
    """Hoop utilisation for 10 MPa net pressure on standard pipe < 1.0."""
    results = strategy.run_checks(geometry, material, loads, factors)
    util, _ = results["hoop"]
    assert util < 1.0


def test_plastic_moment_matches_api_1111(strategy, geometry, material):
    """API RP 2RD uses same plastic moment formula as API RP 1111."""
    from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import (
        ApiRp1111Strategy,
    )

    M_p_2rd = strategy.compute_plastic_moment(geometry, material)
    M_p_1111 = ApiRp1111Strategy().compute_plastic_moment(geometry, material)
    assert M_p_2rd == pytest.approx(M_p_1111, rel=1e-10)


def test_plastic_tension_matches_api_1111(strategy, geometry, material):
    """API RP 2RD uses same plastic tension formula as API RP 1111."""
    from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import (
        ApiRp1111Strategy,
    )

    T_y_2rd = strategy.compute_plastic_tension(geometry, material)
    T_y_1111 = ApiRp1111Strategy().compute_plastic_tension(geometry, material)
    assert T_y_2rd == pytest.approx(T_y_1111, rel=1e-10)


def test_burst_more_conservative_than_api_1111(strategy, geometry, material, loads, factors):
    """API RP 2RD Operating (f_d=0.667) is more conservative than API RP 1111 (f_d=0.72)."""
    from digitalmodel.structural.analysis.wall_thickness_codes.api_rp_1111 import (
        ApiRp1111Strategy,
    )

    results_2rd = strategy.run_checks(geometry, material, loads, factors)
    results_1111 = ApiRp1111Strategy().run_checks(geometry, material, loads, factors)

    util_2rd = results_2rd["burst"][0]
    util_1111 = results_1111["burst"][0]

    # Higher utilisation = more conservative (lower allowable)
    assert util_2rd > util_1111
