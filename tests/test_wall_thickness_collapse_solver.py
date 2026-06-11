# ABOUTME: Regression tests for the DNV-ST-F101 Eq 5.9 collapse-pressure solver bracket.
# ABOUTME: Pins the deepwater (p_p > p_el) case that previously converged to a spurious high root.

"""DNV collapse cubic solver — bracket regression (deckhand#227).

The characteristic collapse pressure p_c solves Eq 5.9:

    (p_c - p_el)(p_c^2 - p_p^2) = p_c * p_el * p_p * f_0 * D/t2

The physical root is the smallest positive one and always lies in
(0, min(p_el, p_p)): collapse cannot exceed either the elastic or the plastic
limit. The previous bisection bracket (0, max(p_el, p_p)*1.5) converged to a
spurious high root of the cubic whenever p_p > p_el — true for thin t2/D in
deepwater — overstating collapse resistance roughly 5x. Live consequence: a
12" X65 export line at 1500 m screened as PASSING collapse at 12.7 mm wall
(U=0.245) when the true utilisation is ~1.76.
"""

import pytest

from digitalmodel.structural.analysis.wall_thickness import (
    DesignCode,
    DesignFactors,
    DesignLoads,
    PipeGeometry,
    PipeMaterial,
    WallThicknessAnalyzer,
)
from digitalmodel.structural.analysis.wall_thickness_codes.dnv_st_f101 import (
    DnvStF101Strategy,
)

SEAWATER_1500M_PA = 1025.0 * 9.81 * 1500.0  # ~15.08 MPa


def make_x65():
    return PipeMaterial(grade="X65", smys=448e6, smts=531e6)


def dnv_collapse(t_mm, depth_pa=SEAWATER_1500M_PA, ca=0.003):
    geometry = PipeGeometry(
        outer_diameter=0.3239, wall_thickness=t_mm / 1000.0, corrosion_allowance=ca
    )
    loads = DesignLoads(internal_pressure=0.0, external_pressure=depth_pa)
    result = WallThicknessAnalyzer(
        geometry, make_x65(), loads, DesignFactors(), DesignCode.DNV_ST_F101
    ).perform_analysis()
    return result.checks["collapse"], result.details["collapse"]


@pytest.mark.unit
def test_deepwater_root_below_elastic_and_plastic_limits():
    # p_p > p_el regime (the bug trigger): 12.7 mm gross, t2 = 9.7 mm.
    util, d = dnv_collapse(12.7)
    assert d["p_p"] > d["p_el"], "fixture must exercise the p_p > p_el regime"
    assert 0.0 < d["p_c"] < min(d["p_el"], d["p_p"])


@pytest.mark.unit
def test_deepwater_12in_1500m_fails_collapse_at_12p7mm():
    # Regression pin: the spurious root reported U=0.245 here. True value
    # ~1.76 (p_el=12.2 MPa caps resistance; p_c≈11.2 MPa; /1.31 design).
    util, _ = dnv_collapse(12.7)
    assert 1.6 < util < 1.9


@pytest.mark.unit
def test_collapse_governed_minimum_matches_hand_calc():
    # First passing ASME standard wall for this case is 15.88 mm (0.625 in):
    # U(14.27) > 1, U(15.88) < 1.
    util_14, _ = dnv_collapse(14.27)
    util_16, _ = dnv_collapse(15.88)
    assert util_14 > 1.0
    assert util_16 < 1.0


@pytest.mark.unit
def test_root_satisfies_eq_5_9():
    _, d = dnv_collapse(12.7)
    p_c, p_el, p_p, f_0 = d["p_c"], d["p_el"], d["p_p"], d["f_0"]
    t2 = d["t2"]
    residual = (p_c - p_el) * (p_c**2 - p_p**2) - p_c * p_el * p_p * f_0 * (
        0.3239 / t2
    )
    # Pa^3-scale equation; normalise by p_el^3 for a dimensionless residual.
    assert abs(residual) / p_el**3 < 1e-3


@pytest.mark.unit
def test_thick_wall_regime_unaffected():
    # p_el > p_p regime (the old bracket happened to work here): the root
    # must still be below the plastic limit and utilisation comfortably < 1.
    util, d = dnv_collapse(23.83)
    assert d["p_el"] > d["p_p"]
    assert 0.0 < d["p_c"] < min(d["p_el"], d["p_p"])
    assert util < 0.5


@pytest.mark.unit
def test_strategy_solver_directly():
    # The strategy's static solver, exercised without the analyzer wrapper.
    p_el, p_p, f_0, D, t2 = 12.22e6, 26.83e6, 0.005, 0.3239, 0.0097
    p_c = DnvStF101Strategy._solve_collapse_pressure(p_el, p_p, f_0, D, t2)
    assert 0.0 < p_c < min(p_el, p_p)
