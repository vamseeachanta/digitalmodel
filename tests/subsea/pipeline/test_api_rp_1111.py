import pytest
import math
from digitalmodel.subsea.pipeline.pipeline_pressure import (
    api_burst_pressure,
    api_collapse_pressure,
    api_external_collapse_check,
    api_combined_loading_external_check,
    api_combined_loading_internal_check,
    api_propagating_buckle_pressure,
    api_propagating_buckle_check,
    api_transition_water_depth,
    PipeMaterial,
)

def test_api_burst_pressure():
    # Example: OD=12.75", WT=0.5", X65 (SMYS=448.16, SMTS=530.9)
    # D/t = 12.75/0.5 = 25.5 > 15
    # p_b = 0.9 * (448.16 + 530.9) * 0.5 / (12.75 - 0.5)
    # p_b = 0.9 * 979.06 * 0.5 / 12.25 = 36.0 MPa
    D = 12.75 * 0.0254
    t = 0.5 * 0.0254
    mat = PipeMaterial(name="X65", SMYS=448.16, SMTS=530.9)
    pb = api_burst_pressure(D, t, mat)
    assert pb == pytest.approx(36.0, rel=1e-2)

    # Example D/t <= 15
    # WT = 1.0", D=12.75", D/t = 12.75
    # p_b = 0.45 * (448.16 + 530.9) * ln(12.75 / (12.75 - 2))
    # p_b = 0.45 * 979.06 * ln(12.75 / 10.75) = 440.57 * 0.170 = 75.1 MPa
    t2 = 1.0 * 0.0254
    pb2 = api_burst_pressure(D, t2, mat)
    assert pb2 == pytest.approx(75.1, rel=1e-2)

def test_api_collapse_pressure():
    # Example: OD=12.75", WT=0.5", X65
    # P_y = 2 * 448.16 * (0.5 / 12.75) = 35.15 MPa
    # P_e = 2 * 207000 * (0.5/12.75)^3 / (1-0.3^2) = 454945 * 6.04e-5 = 27.5 MPa
    # P_c = (35.15 * 27.5) / sqrt(35.15^2 + 27.5^2) = 966.6 / 44.63 = 21.66 MPa
    D = 12.75 * 0.0254
    t = 0.5 * 0.0254
    smys = 448.16
    pc = api_collapse_pressure(D, t, smys)
    assert pc == pytest.approx(21.66, rel=1e-2)

def test_api_external_collapse_check():
    D = 12.75 * 0.0254
    t = 0.5 * 0.0254
    smys = 448.16
    # p_allowable = 0.7 * 21.66 = 15.16 MPa
    # depth = 1000m => p_water ~ 10.0 MPa
    res = api_external_collapse_check(depth=1000.0, D=D, t=t, SMYS=smys)
    assert res["pass"] is True
    assert res["p_allowable"] == pytest.approx(15.16, rel=1e-2)

def test_api_combined_loading_external():
    D = 0.32385
    t = 0.0127
    smys = 448.16
    epsilon = 0.001
    pe = 10.0
    pi = 0.0
    
    # epsilon_b = t/2D = 0.0127 / 0.6477 = 0.0196
    # p_c = 21.66
    # Term 1: 3.33 * 0.001 / 0.0196 = 0.170
    # Term 2: 10.0 / (0.7 * 21.66) = 10.0 / 15.16 = 0.660
    # U = 0.170 + 0.660 = 0.83
    res = api_combined_loading_external_check(
        epsilon=epsilon, p_e=pe, p_i=pi, D=D, t=t, SMYS=smys,
        condition="installation"
    )
    assert res["pass"] is True
    assert res["utilization"] == pytest.approx(0.83, rel=1e-2)

def test_api_combined_loading_internal():
    mat = PipeMaterial(name="X65", SMYS=448.16, SMTS=530.9)
    D = 0.32385
    t = 0.0127
    pi = 20.0
    pe = 0.0
    Te = 500000.0 # 500 kN
    M = 100000.0 # 100 kNm
    
    # p_b = 36.0
    # Ty = 448.16 * pi * (0.32385 - 0.0127) * 0.0127 = 5.56 MN
    # Mp = 448.16 * (0.32385 - 0.0127)^2 * 0.0127 = 0.550 MNm
    # Term P: (20 / 36)^2 = 0.308
    # Term T: (0.5 / 5.56)^2 = 0.008
    # Term M: (0.1 / 0.55)^2 = 0.033
    # sqrt(0.3086 + 0.00809 + 0.0330) = sqrt(0.34969) = 0.5913
    # U = 0.5913 / 0.9 = 0.657
    res = api_combined_loading_internal_check(
        p_i=pi, p_e=pe, T_e=Te, M=M, D=D, t=t, mat=mat
    )
    assert res["pass"] is True
    assert res["utilization"] == pytest.approx(0.657, rel=1e-2)


# ---------------------------------------------------------------------------
# Propagating Buckle — API RP 1111 Section 4.3.2
# ---------------------------------------------------------------------------

def test_api_propagating_buckle_pressure():
    """API RP 1111 Eq. 4.3.2-1: p_p = 24 * SMYS * (t/D)^2.4."""
    D = 12.75 * 0.0254   # 0.32385 m
    t = 0.5 * 0.0254     # 0.0127 m
    smys = 448.16
    pp = api_propagating_buckle_pressure(D, t, smys)
    assert pp == pytest.approx(4.528, rel=1e-2)


def test_api_transition_water_depth():
    """Transition depth = p_p * 1e6 / (rho_sw * g)."""
    D = 12.75 * 0.0254
    t = 0.5 * 0.0254
    smys = 448.16
    depth = api_transition_water_depth(D, t, smys)
    assert depth == pytest.approx(450.4, rel=1e-2)


def test_api_propagating_buckle_check_pass():
    """At 200 m depth: p_water < p_p_allow -> no arrestors needed."""
    D = 12.75 * 0.0254
    t = 0.5 * 0.0254
    smys = 448.16
    res = api_propagating_buckle_check(depth=200.0, D=D, t=t, SMYS=smys)
    assert res["pass"] is True
    assert res["arrestors_required"] is False
    assert res["utilization"] == pytest.approx(0.577, rel=2e-2)


def test_api_propagating_buckle_check_fail():
    """At 600 m depth: p_water > p_p_allow -> arrestors required."""
    D = 12.75 * 0.0254
    t = 0.5 * 0.0254
    smys = 448.16
    res = api_propagating_buckle_check(depth=600.0, D=D, t=t, SMYS=smys)
    assert res["pass"] is False
    assert res["arrestors_required"] is True
