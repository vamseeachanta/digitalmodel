"""#1345 (C2): conductor wellhead-moment model (Hetényi beam-on-elastic-foundation).

Oracles are the superposed semi-infinite-beam closed form for a top end-force H
(the riser lower-flex-joint reaction) plus a stand-off end moment
``M0 = H * h_stack`` (the flex-joint sits above the mudline atop the BOP/LMRP
stack, so H acts over that arm):

    M(x) = M0 e^{-bx}(cos bx + sin bx) - (H/b) e^{-bx} sin bx,   b = (k/4EI)^{1/4}

The peak is at the mudline (x=0), dominated by M0 — NOT an interior peak. The
stand-off term M0 dwarfs the pure soil-reaction peak (~0.32 H/b), so omitting it
would understate the wellhead moment ~10x (non-conservative).
"""
from __future__ import annotations

import math

import numpy as np
import pytest

from digitalmodel.drilling_riser.conductor_response import (
    ConductorResponse,
    solve_conductor_moment,
)

# 30" conductor in firm soil (SI, synthetic screening values).
_OD, _WT, _E = 0.762, 0.0254, 2.07e11
_I = math.pi / 64.0 * (_OD**4 - (_OD - 2 * _WT) ** 4)
_EI = _E * _I
_BASE = dict(soil_modulus_n_per_m2=5.0e6, ei_nm2=_EI)


def test_zero_shear_zero_moment():
    r = solve_conductor_moment(shear_n=0.0, stand_off_m=18.0, **_BASE)
    assert isinstance(r, ConductorResponse)
    assert r.max_moment_nm == pytest.approx(0.0, abs=1e-6)


def test_peak_at_mudline_equals_standoff_moment():
    H, h = 5.0e5, 18.0
    r = solve_conductor_moment(shear_n=H, stand_off_m=h, **_BASE)
    M0 = H * h
    assert r.stand_off_moment_nm == pytest.approx(M0)
    # peak is AT the mudline (x=0) and equals the stand-off moment
    assert abs(r.moment_nm[0]) == pytest.approx(M0, rel=1e-9)
    assert r.max_moment_nm == pytest.approx(M0, rel=1e-6)
    assert int(np.argmax(np.abs(r.moment_nm))) == 0


def test_standoff_moment_dominates_soil_reaction_term():
    H, h = 5.0e5, 18.0
    r = solve_conductor_moment(shear_n=H, stand_off_m=h, **_BASE)
    soil_reaction_peak = 0.3224 * H / r.beta_per_m  # pure end-force Hetényi peak
    # the stand-off moment is ~an order of magnitude larger (non-conservative to omit)
    assert r.max_moment_nm > 5.0 * soil_reaction_peak


def test_monotone_in_shear_and_standoff():
    a = solve_conductor_moment(shear_n=2.0e5, stand_off_m=18.0, **_BASE)
    b = solve_conductor_moment(shear_n=6.0e5, stand_off_m=18.0, **_BASE)
    c = solve_conductor_moment(shear_n=2.0e5, stand_off_m=25.0, **_BASE)
    assert b.max_moment_nm > a.max_moment_nm > 0.0     # more shear -> more moment
    assert c.max_moment_nm > a.max_moment_nm            # more stand-off -> more moment


def test_moment_decays_with_depth():
    r = solve_conductor_moment(shear_n=5.0e5, stand_off_m=18.0, **_BASE)
    # decayed to a small fraction of the mudline peak by the far end of the grid
    assert abs(r.moment_nm[-1]) < 0.05 * r.max_moment_nm
    assert r.beta_per_m > 0.0
