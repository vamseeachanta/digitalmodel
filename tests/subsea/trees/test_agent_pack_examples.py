# # Copyright (c) 2024 Digital Model Project
# # Licensed under the MIT License. See LICENSE file for details.

"""Independent synthetic arithmetic; equipment qualification is NOT_EVALUATED."""

import pytest

from digitalmodel.subsea.trees import (
    bore_capacity,
    liquid_choke_flow,
    liquid_critical_drop,
    series_pressure_drop,
    trim_cv,
)


def test_series_loss_literal():
    """Fisher (2013), ch. 3 pp. 3-2–3-3, Table 3-2, N1=1.

    Synthetic Q=20 US gpm, SG=1, Cv=(10, 20):
    sum SG*(Q/Cv)^2 = (20/10)^2+(20/20)^2 = 4+1 = 5 psi.
    """
    assert series_pressure_drop(20, (10, 20), 1) == pytest.approx(5.0)


def test_critical_drop_and_flow_literals():
    """Fisher (2013), ch. 3 p. 3-5, no-fittings sizing differential.

    Synthetic absolute P1=100 psi, Pv=20 psi, FL=0.5, FF=0.8:
    0.5^2*(100-0.8*20)=21 psi. With P2=70 psi, Cv=10,
    SG=0.84: Q=10*sqrt(min(30,21)/0.84)=50 US gpm.
    Factors are example inputs, not equipment limits.
    """
    critical = liquid_critical_drop(100, 20, 0.5, 0.8)
    assert critical == pytest.approx(21.0)
    result = liquid_choke_flow(100, 70, 10, 0.84, critical)
    assert result.flow_gpm == pytest.approx(50.0)
    assert result.effective_drop_psi == pytest.approx(21.0)
    assert result.regime == "critical"


def test_capacity_literals():
    """Independent circle-area derivation; velocity model NOT_EVALUATED.

    Synthetic SI C=20, rho=100: v=20/sqrt(100)=2 m/s.
    Choose diameter=2 m: A=pi*(2/2)^2=pi m2, Q=2*pi m3/s.
    The decimal literal is independently evaluated circle arithmetic.
    """
    result = bore_capacity(2, 100, 20)
    assert result.velocity_m_s == pytest.approx(2.0)
    assert result.rate_m3_s == pytest.approx(6.283185307179586)


def test_trim_literal():
    """Synthetic linear interpolation: Cv=80*0.25=20; trim basis UNVERIFIED."""
    assert trim_cv(80, 0.25, "linear") == pytest.approx(20.0)
