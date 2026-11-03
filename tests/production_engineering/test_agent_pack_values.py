# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.
"""Independent arithmetic anchors, not original-paper or field validation.

IPR equation source: open university thesis, Inflow Performance Relationships
(IPR) for Solution Gas Drive Reservoirs — a Semi-Analytical Approach (2010),
sections 1.2.1–1.2.2, equations (1.7), (1.12), (1.16).
See the pack's source link and composite slope-matching derivation. All case
inputs are synthetic choices; no empirical design limits are introduced.
"""

import pytest

from digitalmodel.production_engineering.ipr_models import (
    CompositeIpr,
    FetkovichIpr,
    LinearIpr,
    ReservoirConditions,
    VogelIpr,
)
from digitalmodel.production_engineering.vlp_correlations import (
    FlowConditions,
    FluidProperties,
    TubingConfig,
    beggs_brill_pwf,
    hagedorn_brown_pwf,
    simplified_homogeneous_pwf,
)


def test_linear_hand_value():
    """Eq. (1.7): J=2, Pr=2000, Pwf=1500 -> 2*(2000-1500)=1000 bbl/d."""
    model = LinearIpr(ReservoirConditions(2000, 1000, 2), 2)
    assert model.flow_rate(1500) == 1000
    assert model.flowing_pressure(1000) == 1500


def test_vogel_hand_value():
    """Eq. (1.12): r=1000/2000=0.5; 1000*(1-0.1-0.2)=700 bbl/d.

    The inverse is the positive quadratic root, r=0.5 -> Pwf=1000 psi.
    Slope at r=1 is -(0.2+2*0.8)=-1.8: J=0.9 yields qmax=1000.
    """
    reservoir = ReservoirConditions(2000, 2000, 0.9)
    model = VogelIpr(reservoir, 1000)
    assert model.flow_rate(1000) == 700
    assert model.flowing_pressure(700) == 1000
    assert VogelIpr.from_productivity_index(reservoir, 0.9).qmax_bopd == 1000


def test_fetkovich_hand_value():
    """Eq. (1.16): C=0.001, n=1; C*(2000²-1000²)=3000 bbl/d.

    qmax=C*Pr²=4000, so normalized equation also gives 4000*(1-0.25)=3000.
    """
    model = FetkovichIpr(ReservoirConditions(2000, 2000, 1), 0.001, 1)
    assert model.flow_rate(1000) == 3000
    assert model.flowing_pressure(3000) == 1000


def test_composite_hand_value():
    """Slope-matched extension of Eq. (1.12), not a published example.

    J=2, Pr=2400, Pb=1800: qb=2*600=1200; A=2*1800/1.8=2000.
    Pwf=900 gives r=0.5 and q=1200+2000*0.7=2600 bbl/d.
    At Pb, q=1200; at zero, q=3200. No empirical tolerance is used.
    """
    model = CompositeIpr(ReservoirConditions(2400, 1800, 2))
    assert model.flow_rate(900) == 2600
    assert model.flow_rate(1800) == 1200
    assert model.flow_rate(0) == 3200


@pytest.mark.parametrize("method", [
    simplified_homogeneous_pwf, hagedorn_brown_pwf, beggs_brill_pwf,
])
def test_vlp_zero_length_arithmetic(method):
    """NOT_EVALUATED published basis: branch arithmetic only.

    Code-pin traverse: dp_h=rho*L/144, dp_f proportional to L. Synthetic
    L=0 ft, q=100 bbl/d, WC=1, GOR=0, WHP=100 psi -> 100+0+0=100 psi.
    Fluid values are explicit synthetic inputs, not recommended defaults.
    The two deprecated names share this traverse, not their published methods.
    """
    flow = FlowConditions(100, 1, 0)
    tubing = TubingConfig(0, 2, 0)
    fluid = FluidProperties(oil_api=35, gas_gravity=0.65, temperature_f=150)
    assert method(flow, tubing, fluid, 100) == 100
