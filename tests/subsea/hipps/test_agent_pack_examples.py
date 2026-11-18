"""Synthetic arithmetic and a sourced band example; no API or IEC compliance."""

import runpy
from pathlib import Path

import pytest

from digitalmodel.subsea.hipps import (
    SIL,
    classify_sil,
    closure_fraction,
    liquid_capacity,
    overpressure_envelope,
    pfd_avg_1oo1,
    pfd_avg_1oo2,
    required_liquid_coefficient,
    stroke_time,
)

ROOT = Path(__file__).resolve().parents[3]


def test_pfd_hand_calculation():
    """Implementation arithmetic only: sil_analysis.py eqs 2,3; source UNVERIFIED.

    Synthetic lambda=0.000002/h, ti=1000 h, beta=0.5. Single channel:
    0.000002*1000/2=0.001. Dual: (0.5*0.000002)^2*1000^2/3
    +0.5*0.000002*1000/2=0.0005003333333333333. IEC clause text not in hand;
    this literal checks arithmetic, not a qualified reliability prediction.
    """
    assert pfd_avg_1oo1(0.000002, 1000) == pytest.approx(0.001)
    assert pfd_avg_1oo2(0.000002, 1000, beta=0.5) == pytest.approx(0.0005003333333333333)


def test_envelope_hand_calculation():
    """Independent linear bound; overpressure.py eqs 1-5, standards NOT_EVALUATED.

    Synthetic pressures MPa, times s: detect=.5, logic=.5, stroke=2, dpdt=2.
    t=3 s; p_close=10+2*3=16 MPa; margin=20-16=4 MPa; U=16/20=0.8.
    The caller must establish that the specified slope bounds the actual demand.
    """
    result = overpressure_envelope(10, 20, 2, 2, t_detect=0.5, t_logic=0.5)
    assert result.t_response == 3
    assert result.p_at_close == 16
    assert result.margin == 4
    assert result.utilisation == 0.8
    assert result.passed


def test_liquid_hand_calculation():
    """Emerson Control Valve Handbook, sixth ed. (2023), 5.8.2-5.8.3.

    All coefficients are synthetic unit-consistent inputs: p1=100,p2=0,SG=1,
    N1=Fp=FF=1,FLP=.5,pv=0,C=2. Limit=(.5/1)^2*(100-1*0)=25;
    effective=min(100,25)=25; q=1*1*2*sqrt(25/1)=10; inverse C=10/5=2.
    No unit constant, fluid property or equipment factor is taken from a table.
    """
    inputs = dict(p_upstream=100, p_downstream=0, specific_gravity=1,
                  unit_constant=1, piping_factor=1, recovery_factor=0.5,
                  critical_pressure_ratio=1, vapor_pressure=0)
    result = liquid_capacity(coefficient=2, **inputs)
    assert result.effective_drop == 25
    assert result.rate == 10
    assert result.choked
    assert required_liquid_coefficient(rate=10, **inputs) == 2


def test_closure_hand_calculation():
    """Definition of uniform travel; idealized conductance remains UNVERIFIED.

    Synthetic travel=.3 m at .1 m/s gives 3 s. Linear conductance at 1 of 2 s
    is 1-1/2=.5. Equal-percentage R=100 gives 100^(-1/2)=.1; at 2 s it is 0.
    A mathematical law is checked; no actuator performance evidence is implied.
    """
    assert stroke_time(stroke_length=0.3, actuator_speed=0.1) == pytest.approx(3)
    assert closure_fraction(1, 2, "linear") == 0.5
    assert closure_fraction(1, 2, "equal_percentage", rangeability=100) == pytest.approx(0.1)
    assert closure_fraction(2, 2, "equal_percentage", rangeability=100) == 0


def test_classification_literal():
    """Emerson, Safety Integrity Level (SIL), January 2015, p.3,
    section "IEC 61508 and IEC 61511": the SIL2 PFD range is 0.01-0.001.

    Synthetic PFD=.005 lies strictly inside that independently sourced range:
    .001 < .005 < .01, so the expected band is SIL2; 1/.005=200.
    Endpoint inclusion and architectural suitability are not established here.
    """
    result = classify_sil(0.005, target_sil=2)
    assert result.sil is SIL.SIL2
    assert result.rrf == 200
    assert result.meets_target


def test_pack_node_ids_collect(tmp_path):
    collect = runpy.run_path(str(ROOT / "tests/docs/test_agent_pack_node_ids.py"))["collect_pack"]
    collect(ROOT / "src/digitalmodel/subsea/hipps/docs/agent-pack.md",
            ROOT, tmp_path / "collection", include_gates=True)
