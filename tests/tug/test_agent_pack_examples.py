"""Synthetic arithmetic anchors for the tug pack; no equipment acceptance.

Inputs are chosen for arithmetic, not taken from reference equipment.
Sources: OpenStax University Physics volume 1 (2016), sections 7.2
(kinetic energy), 7.4 (power), 10.6 (torque). Coefficients supplied below
are synthetic inputs, not recommended design coefficients. Approximate
comparisons allow floating-point roundoff, not engineering tolerances.
"""

import pytest

from digitalmodel.tug.bollard_pull import bollard_pull
from digitalmodel.tug.emissions import OperatingPhase, cycle_energy, size_hybrid
from digitalmodel.tug.fendering import AssistedVessel, berthing_energy, optimize_fender_height
from digitalmodel.tug.stability_girting import heeling_arm_curve
from digitalmodel.tug.towline import speed_amplification_factor


def test_bollard_ahead_explicit_factor():
    """Independent product: 1000 kW * synthetic 0.02 t-force/kW = 20 t-force.

    Astern and transverse factors remain NOT_EVALUATED; no assertion here
    qualifies those unsourced defaults. This is an algebra contract only.
    """
    result = bollard_pull(1000, "asd", factor_t_per_kw=0.02)
    assert result.bp_ahead_t == pytest.approx(20.0)


def test_heeling_arm_upright():
    """Torque balance (OpenStax 2016, 10.6): (10/100)*2*cos(0) = 0.2 m."""
    result = heeling_arm_curve(10, 2, 100, heel_angles_deg=[0])
    assert result == [(0, pytest.approx(0.2))]


def test_berthing_energy_unit_coefficients():
    """OpenStax 2016, 7.2: (1/2)*100000 kg*(2 m/s)^2 = 200 kJ.

    All three correction coefficients are explicitly the synthetic identity;
    no PIANC coefficient or fender qualification is inferred. Rating 400 kJ
    is a synthetic input, giving 200/400 = 0.5 utilisation.
    """
    result = berthing_energy(100, 2, 400, 1, 1, 1)
    assert result.berthing_energy_kj == pytest.approx(200.0)
    assert result.utilisation == pytest.approx(0.5)
    assert result.adequate is True


def test_hybrid_cycle_energy_and_peak():
    """OpenStax 2016, 7.4: E = P*t for each constant-power phase.

    100*(0.25*2 + 1*1) = 150 kWh; engine baseline = 100*0.5 = 50 kW.
    Excess = max(25-50,0)*2 + max(100-50,0)*1 = 50 kWh;
    battery power = 100-50 = 50 kW; share = 50/150 = 1/3.
    Baseline is synthetic; no charging, efficiency or reserve is assumed.
    """
    phases = [OperatingPhase("low", 0.25, 2), OperatingPhase("high", 1, 1)]
    assert cycle_energy(100, phases) == pytest.approx(150.0)
    result = size_hybrid(100, phases, baseline_fraction=0.5)
    assert result.total_energy_kwh == pytest.approx(150.0)
    assert result.peak_power_kw == pytest.approx(100.0)
    assert result.baseline_engine_kw == pytest.approx(50.0)
    assert result.battery_power_kw == pytest.approx(50.0)
    assert result.battery_energy_kwh == pytest.approx(50.0)
    assert result.peak_energy_fraction == pytest.approx(1 / 3)


def test_fender_overlap_identity():
    """Interval arithmetic: band [0,2] m and 2 m face at centre 1 m overlap 2/2.

    Synthetic search centres 0,1,2 m give overlap fractions 1/2,1,1/2;
    the unique best centre is 1 m. No empirical constants are used.
    """
    result = optimize_fender_height([AssistedVessel("synthetic", 0, 2)], 2, 1)
    assert result.fender_height_m == pytest.approx(1.0)
    assert result.mean_overlap_fraction == pytest.approx(1.0)
    assert result.worst_overlap_fraction == pytest.approx(1.0)
    assert result.all_vessels_contact is True


def test_speed_amplification_synthetic_inputs():
    """Algebra contract: min(5,max(1,(2/1)^2)) = 4; no calibrated speed rule.

    Reference speed, exponent and cap are explicit synthetic inputs. The
    empirical relationship and its defaults remain NOT_EVALUATED.
    """
    assert speed_amplification_factor(2, 1, 2, 5) == pytest.approx(4.0)
