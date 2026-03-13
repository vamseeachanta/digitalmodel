"""Tests for BESSController — SOC management, charge/discharge limiting."""

from __future__ import annotations

import pytest

from digitalmodel.power.microgrid.bess_controller import BESSController


class TestBESSControllerInit:
    def test_default_parameters(self):
        """Controller with defaults has sensible SOC limits."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0)
        assert ctrl.capacity_kwh == 100.0
        assert ctrl.soc == 0.5  # default initial SOC
        assert ctrl.soc_min == 0.10
        assert ctrl.soc_max == 0.90

    def test_invalid_soc_range_raises(self):
        """soc_min >= soc_max should raise ValueError."""
        with pytest.raises(ValueError, match="soc_min"):
            BESSController(
                capacity_kwh=100.0, max_power_kw=50.0,
                soc_min=0.9, soc_max=0.1,
            )

    def test_initial_soc_out_of_range_raises(self):
        """Initial SOC outside [soc_min, soc_max] should raise."""
        with pytest.raises(ValueError, match="soc"):
            BESSController(
                capacity_kwh=100.0, max_power_kw=50.0, soc=0.05,
            )


class TestMaxDischargeRate:
    def test_full_power_at_normal_soc(self):
        """SOC well above min → full discharge power available."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.5)
        assert ctrl.max_discharge_kw() == pytest.approx(50.0)

    def test_derates_near_soc_min(self):
        """SOC at 0.12 (2% above min 0.10) → derates within 5% band.

        Derating factor = (0.12 - 0.10) / 0.05 = 0.4 → 20 kW.
        """
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.12)
        assert ctrl.max_discharge_kw() == pytest.approx(20.0)

    def test_zero_at_soc_min(self):
        """SOC exactly at min → zero discharge."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.10)
        assert ctrl.max_discharge_kw() == pytest.approx(0.0)


class TestMaxChargeRate:
    def test_full_charge_at_normal_soc(self):
        """SOC well below max → full charge power available."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.5)
        assert ctrl.max_charge_kw() == pytest.approx(50.0)

    def test_zero_at_soc_max(self):
        """SOC exactly at max → zero charge."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.90)
        assert ctrl.max_charge_kw() == pytest.approx(0.0)


class TestComputeSetpoint:
    def test_discharge_clamped_to_max(self):
        """Requested discharge exceeding max is clamped."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.5)
        setpoint = ctrl.compute_power_setpoint(requested_kw=80.0)
        assert setpoint == pytest.approx(50.0)

    def test_charge_clamped_to_max(self):
        """Requested charge exceeding max is clamped (negative = charge)."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.5)
        setpoint = ctrl.compute_power_setpoint(requested_kw=-80.0)
        assert setpoint == pytest.approx(-50.0)

    def test_island_mode_reserves_extra_soc(self):
        """Island mode reserves extra 20% SOC headroom.

        SOC=0.35, soc_min=0.10, island reserve=0.20 → effective min=0.30.
        SOC above effective min by 0.05 → within derating band → partial.
        Derating = (0.35 - 0.30) / 0.05 = 1.0 → full power.
        """
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.35)
        setpoint = ctrl.compute_power_setpoint(
            requested_kw=50.0, island_mode=True,
        )
        assert setpoint == pytest.approx(50.0)

    def test_island_mode_blocks_discharge_low_soc(self):
        """Island mode with SOC at effective min → zero discharge.

        SOC=0.30, effective min=0.30 → zero.
        """
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.30)
        setpoint = ctrl.compute_power_setpoint(
            requested_kw=50.0, island_mode=True,
        )
        assert setpoint == pytest.approx(0.0)


class TestUpdateSOC:
    def test_discharge_decreases_soc(self):
        """Discharging 50 kW for 1 hour from 100 kWh battery.

        SOC change = 50 / 100 = 0.5; new SOC = 0.5 - 0.5 = 0.0... but
        clamped to soc_min.
        """
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.5)
        ctrl.update_soc(power_kw=50.0, dt_hours=0.5)
        # 50 * 0.5 / 100 = 0.25 consumed; 0.5 - 0.25 = 0.25
        assert ctrl.soc == pytest.approx(0.25)

    def test_charge_increases_soc(self):
        """Charging (negative power) should increase SOC."""
        ctrl = BESSController(capacity_kwh=100.0, max_power_kw=50.0, soc=0.5)
        ctrl.update_soc(power_kw=-25.0, dt_hours=1.0)
        # -25 * 1 / 100 = -0.25; SOC = 0.5 + 0.25 = 0.75
        assert ctrl.soc == pytest.approx(0.75)
