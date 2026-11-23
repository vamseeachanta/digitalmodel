"""Tests for droop control simulator — governor/AVR droop, load sharing."""

from __future__ import annotations

import pytest

from digitalmodel.power.microgrid.droop_control import (
    AVRDroop,
    Generator,
    GovernorDroop,
    parallel_load_sharing,
    power_at_frequency,
    reactive_at_voltage,
    recommend_droop_settings,
)


class TestGovernorDroop:
    def test_valid_creation(self):
        """GovernorDroop with typical 5% droop is valid."""
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        assert gov.rated_power_kw == 500.0
        assert gov.droop_percent == 5.0
        assert gov.nominal_freq_hz == 60.0

    def test_droop_percent_below_zero_raises(self):
        """droop_percent must be >= 0."""
        with pytest.raises(ValueError, match="droop_percent"):
            GovernorDroop(rated_power_kw=500.0, droop_percent=-1.0)

    def test_droop_percent_above_ten_raises(self):
        """droop_percent must be <= 10."""
        with pytest.raises(ValueError, match="droop_percent"):
            GovernorDroop(rated_power_kw=500.0, droop_percent=10.5)

    def test_nominal_freq_default(self):
        """Default nominal frequency is 60 Hz."""
        gov = GovernorDroop(rated_power_kw=100.0, droop_percent=4.0)
        assert gov.nominal_freq_hz == 60.0

    def test_custom_nominal_freq(self):
        """50 Hz systems should work."""
        gov = GovernorDroop(
            rated_power_kw=100.0, droop_percent=4.0, nominal_freq_hz=50.0
        )
        assert gov.nominal_freq_hz == 50.0


class TestAVRDroop:
    def test_valid_creation(self):
        """AVRDroop with typical 5% droop is valid."""
        avr = AVRDroop(rated_reactive_kvar=300.0, droop_percent=5.0)
        assert avr.rated_reactive_kvar == 300.0
        assert avr.nominal_voltage_v == 480.0

    def test_droop_percent_below_zero_raises(self):
        """droop_percent must be >= 0."""
        with pytest.raises(ValueError, match="droop_percent"):
            AVRDroop(rated_reactive_kvar=300.0, droop_percent=-0.1)

    def test_droop_percent_above_ten_raises(self):
        """droop_percent must be <= 10."""
        with pytest.raises(ValueError, match="droop_percent"):
            AVRDroop(rated_reactive_kvar=300.0, droop_percent=11.0)

    def test_nominal_voltage_default(self):
        """Default nominal voltage is 480 V."""
        avr = AVRDroop(rated_reactive_kvar=200.0, droop_percent=4.0)
        assert avr.nominal_voltage_v == 480.0


class TestGenerator:
    def test_valid_creation(self):
        """Generator with governor and AVR settings is valid."""
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        avr = AVRDroop(rated_reactive_kvar=300.0, droop_percent=5.0)
        gen = Generator(gen_id="G1", governor=gov, avr=avr)
        assert gen.gen_id == "G1"
        assert gen.is_isochronous is False

    def test_isochronous_flag(self):
        """Isochronous generator holds frequency."""
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        avr = AVRDroop(rated_reactive_kvar=300.0, droop_percent=5.0)
        gen = Generator(gen_id="G1", governor=gov, avr=avr, is_isochronous=True)
        assert gen.is_isochronous is True


class TestPowerAtFrequency:
    def test_at_nominal_frequency_returns_zero(self):
        """At nominal frequency, droop gen outputs zero power."""
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        assert power_at_frequency(gov, 60.0) == pytest.approx(0.0)

    def test_at_full_droop_returns_rated(self):
        """At full droop frequency drop, output equals rated power.

        5% droop on 60 Hz: f_min = 60 - 60*0.05 = 57 Hz.
        """
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        assert power_at_frequency(gov, 57.0) == pytest.approx(500.0)

    def test_at_half_droop_returns_half_rated(self):
        """Halfway frequency drop gives half rated power.

        5% droop on 60 Hz: midpoint = 60 - 1.5 = 58.5 Hz.
        """
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        assert power_at_frequency(gov, 58.5) == pytest.approx(250.0)

    def test_above_nominal_clamps_to_zero(self):
        """Frequency above nominal should clamp output to zero."""
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        assert power_at_frequency(gov, 61.0) == pytest.approx(0.0)

    def test_below_full_droop_clamps_to_rated(self):
        """Frequency well below full-droop should clamp to rated power."""
        gov = GovernorDroop(rated_power_kw=500.0, droop_percent=5.0)
        assert power_at_frequency(gov, 55.0) == pytest.approx(500.0)


class TestReactiveAtVoltage:
    def test_at_nominal_voltage_returns_zero(self):
        """At nominal voltage, reactive output is zero."""
        avr = AVRDroop(rated_reactive_kvar=300.0, droop_percent=5.0)
        assert reactive_at_voltage(avr, 480.0) == pytest.approx(0.0)

    def test_at_full_droop_voltage_returns_rated(self):
        """At full voltage droop, output equals rated reactive power.

        5% droop on 480 V: v_min = 480 - 480*0.05 = 456 V.
        """
        avr = AVRDroop(rated_reactive_kvar=300.0, droop_percent=5.0)
        assert reactive_at_voltage(avr, 456.0) == pytest.approx(300.0)

    def test_above_nominal_clamps_to_zero(self):
        """Voltage above nominal clamps reactive output to zero."""
        avr = AVRDroop(rated_reactive_kvar=300.0, droop_percent=5.0)
        assert reactive_at_voltage(avr, 490.0) == pytest.approx(0.0)


class TestParallelLoadSharing:
    @staticmethod
    def _make_gen(
        gen_id: str,
        rated_kw: float,
        droop: float = 5.0,
        is_iso: bool = False,
    ) -> Generator:
        gov = GovernorDroop(rated_power_kw=rated_kw, droop_percent=droop)
        avr = AVRDroop(rated_reactive_kvar=rated_kw * 0.6, droop_percent=5.0)
        return Generator(gen_id=gen_id, governor=gov, avr=avr, is_isochronous=is_iso)

    def test_two_identical_generators_share_equally(self):
        """Two identical droop generators split load 50/50."""
        g1 = self._make_gen("G1", 500.0, droop=5.0)
        g2 = self._make_gen("G2", 500.0, droop=5.0)
        result = parallel_load_sharing([g1, g2], total_load_kw=400.0)
        outputs = {o["gen_id"]: o["power_kw"] for o in result["gen_outputs"]}
        assert outputs["G1"] == pytest.approx(200.0, abs=1.0)
        assert outputs["G2"] == pytest.approx(200.0, abs=1.0)
        assert result["unserved_kw"] == pytest.approx(0.0, abs=0.1)

    def test_two_generators_different_droop(self):
        """Generator with lower droop (stiffer) picks up more load.

        G1: 500 kW, 4% droop → stiffer, carries more.
        G2: 500 kW, 8% droop → softer, carries less.
        At equilibrium freq, P1/P2 = droop2/droop1 = 8/4 = 2.
        For 300 kW total: G1 ~ 200 kW, G2 ~ 100 kW.
        """
        g1 = self._make_gen("G1", 500.0, droop=4.0)
        g2 = self._make_gen("G2", 500.0, droop=8.0)
        result = parallel_load_sharing([g1, g2], total_load_kw=300.0)
        outputs = {o["gen_id"]: o["power_kw"] for o in result["gen_outputs"]}
        assert outputs["G1"] == pytest.approx(200.0, abs=2.0)
        assert outputs["G2"] == pytest.approx(100.0, abs=2.0)

    def test_three_generators_load_sharing(self):
        """Three generators share load proportional to inverse droop."""
        g1 = self._make_gen("G1", 500.0, droop=5.0)
        g2 = self._make_gen("G2", 500.0, droop=5.0)
        g3 = self._make_gen("G3", 500.0, droop=5.0)
        result = parallel_load_sharing([g1, g2, g3], total_load_kw=600.0)
        outputs = {o["gen_id"]: o["power_kw"] for o in result["gen_outputs"]}
        for gid in ("G1", "G2", "G3"):
            assert outputs[gid] == pytest.approx(200.0, abs=1.0)

    def test_isochronous_plus_droop(self):
        """Isochronous gen holds frequency; droop gen shares per droop.

        G1 iso 500kW, G2 droop 500kW 5%. Total load 300 kW.
        Iso gen picks up slack so frequency stays nominal.
        At nominal freq, droop gen outputs 0 kW. Iso gen takes all 300 kW.
        """
        g1 = self._make_gen("G1", 500.0, droop=5.0, is_iso=True)
        g2 = self._make_gen("G2", 500.0, droop=5.0)
        result = parallel_load_sharing([g1, g2], total_load_kw=300.0)
        assert result["frequency_hz"] == pytest.approx(60.0, abs=0.01)
        outputs = {o["gen_id"]: o["power_kw"] for o in result["gen_outputs"]}
        assert outputs["G1"] == pytest.approx(300.0, abs=1.0)
        assert outputs["G2"] == pytest.approx(0.0, abs=1.0)

    def test_frequency_drops_under_load(self):
        """Without isochronous gen, frequency drops below nominal."""
        g1 = self._make_gen("G1", 500.0, droop=5.0)
        result = parallel_load_sharing([g1], total_load_kw=250.0)
        assert result["frequency_hz"] < 60.0

    def test_overload_reports_unserved(self):
        """If total load exceeds capacity, unserved_kw > 0."""
        g1 = self._make_gen("G1", 500.0, droop=5.0)
        result = parallel_load_sharing([g1], total_load_kw=600.0)
        assert result["unserved_kw"] == pytest.approx(100.0, abs=1.0)

    def test_zero_load(self):
        """Zero load: all generators output zero, frequency at nominal."""
        g1 = self._make_gen("G1", 500.0, droop=5.0)
        g2 = self._make_gen("G2", 500.0, droop=5.0)
        result = parallel_load_sharing([g1, g2], total_load_kw=0.0)
        assert result["frequency_hz"] == pytest.approx(60.0, abs=0.01)
        for o in result["gen_outputs"]:
            assert o["power_kw"] == pytest.approx(0.0, abs=0.1)
        assert result["unserved_kw"] == pytest.approx(0.0)

    def test_pct_rated_in_output(self):
        """Each gen output should include pct_rated field."""
        g1 = self._make_gen("G1", 500.0, droop=5.0)
        result = parallel_load_sharing([g1], total_load_kw=250.0)
        out = result["gen_outputs"][0]
        assert "pct_rated" in out
        assert out["pct_rated"] == pytest.approx(
            out["power_kw"] / 500.0 * 100.0, abs=0.5
        )


class TestRecommendDroopSettings:
    @staticmethod
    def _make_gen(gen_id: str, rated_kw: float, droop: float = 5.0) -> Generator:
        gov = GovernorDroop(rated_power_kw=rated_kw, droop_percent=droop)
        avr = AVRDroop(rated_reactive_kvar=rated_kw * 0.6, droop_percent=5.0)
        return Generator(gen_id=gen_id, governor=gov, avr=avr)

    def test_equal_split_same_rating_same_droop(self):
        """Equal split with same rating should recommend equal droop."""
        g1 = self._make_gen("G1", 500.0)
        g2 = self._make_gen("G2", 500.0)
        droops = recommend_droop_settings([g1, g2], [0.5, 0.5])
        assert droops[0] == pytest.approx(droops[1], abs=0.1)

    def test_60_40_split_same_rating(self):
        """60/40 split: gen carrying more load needs lower droop (stiffer).

        ratio: droop2/droop1 = split1/split2 = 0.6/0.4 = 1.5
        If droop1 = d, droop2 = 1.5d.
        """
        g1 = self._make_gen("G1", 500.0)
        g2 = self._make_gen("G2", 500.0)
        droops = recommend_droop_settings([g1, g2], [0.6, 0.4])
        # G1 should have lower droop than G2
        assert droops[0] < droops[1]
        ratio = droops[1] / droops[0]
        assert ratio == pytest.approx(1.5, abs=0.1)
