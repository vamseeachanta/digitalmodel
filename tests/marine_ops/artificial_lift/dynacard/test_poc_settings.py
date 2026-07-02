# ABOUTME: Tests for POC setpoint recommendations and alarm evaluation.
# ABOUTME: Golden values come from published field-practice worked examples.

import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.poc_settings import (
    automatic_idle_time,
    card_area_change_pct,
    estimate_production_bpd,
    evaluate_alarms,
    recommend_setpoints,
    rods_parted,
    separator_capacity_bpd,
)


class TestRecommendSetpoints:
    def test_training_deck_worked_example(self):
        """PPRL 20,000 / MPRL 10,000 -> 24,000 / 22,000 / 8,500 / 7,500."""
        sp = recommend_setpoints(20000.0, 10000.0)
        assert sp.high_load_shutdown_lbs == 24000.0
        assert sp.high_load_alarm_lbs == 22000.0
        assert sp.low_load_alarm_lbs == 8500.0
        assert sp.low_load_shutdown_lbs == 7500.0
        assert not sp.shallow_low_load_rule_used

    def test_vendor_manual_fraction_recipe(self):
        """Peak 11,440 x 1.2 = 13,728; peak 11,337 x 1.1 = 12,470.7."""
        assert recommend_setpoints(11440.0, 5000.0).high_load_shutdown_lbs == 13728.0
        assert recommend_setpoints(11337.0, 5000.0).high_load_alarm_lbs == pytest.approx(
            12471.0, abs=1.0
        )

    def test_structure_rating_cap(self):
        """PPRL 32,000 -> shutdown 38,400 capped at the 36,500 lb structure."""
        sp = recommend_setpoints(32000.0, 10000.0, structure_rating_lbs=36500.0)
        assert sp.high_load_shutdown_lbs == 36500.0
        assert sp.structure_rating_capped

    def test_shallow_well_rule(self):
        """MPRL 2,000: offsets go non-positive -> fractions x0.9 / x0.8."""
        sp = recommend_setpoints(9000.0, 2000.0)
        assert sp.shallow_low_load_rule_used
        assert sp.low_load_alarm_lbs == 1800.0
        assert sp.low_load_shutdown_lbs == 1600.0

    def test_load_span_malfunction_worked_example(self):
        """(19,500 - 12,000) x 0.6 = 4,500 lb."""
        sp = recommend_setpoints(19500.0, 12000.0)
        assert sp.load_span_malfunction_lbs == 4500.0

    def test_gassy_well_strokes_and_vsd_limits(self):
        sp = recommend_setpoints(20000.0, 10000.0, stroke_length_in=144.0,
                                 gassy_well=True)
        assert sp.pump_off_strokes == 10
        assert sp.vsd_max_spm == pytest.approx(10.0)  # 1440 / 144
        assert sp.vsd_min_spm == 4.0
        assert recommend_setpoints(20000.0, 10000.0, low_speed_lube=True).vsd_min_spm == 1.0


class TestEvaluateAlarms:
    def _setpoints(self):
        normal = ALL_GENERATORS["NORMAL"](seed=711)
        return recommend_setpoints(max(normal.load), min(normal.load)), normal

    def test_normal_card_raises_nothing(self):
        sp, normal = self._setpoints()
        assert evaluate_alarms(normal, sp) == []

    def test_pump_tagging_trips_high_load_shutdown(self):
        sp, _ = self._setpoints()
        events = evaluate_alarms(ALL_GENERATORS["PUMP_TAGGING"](seed=711), sp)
        assert any(e.name == "high_load" and e.severity == "shutdown" for e in events)

    def test_rod_parting_trips_low_load_and_span(self):
        sp, normal = self._setpoints()
        card = ALL_GENERATORS["ROD_PARTING"](seed=711)
        events = evaluate_alarms(card, sp, reference_card=normal)
        names = {(e.name, e.severity) for e in events}
        assert ("low_load", "shutdown") in names
        assert ("load_span", "malfunction") in names
        assert ("card_area_low", "malfunction") in names

    def test_card_area_change_reference(self):
        normal = ALL_GENERATORS["NORMAL"](seed=711)
        assert card_area_change_pct(normal, normal) == 0.0
        drop = card_area_change_pct(ALL_GENERATORS["ROD_PARTING"](seed=711), normal)
        assert drop < -95.0


class TestOperatingCalculations:
    def test_automatic_idle_time_holds_cycle(self):
        out = automatic_idle_time(target_cycle_min=20.0, average_run_min=15.0)
        assert out == dict(next_idle_min=5.0, target_cycle_min=20.0, over_pumped=False)

    def test_automatic_idle_time_halves_when_over_pumped(self):
        out = automatic_idle_time(target_cycle_min=20.0, average_run_min=8.0)
        assert out["over_pumped"] and out["target_cycle_min"] == 10.0
        assert out["next_idle_min"] == 2.0  # floored at the minimum idle

    def test_displacement_constant_matches_published_pump_table(self):
        """0.1166 d^2: 1-1/16 in -> 0.132, 2-1/4 in -> 0.590 (published)."""
        assert 0.1166 * 1.0625 ** 2 == pytest.approx(0.132, abs=0.001)
        assert 0.1166 * 2.25 ** 2 == pytest.approx(0.590, abs=0.001)
        assert estimate_production_bpd(1.5, 6.0, 100.0) == pytest.approx(
            0.1166 * 2.25 * 6.0 * 100.0 * 0.85, abs=0.1
        )

    def test_separator_capacity_rule(self):
        """1 in^2 of quiet zone at 6 in/s bubble-rise limit = 50 BPD."""
        assert separator_capacity_bpd(1.0) == 50.0
        assert separator_capacity_bpd(2.3) == 115.0

    def test_rod_part_screening_rule(self):
        # buoyant weight = 10,000 x 0.87 = 8,700 lb
        assert rods_parted(8000.0, 10000.0) is True
        assert rods_parted(9000.0, 10000.0) is False
