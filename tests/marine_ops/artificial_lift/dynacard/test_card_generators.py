# ABOUTME: Tests for synthetic dynacard generators.
# ABOUTME: Validates all 18 failure mode generators produce valid CardData.

import pytest
import numpy as np
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    generate_normal_card,
    generate_gas_interference_card,
    generate_fluid_pound_card,
    generate_pump_tagging_card,
    generate_tubing_movement_card,
    generate_valve_leak_tv_card,
    generate_valve_leak_sv_card,
    generate_rod_parting_card,
    generate_stuck_pump_card,
    generate_worn_barrel_card,
    generate_gas_lock_card,
    generate_delayed_tv_closure_card,
    generate_excessive_friction_card,
    generate_plunger_undertravel_card,
    generate_paraffin_restriction_card,
    generate_bent_barrel_card,
    generate_sand_abrasion_card,
    generate_excessive_vibration_card,
    generate_training_dataset,
    ALL_GENERATORS,
)


def _validate_card(card: CardData):
    """Common validation for all generated cards."""
    assert isinstance(card, CardData)
    assert len(card.position) == len(card.load)
    assert len(card.position) >= 20
    pos = np.array(card.position)
    load = np.array(card.load)
    assert not np.any(np.isnan(pos)), "Position contains NaN"
    assert not np.any(np.isnan(load)), "Load contains NaN"
    assert not np.any(np.isinf(pos)), "Position contains Inf"
    assert not np.any(np.isinf(load)), "Load contains Inf"


class TestAllGeneratorsProduceValidCards:
    """Every generator should produce valid CardData."""

    @pytest.mark.parametrize("name,gen_func", list(ALL_GENERATORS.items()))
    def test_generator_produces_valid_card(self, name, gen_func):
        card = gen_func(seed=42)
        _validate_card(card)

    @pytest.mark.parametrize("name,gen_func", list(ALL_GENERATORS.items()))
    def test_generator_is_deterministic(self, name, gen_func):
        card1 = gen_func(seed=42)
        card2 = gen_func(seed=42)
        np.testing.assert_array_equal(card1.position, card2.position)
        np.testing.assert_array_equal(card1.load, card2.load)

    @pytest.mark.parametrize("name,gen_func", list(ALL_GENERATORS.items()))
    def test_generator_different_seeds_differ(self, name, gen_func):
        card1 = gen_func(seed=1)
        card2 = gen_func(seed=2)
        assert not np.array_equal(card1.load, card2.load)


class TestSpecificCardProperties:
    """Test distinguishing properties of specific failure modes."""

    def test_normal_card_reasonable_loads(self):
        card = generate_normal_card(seed=0)
        load = np.array(card.load)
        assert np.max(load) < 30000
        assert np.min(load) > 1000

    def test_pump_tagging_extreme_load(self):
        card = generate_pump_tagging_card(seed=0)
        load = np.array(card.load)
        assert np.max(load) > 30000

    def test_gas_interference_low_min_load(self):
        card = generate_gas_interference_card(seed=0)
        load = np.array(card.load)
        assert np.min(load) < 3000

    def test_fluid_pound_has_sharp_drop(self):
        card = generate_fluid_pound_card(seed=0)
        load = np.array(card.load)
        diffs = np.abs(np.diff(load))
        assert np.max(diffs) > 2000

    def test_rod_parting_very_low_loads(self):
        card = generate_rod_parting_card(seed=0)
        load = np.array(card.load)
        assert np.max(load) - np.min(load) < 5000

    def test_stuck_pump_tiny_position_range(self):
        card = generate_stuck_pump_card(seed=0)
        pos = np.array(card.position)
        assert np.max(pos) - np.min(pos) < 20

    def test_gas_lock_near_zero_area(self):
        card = generate_gas_lock_card(seed=0)
        load = np.array(card.load)
        # Gas lock should have very small load range (collapsed card)
        assert np.max(load) - np.min(load) < 5000

    def test_excessive_friction_large_hysteresis(self):
        card = generate_excessive_friction_card(seed=0)
        pos = np.array(card.position)
        load = np.array(card.load)
        # The card should have a "thick" loop
        mid_pos = (np.max(pos) + np.min(pos)) / 2
        n = len(pos)
        # Find loads near midpoint for each half
        first_half = load[:n // 2]
        second_half = load[n // 2:]
        # There should be a measurable gap between halves
        assert np.mean(first_half) != pytest.approx(np.mean(second_half), abs=100)


class TestTrainingDataset:
    """Test bulk training dataset generation."""

    def test_generates_correct_count(self):
        cards, labels = generate_training_dataset(samples_per_mode=5)
        assert len(cards) == len(labels)
        assert len(cards) == 5 * 18  # 5 per mode x 18 modes

    def test_all_modes_represented(self):
        cards, labels = generate_training_dataset(samples_per_mode=3)
        unique_labels = set(labels)
        assert len(unique_labels) == 18

    def test_all_cards_valid(self):
        cards, labels = generate_training_dataset(samples_per_mode=2)
        for card in cards:
            _validate_card(card)

    def test_labels_are_strings(self):
        cards, labels = generate_training_dataset(samples_per_mode=2)
        for label in labels:
            assert isinstance(label, str)
            assert len(label) > 0
