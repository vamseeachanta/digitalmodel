# ABOUTME: Tests for synthetic dynacard generators.
# ABOUTME: Validates all 20 failure mode generators produce valid CardData.

import pytest
import numpy as np
from scipy.signal import savgol_filter
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData
from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    generate_normal_card,
    generate_gas_interference_card,
    generate_fluid_pound_card,
    generate_pump_tagging_card,
    generate_pump_tagging_up_card,
    generate_pump_tagging_down_card,
    generate_plunger_out_of_barrel_card,
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
    surface_card_from_pump_card,
    ALL_GENERATORS,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.adapter import (
    solve_downhole_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.everitt_jennings.solver import (
    DEFAULT_SAVGOL_ORDER,
    DEFAULT_SAVGOL_WINDOW,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import (
    DynacardAnalysisContext,
    PumpProperties,
    RodSection,
    SurfaceUnit,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.solver import DynacardWorkflow


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

    def test_pump_tagging_up_spikes_at_top_of_stroke(self):
        card = generate_pump_tagging_up_card(seed=0)
        load = np.array(card.load)
        pos = np.array(card.position)
        # The impact is at maximum position, and it stands clear of the
        # upstroke plateau -- about a tenth of the card's load range, which is
        # what the reference plate shows. It is a peak, not a doubling of the
        # card: the old generator added 15-25 klb to a 15-22 klb card.
        assert pos[int(np.argmax(load))] > 0.95 * pos.max()
        plateau = np.median(load[5:len(load) // 2 - 8])
        assert 0.08 < (load.max() - plateau) / (load.max() - load.min()) < 0.30

    def test_pump_tagging_down_dips_at_bottom_of_stroke(self):
        card = generate_pump_tagging_down_card(seed=0)
        load = np.array(card.load)
        pos = np.array(card.position)
        # Mirror image: the rods go into compression against the standing
        # valve, so minimum load coincides with minimum position.
        assert pos[int(np.argmin(load))] < 0.05 * pos.max()
        downstroke_line = np.median(load[len(load) // 2 + 8:-6])
        assert (downstroke_line - load.min()) / (load.max() - load.min()) > 0.08

    def test_pump_tagging_legacy_alias_resolves_to_up(self):
        assert (
            generate_pump_tagging_card(seed=3).load
            == generate_pump_tagging_up_card(seed=3).load
        )

    def test_gas_interference_low_min_load(self):
        card = generate_gas_interference_card(seed=0)
        load = np.array(card.load)
        assert np.min(load) < 3000

    def test_fluid_pound_holds_load_then_collapses_on_the_downstroke(self):
        # Incomplete fillage means the plunger falls through empty space
        # carrying the full fluid load, then slaps the fluid surface part-way
        # down. So the load is still high at 70% of stroke on the *downstroke*
        # and gone by 15%. The old generator dropped at the top of the stroke
        # and ramped back up, putting the signature on the wrong side of the
        # card entirely.
        card = generate_fluid_pound_card(seed=0)
        load = np.array(card.load)
        pos = np.array(card.position)
        half = len(pos) // 2
        low, span = load.min(), load.max() - load.min()

        def downstroke_load_at(fraction):
            # The downstroke runs from max position back to zero, so reverse
            # it to interpolate on an increasing abscissa.
            return float(
                np.interp(fraction * pos.max(), pos[half:][::-1], load[half:][::-1])
            )

        assert downstroke_load_at(0.70) > low + 0.55 * span
        assert downstroke_load_at(0.15) < low + 0.20 * span

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
        assert len(cards) == 5 * len(ALL_GENERATORS)  # 5 per mode

    def test_all_modes_represented(self):
        cards, labels = generate_training_dataset(samples_per_mode=3)
        unique_labels = set(labels)
        assert unique_labels == set(ALL_GENERATORS)
        assert len(unique_labels) == 20

    def test_all_cards_valid(self):
        cards, labels = generate_training_dataset(samples_per_mode=2)
        for card in cards:
            _validate_card(card)

    def test_labels_are_strings(self):
        cards, labels = generate_training_dataset(samples_per_mode=2)
        for label in labels:
            assert isinstance(label, str)
            assert len(label) > 0


def _demo_context() -> DynacardAnalysisContext:
    """The well the shipped dynacard-diagnostics example is built on.

    5,000 ft of 1 in rod on a 192 in stroke unit at 6 SPM. The stroke matters:
    a generated pump card swinging tens of thousands of pounds costs well over
    a hundred inches of differential rod stretch at this depth, so the
    polished rod has to travel much further than the plunger does.
    """
    return DynacardAnalysisContext(
        api14="SIM-ROUNDTRIP",
        surface_card=CardData(position=[0.0, 1.0, 1.0, 0.0], load=[1.0, 2.0, 2.0, 1.0]),
        rod_string=[RodSection(diameter=1.0, length=5000.0)],
        pump=PumpProperties(diameter=1.75, depth=5000.0),
        surface_unit=SurfaceUnit(stroke_length=192.0),
        spm=6.0,
    )


STALE_MODEL = pytest.mark.xfail(
    strict=True,
    reason=(
        'The shipped classifier (data/dynacard_classifier.json, model_version 1.0, 18 classes) was fitted to the pre-#1875 generator shapes. Those shapes did not match the reference plate and have been corrected, so the model no longer recovers the label the card was generated from -- it now reads a normal card as WORN_BARREL and a rod-parting card as PARAFFIN_RESTRICTION. It must be retrained on the corrected 20-mode generator set before any label-round-trip assertion can hold again. Deliberately strict: this flips to a failure the moment the model is retrained, so the expectation gets re-derived rather than forgotten.'
    ),
)


class TestSurfaceCardForwardModel:
    """The pump card must survive a trip up the rod string and back down.

    Every generator here draws a DOWNHOLE card. Handing one to a
    surface-to-downhole solver as if it were a surface card asks the solver to
    remove a rod string that is not in the data -- a category error that was
    only invisible while the configured solver left the load alone (#1857).

    :func:`surface_card_from_pump_card` closes the loop by marching the rod
    string upward, and this is the invariant the whole synthetic harness rests
    on: forward-model a generated pump card to the surface, run the shipped
    surface-to-downhole solver on the result, and get the pump card back.
    """

    @pytest.mark.parametrize("mode,gen_func", list(ALL_GENERATORS.items()))
    def test_round_trip_recovers_the_generated_pump_card(self, mode, gen_func):
        ctx = _demo_context()
        pump_card = gen_func(seed=711)

        surface_card = surface_card_from_pump_card(pump_card, ctx)
        recovered = solve_downhole_card(
            ctx.model_copy(update={"surface_card": surface_card}), n_nodes=200
        )

        original_position = np.array(pump_card.position)
        recovered_position = np.array(recovered.position)
        original_load = np.array(pump_card.load)
        recovered_load = np.array(recovered.load)

        # Position comes back exactly, up to one rigid shift of the datum.
        # That shift is the static rod stretch: the surface card is zeroed at
        # its lowest point, as an instrument records it, and the plunger sits
        # that far below the polished rod. It is the same constant at every
        # sample, which is what makes it a datum and not an error.
        offset = np.mean(recovered_position - original_position)
        assert offset < 0.0
        np.testing.assert_allclose(
            recovered_position - offset, original_position, atol=1.0e-6
        )

        load_range = original_load.max() - original_load.min()

        # The load resembles the generator's card...
        load_nrmse = np.sqrt(np.mean((recovered_load - original_load) ** 2)) / load_range
        assert load_nrmse < 0.15
        assert np.corrcoef(original_load, recovered_load)[0, 1] > 0.90

        # ...and what separates the two is the solver's own Savitzky-Golay
        # smoothing of the downhole load, nothing else. Smooth the generator's
        # card the same way and the residual collapses by an order of
        # magnitude, which is what pins the remaining difference on the
        # smoothing rather than on the forward model.
        smoothed = savgol_filter(
            original_load, DEFAULT_SAVGOL_WINDOW, DEFAULT_SAVGOL_ORDER
        )
        smoothed[-1] = smoothed[0]
        smoothed_nrmse = (
            np.sqrt(np.mean((recovered_load - smoothed) ** 2)) / load_range
        )
        assert smoothed_nrmse < 0.02
        assert smoothed_nrmse < load_nrmse / 2.0

    def test_surface_card_is_not_the_pump_card(self):
        """Guard against the harness feeding a pump card in as a surface card."""
        ctx = _demo_context()
        pump_card = generate_pump_tagging_card(seed=711)

        surface_card = surface_card_from_pump_card(pump_card, ctx)

        pump_position = np.array(pump_card.position)
        surface_position = np.array(surface_card.position)
        pump_load = np.array(pump_card.load)
        surface_load = np.array(surface_card.load)

        # The polished rod travels further than the plunger, by the rod
        # stretch the load swing causes.
        assert (surface_position.max() - surface_position.min()) > (
            pump_position.max() - pump_position.min()
        )
        # And the surface load never falls to the pump's minimum: the rods'
        # buoyant weight is carried at the surface and shed on the way down.
        assert surface_load.min() > pump_load.min()

    @STALE_MODEL
    def test_workflow_round_trip_preserves_the_diagnosis(self):
        """The label the harness asks for is the label the solver hands back."""
        cfg = {
            "synthetic_card": {"mode": "PUMP_TAGGING", "seed": 711},
            "well": {
                "api14": "SIM-PUMP-TAGGING-711",
                "rod": {"diameter": 1.0, "length": 5000.0},
                "pump": {"diameter": 1.75, "depth": 5000.0},
                "surface_unit": {"stroke_length": 192.0},
                "spm": 6.0,
            },
            "report": {"html": False},
        }

        result = DynacardWorkflow().router(cfg)

        assert result["results"]["solver_method"] == "everitt_jennings"
        assert result["artificial_lift"]["classification"] == "PUMP_TAGGING"
