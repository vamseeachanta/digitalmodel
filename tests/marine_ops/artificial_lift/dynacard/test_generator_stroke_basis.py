# Copyright (c) 2024 Digital Model Project
# Licensed under the MIT License. See LICENSE file for details.

"""Stroke regressions and independently calculated scale-audit examples."""

import itertools
from pathlib import Path
import runpy

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
    generate_normal_card,
    generate_plunger_undertravel_card,
    generate_tubing_movement_card,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData


GENERATORS = (
    generate_normal_card,
    generate_tubing_movement_card,
    generate_plunger_undertravel_card,
)
LOSS_GENERATORS = GENERATORS[1:]


def test_default_stroke_cannot_identify_the_three_modes():
    """Equal seeds must give equal travel, eliminating a stroke-only label rule.

    The seed count is a test workload, not an engineering acceptance limit.
    """
    for seed in range(32):
        cards = [generator(seed=seed) for generator in GENERATORS]
        for card in cards[1:]:
            np.testing.assert_array_equal(card.position, cards[0].position)


def test_undertravel_has_no_noise_only_label():
    """With no matched loss inputs, these synthetic rectangles are identical."""
    for seed in range(8):
        assert (
            generate_plunger_undertravel_card(seed).load
            == generate_normal_card(seed).load
        )


def test_training_features_preserve_indistinguishable_defaults():
    """Identical inputs have identical scaled features, regardless of the label.

    Expected equality follows directly from the common card and scale basis;
    no classifier accuracy or learned result supplies the expected value.
    """
    from digitalmodel.marine_ops.artificial_lift.dynacard.training import (
        _extract_features_for_dataset,
    )

    cards = [generate_normal_card(), generate_plunger_undertravel_card()]
    labels = ["NORMAL", "PLUNGER_UNDERTRAVEL"]
    features, returned_labels, _ = _extract_features_for_dataset(cards, labels)
    np.testing.assert_array_equal(features[0], features[1])
    assert returned_labels.tolist() == labels


def _normal_noise_sigma(seed):
    """Configured noise scale, independent of the observed card variation."""
    rng = np.random.default_rng(seed)
    high = rng.uniform(12000, 18000)
    low = rng.uniform(4000, 7000)
    return 0.03 * (high - low)


def test_tubing_defaults_need_exclusion_or_distinguishing_signature():
    """This screening criterion does not establish statistical separability."""
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        SYNTHETIC_LABEL_EXCLUSIONS,
    )

    for seed in (*range(8), *range(1000, 1008)):
        normal = generate_normal_card(seed)
        tubing = generate_tubing_movement_card(seed)
        if normal.position == tubing.position:
            difference = np.max(np.abs(np.asarray(normal.load) - tubing.load))
            assert (
                "TUBING_MOVEMENT" in SYNTHETIC_LABEL_EXCLUSIONS
                or difference >= _normal_noise_sigma(seed)
            )


@pytest.mark.parametrize("builder", ["training", "hold_out"])
def test_labelled_synthetic_sets_exclude_indistinguishable_defaults(builder):
    """All seed-matched label pairs must pass the requested noise screen."""
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        ALL_GENERATORS,
        SYNTHETIC_LABEL_EXCLUSIONS,
        generate_training_dataset,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.benchmark.test_set_builder import (
        build_hold_out_test_set,
    )

    if builder == "training":
        cards, labels = generate_training_dataset(samples_per_mode=8)
        first_seed = 0
    else:
        records = build_hold_out_test_set(samples_per_mode=8)
        cards = [record.card for record in records]
        labels = [record.label for record in records]
        first_seed = 1000

    for offset in range(8):
        for (left, left_label), (right, right_label) in itertools.combinations(
            zip(cards[offset::8], labels[offset::8]), 2
        ):
            if left.position == right.position:
                difference = np.max(np.abs(np.asarray(left.load) - right.load))
                assert difference >= _normal_noise_sigma(first_seed + offset), (
                    f"Sub-noise synthetic cards have labels {left_label} and {right_label}"
                )
    excluded = {"PLUNGER_UNDERTRAVEL", "TUBING_MOVEMENT"}
    assert set(SYNTHETIC_LABEL_EXCLUSIONS) == excluded
    assert all(reason.strip() for reason in SYNTHETIC_LABEL_EXCLUSIONS.values())
    assert set(labels) == set(ALL_GENERATORS) - excluded
    assert len(cards) == 18 * 8


def test_training_progress_counts_generated_labels(monkeypatch, capsys):
    from digitalmodel.marine_ops.artificial_lift.dynacard import training

    class GenerationObserved(Exception):
        pass

    counts = []

    def stop_before_fitting(cards, labels):
        counts.append(len(set(labels)))
        raise GenerationObserved

    monkeypatch.setattr(training, "_extract_features_for_dataset", stop_before_fitting)
    with pytest.raises(GenerationObserved):
        training.train_and_export(samples_per_mode=1)
    assert f"({counts[0]} modes)" in capsys.readouterr().out


@pytest.mark.parametrize("generator", LOSS_GENERATORS)
def test_published_lost_motion_example(generator):
    """SWPSC (2017), Definition of Over Travel and Under Travel, Fig. 1.

    Published surface travel 45.4 in, rod stretch 30.7 in, tubing stretch
    8.3 in and dynamic overtravel 12.2 in imply an anchored comparison travel
    of 45.4 - 30.7 + 12.2 = 26.9 in. Subtracting tubing loss gives
    26.9 - 8.3 = 18.6 in, the paper's stated downhole travel. This is a
    kinematic subtraction check, not a simulation of that well's dynamics.
    """
    card = generator(seed=0, anchored_stroke_in=26.9, lost_motion_in=8.3)
    assert np.ptp(card.position) == pytest.approx(18.6)
    anchored = generate_normal_card(seed=0, stroke_in=26.9)
    assert np.ptp(card.position) < np.ptp(anchored.position)
    # Another installation's healthy travel can equal this fault's travel.
    healthy = generate_normal_card(seed=0, stroke_in=18.6)
    np.testing.assert_allclose(card.position, healthy.position)


@pytest.mark.parametrize("generator", LOSS_GENERATORS)
def test_zero_loss_recovers_matched_travel(generator):
    """Independent identity: 100 - 0 = 100 in; inputs are illustrative."""
    card = generator(anchored_stroke_in=100, lost_motion_in=0)
    np.testing.assert_array_equal(
        card.position, generate_normal_card(stroke_in=100).position
    )


@pytest.mark.parametrize("generator", LOSS_GENERATORS)
@pytest.mark.parametrize(
    "inputs",
    [
        {"anchored_stroke_in": 100},
        {"lost_motion_in": 20},
        {"anchored_stroke_in": 100, "lost_motion_in": -1},
        {"anchored_stroke_in": 100, "lost_motion_in": 100},
        {"anchored_stroke_in": 100, "lost_motion_in": 101},
        {"anchored_stroke_in": 0, "lost_motion_in": 0},
        {"anchored_stroke_in": float("inf"), "lost_motion_in": 0},
        {"anchored_stroke_in": 100, "lost_motion_in": float("nan")},
    ],
)
def test_invalid_matched_inputs_rejected(generator, inputs):
    """Finite positive travel and nonnegative loss below travel are identities."""
    with pytest.raises(ValueError):
        generator(**inputs)


@pytest.mark.parametrize("stroke", [0, -1, float("nan"), float("inf")])
def test_invalid_normal_stroke_rejected(stroke):
    with pytest.raises(ValueError):
        generate_normal_card(stroke_in=stroke)


def _rectangle(width):
    return CardData(position=[0, width, width, 0], load=[1, 1, 201, 201])


def test_scale_audit_uses_independent_rectangle_metrics():
    """Hand calculation: widths 3,4; height 201-1=200; areas 600,800.

    Singleton envelopes of distinct widths/areas are disjoint; equal load
    ranges overlap. The position offset and constant load cancel in metrics.
    """
    from digitalmodel.marine_ops.artificial_lift.dynacard.generator_audit import (
        audit_generator_scale_ranges,
    )

    report = audit_generator_scale_ranges(
        samples_per_mode=1,
        generators={"A": lambda seed: _rectangle(3), "B": lambda seed: _rectangle(4)},
    )
    assert report["ranges"]["A"] == {
        "stroke_length": {"min": 3, "max": 3},
        "load_range": {"min": 200, "max": 200},
        "card_area": {"min": 600, "max": 600},
    }
    assert report["ranges"]["B"]["card_area"] == {"min": 800, "max": 800}
    assert report["disjoint_pairs"] == {
        "stroke_length": [["A", "B"]],
        "load_range": [],
        "card_area": [["A", "B"]],
    }


def test_scale_audit_boundary_contact_is_overlap():
    """Hand calculation: sampled stroke envelopes [2,3] and [3,4] touch."""
    from digitalmodel.marine_ops.artificial_lift.dynacard.generator_audit import (
        audit_generator_scale_ranges,
    )

    report = audit_generator_scale_ranges(
        samples_per_mode=2,
        generators={
            "A": lambda seed: _rectangle(seed + 2),
            "B": lambda seed: _rectangle(seed + 3),
        },
    )
    assert all(not pairs for pairs in report["disjoint_pairs"].values())


# Observed at 32 seeds before the regression guard; reductions remain allowed.
BASELINE_DISJOINT_PAIRS = {
    "stroke_length": {
        ("BENT_BARREL", "STUCK_PUMP"),
        ("DELAYED_TV_CLOSURE", "STUCK_PUMP"),
        ("EXCESSIVE_FRICTION", "STUCK_PUMP"),
        ("EXCESSIVE_VIBRATION", "STUCK_PUMP"),
        ("FLUID_POUND", "STUCK_PUMP"),
        ("GAS_INTERFERENCE", "STUCK_PUMP"),
        ("GAS_LOCK", "STUCK_PUMP"),
        ("NORMAL", "STUCK_PUMP"),
        ("PARAFFIN_RESTRICTION", "STUCK_PUMP"),
        ("PLUNGER_OUT_OF_BARREL", "STUCK_PUMP"),
        ("PLUNGER_UNDERTRAVEL", "STUCK_PUMP"),
        ("PUMP_TAGGING_DOWN", "STUCK_PUMP"),
        ("PUMP_TAGGING_UP", "STUCK_PUMP"),
        ("ROD_PARTING", "STUCK_PUMP"),
        ("SAND_ABRASION", "STUCK_PUMP"),
        ("STUCK_PUMP", "TUBING_MOVEMENT"),
        ("STUCK_PUMP", "VALVE_LEAK_SV"),
        ("STUCK_PUMP", "VALVE_LEAK_TV"),
        ("STUCK_PUMP", "WORN_BARREL"),
    },
    "load_range": {
        ("BENT_BARREL", "GAS_LOCK"),
        ("BENT_BARREL", "ROD_PARTING"),
        ("BENT_BARREL", "STUCK_PUMP"),
        ("DELAYED_TV_CLOSURE", "GAS_LOCK"),
        ("DELAYED_TV_CLOSURE", "ROD_PARTING"),
        ("DELAYED_TV_CLOSURE", "STUCK_PUMP"),
        ("EXCESSIVE_FRICTION", "GAS_LOCK"),
        ("EXCESSIVE_FRICTION", "ROD_PARTING"),
        ("EXCESSIVE_FRICTION", "STUCK_PUMP"),
        ("EXCESSIVE_VIBRATION", "GAS_LOCK"),
        ("EXCESSIVE_VIBRATION", "ROD_PARTING"),
        ("EXCESSIVE_VIBRATION", "STUCK_PUMP"),
        ("FLUID_POUND", "GAS_LOCK"),
        ("FLUID_POUND", "ROD_PARTING"),
        ("FLUID_POUND", "STUCK_PUMP"),
        ("GAS_INTERFERENCE", "GAS_LOCK"),
        ("GAS_INTERFERENCE", "ROD_PARTING"),
        ("GAS_INTERFERENCE", "STUCK_PUMP"),
        ("GAS_LOCK", "NORMAL"),
        ("GAS_LOCK", "PARAFFIN_RESTRICTION"),
        ("GAS_LOCK", "PLUNGER_OUT_OF_BARREL"),
        ("GAS_LOCK", "PLUNGER_UNDERTRAVEL"),
        ("GAS_LOCK", "PUMP_TAGGING_DOWN"),
        ("GAS_LOCK", "PUMP_TAGGING_UP"),
        ("GAS_LOCK", "ROD_PARTING"),
        ("GAS_LOCK", "SAND_ABRASION"),
        ("GAS_LOCK", "TUBING_MOVEMENT"),
        ("GAS_LOCK", "VALVE_LEAK_SV"),
        ("GAS_LOCK", "VALVE_LEAK_TV"),
        ("GAS_LOCK", "WORN_BARREL"),
        ("NORMAL", "ROD_PARTING"),
        ("NORMAL", "STUCK_PUMP"),
        ("PARAFFIN_RESTRICTION", "ROD_PARTING"),
        ("PARAFFIN_RESTRICTION", "STUCK_PUMP"),
        ("PLUNGER_OUT_OF_BARREL", "ROD_PARTING"),
        ("PLUNGER_OUT_OF_BARREL", "STUCK_PUMP"),
        ("PLUNGER_UNDERTRAVEL", "ROD_PARTING"),
        ("PLUNGER_UNDERTRAVEL", "STUCK_PUMP"),
        ("PUMP_TAGGING_DOWN", "ROD_PARTING"),
        ("PUMP_TAGGING_DOWN", "STUCK_PUMP"),
        ("PUMP_TAGGING_UP", "ROD_PARTING"),
        ("PUMP_TAGGING_UP", "STUCK_PUMP"),
        ("ROD_PARTING", "SAND_ABRASION"),
        ("ROD_PARTING", "STUCK_PUMP"),
        ("ROD_PARTING", "TUBING_MOVEMENT"),
        ("ROD_PARTING", "VALVE_LEAK_SV"),
        ("ROD_PARTING", "VALVE_LEAK_TV"),
        ("ROD_PARTING", "WORN_BARREL"),
        ("SAND_ABRASION", "STUCK_PUMP"),
        ("STUCK_PUMP", "TUBING_MOVEMENT"),
        ("STUCK_PUMP", "VALVE_LEAK_SV"),
        ("STUCK_PUMP", "VALVE_LEAK_TV"),
        ("STUCK_PUMP", "WORN_BARREL"),
    },
    "card_area": {
        ("BENT_BARREL", "GAS_LOCK"),
        ("BENT_BARREL", "ROD_PARTING"),
        ("BENT_BARREL", "STUCK_PUMP"),
        ("DELAYED_TV_CLOSURE", "GAS_LOCK"),
        ("DELAYED_TV_CLOSURE", "ROD_PARTING"),
        ("DELAYED_TV_CLOSURE", "STUCK_PUMP"),
        ("EXCESSIVE_FRICTION", "FLUID_POUND"),
        ("EXCESSIVE_FRICTION", "GAS_LOCK"),
        ("EXCESSIVE_FRICTION", "PLUNGER_OUT_OF_BARREL"),
        ("EXCESSIVE_FRICTION", "ROD_PARTING"),
        ("EXCESSIVE_FRICTION", "STUCK_PUMP"),
        ("EXCESSIVE_VIBRATION", "GAS_LOCK"),
        ("EXCESSIVE_VIBRATION", "ROD_PARTING"),
        ("EXCESSIVE_VIBRATION", "STUCK_PUMP"),
        ("FLUID_POUND", "GAS_LOCK"),
        ("FLUID_POUND", "PUMP_TAGGING_DOWN"),
        ("FLUID_POUND", "PUMP_TAGGING_UP"),
        ("FLUID_POUND", "ROD_PARTING"),
        ("FLUID_POUND", "STUCK_PUMP"),
        ("GAS_INTERFERENCE", "GAS_LOCK"),
        ("GAS_INTERFERENCE", "ROD_PARTING"),
        ("GAS_INTERFERENCE", "STUCK_PUMP"),
        ("GAS_LOCK", "NORMAL"),
        ("GAS_LOCK", "PARAFFIN_RESTRICTION"),
        ("GAS_LOCK", "PLUNGER_OUT_OF_BARREL"),
        ("GAS_LOCK", "PLUNGER_UNDERTRAVEL"),
        ("GAS_LOCK", "PUMP_TAGGING_DOWN"),
        ("GAS_LOCK", "PUMP_TAGGING_UP"),
        ("GAS_LOCK", "SAND_ABRASION"),
        ("GAS_LOCK", "STUCK_PUMP"),
        ("GAS_LOCK", "TUBING_MOVEMENT"),
        ("GAS_LOCK", "VALVE_LEAK_SV"),
        ("GAS_LOCK", "VALVE_LEAK_TV"),
        ("GAS_LOCK", "WORN_BARREL"),
        ("NORMAL", "ROD_PARTING"),
        ("NORMAL", "STUCK_PUMP"),
        ("PARAFFIN_RESTRICTION", "ROD_PARTING"),
        ("PARAFFIN_RESTRICTION", "STUCK_PUMP"),
        ("PLUNGER_OUT_OF_BARREL", "ROD_PARTING"),
        ("PLUNGER_OUT_OF_BARREL", "STUCK_PUMP"),
        ("PLUNGER_UNDERTRAVEL", "ROD_PARTING"),
        ("PLUNGER_UNDERTRAVEL", "STUCK_PUMP"),
        ("PUMP_TAGGING_DOWN", "ROD_PARTING"),
        ("PUMP_TAGGING_DOWN", "STUCK_PUMP"),
        ("PUMP_TAGGING_UP", "ROD_PARTING"),
        ("PUMP_TAGGING_UP", "STUCK_PUMP"),
        ("ROD_PARTING", "SAND_ABRASION"),
        ("ROD_PARTING", "TUBING_MOVEMENT"),
        ("ROD_PARTING", "VALVE_LEAK_SV"),
        ("ROD_PARTING", "VALVE_LEAK_TV"),
        ("ROD_PARTING", "WORN_BARREL"),
        ("SAND_ABRASION", "STUCK_PUMP"),
        ("STUCK_PUMP", "TUBING_MOVEMENT"),
        ("STUCK_PUMP", "VALVE_LEAK_SV"),
        ("STUCK_PUMP", "VALVE_LEAK_TV"),
        ("STUCK_PUMP", "WORN_BARREL"),
    },
}


def test_all_modes_audited_and_target_stroke_pairs_overlap():
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        ALL_GENERATORS,
    )
    from digitalmodel.marine_ops.artificial_lift.dynacard.generator_audit import (
        audit_generator_scale_ranges,
    )

    report = audit_generator_scale_ranges(samples_per_mode=32)
    assert set(report["ranges"]) == set(ALL_GENERATORS)
    targets = ("NORMAL", "TUBING_MOVEMENT", "PLUNGER_UNDERTRAVEL")
    for pair in itertools.combinations(targets, 2):
        assert sorted(pair) not in report["disjoint_pairs"]["stroke_length"]
    # A removed pair is an improvement; every newly separated pair is a regression.
    for metric, pairs in report["disjoint_pairs"].items():
        current = {tuple(pair) for pair in pairs}
        assert current <= BASELINE_DISJOINT_PAIRS[metric], (
            metric, current - BASELINE_DISJOINT_PAIRS[metric]
        )


@pytest.mark.parametrize("count", [0, -1, True, 1.5])
def test_scale_audit_rejects_invalid_sample_count(count):
    from digitalmodel.marine_ops.artificial_lift.dynacard.generator_audit import (
        audit_generator_scale_ranges,
    )

    with pytest.raises(ValueError):
        audit_generator_scale_ranges(samples_per_mode=count)


def test_scale_audit_rejects_empty_registry():
    from digitalmodel.marine_ops.artificial_lift.dynacard.generator_audit import (
        audit_generator_scale_ranges,
    )

    with pytest.raises(ValueError):
        audit_generator_scale_ranges(generators={})


def test_scale_audit_rejects_nonfinite_metrics():
    from digitalmodel.marine_ops.artificial_lift.dynacard.generator_audit import (
        audit_generator_scale_ranges,
    )

    with pytest.raises(ValueError, match="nonfinite"):
        audit_generator_scale_ranges(
            samples_per_mode=1,
            generators={"A": lambda seed: _rectangle(float("nan"))},
        )


def test_executable_worked_example():
    """SWPSC (2017), Fig. 1: (45.4 - 30.7 + 12.2) - 8.3 = 18.6 in."""
    path = (
        Path(__file__).resolve().parents[4]
        / "examples/domains/artificial_lift/dynacard_stroke_basis.py"
    )
    example = runpy.run_path(str(path))["worked_example"]()
    assert example["anchored_travel_in"] == 26.9
    assert example["specified_tubing_loss_in"] == 8.3
    assert example["pump_travel_in"] == 18.6
    assert example["other_installation_healthy_travel_in"] == 18.6
