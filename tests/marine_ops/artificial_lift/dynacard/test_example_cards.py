# ABOUTME: Tests for the dynacard example-card library and its loader.
# ABOUTME: Includes the synthetic re-classification drift guard.

import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.diagnostics import (
    PumpDiagnostics,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.example_cards import (
    VALID_SOURCES,
    VALID_UNITS,
    get_card,
    load_example_cards,
    phenomena,
)


def test_library_is_large_and_well_formed():
    cards = load_example_cards()
    assert len(cards) >= 300, "example library shrank below 300 cards"
    ids = [c.id for c in cards]
    assert len(ids) == len(set(ids))
    for c in cards:
        assert c.source in VALID_SOURCES
        assert c.units in VALID_UNITS
        assert c.render in {"line", "dots"}
        assert len(c.position) == len(c.load) >= 20


def test_all_sources_present():
    sources = {c.source for c in load_example_cards()}
    assert sources == VALID_SOURCES


def test_phenomenon_filter_and_get_card():
    assert "BUTTERFLY" in phenomena()
    butterflies = load_example_cards(phenomenon="BUTTERFLY")
    assert len(butterflies) >= 50
    card = get_card(butterflies[0].id)
    assert card.phenomenon == "BUTTERFLY"
    with pytest.raises(KeyError):
        get_card("nope-not-a-card")


def test_normalized_cards_stay_in_unit_box():
    for c in load_example_cards(source="field-archive-digitized"):
        assert 0.0 <= min(c.position) and max(c.position) <= 1.0
        assert 0.0 <= min(c.load) and max(c.load) <= 1.0
        assert c.render == "dots"


def test_measured_cards_have_context():
    measured = load_example_cards(source="field-measured")
    assert len(measured) >= 3
    for c in measured:
        assert c.units == "oilfield"
        assert c.meta.get("spm") and c.meta.get("stroke_in")
        # anonymized ids only - never a real well name
        assert c.id.startswith("field-well-")


@pytest.mark.parametrize(
    "current_arrays", [False, True], ids=["stored", "current-at-library-precision"]
)
def test_changed_stroke_examples_reproduce_or_declare_historical_basis(current_arrays):
    from digitalmodel.marine_ops.artificial_lift.dynacard.card_generators import (
        ALL_GENERATORS,
    )

    for card in load_example_cards(source="synthetic-verified"):
        if card.phenomenon not in {"PLUNGER_UNDERTRAVEL", "TUBING_MOVEMENT"}:
            continue
        current = ALL_GENERATORS[card.phenomenon](seed=card.meta["seed"])
        if current_arrays:
            # Test published precision independently of the stored-model filter.
            card = card.model_copy(update={
                "position": [round(p, 2) for p in current.position],
                "load": [round(v, 1) for v in current.load],
                "meta": {"seed": card.meta["seed"]},
            })
        reproduces = np.array_equal(
            card.position, [round(p, 2) for p in current.position]
        ) and np.array_equal(card.load, [round(v, 1) for v in current.load])
        if reproduces:
            assert card.meta.get("generator_basis") != "historical-disjoint-stroke-ranges"
            assert card.meta.get("seed_reproduces_current_generator", True) is True
        else:
            assert card.meta.get("generator_basis") == "historical-disjoint-stroke-ranges"
            assert card.meta.get("seed_reproduces_current_generator") is False


@pytest.mark.parametrize(
    "card", load_example_cards(source="synthetic-verified"),
    ids=lambda c: c.id,
)
def test_synthetic_cards_still_classify_to_their_mode(card):
    """Drift guard: every published synthetic variant must keep classifying
    back to its labeled phenomenon when the model or generators change."""
    live = PumpDiagnostics.classify_card(card.as_card_data())
    assert live == card.phenomenon, (
        f"{card.id} now classifies as {live}; rebuild the library with "
        "scripts/capabilities/build_dynacard_example_library.py and re-review."
    )
