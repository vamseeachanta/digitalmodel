import numpy as np
import pytest

from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData


def _card(position, load):
    return CardData(position=position, load=load)


def _assert_zero_error(comparison):
    assert comparison.load_nrmse_pct == pytest.approx(0.0, abs=1.0e-12)
    assert comparison.position_nrmse_pct == pytest.approx(0.0, abs=1.0e-12)
    assert comparison.peak_load_error_pct == pytest.approx(0.0, abs=1.0e-12)
    assert comparison.minimum_load_error_pct == pytest.approx(0.0, abs=1.0e-12)
    assert comparison.stroke_error_pct == pytest.approx(0.0, abs=1.0e-12)
    assert comparison.enclosed_area_error_pct == pytest.approx(0.0, abs=1.0e-12)


def test_compare_cards_is_invariant_to_traversal_direction():
    from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import (
        compare_cards,
    )

    card = _card(
        position=[0.0, 2.0, 3.0, 1.0, -1.0],
        load=[10.0, 9.0, 12.0, 15.0, 13.0],
    )
    reversed_card = _card(
        position=list(reversed(card.position)),
        load=list(reversed(card.load)),
    )

    _assert_zero_error(compare_cards(reversed_card, card))


def test_compare_cards_is_invariant_to_circular_index_origin():
    from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import (
        compare_cards,
    )

    card = _card(
        position=[0.0, 2.0, 3.0, 1.0, -1.0],
        load=[10.0, 9.0, 12.0, 15.0, 13.0],
    )
    rotated_card = _card(
        position=np.roll(card.position, 2).tolist(),
        load=np.roll(card.load, 2).tolist(),
    )

    _assert_zero_error(compare_cards(rotated_card, card))


def test_compare_cards_reports_a_genuine_shape_difference():
    from digitalmodel.marine_ops.artificial_lift.dynacard.comparison import (
        compare_cards,
    )

    reference = _card(
        position=[0.0, 2.0, 2.0, 0.0],
        load=[10.0, 10.0, 14.0, 14.0],
    )
    taller = _card(
        position=[0.0, 2.0, 2.0, 0.0],
        load=[10.0, 10.0, 16.0, 16.0],
    )

    comparison = compare_cards(taller, reference)

    # RMSE = sqrt((0^2 + 0^2 + 2^2 + 2^2) / 4) = sqrt(2).
    # Normalising by the 4 lb reference range gives 100*sqrt(2)/4.
    assert comparison.load_nrmse_pct == pytest.approx(35.3553390593)
    assert comparison.position_nrmse_pct == pytest.approx(0.0)
    assert comparison.peak_load_error_pct == pytest.approx(100.0 * 2.0 / 14.0)
    assert comparison.minimum_load_error_pct == pytest.approx(0.0)
    assert comparison.stroke_error_pct == pytest.approx(0.0)
    # Reference area = 2*4 = 8; taller area = 2*6 = 12.
    assert comparison.enclosed_area_error_pct == pytest.approx(
        100.0 * (12.0 - 8.0) / 8.0
    )
