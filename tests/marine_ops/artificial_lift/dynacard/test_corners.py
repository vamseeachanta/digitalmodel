# ABOUTME: Unit tests for dynacard corner detection module.
# ABOUTME: Validates corner detection, ordering, and load extraction logic.

import pytest
import numpy as np

from digitalmodel.marine_ops.artificial_lift.dynacard.corners import (
    CornerDetector,
    calculate_corners,
    get_corner_loads,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.calculations import (
    calculate_fluid_load,
    calculate_pump_fillage,
)
from digitalmodel.marine_ops.artificial_lift.dynacard.models import CardData


def _make_rectangular_card() -> CardData:
    """Build a rectangular card traversed clockwise: BL -> TL -> TR -> BR."""
    n = 25
    bl_to_tl = [(0.0, 5000 + i * (15000 - 5000) / n) for i in range(n)]
    tl_to_tr = [(i * 100 / n, 15000.0) for i in range(n)]
    tr_to_br = [(100.0, 15000 - i * (15000 - 5000) / n) for i in range(n)]
    br_to_bl = [(100 - i * 100 / n, 5000.0) for i in range(n)]
    points = bl_to_tl + tl_to_tr + tr_to_br + br_to_bl
    pos = [p[0] for p in points]
    load = [p[1] for p in points]
    return CardData(position=pos, load=load)


def _make_sinusoidal_card() -> CardData:
    """Build a sinusoidal card typical of a pumping unit stroke."""
    t = np.linspace(0, 2 * np.pi, 100)
    pos = 100 * (1 - np.cos(t))
    load = 25000 + 5000 * np.sin(t)
    return CardData(position=pos.tolist(), load=load.tolist())


def _make_degenerate_card() -> CardData:
    """Build a degenerate card where all points are identical.

    ConvexHull will fail on identical points, forcing the extrema
    fallback path.
    """
    return CardData(
        position=[0.0, 0.0, 0.0],
        load=[100.0, 100.0, 100.0],
    )


def _make_partial_fillage_card() -> CardData:
    """Build a card whose downstroke load transfer finishes at 60% stroke."""
    points = [
        (0.0, 0.0),   # BL
        (0.0, 10.0),  # TL
        (25.0, 10.0),
        (50.0, 10.0),
        (75.0, 10.0),
        (100.0, 10.0),  # TR
        (90.0, 8.0),
        (80.0, 4.0),
        (70.0, 0.5),
        (60.0, 0.0),  # BR: load transfer has finished
        (40.0, 0.0),
        (20.0, 0.0),
    ]
    return CardData(
        position=[point[0] for point in points],
        load=[point[1] for point in points],
    )


def _make_multistage_transfer_card() -> CardData:
    """Build a partial card with a brief lull inside its downstroke drop."""
    points = [
        (0.0, 0.0),
        (0.0, 10.0),
        (50.0, 10.0),
        (100.0, 10.0),
        (90.0, 6.0),
        (80.0, 5.5),  # brief lull; load transfer is not complete
        (70.0, 2.5),
        (60.0, 0.0),  # BR
        (40.0, 0.0),
        (20.0, 0.0),
    ]
    return CardData(
        position=[point[0] for point in points],
        load=[point[1] for point in points],
    )


class TestCornerDetector:
    """Tests for the CornerDetector class."""

    def test_detect_corners_rectangular_card(self):
        """Rectangular card should yield exactly 4 corner indices."""
        card = _make_rectangular_card()
        detector = CornerDetector(card)
        corners = detector.detect_corners()
        assert len(corners) == 4

    def test_detect_corners_sinusoidal_card(self):
        """Sinusoidal card should produce 4 distinct corner indices."""
        card = _make_sinusoidal_card()
        detector = CornerDetector(card)
        corners = detector.detect_corners()
        assert len(corners) == 4
        assert len(set(corners)) == 4, "All corner indices must be distinct"

    def test_corner_ordering_bl_is_min_pos_min_load(self):
        """BL corner (index 0) must have the minimum position among left corners."""
        card = _make_sinusoidal_card()
        detector = CornerDetector(card)
        corners = detector.detect_corners()

        bl_idx, tl_idx = corners[0], corners[1]
        pos = np.array(card.position)
        load = np.array(card.load)

        # BL and TL are the two left-side corners (lower position values).
        # BL must have lower load than TL.
        assert load[bl_idx] < load[tl_idx], (
            "BL corner must have lower load than TL corner"
        )

    def test_corner_ordering_tr_is_max_pos_max_load(self):
        """TR corner (index 2) must have max position and higher load than BR."""
        card = _make_sinusoidal_card()
        detector = CornerDetector(card)
        corners = detector.detect_corners()

        tr_idx, br_idx = corners[2], corners[3]
        pos = np.array(card.position)
        load = np.array(card.load)

        # TR should have higher load than BR among the right-side corners.
        assert load[tr_idx] > load[br_idx], (
            "TR corner must have higher load than BR corner"
        )
        # TR and BR should be on the right side (higher position values).
        assert pos[tr_idx] >= pos[corners[0]], (
            "TR position must be >= BL position"
        )

    def test_detect_via_extrema_fallback(self):
        """Degenerate points should trigger the extrema fallback path."""
        card = _make_degenerate_card()
        detector = CornerDetector(card)
        corners = detector.detect_corners()
        assert len(corners) == 4
        # All indices must be valid for the 3-point card.
        for idx in corners:
            assert 0 <= idx < len(card.position)

    def test_find_bottom_right_corner(self):
        """BR corner must be located in the second half of the stroke."""
        card = _make_sinusoidal_card()
        detector = CornerDetector(card)
        br_idx = detector._find_bottom_right_corner()
        mid = len(card.position) // 2
        assert br_idx >= mid, "BR corner must be in the second half of the stroke"

    def test_detect_corners_returns_four_indices(self):
        """detect_corners must always return exactly 4 indices."""
        card = _make_sinusoidal_card()
        detector = CornerDetector(card)
        corners = detector.detect_corners()
        assert len(corners) == 4

    def test_corner_indices_in_valid_range(self):
        """All corner indices must be between 0 and n_points - 1."""
        card = _make_sinusoidal_card()
        detector = CornerDetector(card)
        corners = detector.detect_corners()
        n = len(card.position)
        for idx in corners:
            assert 0 <= idx < n, f"Index {idx} out of range [0, {n - 1}]"

    @pytest.mark.parametrize(
        "transform",
        [
            lambda values: np.roll(values, 4).tolist(),
            lambda values: list(reversed(np.roll(values, 4).tolist())),
        ],
        ids=["shifted-origin", "shifted-origin-reversed-direction"],
    )
    def test_partial_fillage_uses_phase_order_not_position_extrema(
        self, transform
    ):
        """BR remains the end of load transfer regardless of card storage."""
        card = _make_partial_fillage_card()
        stored_card = CardData(
            position=transform(card.position),
            load=transform(card.load),
        )

        result = calculate_pump_fillage(stored_card)

        # Gross stroke = 100 - 0 = 100; load transfer finishes at position 60.
        assert result.net_stroke == pytest.approx(60.0)
        assert result.fillage == pytest.approx(60.0)
        assert calculate_fluid_load(stored_card).fluid_load == pytest.approx(10.0)

    def test_full_card_stays_full_at_every_origin_and_direction(self):
        """A flat lower branch starts at BR, not one sample after it."""
        card = _make_rectangular_card()

        for reverse in (False, True):
            for shift in range(len(card.position)):
                position = np.roll(card.position, shift)
                load = np.roll(card.load, shift)
                if reverse:
                    position = position[::-1]
                    load = load[::-1]
                stored_card = CardData(position=position, load=load)

                assert calculate_pump_fillage(stored_card).fillage == pytest.approx(
                    100.0
                )

    def test_br_requires_sustained_end_of_load_transfer(self):
        """A one-sample lull does not terminate a multi-stage load drop."""
        result = calculate_pump_fillage(_make_multistage_transfer_card())

        assert result.net_stroke == pytest.approx(60.0)
        assert result.fillage == pytest.approx(60.0)


class TestCalculateCorners:
    """Tests for the calculate_corners helper function."""

    def test_calculate_corners_returns_tuple(self):
        """calculate_corners must return a (corners, box) tuple."""
        card = _make_sinusoidal_card()
        result = calculate_corners(card)
        assert isinstance(result, tuple)
        assert len(result) == 2

    def test_calculate_corners_box_is_closed(self):
        """The box array must have 5 elements (4 corners + first repeated)."""
        card = _make_sinusoidal_card()
        corners, box = calculate_corners(card)
        assert len(box) == 5

    def test_calculate_corners_box_first_equals_last(self):
        """The first and last elements of the box must be equal (closed loop)."""
        card = _make_sinusoidal_card()
        corners, box = calculate_corners(card)
        assert box[0] == box[4]


class TestGetCornerLoads:
    """Tests for the get_corner_loads helper function."""

    def test_get_corner_loads_keys(self):
        """Returned dict must contain all 8 expected keys."""
        card = _make_sinusoidal_card()
        corners, _ = calculate_corners(card)
        result = get_corner_loads(card, corners)
        expected_keys = {
            "bl_load", "tl_load", "tr_load", "br_load",
            "bl_position", "tl_position", "tr_position", "br_position",
        }
        assert set(result.keys()) == expected_keys

    def test_get_corner_loads_values(self):
        """Load values must match the card data at the corner indices."""
        card = _make_sinusoidal_card()
        corners, _ = calculate_corners(card)
        result = get_corner_loads(card, corners)
        load = np.array(card.load)

        assert result["bl_load"] == pytest.approx(load[corners[0]])
        assert result["tl_load"] == pytest.approx(load[corners[1]])
        assert result["tr_load"] == pytest.approx(load[corners[2]])
        assert result["br_load"] == pytest.approx(load[corners[3]])

    def test_get_corner_loads_positions(self):
        """Position values must match the card data at the corner indices."""
        card = _make_sinusoidal_card()
        corners, _ = calculate_corners(card)
        result = get_corner_loads(card, corners)
        position = np.array(card.position)

        assert result["bl_position"] == pytest.approx(position[corners[0]])
        assert result["tl_position"] == pytest.approx(position[corners[1]])
        assert result["tr_position"] == pytest.approx(position[corners[2]])
        assert result["br_position"] == pytest.approx(position[corners[3]])
