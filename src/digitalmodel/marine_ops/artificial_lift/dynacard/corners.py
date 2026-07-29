# ABOUTME: Corner detection algorithms for dynamometer card analysis.
# ABOUTME: Identifies the four corners of the pump card for fillage and fluid load calculations.

import numpy as np
from scipy.spatial import ConvexHull
from typing import List, Tuple
from .comparison import canonicalize_card_direction
from .models import CardData


class CornerDetector:
    """
    Detects the four characteristic corners of a dynamometer card.

    Corner definitions:
        0 - Bottom Left (BL): Start of upstroke, standing valve closes
        1 - Top Left (TL): End of upstroke load transfer
        2 - Top Right (TR): Maximum plunger travel, start of downstroke
        3 - Bottom Right (BR): Traveling valve opens, effective plunger travel

    The corners are critical for:
        - Pump fillage calculation (BR corner determines net stroke)
        - Fluid load calculation (difference between upstroke and downstroke loads)
    """

    def __init__(self, card: CardData):
        """Initialize corner detector with card data.

        Args:
            card: Dynamometer card containing position and load arrays.
        """
        position = np.asarray(card.position, dtype=np.float64)
        load = np.asarray(card.load, dtype=np.float64)
        (
            self.position,
            self.load,
            self._source_indices,
        ) = canonicalize_card_direction(position, load, clockwise=True)
        self.n_points = len(self.position)

    def detect_corners(self) -> List[int]:
        """
        Detect the four corners of the dynamometer card.
        Returns list of 4 indices: [BL, TL, TR, BR]
        """
        # Method 1: Use convex hull to find extreme points
        corners = self._detect_via_convex_hull()

        # Validate and order corners
        corners = self._order_corners(corners)

        return [int(self._source_indices[index]) for index in corners]

    def _detect_via_convex_hull(self) -> List[int]:
        """
        Use convex hull to find corner candidates.
        """
        points = np.column_stack((self.position, self.load))
        try:
            hull = ConvexHull(points)
            hull_vertices = hull.vertices
        except Exception:
            return self._detect_via_extrema()

        hull_pos = self.position[hull_vertices]
        min_pos_mask = hull_pos <= np.percentile(hull_pos, 25)
        max_pos_mask = hull_pos >= np.percentile(hull_pos, 75)

        # Bottom Left: min position, min load in that region
        bl_candidates = hull_vertices[min_pos_mask]
        if len(bl_candidates) > 0:
            bl_idx = bl_candidates[np.argmin(self.load[bl_candidates])]
        else:
            bl_idx = 0

        # Top Left: min position, max load in that region
        tl_candidates = hull_vertices[min_pos_mask]
        if len(tl_candidates) > 0:
            tl_idx = tl_candidates[np.argmax(self.load[tl_candidates])]
        else:
            tl_idx = np.argmax(self.load[:self.n_points // 2])

        # Top Right: max position, max load
        tr_candidates = hull_vertices[max_pos_mask]
        if len(tr_candidates) > 0:
            tr_idx = tr_candidates[np.argmax(self.load[tr_candidates])]
        else:
            tr_idx = np.argmax(self.position)

        # Bottom Right: end of fluid-load transfer on the downstroke. It can
        # sit well below maximum position when fillage is incomplete.
        br_idx = self._find_bottom_right_corner()

        return [int(bl_idx), int(tl_idx), int(tr_idx), int(br_idx)]

    def _detect_via_extrema(self) -> List[int]:
        """
        Fallback corner detection using simple extrema.
        """
        # Find the midpoint of the stroke
        mid_idx = self.n_points // 2

        # BL: Start of card (index 0)
        bl_idx = 0

        # TL: Maximum load in first half
        tl_idx = np.argmax(self.load[:mid_idx])

        # TR: Maximum position
        tr_idx = np.argmax(self.position)

        # BR: Find where traveling valve opens (sharp load drop in second half)
        br_idx = self._find_bottom_right_corner()

        return [bl_idx, tl_idx, tr_idx, br_idx]

    def _find_bottom_right_corner(self) -> int:
        """
        Find the bottom right corner (traveling valve opening point).
        This is where the load drops sharply during downstroke.
        """
        top_candidates = np.flatnonzero(self.position == np.max(self.position))
        bottom_candidates = np.flatnonzero(self.position == np.min(self.position))
        top_idx = int(top_candidates[np.argmax(self.load[top_candidates])])
        bottom_idx = int(
            bottom_candidates[np.argmin(self.load[bottom_candidates])]
        )
        downstroke = self._cyclic_indices(top_idx, bottom_idx, include_stop=True)
        position_span = float(np.ptp(self.position))
        load_span = float(np.ptp(self.load))
        if (
            len(downstroke) < 2
            or position_span <= np.finfo(np.float64).eps
            or load_span <= np.finfo(np.float64).eps
        ):
            return top_idx

        retained_stroke = (
            self.position[downstroke] - np.min(self.position)
        ) / position_span
        remaining_load = (
            self.load[downstroke] - np.min(self.load)
        ) / load_span
        knee_offset = int(np.argmax(retained_stroke - remaining_load))

        # A discretised taper can leave the knee one material load step before
        # transfer finishes. Include that endpoint without chasing gradual
        # lower-branch load variation.
        if knee_offset + 1 < len(downstroke):
            taper_drop = (
                self.load[downstroke[knee_offset]]
                - self.load[downstroke[knee_offset + 1]]
            )
            if taper_drop > 0.01 * load_span:
                knee_offset += 1
        return int(downstroke[knee_offset])

    def _order_corners(self, corners: List[int]) -> List[int]:
        """
        Ensure corners are in proper order: BL, TL, TR, BR.
        """
        # The detector identifies BL first. In canonical clockwise traversal,
        # the remaining phases follow BL -> TL -> TR -> BR even when BR is not
        # one of the two highest-position candidates.
        bl_idx = corners[0]
        return sorted(corners, key=lambda index: (index - bl_idx) % self.n_points)

    def _cyclic_indices(
        self, start: int, stop: int, *, include_stop: bool = False
    ) -> np.ndarray:
        """Return forward traversal indices across an arbitrary array origin."""
        count = (stop - start) % self.n_points
        if include_stop:
            count += 1
        return (start + np.arange(count)) % self.n_points


def calculate_corners(card: CardData) -> Tuple[List[int], np.ndarray]:
    """
    Calculate corners of a dynamometer card.

    Args:
        card: CardData with position and load arrays

    Returns:
        (corners, box): corners is list of 4 indices, box is closed polygon
    """
    detector = CornerDetector(card)
    corners = detector.detect_corners()

    # Create closed box polygon for visualization
    box = np.array(corners + [corners[0]])

    return corners, box


def get_corner_loads(card: CardData, corners: List[int]) -> dict:
    """
    Extract load values at each corner.
    """
    load = np.array(card.load)
    position = np.array(card.position)

    return {
        'bl_load': load[corners[0]],
        'tl_load': load[corners[1]],
        'tr_load': load[corners[2]],
        'br_load': load[corners[3]],
        'bl_position': position[corners[0]],
        'tl_position': position[corners[1]],
        'tr_position': position[corners[2]],
        'br_position': position[corners[3]],
    }
