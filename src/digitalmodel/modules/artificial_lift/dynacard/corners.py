# ABOUTME: Corner detection algorithms for dynamometer card analysis.
# ABOUTME: Identifies the four corners of the pump card for fillage and fluid load calculations.

import numpy as np
from scipy.spatial import ConvexHull
from typing import List, Tuple, Optional
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
        self.position = np.array(card.position)
        self.load = np.array(card.load)
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

        return corners

    def _detect_via_convex_hull(self) -> List[int]:
        """
        Use convex hull to find corner candidates.
        """
        # Stack position and load as 2D points
        points = np.column_stack((self.position, self.load))

        try:
            hull = ConvexHull(points)
            hull_vertices = hull.vertices
        except Exception:
            # Fallback to simple extrema detection
            return self._detect_via_extrema()

        # Find the 4 most extreme points on the hull
        # Sort hull vertices by position
        hull_pos = self.position[hull_vertices]
        hull_load = self.load[hull_vertices]

        # Find corners based on position and load extrema
        # BL: minimum position, low load
        # TL: minimum position, high load
        # TR: maximum position, high load
        # BR: maximum position, low load

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

        # Bottom Right: max position, min load in that region
        br_candidates = hull_vertices[max_pos_mask]
        if len(br_candidates) > 0:
            br_idx = br_candidates[np.argmin(self.load[br_candidates])]
        else:
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
        # Look in second half of stroke
        mid_idx = self.n_points // 2
        second_half = self.load[mid_idx:]

        # Find maximum rate of load decrease
        load_diff = np.diff(second_half)

        # Find the point with largest negative slope
        min_slope_idx = np.argmin(load_diff)

        # The corner is where the slope changes (after the drop)
        br_idx = mid_idx + min_slope_idx + 1

        # Clamp to valid range
        br_idx = min(br_idx, self.n_points - 1)

        return br_idx

    def _order_corners(self, corners: List[int]) -> List[int]:
        """
        Ensure corners are in proper order: BL, TL, TR, BR.
        """
        # Sort by position first
        pos_at_corners = self.position[corners]
        load_at_corners = self.load[corners]

        # Identify left side (min position) and right side (max position)
        sorted_by_pos = np.argsort(pos_at_corners)

        # Left corners (indices 0,1 in sorted)
        left_corners = [corners[sorted_by_pos[0]], corners[sorted_by_pos[1]]]
        right_corners = [corners[sorted_by_pos[2]], corners[sorted_by_pos[3]]]

        # Sort left by load (BL is lower, TL is higher)
        if self.load[left_corners[0]] > self.load[left_corners[1]]:
            bl_idx, tl_idx = left_corners[1], left_corners[0]
        else:
            bl_idx, tl_idx = left_corners[0], left_corners[1]

        # Sort right by load (BR is lower, TR is higher)
        if self.load[right_corners[0]] > self.load[right_corners[1]]:
            br_idx, tr_idx = right_corners[1], right_corners[0]
        else:
            br_idx, tr_idx = right_corners[0], right_corners[1]

        return [bl_idx, tl_idx, tr_idx, br_idx]


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
