# ABOUTME: Card geometry analysis for dynacard diagnostics.
# ABOUTME: Calculates area, perimeter, centroid, and zoned distribution for card shape analysis.

from typing import Optional, List, Tuple
import numpy as np

from .models import (
    DynacardAnalysisContext,
    CardData,
    CardGeometryAnalysis,
)
from .base import BaseCalculator
from .exceptions import DynacardException, ValidationError


class CardGeometryCalculator(BaseCalculator[CardGeometryAnalysis]):
    """
    Calculates geometric properties of dynamometer cards.

    Card geometry is used for:
    - Work calculation (area = work per stroke)
    - Shape analysis for diagnostics
    - Zoned area distribution for pattern recognition
    """

    def _create_result(self) -> CardGeometryAnalysis:
        return CardGeometryAnalysis()

    def calculate(
        self,
        surface_card: Optional[CardData] = None,
    ) -> CardGeometryAnalysis:
        """
        Calculate card geometry metrics.

        Args:
            surface_card: Optional surface card to analyze.
                         Uses context surface_card if not provided.

        Returns:
            CardGeometryAnalysis with area, perimeter, centroid, and zones.

        Raises:
            ValidationError: If card data is missing or invalid.
        """
        card = surface_card or self.ctx.surface_card

        if card is None:
            raise ValidationError(
                "Card data is required for geometry calculation",
                field="surface_card",
                details={"error_type": "missing_data"}
            )

        if len(card.position) < 3:
            raise ValidationError(
                "Card must have at least 3 data points for geometry calculation",
                field="surface_card",
                details={"num_points": len(card.position), "minimum_required": 3}
            )

        # Convert to numpy arrays
        position = np.array(card.position, dtype=np.float64)
        load = np.array(card.load, dtype=np.float64)

        if len(position) != len(load):
            raise ValidationError(
                f"Position ({len(position)}) and load ({len(load)}) arrays have different lengths",
                field="surface_card",
                details={"position_length": len(position), "load_length": len(load)}
            )

        # Calculate range metrics
        self.result.position_range = float(np.max(position) - np.min(position))
        self.result.load_range = float(np.max(load) - np.min(load))

        # Build polygon (position, load pairs)
        polygon = np.column_stack([position, load])

        # Calculate area using shoelace formula
        self.result.area = self._calculate_polygon_area(polygon)

        # Path length in plotted card space. DEPRECATED: mixes inches and
        # pounds under one sqrt, so it is not a physical length. Retained only
        # because external consumers still read it; see the dimensionally
        # meaningful replacements set immediately below.
        self.result.perimeter = self._calculate_polygon_perimeter(polygon)

        # Dimensionally meaningful replacements for `perimeter`
        self.result.position_path_length = float(
            np.sum(np.abs(np.roll(position, -1) - position))
        )
        self.result.load_path_length = float(
            np.sum(np.abs(np.roll(load, -1) - load))
        )
        self.result.normalized_perimeter = self._calculate_normalized_perimeter(
            position, load
        )

        # Calculate centroid
        cx, cy = self._calculate_centroid(polygon)
        self.result.centroid_position = cx
        self.result.centroid_load = cy

        # Calculate zoned areas (quadrant distribution)
        self._calculate_zoned_areas(polygon)

        return self.result

    def _calculate_polygon_area(self, polygon: np.ndarray) -> float:
        """
        Calculate polygon area using shoelace formula.

        The shoelace formula computes the area of a simple polygon
        from its vertex coordinates.

        Args:
            polygon: Nx2 array of (x, y) coordinates

        Returns:
            Absolute area of the polygon
        """
        n = len(polygon)
        if n < 3:
            return 0.0

        area = 0.0
        for i in range(n):
            j = (i + 1) % n
            area += (polygon[j, 0] - polygon[i, 0]) * (polygon[j, 1] + polygon[i, 1])

        return abs(area * 0.5)

    def _calculate_polygon_perimeter(self, polygon: np.ndarray) -> float:
        """
        Calculate polygon perimeter (sum of edge lengths).

        .. deprecated::
            The card polygon has inches on one axis and pounds on the other, so
            ``sqrt(dx^2 + dy^2)`` adds inches to pounds and the result is NOT a
            physical length. Use ``position_path_length`` / ``load_path_length``
            (each dimensionally consistent) or ``normalized_perimeter``
            (dimensionless) instead.

        Args:
            polygon: Nx2 array of (x, y) coordinates

        Returns:
            Total edge-length sum in mixed (in, lb) card space
        """
        n = len(polygon)
        if n < 2:
            return 0.0

        # Shift polygon by one position
        shifted = np.roll(polygon, -1, axis=0)

        # Calculate distances between consecutive points
        distances = np.sqrt(
            (shifted[:, 0] - polygon[:, 0]) ** 2
            + (shifted[:, 1] - polygon[:, 1]) ** 2
        )

        return float(np.sum(distances))

    def _calculate_normalized_perimeter(
        self,
        position: np.ndarray,
        load: np.ndarray,
    ) -> float:
        """
        Perimeter of the card after each axis is scaled by its own range.

        This is the dimensionless, scale-free shape-complexity measure that a
        raw mixed-unit perimeter cannot provide. It equals 4.0 for any
        rectangle (regardless of aspect ratio or units) and grows as the card
        outline becomes longer relative to its bounding box.

        Returns 0.0 if either axis is degenerate (zero range).
        """
        pos_range = float(np.max(position) - np.min(position))
        load_range = float(np.max(load) - np.min(load))
        if pos_range <= 0.0 or load_range <= 0.0:
            return 0.0

        x = position / pos_range
        y = load / load_range
        dx = np.roll(x, -1) - x
        dy = np.roll(y, -1) - y
        return float(np.sum(np.sqrt(dx ** 2 + dy ** 2)))

    @staticmethod
    def _signed_area(polygon: np.ndarray) -> float:
        """Signed shoelace area (positive for counter-clockwise vertex order)."""
        if len(polygon) < 3:
            return 0.0
        x = polygon[:, 0]
        y = polygon[:, 1]
        return 0.5 * float(np.sum(x * np.roll(y, -1) - np.roll(x, -1) * y))

    def _calculate_centroid(
        self,
        polygon: np.ndarray,
    ) -> Tuple[Optional[float], Optional[float]]:
        """
        Calculate the area-weighted polygon centroid (centre of area).

        Uses the standard shoelace centroid formula::

            A  = 1/2 * sum( x_i*y_j - x_j*y_i )
            Cx = 1/(6A) * sum( (x_i + x_j) * (x_i*y_j - x_j*y_i) )
            Cy = 1/(6A) * sum( (y_i + y_j) * (x_i*y_j - x_j*y_i) )

        This depends on the enclosed shape, not on how the outline happens to
        be sampled - unlike the mean of the vertices, which moves when a card
        is re-sampled and does not move when the enclosed area changes.

        Args:
            polygon: Nx2 array of (x, y) coordinates

        Returns:
            (cx, cy) centroid coordinates, or (None, None) when the polygon
            encloses no area (fewer than 3 points, or collinear/degenerate),
            in which case the area centroid is undefined.
        """
        n = len(polygon)
        if n < 3:
            return (None, None)

        x = polygon[:, 0]
        y = polygon[:, 1]
        x_next = np.roll(x, -1)
        y_next = np.roll(y, -1)

        cross = x * y_next - x_next * y
        signed_area = 0.5 * float(np.sum(cross))

        # Degenerate (zero-area) polygon: the area centroid does not exist.
        # Scale the tolerance by the coordinate magnitudes so it is meaningful
        # for both unit squares and 1e6-magnitude cards.
        scale = float(np.max(np.abs(x)) + 1.0) * float(np.max(np.abs(y)) + 1.0)
        if abs(signed_area) <= 1e-12 * scale:
            return (None, None)

        cx = float(np.sum((x + x_next) * cross) / (6.0 * signed_area))
        cy = float(np.sum((y + y_next) * cross) / (6.0 * signed_area))

        return (cx, cy)

    @staticmethod
    def _clip_halfplane(
        polygon: np.ndarray,
        axis: int,
        threshold: float,
        keep_below: bool,
    ) -> np.ndarray:
        """
        Sutherland-Hodgman clip of a polygon against an axis-aligned half-plane.

        Args:
            polygon: Nx2 array of (x, y) coordinates
            axis: 0 to clip on x, 1 to clip on y
            threshold: the half-plane boundary value
            keep_below: True keeps coordinate <= threshold, False keeps >=

        Returns:
            Mx2 array of the clipped polygon (possibly empty).
        """
        n = len(polygon)
        if n == 0:
            return np.empty((0, 2), dtype=np.float64)

        def inside(point: np.ndarray) -> bool:
            return point[axis] <= threshold if keep_below else point[axis] >= threshold

        def intersect(a: np.ndarray, b: np.ndarray) -> np.ndarray:
            denom = b[axis] - a[axis]
            if denom == 0.0:
                return a.copy()
            t = (threshold - a[axis]) / denom
            return a + t * (b - a)

        out: List[np.ndarray] = []
        for i in range(n):
            prev = polygon[i - 1]
            curr = polygon[i]
            prev_in = inside(prev)
            curr_in = inside(curr)
            if curr_in:
                if not prev_in:
                    out.append(intersect(prev, curr))
                out.append(curr)
            elif prev_in:
                out.append(intersect(prev, curr))

        if not out:
            return np.empty((0, 2), dtype=np.float64)
        return np.asarray(out, dtype=np.float64)

    def _calculate_zoned_areas(self, polygon: np.ndarray) -> None:
        """
        Calculate zoned area distribution (quadrants).

        Splits the card at the mid-point of each axis and computes the TRUE
        enclosed sub-area of each quadrant by clipping the card polygon to that
        quadrant (Sutherland-Hodgman) and taking the shoelace area of the
        clipped piece. For a simple closed card the four sub-areas sum to the
        total card area exactly.

        (The previous implementation counted sample POINTS per quadrant and
        multiplied by the total area, which reports sampling density rather
        than area.)

        Zone layout:
            Zone 2 | Zone 3  (top)
            -------|-------
            Zone 0 | Zone 1  (bottom)
            (left)  (right)
        """
        if len(polygon) < 3:
            self.result.zone_areas = []
            self.result.zone_area_fractions = []
            return

        position = polygon[:, 0]
        load = polygon[:, 1]
        pos_mid = float(np.min(position) + np.max(position)) / 2.0
        load_mid = float(np.min(load) + np.max(load)) / 2.0

        total_signed = self._signed_area(polygon)
        if total_signed == 0.0:
            # Degenerate card - no enclosed area to distribute.
            self.result.zone_areas = []
            self.result.zone_area_fractions = []
            return

        # All clipped pieces inherit the parent's vertex orientation, so
        # normalising by its sign makes every sub-area non-negative.
        orientation = 1.0 if total_signed > 0.0 else -1.0

        # (keep_below_position, keep_below_load) per zone index
        quadrants = [
            (True, True),    # 0: bottom-left
            (False, True),   # 1: bottom-right
            (True, False),   # 2: top-left
            (False, False),  # 3: top-right
        ]

        areas: List[float] = []
        for pos_below, load_below in quadrants:
            clipped = self._clip_halfplane(polygon, 0, pos_mid, pos_below)
            clipped = self._clip_halfplane(clipped, 1, load_mid, load_below)
            areas.append(max(0.0, orientation * self._signed_area(clipped)))

        total = sum(areas)
        self.result.zone_areas = areas
        if total > 0.0:
            self.result.zone_area_fractions = [a / total for a in areas]
        else:
            self.result.zone_area_fractions = [0.0, 0.0, 0.0, 0.0]


def calculate_card_geometry(
    context: DynacardAnalysisContext,
    surface_card: Optional[CardData] = None,
    raise_on_error: bool = False,
) -> CardGeometryAnalysis:
    """
    Convenience function to calculate card geometry.

    Args:
        context: Complete analysis context
        surface_card: Optional surface card to analyze
        raise_on_error: If True, raises exceptions on validation errors.
                       If False, returns result with zero values.

    Returns:
        CardGeometryAnalysis with area, perimeter, centroid, and zones

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
    """
    calculator = CardGeometryCalculator(context)
    if raise_on_error:
        return calculator.calculate(surface_card)

    try:
        return calculator.calculate(surface_card)
    except DynacardException:
        return calculator.result


def calculate_card_area(
    position: List[float],
    load: List[float],
) -> float:
    """
    Calculate card area directly from position and load arrays.

    Uses the shoelace formula for polygon area calculation.

    Args:
        position: Position array (inches)
        load: Load array (lbs)

    Returns:
        Card area in in-lbs
    """
    if len(position) != len(load) or len(position) < 3:
        return 0.0

    polygon = np.column_stack([position, load])

    n = len(polygon)
    area = 0.0
    for i in range(n):
        j = (i + 1) % n
        area += (polygon[j, 0] - polygon[i, 0]) * (polygon[j, 1] + polygon[i, 1])

    return abs(area * 0.5)


def calculate_card_perimeter(
    position: List[float],
    load: List[float],
) -> float:
    """
    Calculate card perimeter directly from position and load arrays.

    .. deprecated::
        Position is in inches and load in pounds, so ``sqrt(dx^2 + dy^2)`` adds
        inches to pounds: the result is NOT a physical length and must not be
        interpreted as one. Use ``CardGeometryAnalysis.position_path_length`` /
        ``load_path_length`` (dimensionally consistent) or
        ``normalized_perimeter`` (dimensionless) instead.

    Args:
        position: Position array (inches)
        load: Load array (lbs)

    Returns:
        Sum of edge lengths in mixed (in, lb) card space
    """
    if len(position) != len(load) or len(position) < 2:
        return 0.0

    polygon = np.column_stack([position, load])
    shifted = np.roll(polygon, -1, axis=0)

    distances = np.sqrt(
        (shifted[:, 0] - polygon[:, 0]) ** 2
        + (shifted[:, 1] - polygon[:, 1]) ** 2
    )

    return float(np.sum(distances))
