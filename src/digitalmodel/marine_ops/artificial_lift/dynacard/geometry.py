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

        # Calculate perimeter
        self.result.perimeter = self._calculate_polygon_perimeter(polygon)

        # Calculate centroid
        cx, cy = self._calculate_centroid(polygon)
        self.result.centroid_position = cx
        self.result.centroid_load = cy

        # Calculate zoned areas (quadrant distribution)
        self._calculate_zoned_areas(position, load)

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

        Args:
            polygon: Nx2 array of (x, y) coordinates

        Returns:
            Total perimeter length
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

    def _calculate_centroid(self, polygon: np.ndarray) -> Tuple[float, float]:
        """
        Calculate polygon centroid (center of mass).

        Args:
            polygon: Nx2 array of (x, y) coordinates

        Returns:
            (cx, cy) centroid coordinates
        """
        n = len(polygon)
        if n < 3:
            return (0.0, 0.0)

        # Simple centroid (average of vertices)
        cx = float(np.mean(polygon[:, 0]))
        cy = float(np.mean(polygon[:, 1]))

        return (cx, cy)

    def _calculate_zoned_areas(
        self,
        position: np.ndarray,
        load: np.ndarray,
    ) -> None:
        """
        Calculate zoned area distribution (quadrants).

        Divides the card into 4 quadrants and calculates
        the approximate area fraction in each zone.

        Zone layout:
            Zone 2 | Zone 3  (top)
            -------|-------
            Zone 0 | Zone 1  (bottom)
            (left)  (right)
        """
        # Find midpoints
        pos_mid = (np.min(position) + np.max(position)) / 2.0
        load_mid = (np.min(load) + np.max(load)) / 2.0

        # Count points in each quadrant
        counts = [0, 0, 0, 0]
        n = len(position)

        for i in range(n):
            pos = position[i]
            ld = load[i]

            # Determine quadrant
            if pos < pos_mid:
                if ld < load_mid:
                    counts[0] += 1  # bottom-left
                else:
                    counts[2] += 1  # top-left
            else:
                if ld < load_mid:
                    counts[1] += 1  # bottom-right
                else:
                    counts[3] += 1  # top-right

        # Normalize to fractions
        total = sum(counts)
        if total > 0:
            fractions = [c / total for c in counts]
        else:
            fractions = [0.0, 0.0, 0.0, 0.0]

        # Approximate zone areas using total area and fractions
        total_area = self.result.area
        self.result.zone_areas = [f * total_area for f in fractions]
        self.result.zone_area_fractions = fractions


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

    Args:
        position: Position array (inches)
        load: Load array (lbs)

    Returns:
        Card perimeter length
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
