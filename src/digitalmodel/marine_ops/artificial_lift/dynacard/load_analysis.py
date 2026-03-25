# ABOUTME: Load ratio analysis for rod pump systems.
# ABOUTME: Calculates peak and minimum load ratios for equipment monitoring.

from typing import Optional
from .models import (
    DynacardAnalysisContext,
    LoadRatioAnalysis,
    CardData,
)
from .base import BaseCalculator


class LoadRatioCalculator(BaseCalculator[LoadRatioAnalysis]):
    """
    Calculates load ratios for rod pump system monitoring.

    Load ratios compare actual measured loads to expected "normal" loads,
    helping identify potential equipment issues:
    - Peak Load Ratio > 1.0: Higher than expected loads (potential overload)
    - Peak Load Ratio < 1.0: Lower than expected loads
    - Low Load Ratio > 1.0: Higher minimum loads
    - Low Load Ratio < 1.0: Lower minimum loads (potential issues)
    """

    def _create_result(self) -> LoadRatioAnalysis:
        return LoadRatioAnalysis()

    def calculate(
        self,
        surface_card: Optional[CardData] = None,
    ) -> LoadRatioAnalysis:
        """
        Execute load ratio analysis.

        Args:
            surface_card: Optional surface card to extract actual loads from.
                         If not provided, uses input_params load values.

        Returns:
            LoadRatioAnalysis with peak and low load ratios
        """
        # Get actual load values
        actual_peak, actual_min = self._get_actual_loads(surface_card)

        # Get normal (expected) load values
        normal_peak, normal_min = self._get_normal_loads()

        # Calculate peak load ratio
        if actual_peak is not None and normal_peak is not None and normal_peak > 0:
            self.result.peak_load_ratio = round(actual_peak / normal_peak, 4)
            self.result.actual_peak_load = actual_peak
            self.result.normal_peak_load = normal_peak

        # Calculate low load ratio
        if actual_min is not None and normal_min is not None and normal_min != 0:
            self.result.low_load_ratio = round(actual_min / normal_min, 4)
            self.result.actual_min_load = actual_min
            self.result.normal_min_load = normal_min

        return self.result

    def _get_actual_loads(
        self,
        surface_card: Optional[CardData],
    ) -> tuple[Optional[float], Optional[float]]:
        """
        Get actual peak and minimum loads.

        Priority:
        1. InputParameters stroke_load_peak/stroke_load_min
        2. Extract from surface card data
        3. Extract from context surface_card
        """
        actual_peak = None
        actual_min = None

        # Check input parameters first
        if self.ctx.input_params:
            if self.ctx.input_params.stroke_load_peak is not None:
                actual_peak = self.ctx.input_params.stroke_load_peak
            if self.ctx.input_params.stroke_load_min is not None:
                actual_min = self.ctx.input_params.stroke_load_min

        # If not in input params, extract from card
        if actual_peak is None or actual_min is None:
            card = surface_card if surface_card is not None else self.ctx.surface_card

            if card is not None and len(card.load) > 0:
                if actual_peak is None:
                    actual_peak = max(card.load)
                if actual_min is None:
                    actual_min = min(card.load)

        return actual_peak, actual_min

    def _get_normal_loads(self) -> tuple[Optional[float], Optional[float]]:
        """
        Get normal (expected) peak and minimum loads.

        These values typically come from setpoints or calculated design values.
        """
        normal_peak = None
        normal_min = None

        if self.ctx.input_params:
            if self.ctx.input_params.normal_peak_load is not None:
                normal_peak = self.ctx.input_params.normal_peak_load
            if self.ctx.input_params.normal_min_load is not None:
                normal_min = self.ctx.input_params.normal_min_load

        # Fallback to load_max_sp / load_min_sp if available
        if normal_peak is None and self.ctx.input_params:
            normal_peak = self.ctx.input_params.load_max_sp
        if normal_min is None and self.ctx.input_params:
            normal_min = self.ctx.input_params.load_min_sp

        return normal_peak, normal_min


def calculate_load_ratios(
    context: DynacardAnalysisContext,
    surface_card: Optional[CardData] = None,
) -> LoadRatioAnalysis:
    """
    Convenience function to calculate load ratios.

    Args:
        context: Complete analysis context
        surface_card: Optional surface card to extract loads from

    Returns:
        LoadRatioAnalysis with peak and low load ratios
    """
    calculator = LoadRatioCalculator(context)
    return calculator.calculate(surface_card)


def calculate_peak_load_ratio(
    actual_peak: float,
    normal_peak: float,
) -> float:
    """
    Calculate peak load ratio directly from values.

    Args:
        actual_peak: Actual measured peak load (lbs)
        normal_peak: Normal expected peak load (lbs)

    Returns:
        Peak load ratio (actual/normal)
    """
    if normal_peak <= 0:
        return 0.0
    return actual_peak / normal_peak


def calculate_low_load_ratio(
    actual_min: float,
    normal_min: float,
) -> float:
    """
    Calculate low load ratio directly from values.

    Args:
        actual_min: Actual measured minimum load (lbs)
        normal_min: Normal expected minimum load (lbs)

    Returns:
        Low load ratio (actual/normal)
    """
    if normal_min == 0:
        return 0.0
    return actual_min / normal_min
