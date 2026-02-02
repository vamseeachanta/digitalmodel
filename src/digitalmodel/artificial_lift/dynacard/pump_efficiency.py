# ABOUTME: Pump efficiency calculation for rod pump systems.
# ABOUTME: Compares actual production to theoretical production for efficiency rating.

from typing import Optional
from .models import (
    DynacardAnalysisContext,
    ProductionAnalysis,
    WellTestData,
)


class PumpEfficiencyCalculator:
    """
    Calculates pump efficiency for rod pump systems.

    Pump efficiency is the ratio of actual liquid production
    to theoretical (calculated) production capacity.

    Formula: E_p = (Q_oil + Q_water) / Q_theoretical * 100

    Where:
        Q_oil = actual oil rate from well test (BPD)
        Q_water = actual water rate from well test (BPD)
        Q_theoretical = calculated theoretical production (BPD)
    """

    def __init__(self, context: DynacardAnalysisContext):
        self.ctx = context

    def calculate(
        self,
        theoretical_production: Optional[float] = None,
        production_analysis: Optional[ProductionAnalysis] = None,
    ) -> float:
        """
        Calculate pump efficiency.

        Args:
            theoretical_production: Optional theoretical production (BPD).
                                   If not provided, uses production_analysis.
            production_analysis: Optional production analysis with theoretical value.

        Returns:
            Pump efficiency as percentage (0-100+)
        """
        # Get theoretical production
        theo_prod = self._get_theoretical_production(
            theoretical_production, production_analysis
        )

        if theo_prod is None or theo_prod <= 0:
            return 0.0

        # Get actual production from well test
        actual_prod = self._get_actual_production()

        if actual_prod is None:
            return 0.0

        # Calculate efficiency
        efficiency = (actual_prod / theo_prod) * 100.0

        return round(efficiency, 2)

    def _get_theoretical_production(
        self,
        theoretical_production: Optional[float],
        production_analysis: Optional[ProductionAnalysis],
    ) -> Optional[float]:
        """Get theoretical production value."""
        if theoretical_production is not None and theoretical_production > 0:
            return theoretical_production

        if production_analysis is not None:
            return production_analysis.theoretical_production

        return None

    def _get_actual_production(self) -> Optional[float]:
        """
        Get actual liquid production from well test data.

        Returns total liquid rate (oil + water).
        """
        well_test = self.ctx.well_test

        if well_test is None:
            return None

        oil_rate = well_test.oil_rate or 0.0
        water_rate = well_test.water_rate or 0.0

        # Need at least one positive rate
        if oil_rate <= 0 and water_rate <= 0:
            return None

        return oil_rate + water_rate


def calculate_pump_efficiency(
    context: DynacardAnalysisContext,
    theoretical_production: Optional[float] = None,
    production_analysis: Optional[ProductionAnalysis] = None,
) -> float:
    """
    Convenience function to calculate pump efficiency.

    Args:
        context: Complete analysis context with well test data
        theoretical_production: Optional theoretical production (BPD)
        production_analysis: Optional production analysis

    Returns:
        Pump efficiency as percentage (0-100+)
    """
    calculator = PumpEfficiencyCalculator(context)
    return calculator.calculate(theoretical_production, production_analysis)


def calculate_efficiency_from_rates(
    oil_rate: float,
    water_rate: float,
    theoretical_production: float,
) -> float:
    """
    Calculate pump efficiency directly from rate values.

    Args:
        oil_rate: Actual oil production rate (BPD)
        water_rate: Actual water production rate (BPD)
        theoretical_production: Calculated theoretical production (BPD)

    Returns:
        Pump efficiency as percentage (0-100+)
    """
    if theoretical_production <= 0:
        return 0.0

    actual_production = oil_rate + water_rate
    efficiency = (actual_production / theoretical_production) * 100.0

    return round(efficiency, 2)
