# ABOUTME: Lift capacity calculation for rod pump systems.
# ABOUTME: Calculates maximum fluid lifting capability in barrels per day.

import numpy as np
from typing import Optional
from .models import (
    DynacardAnalysisContext,
    LiftCapacityAnalysis,
    PumpFillageAnalysis,
)
from .base import BaseCalculator
from .exceptions import DynacardException, ValidationError


# Conversion constant: (1440 min/day) / (9702 in^3/bbl) = 0.1484
# where 9702 = 5.615 gal/ft^3 * 231 in^3/gal * (1/12)^3 ft^3/in^3
BPD_CONVERSION = 1440.0 / 9702.0


class LiftCapacityCalculator(BaseCalculator[LiftCapacityAnalysis]):
    """
    Calculates lift capacity for rod pump systems.

    Lift capacity represents the maximum theoretical fluid volume
    the pump can lift per day at 100% efficiency.

    Formula: LC = (π/4) * d_p² * SPM_avg * SL_dh * E_p * (1440/9702)

    Where:
        d_p = plunger diameter (inches)
        SPM_avg = 24-hour average strokes per minute
        SL_dh = downhole stroke length (inches)
        E_p = assumed pump efficiency (0-1)
    """

    def _create_result(self) -> LiftCapacityAnalysis:
        return LiftCapacityAnalysis()

    def calculate(
        self,
        downhole_stroke: Optional[float] = None,
        fillage_analysis: Optional[PumpFillageAnalysis] = None,
    ) -> LiftCapacityAnalysis:
        """
        Execute lift capacity calculation.

        Args:
            downhole_stroke: Optional downhole stroke length in inches.
                             If not provided, uses gross_stroke from fillage_analysis
                             or falls back to surface stroke length.
            fillage_analysis: Optional fillage analysis with gross stroke data.

        Returns:
            LiftCapacityAnalysis with lift capacity in BPD

        Raises:
            ValidationError: If pump data is missing or invalid.
        """
        # Validate pump data (raises exception on failure)
        self.validate_pump()

        # Get plunger diameter (inches)
        plunger_diameter = self.ctx.pump.diameter

        # Get 24-hour average SPM
        spm_24hr = self._get_spm_24hr_avg()
        if spm_24hr is None or spm_24hr <= 0:
            raise ValidationError(
                "SPM 24-hour average must be positive",
                field="spm_24hr_avg",
                details={"value": spm_24hr}
            )

        # Get downhole stroke length (inches)
        stroke_length = self._get_stroke_length(downhole_stroke, fillage_analysis)
        if stroke_length <= 0:
            raise ValidationError(
                "Stroke length must be positive",
                field="stroke_length",
                details={"value": stroke_length}
            )

        # Get assumed pump efficiency
        efficiency = self._get_pump_efficiency()

        # Calculate lift capacity
        # LC = (π/4) * d² * SPM * SL * E * (1440/9702)
        plunger_area = (np.pi / 4.0) * (plunger_diameter ** 2)
        lift_capacity = plunger_area * spm_24hr * stroke_length * efficiency * BPD_CONVERSION

        # Populate results
        self.result.lift_capacity = round(lift_capacity, 2)
        self.result.plunger_diameter = plunger_diameter
        self.result.stroke_length = round(stroke_length, 3)
        self.result.spm_24hr_avg = spm_24hr
        self.result.assumed_efficiency = efficiency

        return self.result


    def _get_spm_24hr_avg(self) -> Optional[float]:
        """
        Get 24-hour average SPM for lift capacity calculation.

        Priority:
        1. InputParameters.spm_24hr_avg (if available)
        2. Fall back to context spm (instantaneous)
        """
        if self.ctx.input_params and self.ctx.input_params.spm_24hr_avg:
            return self.ctx.input_params.spm_24hr_avg

        # Fall back to instantaneous SPM
        return self.ctx.spm

    def _get_stroke_length(
        self,
        downhole_stroke: Optional[float],
        fillage_analysis: Optional[PumpFillageAnalysis],
    ) -> float:
        """
        Get downhole stroke length for lift capacity calculation.

        Priority:
        1. Explicitly provided downhole_stroke
        2. Fillage analysis gross_stroke (maximum plunger travel)
        3. Surface unit stroke length (fallback)
        """
        if downhole_stroke is not None and downhole_stroke > 0:
            return downhole_stroke

        if fillage_analysis is not None and fillage_analysis.gross_stroke > 0:
            return fillage_analysis.gross_stroke

        # Fall back to surface stroke length
        if self.ctx.surface_unit and self.ctx.surface_unit.stroke_length > 0:
            return self.ctx.surface_unit.stroke_length

        return 0.0

    def _get_pump_efficiency(self) -> float:
        """
        Get assumed pump efficiency for lift capacity calculation.

        Priority:
        1. InputParameters.assumed_pump_efficiency
        2. Pump.efficiency
        3. Default 1.0 (100%)
        """
        if self.ctx.input_params and self.ctx.input_params.assumed_pump_efficiency:
            return self.ctx.input_params.assumed_pump_efficiency

        if self.ctx.pump and self.ctx.pump.efficiency > 0:
            return self.ctx.pump.efficiency

        return 1.0


def calculate_lift_capacity(
    context: DynacardAnalysisContext,
    downhole_stroke: Optional[float] = None,
    fillage_analysis: Optional[PumpFillageAnalysis] = None,
    raise_on_error: bool = False,
) -> LiftCapacityAnalysis:
    """
    Convenience function to calculate lift capacity.

    Args:
        context: Complete analysis context with pump data
        downhole_stroke: Optional downhole stroke length (inches)
        fillage_analysis: Optional fillage analysis with stroke data
        raise_on_error: If True, raises exceptions on validation errors.
                       If False, returns result with zero values.

    Returns:
        LiftCapacityAnalysis with lift capacity in BPD

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
    """
    calculator = LiftCapacityCalculator(context)
    if raise_on_error:
        return calculator.calculate(downhole_stroke, fillage_analysis)

    try:
        return calculator.calculate(downhole_stroke, fillage_analysis)
    except DynacardException:
        # Return zero values on error
        return calculator.result
