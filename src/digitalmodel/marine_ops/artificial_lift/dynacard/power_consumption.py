# ABOUTME: Power consumption analysis for rod pump systems.
# ABOUTME: Calculates polished rod HP, prime mover HP, and daily energy consumption.

import numpy as np
from typing import Optional
from .models import (
    DynacardAnalysisContext,
    PowerConsumptionAnalysis,
    CardData,
)
from .base import BaseCalculator
from .exceptions import DynacardException


# Cyclic Load Factor (F_CL) lookup table per API RP 11L
# Based on motor design (Mark II vs Others) and NEMA motor code
F_CL_TABLE = {
    'Mark II': {'NEMA B': 1.517, 'NEMA D': 1.1},
    'Others': {'NEMA B': 1.897, 'NEMA D': 1.375}
}

# Conversion factors
HP_TO_KW = 0.7457  # Horsepower to kilowatts
NM_TO_LBFT = 0.737562  # Newton-meters to lb-ft


class PowerConsumptionCalculator(BaseCalculator[PowerConsumptionAnalysis]):
    """
    Calculates power consumption for rod pump systems.

    Uses the polished rod horsepower method based on card area
    and cyclic load factors per API RP 11L.

    Raises:
        ValidationError: If surface card data is missing or invalid.
    """

    def _create_result(self) -> PowerConsumptionAnalysis:
        return PowerConsumptionAnalysis()

    def calculate(self) -> PowerConsumptionAnalysis:
        """
        Execute power consumption analysis.

        Returns:
            PowerConsumptionAnalysis with HP and energy values

        Raises:
            ValidationError: If input validation fails.
        """
        # Validate inputs using exception-raising validators
        self.validate_common()

        # Calculate card area (work per stroke)
        card_area = self._calculate_card_area(self.ctx.surface_card)

        # Determine motor design and NEMA code
        motor_design = self._get_motor_design()
        nema_code = self._get_nema_code()

        # Get cyclic load factor
        f_cl = self._get_cyclic_load_factor(motor_design, nema_code)

        # Calculate polished rod horsepower
        # P_PR = (card_area * SPM) / 33000
        # 33000 = ft-lbs/min per HP
        spm = self.ctx.spm
        p_pr = (card_area * spm) / 33000.0

        # Get efficiency factors
        calc_params = self.ctx.calc_params
        efficiency_pm = calc_params.efficiency_prime_mover
        efficiency_u = calc_params.efficiency_pumping_unit

        # Calculate prime mover horsepower
        # P_PM = F_CL * P_PR / (efficiency_pm * efficiency_u)
        if efficiency_pm * efficiency_u > 0:
            p_pm = f_cl * p_pr / (efficiency_pm * efficiency_u)
        else:
            p_pm = f_cl * p_pr / 0.765  # Default 85% * 90%

        # Convert to kW
        p_kw = p_pm * HP_TO_KW

        # Calculate daily consumption
        runtime = self.ctx.runtime
        daily_kwh = p_kw * runtime

        # Populate results
        self.result.card_area = round(card_area, 2)
        self.result.polished_rod_horsepower = round(p_pr, 3)
        self.result.prime_mover_horsepower = round(p_pm, 3)
        self.result.power_consumption_kw = round(p_kw, 3)
        self.result.daily_energy_consumption = round(daily_kwh, 2)
        self.result.motor_design = motor_design
        self.result.nema_code = nema_code
        self.result.cyclic_load_factor = f_cl

        return self.result


    def _calculate_card_area(self, card: CardData) -> float:
        """
        Calculate dynamometer card area using the shoelace formula.

        The card area represents work per stroke in in-lbs (inch-pounds).
        Converting to ft-lbs: area_in_lbs / 12

        Args:
            card: Surface card with position and load data

        Returns:
            Card area in ft-lbs
        """
        position = np.array(card.position)
        load = np.array(card.load)

        # Shoelace formula for polygon area
        # Area = 0.5 * |sum((x[i+1] - x[i]) * (y[i+1] + y[i]))|
        n = len(position)
        area = 0.0

        for i in range(n):
            j = (i + 1) % n
            area += (position[j] - position[i]) * (load[j] + load[i])

        area = abs(area) / 2.0

        # Convert from in-lbs to ft-lbs (divide by 12)
        area_ft_lbs = area / 12.0

        return area_ft_lbs

    def _get_motor_design(self) -> str:
        """
        Determine motor design category based on pumping unit geometry.

        Mark II units have distinctive geometry with non-zero phase angle.
        All others (Conventional, Air Balanced, etc.) use "Others" category.

        Returns:
            "Mark II" or "Others"
        """
        su = self.ctx.surface_unit
        unit_type = su.unit_type.upper() if su.unit_type else ""
        geometry = getattr(su, 'geometry', '') or ''

        # Check for Mark II indicators
        # Mark II units typically have "M" or "M-S" geometry designation
        # and non-zero phase angle
        if 'MARK' in unit_type and 'II' in unit_type:
            return 'Mark II'

        if geometry.upper() in ['M', 'M-S']:
            if su.phase_angle != 0:
                return 'Mark II'

        return 'Others'

    def _get_nema_code(self) -> str:
        """
        Determine NEMA motor code from motor model.

        NEMA B motors are general purpose.
        NEMA D motors are high-slip for pumping applications.

        Returns:
            "NEMA B" or "NEMA D"
        """
        motor = self.ctx.motor
        if motor is None or not motor.model:
            # Default to NEMA B for general purpose
            return 'NEMA B'

        model = motor.model.upper()

        if 'NEMA D' in model or 'NEMA-D' in model or 'NEMAD' in model:
            return 'NEMA D'
        elif 'NEMA B' in model or 'NEMA-B' in model or 'NEMAB' in model:
            return 'NEMA B'
        elif 'D' in model and ('NEMA' in model or 'HIGH' in model):
            return 'NEMA D'
        else:
            # Default to NEMA B
            return 'NEMA B'

    def _get_cyclic_load_factor(self, motor_design: str, nema_code: str) -> float:
        """
        Get cyclic load factor (F_CL) from lookup table.

        F_CL accounts for the cyclic nature of the polished rod load
        and its effect on motor sizing.

        Args:
            motor_design: "Mark II" or "Others"
            nema_code: "NEMA B" or "NEMA D"

        Returns:
            Cyclic load factor
        """
        try:
            return F_CL_TABLE[motor_design][nema_code]
        except KeyError:
            # Default to Others/NEMA B
            return F_CL_TABLE['Others']['NEMA B']


def calculate_power_consumption(
    context: DynacardAnalysisContext,
    raise_on_error: bool = False,
) -> PowerConsumptionAnalysis:
    """
    Convenience function to calculate power consumption.

    Args:
        context: Complete analysis context with surface card data
        raise_on_error: If True, raises exceptions on validation errors.
                       If False, returns result with error status set.

    Returns:
        PowerConsumptionAnalysis with HP and energy values

    Raises:
        ValidationError: If raise_on_error=True and validation fails.
    """
    calculator = PowerConsumptionCalculator(context)
    if raise_on_error:
        return calculator.calculate()

    try:
        return calculator.calculate()
    except DynacardException as e:
        calculator.result.motor_design = f"error: {e.message}"
        return calculator.result


def calculate_card_area(card: CardData) -> float:
    """
    Calculate dynamometer card area using shoelace formula.

    Args:
        card: Surface card with position and load data

    Returns:
        Card area in ft-lbs
    """
    position = np.array(card.position)
    load = np.array(card.load)

    n = len(position)
    area = 0.0

    for i in range(n):
        j = (i + 1) % n
        area += (position[j] - position[i]) * (load[j] + load[i])

    area = abs(area) / 2.0

    # Convert from in-lbs to ft-lbs
    return area / 12.0
