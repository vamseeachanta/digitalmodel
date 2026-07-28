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
# Based on motor design (Mark II vs Others) and NEMA motor code.
#
# NEMA design codes classify ELECTRIC INDUCTION MOTORS. F_CL here is a function
# of motor slip, which is undefined for an engine-driven prime mover (e.g. a
# natural gas engine). This table must therefore only be consulted once the
# driver is KNOWN to be an electric motor - see _resolve_prime_mover_type.
F_CL_TABLE = {
    'Mark II': {'NEMA B': 1.517, 'NEMA D': 1.1},
    'Others': {'NEMA B': 1.897, 'NEMA D': 1.375}
}

# Recognised prime mover types
PRIME_MOVER_ELECTRIC = 'electric'
PRIME_MOVER_UNSPECIFIED = 'unspecified'
_ELECTRIC_ALIASES = {'electric', 'electric_motor', 'induction', 'motor'}

DEFAULT_RUNTIME_HOURS = 24.0

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

        warnings = []

        # Determine pumping unit class and prime mover type
        motor_design = self._get_motor_design()
        prime_mover_type = self._resolve_prime_mover_type()

        # Cyclic load factor is an ELECTRIC-motor correction. Apply it only
        # when the driver is known to be an electric motor; otherwise report
        # the unfactored prime mover HP and say so.
        if prime_mover_type == PRIME_MOVER_ELECTRIC:
            nema_code = self._get_nema_code()
            f_cl = self._get_cyclic_load_factor(motor_design, nema_code)
            f_cl_applied = True
        else:
            nema_code = ''
            f_cl = None
            f_cl_applied = False
            warnings.append(
                f"Cyclic load factor NOT applied: prime mover type is "
                f"'{prime_mover_type}'. NEMA design codes classify electric "
                f"induction motors only; F_CL is undefined for a non-electric "
                f"driver. Set motor.prime_mover_type='electric' (or give an "
                f"explicit NEMA code in motor.model) to apply it. Reported "
                f"prime_mover_horsepower is the efficiency-corrected polished "
                f"rod HP with no cyclic load factor."
            )

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
        factor = f_cl if f_cl_applied else 1.0
        if efficiency_pm * efficiency_u > 0:
            p_pm = factor * p_pr / (efficiency_pm * efficiency_u)
        else:
            p_pm = factor * p_pr / 0.765  # Default 85% * 90%

        # Convert to kW
        p_kw = p_pm * HP_TO_KW

        # Calculate daily consumption
        runtime, runtime_source = self._resolve_runtime()
        if runtime_source == 'assumed_default_24h':
            warnings.append(
                f"Runtime was not supplied by the caller; daily_energy_consumption "
                f"assumes {DEFAULT_RUNTIME_HOURS:g} h/day of pumping. Set "
                f"context.runtime or well_test.runtime for a measured value."
            )
        daily_kwh = p_kw * runtime

        # Populate results
        self.result.card_area = round(card_area, 2)
        self.result.polished_rod_horsepower = round(p_pr, 3)
        self.result.prime_mover_horsepower = round(p_pm, 3)
        self.result.power_consumption_kw = round(p_kw, 3)
        self.result.daily_energy_consumption = round(daily_kwh, 2)
        self.result.motor_design = motor_design
        self.result.prime_mover_type = prime_mover_type
        self.result.nema_code = nema_code
        self.result.cyclic_load_factor = f_cl
        self.result.cyclic_load_factor_applied = f_cl_applied
        self.result.runtime_hours = runtime
        self.result.runtime_source = runtime_source
        self.result.warnings = warnings

        return self.result

    def _resolve_prime_mover_type(self) -> str:
        """
        Determine what actually drives the pumping unit.

        The cyclic load factor table is only valid for electric induction
        motors, so the driver has to be stated rather than assumed. It counts
        as stated when either ``motor.prime_mover_type`` is set, or
        ``motor.model`` carries an explicit NEMA designation (which is itself a
        declaration that the driver is an electric motor).

        Returns:
            "electric", the caller-supplied type (e.g. "gas_engine"), or
            "unspecified" when the driver is unknown.
        """
        motor = self.ctx.motor
        if motor is None:
            return PRIME_MOVER_UNSPECIFIED

        declared = (motor.prime_mover_type or '').strip().lower()
        if declared:
            if declared in _ELECTRIC_ALIASES:
                return PRIME_MOVER_ELECTRIC
            return declared

        if motor.model and 'NEMA' in motor.model.upper():
            return PRIME_MOVER_ELECTRIC

        return PRIME_MOVER_UNSPECIFIED

    def _resolve_runtime(self) -> tuple:
        """
        Resolve daily pumping runtime and record where it came from.

        ``DynacardAnalysisContext.runtime`` has a 24 h default, so a context
        that never set it is indistinguishable from one that measured 24 h
        unless the set-ness is checked. Pydantic's ``model_fields_set`` gives
        exactly that.

        Returns:
            (runtime_hours, source) where source is "context", "well_test" or
            "assumed_default_24h".
        """
        if 'runtime' in self.ctx.model_fields_set:
            return float(self.ctx.runtime), 'context'

        well_test = self.ctx.well_test
        if well_test is not None and 'runtime' in well_test.model_fields_set:
            return float(well_test.runtime), 'well_test'

        return float(self.ctx.runtime), 'assumed_default_24h'


    def _calculate_card_area(self, card: CardData) -> float:
        """
        Calculate dynamometer card area using the shoelace formula.

        The raw shoelace area is in in-lbs (inches x pounds); this method
        returns it converted to FT-LBS (divided by 12), which is what the
        33,000 ft-lbs/min-per-HP constant in calculate() expects.

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

        Only called once the prime mover is known to be an electric induction
        motor (see _resolve_prime_mover_type), so falling back to NEMA B here
        is a general-purpose-motor assumption, not an assumption that the
        driver is electric at all.

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
        calculator.result.warnings = [f"error: {e.message}"]
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
