# ABOUTME: Data loader for converting legacy JSON well card data to Pydantic models.
# ABOUTME: Supports loading from files and parsing raw JSON dictionaries.

import json
from pathlib import Path
from typing import Dict, Any, Optional, Union
from .models import (
    DynacardAnalysisContext,
    CardData,
    RodSection,
    PumpProperties,
    SurfaceUnit,
    MotorProperties,
    WellTestData,
    InputParameters,
    CalculationParameters,
)


def load_from_json_file(filepath: Union[str, Path]) -> DynacardAnalysisContext:
    """
    Load dynacard context from a legacy JSON file.

    Args:
        filepath: Path to JSON file

    Returns:
        DynacardAnalysisContext ready for analysis
    """
    with open(filepath, 'r') as f:
        data = json.load(f)
    return parse_legacy_json(data)


def parse_legacy_json(data: Dict[str, Any]) -> DynacardAnalysisContext:
    """
    Parse legacy Oxy.Cipher.DynaCard JSON format to Pydantic models.

    Args:
        data: Raw JSON dictionary

    Returns:
        DynacardAnalysisContext
    """
    # Extract card details
    card_details = data.get('CardDetails', {})
    api14 = card_details.get('Api14', 'UNKNOWN')

    # Extract input parameters
    input_params_raw = data.get('InputParameters', {})
    spm = input_params_raw.get('StrokesPerMinute', 6.0)

    input_params = InputParameters(
        strokes_per_minute=spm,
        tubing_pressure=input_params_raw.get('TubingPressure', 100.0),
        runtime=input_params_raw.get('RunTime', 24.0),
        load_max_sp=input_params_raw.get('LoadMaxSP'),
        load_min_sp=input_params_raw.get('LoadMinSP'),
        fluid_density=input_params_raw.get('FluidDensity'),
    )

    # Extract surface card data
    card_data_raw = data.get('cardData', {})
    surface_card = CardData(
        position=card_data_raw.get('Position', []),
        load=card_data_raw.get('Load', []),
        timestamp=card_details.get('CardTimeStamp')
    )

    # Extract equipment data
    equipment = data.get('equipmentData', {})

    # Parse rod string
    rod_string = []
    for rod in equipment.get('Rods', []):
        weight = rod.get('Weight')
        if weight is None:
            # Calculate weight from diameter
            diameter = rod.get('Diameter', 0.875)
            weight = 2.904 * (diameter ** 2)  # Approximate lbs/ft

        rod_section = RodSection(
            diameter=rod.get('Diameter', 0.875),
            length=rod.get('TotalLength', 0.0),
            count=rod.get('Count', 1),
            modulus_of_elasticity=rod.get('ModulusOfElasticity', 30500000.0),
            speed_of_sound=rod.get('SpeedOfSound', 16300.0),
            weight_per_foot=weight,
            minimum_tensile=rod.get('MinimumTensile', 115000.0),
            grade=rod.get('Grade', 'KD'),
            manufacturer=rod.get('Manufacturer', 'Generic'),
        )
        rod_string.append(rod_section)

    # Parse pump
    pump_raw = equipment.get('Pump', {})
    pump = PumpProperties(
        diameter=pump_raw.get('Diameter', 1.75),
        depth=pump_raw.get('Depth', 5000.0),
    )

    # Parse surface unit
    unit_raw = equipment.get('SurfaceUnit', {})
    surface_unit = SurfaceUnit(
        manufacturer=unit_raw.get('PumpingUnitManufacturer') or 'Generic',
        unit_type=unit_raw.get('Description') or 'Conventional',
        stroke_length=unit_raw.get('StrokeLengthSetting', 0.0),
        gear_box_rating=unit_raw.get('GearBoxRating') or 0.0,
        structural_imbalance=unit_raw.get('StructuralImbalance') or 0.0,
        counterbalance_moment=unit_raw.get('CounterbalanceMoment') or 0.0,
        geometry=unit_raw.get('Rotation', 'C. Clockwise') or 'C. Clockwise',
        dimensional_a=unit_raw.get('DimensionalA') or 0.0,
        dimensional_c=unit_raw.get('DimensionalC') or 0.0,
        dimensional_i=unit_raw.get('DimensionalI') or 0.0,
        dimensional_k=unit_raw.get('DimensionalK') or 0.0,
        dimensional_p=unit_raw.get('DimensionalP') or 0.0,
        radius=unit_raw.get('Radius') or 0.0,
        phase_angle=unit_raw.get('PhaseAngle') or 0.0,
        beam_rating=unit_raw.get('BeamRating') or 0.0,
        max_stroke_length=unit_raw.get('MaxStrokeLength') or 0.0,
    )

    # Parse motor data
    motor_raw = equipment.get('RodPumpMotorData', {})
    motor = MotorProperties(
        model=motor_raw.get('Model') or '',
        horsepower=motor_raw.get('Horsepower') or 0.0,
        voltage=motor_raw.get('Voltage') or 0.0,
        manufacturer=motor_raw.get('Manufacturer') or '',
    )

    # Get runtime from input parameters
    runtime = input_params_raw.get('RunTime', 24.0)

    # Parse well test data
    well_test_raw = data.get('WellTestData', {})
    well_test = None
    if well_test_raw:
        well_test = WellTestData(
            oil_rate=well_test_raw.get('OilRate', 0.0),
            water_rate=well_test_raw.get('WaterRate', 0.0),
            gas_rate=well_test_raw.get('GasRate', 0.0),
            water_cut=well_test_raw.get('WaterCut', 0.0),
            runtime=well_test_raw.get('WellTestRuntime', 24.0),
            test_date=well_test_raw.get('WellTestDate'),
        )

    # Calculate fluid density from wellbore data if available
    wellbore = equipment.get('WellBore', {})
    fluid_density = 62.4  # Default water density
    if wellbore:
        water_sg = wellbore.get('WaterGravity', 1.0)
        oil_api = wellbore.get('OilApi', 35.0)
        oil_sg = 141.5 / (oil_api + 131.5)
        water_cut = well_test.water_cut / 100.0 if well_test else 0.5
        mixed_sg = water_sg * water_cut + oil_sg * (1.0 - water_cut)
        fluid_density = mixed_sg * 62.4

    # Create context
    context = DynacardAnalysisContext(
        api14=api14,
        surface_card=surface_card,
        rod_string=rod_string,
        pump=pump,
        surface_unit=surface_unit,
        motor=motor,
        spm=spm,
        fluid_density=fluid_density,
        runtime=runtime,
        well_test=well_test,
        input_params=input_params,
    )

    return context


def get_expected_results(data: Dict[str, Any]) -> Dict[str, Any]:
    """
    Extract expected analysis results from legacy JSON for validation.

    Args:
        data: Raw JSON dictionary

    Returns:
        Dictionary of expected results
    """
    dh_analysis = data.get('downholeCardAnalysis', {})

    # Parse percentage strings
    def parse_pct(s):
        if s is None:
            return None
        if isinstance(s, (int, float)):
            return float(s)
        return float(s.replace('%', ''))

    return {
        'fluid_load': float(dh_analysis.get('Fluid Load', 0)),
        'fluid_density': float(dh_analysis.get('Fluid Density', 0)),
        'theoretical_production': float(dh_analysis.get('Theoretical Production', 0)),
        'lift_capacity': float(dh_analysis.get('Lift Capacity', 0)),
        'pump_efficiency': parse_pct(dh_analysis.get('Pump Efficiency')),
        'pump_fillage': parse_pct(dh_analysis.get('Pump Fillage')),
        'pump_intake_pressure': float(dh_analysis.get('Pump Intake Pressure', 0)),
        'pump_discharge_pressure': float(dh_analysis.get('Pump Discharge Pressure', 0)),
        'spm': float(dh_analysis.get('SPM', 0)),
    }


def get_expected_downhole_card(data: Dict[str, Any]) -> Optional[CardData]:
    """
    Extract expected downhole card from legacy JSON for validation.

    Args:
        data: Raw JSON dictionary

    Returns:
        CardData with expected downhole values, or None
    """
    dh_card = data.get('downholeCard', {})
    if not dh_card:
        return None

    return CardData(
        position=dh_card.get('Position', []),
        load=dh_card.get('Load', [])
    )
