"""
Private sensor template factory helpers.

Not part of the public API — imported only by structural_health.py.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from .structural_health import SensorTemplate

from .structural_health import (
    AssetType,
    MeasurementType,
    SensorConfig,
    SensorTemplate,
)


def fixed_platform_template() -> SensorTemplate:
    """Default sensor suite for a fixed jacket platform."""
    sensors = [
        # Accelerometers — deck corners
        SensorConfig(
            sensor_id="ACC_DECK_NE",
            sensor_type=MeasurementType.ACCELERATION,
            location_description="Main deck NE corner",
            sampling_rate_hz=100.0,
            warning_threshold=0.15,
            critical_threshold=0.35,
        ),
        SensorConfig(
            sensor_id="ACC_DECK_SW",
            sensor_type=MeasurementType.ACCELERATION,
            location_description="Main deck SW corner",
            sampling_rate_hz=100.0,
            warning_threshold=0.15,
            critical_threshold=0.35,
        ),
        # Strain gauges — jacket nodes
        SensorConfig(
            sensor_id="STR_NODE_A",
            sensor_type=MeasurementType.STRAIN,
            location_description="Jacket K-node at -15m elevation",
            sampling_rate_hz=50.0,
            warning_threshold=150.0,
            critical_threshold=280.0,
        ),
        SensorConfig(
            sensor_id="STR_NODE_B",
            sensor_type=MeasurementType.STRAIN,
            location_description="Jacket K-node at -30m elevation",
            sampling_rate_hz=50.0,
            warning_threshold=150.0,
            critical_threshold=280.0,
        ),
        # Cathodic protection
        SensorConfig(
            sensor_id="CP_MUD_N",
            sensor_type=MeasurementType.CATHODIC_PROTECTION,
            location_description="Mudline north face anode monitoring",
            sampling_rate_hz=0.001,
            warning_threshold=-0.80,
            critical_threshold=-0.70,
        ),
        SensorConfig(
            sensor_id="CP_MID_S",
            sensor_type=MeasurementType.CATHODIC_PROTECTION,
            location_description="Mid-water south face anode monitoring",
            sampling_rate_hz=0.001,
            warning_threshold=-0.80,
            critical_threshold=-0.70,
        ),
    ]
    return SensorTemplate(asset_type=AssetType.FIXED_PLATFORM, sensors=sensors)


def floating_structure_template() -> SensorTemplate:
    """Default sensor suite for a floating offshore structure."""
    sensors = [
        # Motion reference unit
        SensorConfig(
            sensor_id="MRU_SURGE",
            sensor_type=MeasurementType.DISPLACEMENT,
            location_description="MRU — surge displacement at keel",
            sampling_rate_hz=20.0,
            warning_threshold=8.0,
            critical_threshold=15.0,
        ),
        SensorConfig(
            sensor_id="MRU_HEAVE",
            sensor_type=MeasurementType.DISPLACEMENT,
            location_description="MRU — heave displacement at keel",
            sampling_rate_hz=20.0,
            warning_threshold=5.0,
            critical_threshold=10.0,
        ),
        # Riser tension sensors
        SensorConfig(
            sensor_id="RISER_TEN_01",
            sensor_type=MeasurementType.TENSION,
            location_description="Production riser top tension — riser 1",
            sampling_rate_hz=10.0,
            warning_threshold=1_800.0,
            critical_threshold=2_200.0,
        ),
        SensorConfig(
            sensor_id="RISER_TEN_02",
            sensor_type=MeasurementType.TENSION,
            location_description="Production riser top tension — riser 2",
            sampling_rate_hz=10.0,
            warning_threshold=1_800.0,
            critical_threshold=2_200.0,
        ),
        # Mooring load cells
        SensorConfig(
            sensor_id="MOOR_LINE_01",
            sensor_type=MeasurementType.TENSION,
            location_description="Mooring line 1 load cell at fairlead",
            sampling_rate_hz=5.0,
            warning_threshold=2_500.0,
            critical_threshold=3_500.0,
        ),
        SensorConfig(
            sensor_id="MOOR_LINE_04",
            sensor_type=MeasurementType.TENSION,
            location_description="Mooring line 4 load cell at fairlead",
            sampling_rate_hz=5.0,
            warning_threshold=2_500.0,
            critical_threshold=3_500.0,
        ),
        SensorConfig(
            sensor_id="MOOR_LINE_07",
            sensor_type=MeasurementType.TENSION,
            location_description="Mooring line 7 load cell at fairlead",
            sampling_rate_hz=5.0,
            warning_threshold=2_500.0,
            critical_threshold=3_500.0,
        ),
    ]
    return SensorTemplate(
        asset_type=AssetType.FLOATING_STRUCTURE, sensors=sensors
    )


def offshore_wind_template() -> SensorTemplate:
    """Default sensor suite for an offshore wind turbine."""
    sensors = [
        # Blade root strain
        SensorConfig(
            sensor_id="BLD1_ROOT_FLAPWISE",
            sensor_type=MeasurementType.STRAIN,
            location_description="Blade 1 root — flapwise bending strain",
            sampling_rate_hz=50.0,
            warning_threshold=800.0,
            critical_threshold=1_400.0,
        ),
        SensorConfig(
            sensor_id="BLD1_ROOT_EDGEWISE",
            sensor_type=MeasurementType.STRAIN,
            location_description="Blade 1 root — edgewise bending strain",
            sampling_rate_hz=50.0,
            warning_threshold=600.0,
            critical_threshold=1_100.0,
        ),
        # Tower acceleration
        SensorConfig(
            sensor_id="TWR_ACC_MID",
            sensor_type=MeasurementType.ACCELERATION,
            location_description="Tower mid-height fore-aft acceleration",
            sampling_rate_hz=100.0,
            warning_threshold=0.10,
            critical_threshold=0.22,
        ),
        # Foundation tilt
        SensorConfig(
            sensor_id="FND_TILT_X",
            sensor_type=MeasurementType.TILT,
            location_description="Foundation tilt — X axis (fore-aft)",
            sampling_rate_hz=1.0,
            warning_threshold=0.25,
            critical_threshold=0.50,
        ),
        SensorConfig(
            sensor_id="FND_TILT_Y",
            sensor_type=MeasurementType.TILT,
            location_description="Foundation tilt — Y axis (side-side)",
            sampling_rate_hz=1.0,
            warning_threshold=0.25,
            critical_threshold=0.50,
        ),
    ]
    return SensorTemplate(
        asset_type=AssetType.OFFSHORE_WIND, sensors=sensors
    )
