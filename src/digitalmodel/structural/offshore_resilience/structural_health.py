"""
Structural Health Monitoring — sensor configuration templates and alert framework.

Provides:
- StructuralHealthRecord data model (digital twin data integration)
- SensorTemplate for fixed platforms, floating structures, and offshore wind
- AlertThresholdConfig linked to fatigue budget (WRK-157 hook)

References
----------
DNV-ST-0126 — Support Structures for Wind Turbines.
API RP 2SIM — Structural Integrity Management.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import List, Optional


# ---------------------------------------------------------------------------
# Enumerations
# ---------------------------------------------------------------------------

class AlertSeverity(str, Enum):
    """Severity levels for structural health alerts."""
    INFO = "info"
    WARNING = "warning"
    CRITICAL = "critical"


class MeasurementType(str, Enum):
    """Physical quantity measured by the sensor."""
    ACCELERATION = "acceleration"
    STRAIN = "strain"
    DISPLACEMENT = "displacement"
    TENSION = "tension"
    TILT = "tilt"
    CATHODIC_PROTECTION = "cathodic_protection"


class AssetType(str, Enum):
    """Offshore asset class for sensor template selection."""
    FIXED_PLATFORM = "fixed_platform"
    FLOATING_STRUCTURE = "floating_structure"
    OFFSHORE_WIND = "offshore_wind"


# ---------------------------------------------------------------------------
# StructuralHealthRecord
# ---------------------------------------------------------------------------

@dataclass
class StructuralHealthRecord:
    """
    Single timestamped structural health measurement.

    Parameters
    ----------
    asset_id : str
        Unique asset identifier (e.g. "PLAT-001").
    measurement_type : MeasurementType
        Physical quantity measured.
    timestamp : datetime
        UTC timestamp of measurement (must be timezone-aware).
    value : float
        Measured value in engineering units appropriate to measurement_type.
    channel : str
        Sensor channel label (e.g. "ACC_DECK_NE").
    alert_triggered : bool
        True when the measurement exceeded a threshold.
    alert_severity : AlertSeverity | None
        Severity when alert_triggered is True; None otherwise.
    """

    asset_id: str
    measurement_type: MeasurementType
    timestamp: datetime
    value: float
    channel: str
    alert_triggered: bool
    alert_severity: Optional[AlertSeverity]

    def __post_init__(self) -> None:
        # Consistency: alert_triggered and alert_severity must agree
        if not self.alert_triggered and self.alert_severity is not None:
            raise ValueError(
                "alert_severity must be None when alert_triggered is False; "
                f"got alert_severity={self.alert_severity!r}"
            )
        if self.alert_triggered and self.alert_severity is None:
            raise ValueError(
                "alert_severity must not be None when alert_triggered is True"
            )


# ---------------------------------------------------------------------------
# SensorConfig
# ---------------------------------------------------------------------------

@dataclass
class SensorConfig:
    """
    Configuration for a single sensor channel.

    Parameters
    ----------
    sensor_id : str
        Unique sensor identifier.
    sensor_type : MeasurementType
        Physical quantity measured.
    location_description : str
        Human-readable installation location.
    sampling_rate_hz : float
        Data acquisition sampling rate (Hz).
    warning_threshold : float
        Value above which a WARNING alert is raised.
    critical_threshold : float
        Value above which a CRITICAL alert is raised; must exceed warning.
    """

    sensor_id: str
    sensor_type: MeasurementType
    location_description: str
    sampling_rate_hz: float
    warning_threshold: float
    critical_threshold: float

    def __post_init__(self) -> None:
        if self.critical_threshold <= self.warning_threshold:
            raise ValueError(
                "critical_threshold must exceed warning_threshold; "
                f"got warning={self.warning_threshold}, "
                f"critical={self.critical_threshold}"
            )


# ---------------------------------------------------------------------------
# Sensor template
# ---------------------------------------------------------------------------

@dataclass
class SensorTemplate:
    """Collection of sensor configurations for an asset type."""

    asset_type: AssetType
    sensors: List[SensorConfig] = field(default_factory=list)


def build_sensor_template(asset_type: AssetType) -> SensorTemplate:
    """
    Return a default sensor configuration template for the given asset type.

    Asset types and sensor suites
    ------------------------------
    FIXED_PLATFORM
        - Accelerometers at deck corners (2)
        - Strain gauges at critical jacket nodes (2)
        - Cathodic protection monitoring points (2)

    FLOATING_STRUCTURE
        - Motion reference unit / displacement (2)
        - Riser tension sensors (2)
        - Mooring line load cells (3)

    OFFSHORE_WIND
        - Blade root strain gauges (2)
        - Tower mid-height acceleration (1)
        - Foundation tilt sensors (2)

    Parameters
    ----------
    asset_type : AssetType

    Returns
    -------
    SensorTemplate
    """
    # Delegate to private helpers in _sensor_templates to keep file size
    # within the 400-line limit (coding-style.md).
    from . import _sensor_templates as _tmpl  # local import avoids circularity

    _builders = {
        AssetType.FIXED_PLATFORM: _tmpl.fixed_platform_template,
        AssetType.FLOATING_STRUCTURE: _tmpl.floating_structure_template,
        AssetType.OFFSHORE_WIND: _tmpl.offshore_wind_template,
    }
    if asset_type not in _builders:
        raise ValueError(f"Unsupported asset_type: {asset_type!r}")  # pragma: no cover
    return _builders[asset_type]()


# ---------------------------------------------------------------------------
# Alert threshold configuration
# ---------------------------------------------------------------------------

@dataclass
class AlertThresholdConfig:
    """
    Alert threshold configuration linked to fatigue utilisation budget.

    Attributes
    ----------
    fatigue_rate_warning : float
        Fatigue damage accumulation rate (fraction of budget/year) above
        which a WARNING is issued.
    fatigue_rate_critical : float
        Rate above which a CRITICAL alert fires.
    storm_response_warning_g : float | None
        Peak deck acceleration (g) triggering storm-event WARNING;
        None if not applicable.
    frequency_drift_warning_pct : float | None
        Structural natural frequency drift (%) triggering WARNING for
        potential corrosion or damage; None if not applicable.
    """

    fatigue_rate_warning: float
    fatigue_rate_critical: float
    storm_response_warning_g: Optional[float]
    frequency_drift_warning_pct: Optional[float]


def build_alert_thresholds(
    asset_type: AssetType,
    design_life_years: int,
    fatigue_utilisation_budget: float,
) -> AlertThresholdConfig:
    """
    Build alert thresholds scaled to the asset's fatigue utilisation budget.

    The fatigue rate warning is set so that an alert fires when the
    accumulated damage fraction exceeds the budget pace, giving operators
    early warning before the design budget is consumed.

    Parameters
    ----------
    asset_type : AssetType
    design_life_years : int
        Nominal design service life (years).
    fatigue_utilisation_budget : float
        Allowable fatigue utilisation fraction (0 < x <= 1.0).
        Example: 0.85 means 85 % of S-N damage budget may be consumed.

    Returns
    -------
    AlertThresholdConfig
    """
    # Annual budget pace (fraction per year)
    annual_pace = fatigue_utilisation_budget / design_life_years

    # Warning at 120 % of design pace; critical at 160 %
    warning_rate = annual_pace * 1.20
    critical_rate = annual_pace * 1.60

    # Storm response — asset-specific deck-level threshold
    storm_g: Optional[float]
    if asset_type in (AssetType.FIXED_PLATFORM, AssetType.FLOATING_STRUCTURE):
        storm_g = 0.20   # g — representative 100-yr storm trigger
    else:
        storm_g = 0.15   # offshore wind — lower nacelle acceleration limit

    # Frequency drift threshold — 3 % drop from as-installed value
    freq_drift_pct = 3.0

    return AlertThresholdConfig(
        fatigue_rate_warning=warning_rate,
        fatigue_rate_critical=critical_rate,
        storm_response_warning_g=storm_g,
        frequency_drift_warning_pct=freq_drift_pct,
    )
