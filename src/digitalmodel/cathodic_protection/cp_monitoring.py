"""CP monitoring system design and specification.

Covers reference electrode selection, data logger specification, remote
monitoring system architecture, and alarm threshold setting for cathodic
protection monitoring systems.

References
----------
- NACE TM0497 (2018) "Measurement Techniques Related to Criteria for
  Cathodic Protection"
- DNV-RP-B401 (2017) "Cathodic Protection Design" §12
- ISO 13174 (2012) "Cathodic Protection of Port Structures"
- NACE SP0169 (2013) §10 — Monitoring requirements
"""

from __future__ import annotations

from enum import Enum
from typing import Optional

from pydantic import BaseModel, Field


class ReferenceElectrodeType(str, Enum):
    """Reference electrode types for CP monitoring."""

    AG_AGCL = "ag_agcl"  # Silver/Silver Chloride
    CU_CUSO4 = "cu_cuso4"  # Copper/Copper Sulfate
    ZN = "zn"  # Zinc
    MMO = "mmo"  # Mixed Metal Oxide
    SHE = "she"  # Standard Hydrogen Electrode


class MonitoringEnvironment(str, Enum):
    """Installation environment for monitoring equipment."""

    ONSHORE_BURIED = "onshore_buried"
    OFFSHORE_SUBMERGED = "offshore_submerged"
    OFFSHORE_SPLASH = "offshore_splash"
    MARINE_PORT = "marine_port"
    FRESHWATER = "freshwater"


# Reference electrode specifications
REFERENCE_ELECTRODE_SPECS: dict[ReferenceElectrodeType, dict] = {
    ReferenceElectrodeType.AG_AGCL: {
        "potential_vs_she_mV": 222,
        "stability_mV_yr": 2,
        "typical_life_years": 5,
        "environments": ["seawater", "brackish"],
        "temperature_range_c": (-2, 60),
    },
    ReferenceElectrodeType.CU_CUSO4: {
        "potential_vs_she_mV": 316,
        "stability_mV_yr": 5,
        "typical_life_years": 3,
        "environments": ["soil", "freshwater"],
        "temperature_range_c": (-10, 50),
    },
    ReferenceElectrodeType.ZN: {
        "potential_vs_she_mV": -763,
        "stability_mV_yr": 10,
        "typical_life_years": 15,
        "environments": ["seawater", "soil"],
        "temperature_range_c": (-2, 40),
    },
    ReferenceElectrodeType.MMO: {
        "potential_vs_she_mV": 222,
        "stability_mV_yr": 1,
        "typical_life_years": 20,
        "environments": ["seawater", "soil", "freshwater", "brackish"],
        "temperature_range_c": (-10, 80),
    },
}

# Potential conversion offsets [mV] from row to column
# Usage: V_target = V_source + CONVERSION[source][target]
POTENTIAL_CONVERSIONS: dict[str, dict[str, float]] = {
    "ag_agcl": {"cu_cuso4": -94, "zn": 985, "she": 222},
    "cu_cuso4": {"ag_agcl": 94, "zn": 1079, "she": 316},
    "zn": {"ag_agcl": -985, "cu_cuso4": -1079, "she": -763},
}


class ReferenceElectrodeRecommendation(BaseModel):
    """Recommendation for reference electrode selection."""

    electrode_type: str = Field(..., description="Recommended electrode type")
    typical_life_years: float = Field(
        ..., description="Typical service life [years]"
    )
    stability_mV_yr: float = Field(
        ..., description="Typical drift [mV/year]"
    )
    replacement_interval_years: float = Field(
        ..., description="Recommended replacement interval [years]"
    )
    notes: str = Field(default="", description="Selection notes")


class MonitoringSystemSpec(BaseModel):
    """Specification for a CP monitoring system."""

    number_of_channels: int = Field(
        ..., description="Number of measurement channels"
    )
    measurement_range_V: tuple[float, float] = Field(
        ..., description="Voltage measurement range [V]"
    )
    resolution_mV: float = Field(
        ..., description="Measurement resolution [mV]"
    )
    logging_interval_hours: float = Field(
        ..., description="Data logging interval [hours]"
    )
    data_storage_days: int = Field(
        ..., description="On-board data storage capacity [days]"
    )
    communication_type: str = Field(
        ..., description="Communication protocol"
    )
    alarm_thresholds: dict[str, float] = Field(
        default_factory=dict,
        description="Alarm threshold settings [V]",
    )


class AlarmThresholds(BaseModel):
    """CP monitoring alarm threshold settings."""

    underprotection_V: float = Field(
        ..., description="Underprotection alarm [V vs ref]"
    )
    overprotection_V: float = Field(
        ..., description="Overprotection alarm [V vs ref]"
    )
    rapid_change_mV_hr: float = Field(
        ..., description="Rapid potential change alarm [mV/hr]"
    )
    sensor_fault_V: float = Field(
        default=0.0,
        description="Sensor fault detection threshold [V]",
    )


def select_reference_electrode(
    environment: MonitoringEnvironment,
    design_life_years: float = 25.0,
    temperature_c: float = 20.0,
) -> ReferenceElectrodeRecommendation:
    """Select appropriate reference electrode for CP monitoring.

    Recommends the most suitable reference electrode based on the
    installation environment, temperature, and design life.

    Parameters
    ----------
    environment : MonitoringEnvironment
        Installation environment.
    design_life_years : float
        Monitoring system design life [years].
    temperature_c : float
        Expected operating temperature [°C].

    Returns
    -------
    ReferenceElectrodeRecommendation
        Recommended electrode type and specifications.
    """
    # Environment-to-electrode mapping
    env_mapping: dict[MonitoringEnvironment, list[ReferenceElectrodeType]] = {
        MonitoringEnvironment.ONSHORE_BURIED: [
            ReferenceElectrodeType.CU_CUSO4,
            ReferenceElectrodeType.MMO,
            ReferenceElectrodeType.ZN,
        ],
        MonitoringEnvironment.OFFSHORE_SUBMERGED: [
            ReferenceElectrodeType.AG_AGCL,
            ReferenceElectrodeType.ZN,
            ReferenceElectrodeType.MMO,
        ],
        MonitoringEnvironment.OFFSHORE_SPLASH: [
            ReferenceElectrodeType.ZN,
            ReferenceElectrodeType.MMO,
        ],
        MonitoringEnvironment.MARINE_PORT: [
            ReferenceElectrodeType.AG_AGCL,
            ReferenceElectrodeType.ZN,
            ReferenceElectrodeType.MMO,
        ],
        MonitoringEnvironment.FRESHWATER: [
            ReferenceElectrodeType.CU_CUSO4,
            ReferenceElectrodeType.MMO,
        ],
    }

    candidates = env_mapping.get(
        environment,
        [ReferenceElectrodeType.MMO],
    )

    # Score candidates by life and stability
    best = None
    best_score = -1.0

    for electrode in candidates:
        spec = REFERENCE_ELECTRODE_SPECS.get(electrode)
        if spec is None:
            continue

        temp_min, temp_max = spec["temperature_range_c"]
        if temperature_c < temp_min or temperature_c > temp_max:
            continue

        # Score: prefer longer life and better stability
        life_score = spec["typical_life_years"] / design_life_years
        stability_score = 1.0 / (1.0 + spec["stability_mV_yr"])
        score = life_score * 0.6 + stability_score * 0.4

        if score > best_score:
            best_score = score
            best = electrode

    if best is None:
        # Fallback to MMO (most versatile)
        best = ReferenceElectrodeType.MMO

    spec = REFERENCE_ELECTRODE_SPECS[best]
    typical_life = spec["typical_life_years"]

    # Replacement interval: whichever is shorter of electrode life or
    # when cumulative drift exceeds 10 mV
    drift_limit_years = 10.0 / spec["stability_mV_yr"] if spec["stability_mV_yr"] > 0 else typical_life
    replacement_interval = min(typical_life, drift_limit_years)

    notes = f"Suitable for {environment.value}. "
    if typical_life < design_life_years:
        n_replacements = int(design_life_years / replacement_interval)
        notes += f"Plan for {n_replacements} replacements over design life."
    else:
        notes += "Single installation may cover entire design life."

    return ReferenceElectrodeRecommendation(
        electrode_type=best.value,
        typical_life_years=typical_life,
        stability_mV_yr=spec["stability_mV_yr"],
        replacement_interval_years=round(replacement_interval, 1),
        notes=notes,
    )


def design_monitoring_system(
    number_of_structures: int,
    monitoring_points_per_structure: int = 4,
    environment: MonitoringEnvironment = MonitoringEnvironment.OFFSHORE_SUBMERGED,
    logging_interval_hours: float = 4.0,
    remote_access: bool = True,
) -> MonitoringSystemSpec:
    """Design a CP monitoring system specification.

    Parameters
    ----------
    number_of_structures : int
        Number of structures to monitor.
    monitoring_points_per_structure : int
        Number of monitoring points per structure (default 4).
    environment : MonitoringEnvironment
        Installation environment.
    logging_interval_hours : float
        Data logging interval [hours].
    remote_access : bool
        Whether remote telemetry access is required.

    Returns
    -------
    MonitoringSystemSpec
        System specification.
    """
    total_channels = number_of_structures * monitoring_points_per_structure

    # Communication type based on environment
    if remote_access:
        if environment in (
            MonitoringEnvironment.OFFSHORE_SUBMERGED,
            MonitoringEnvironment.OFFSHORE_SPLASH,
        ):
            comm_type = "satellite_or_radio"
        else:
            comm_type = "cellular_4g"
    else:
        comm_type = "local_download"

    # Measurement range based on environment
    if environment in (
        MonitoringEnvironment.OFFSHORE_SUBMERGED,
        MonitoringEnvironment.MARINE_PORT,
    ):
        measurement_range = (-2.5, 0.5)  # V vs Ag/AgCl
    else:
        measurement_range = (-3.0, 0.5)  # V vs CSE

    # Storage: enough for 6 months minimum
    storage_days = max(180, int(365 * 1.0))

    return MonitoringSystemSpec(
        number_of_channels=total_channels,
        measurement_range_V=measurement_range,
        resolution_mV=0.1,
        logging_interval_hours=logging_interval_hours,
        data_storage_days=storage_days,
        communication_type=comm_type,
        alarm_thresholds={},
    )


def set_alarm_thresholds(
    reference_electrode: ReferenceElectrodeType = ReferenceElectrodeType.AG_AGCL,
    environment: MonitoringEnvironment = MonitoringEnvironment.OFFSHORE_SUBMERGED,
) -> AlarmThresholds:
    """Set CP monitoring alarm thresholds per applicable standards.

    Parameters
    ----------
    reference_electrode : ReferenceElectrodeType
        Reference electrode type in use.
    environment : MonitoringEnvironment
        Installation environment.

    Returns
    -------
    AlarmThresholds
        Alarm threshold values.
    """
    if reference_electrode == ReferenceElectrodeType.CU_CUSO4:
        # NACE SP0169 criteria vs CSE
        underprotection = -0.850
        overprotection = -1.200
    elif reference_electrode == ReferenceElectrodeType.AG_AGCL:
        # DNV-RP-B401 criteria vs Ag/AgCl
        underprotection = -0.800
        overprotection = -1.100
    elif reference_electrode == ReferenceElectrodeType.ZN:
        # Relative to zinc: protection ~+0.25 V
        underprotection = 0.250
        overprotection = -0.050
    else:
        # Default to Ag/AgCl equivalent
        underprotection = -0.800
        overprotection = -1.100

    # Rapid change alarm: 50 mV/hr suggests interference or equipment issue
    rapid_change = 50.0

    return AlarmThresholds(
        underprotection_V=underprotection,
        overprotection_V=overprotection,
        rapid_change_mV_hr=rapid_change,
        sensor_fault_V=0.0,
    )
