"""Tests for CP monitoring system design."""

import pytest

from digitalmodel.cathodic_protection.cp_monitoring import (
    MonitoringEnvironment,
    ReferenceElectrodeType,
    design_monitoring_system,
    select_reference_electrode,
    set_alarm_thresholds,
)


def test_select_electrode_offshore():
    """Offshore submerged should recommend Ag/AgCl or Zn."""
    result = select_reference_electrode(
        environment=MonitoringEnvironment.OFFSHORE_SUBMERGED,
        design_life_years=25.0,
    )
    assert result.electrode_type in ("ag_agcl", "zn", "mmo")
    assert result.typical_life_years > 0
    assert result.stability_mV_yr > 0
    assert result.replacement_interval_years > 0


def test_select_electrode_onshore():
    """Onshore buried should recommend Cu/CuSO4 or MMO."""
    result = select_reference_electrode(
        environment=MonitoringEnvironment.ONSHORE_BURIED,
    )
    assert result.electrode_type in ("cu_cuso4", "mmo", "zn")


def test_select_electrode_high_temperature():
    """High temperature should filter out electrodes with limited range."""
    result = select_reference_electrode(
        environment=MonitoringEnvironment.OFFSHORE_SUBMERGED,
        temperature_c=70.0,  # Above most electrode ranges
    )
    # MMO handles up to 80°C
    assert result.electrode_type == "mmo"


def test_monitoring_system_offshore():
    """Offshore platform monitoring system spec."""
    spec = design_monitoring_system(
        number_of_structures=3,
        monitoring_points_per_structure=6,
        environment=MonitoringEnvironment.OFFSHORE_SUBMERGED,
        remote_access=True,
    )
    assert spec.number_of_channels == 18  # 3 * 6
    assert spec.resolution_mV == 0.1
    assert spec.communication_type == "satellite_or_radio"
    assert spec.data_storage_days >= 180


def test_monitoring_system_onshore():
    """Onshore pipeline monitoring system."""
    spec = design_monitoring_system(
        number_of_structures=10,
        monitoring_points_per_structure=2,
        environment=MonitoringEnvironment.ONSHORE_BURIED,
        remote_access=True,
    )
    assert spec.number_of_channels == 20
    assert spec.communication_type == "cellular_4g"


def test_alarm_thresholds_agagcl():
    """Ag/AgCl alarm thresholds per DNV-RP-B401."""
    thresholds = set_alarm_thresholds(
        reference_electrode=ReferenceElectrodeType.AG_AGCL,
        environment=MonitoringEnvironment.OFFSHORE_SUBMERGED,
    )
    assert thresholds.underprotection_V == pytest.approx(-0.800, abs=0.001)
    assert thresholds.overprotection_V == pytest.approx(-1.100, abs=0.001)
    assert thresholds.rapid_change_mV_hr == pytest.approx(50.0)


def test_alarm_thresholds_cse():
    """Cu/CuSO4 alarm thresholds per NACE SP0169."""
    thresholds = set_alarm_thresholds(
        reference_electrode=ReferenceElectrodeType.CU_CUSO4,
    )
    assert thresholds.underprotection_V == pytest.approx(-0.850, abs=0.001)
    assert thresholds.overprotection_V == pytest.approx(-1.200, abs=0.001)
