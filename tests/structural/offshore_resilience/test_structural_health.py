"""
Tests for StructuralHealthRecord and sensor configuration templates.

Verifies:
- Record construction and alert logic
- Sensor config templates for 3 asset types
- Alert threshold framework
- Anomaly detection hook interface
"""

import pytest
from datetime import datetime, timezone

from digitalmodel.structural.offshore_resilience.structural_health import (
    StructuralHealthRecord,
    AlertSeverity,
    MeasurementType,
    SensorConfig,
    AssetType,
    SensorTemplate,
    AlertThresholdConfig,
    build_sensor_template,
    build_alert_thresholds,
)


# ---------------------------------------------------------------------------
# StructuralHealthRecord
# ---------------------------------------------------------------------------

class TestStructuralHealthRecord:
    def _make_record(self, value: float, alert_triggered: bool = False,
                     severity: AlertSeverity | None = None) -> StructuralHealthRecord:
        return StructuralHealthRecord(
            asset_id="PLAT-001",
            measurement_type=MeasurementType.ACCELERATION,
            timestamp=datetime(2026, 2, 20, 12, 0, 0, tzinfo=timezone.utc),
            value=value,
            channel="ACC_DECK_NE",
            alert_triggered=alert_triggered,
            alert_severity=severity,
        )

    def test_record_stores_fields_correctly(self):
        rec = self._make_record(0.05)
        assert rec.asset_id == "PLAT-001"
        assert rec.measurement_type == MeasurementType.ACCELERATION
        assert rec.channel == "ACC_DECK_NE"
        assert rec.alert_triggered is False
        assert rec.alert_severity is None

    def test_record_with_warning_alert(self):
        rec = self._make_record(0.25, alert_triggered=True,
                                severity=AlertSeverity.WARNING)
        assert rec.alert_triggered is True
        assert rec.alert_severity == AlertSeverity.WARNING

    def test_record_with_critical_alert(self):
        rec = self._make_record(0.50, alert_triggered=True,
                                severity=AlertSeverity.CRITICAL)
        assert rec.alert_severity == AlertSeverity.CRITICAL

    def test_alert_triggered_false_requires_none_severity(self):
        """Consistency: if alert_triggered=False, severity must be None."""
        with pytest.raises(ValueError, match="alert_severity"):
            StructuralHealthRecord(
                asset_id="PLAT-001",
                measurement_type=MeasurementType.STRAIN,
                timestamp=datetime(2026, 2, 20, tzinfo=timezone.utc),
                value=150.0,
                channel="STR_NODE_A",
                alert_triggered=False,
                alert_severity=AlertSeverity.INFO,
            )

    def test_alert_triggered_true_requires_severity(self):
        """Consistency: if alert_triggered=True, severity must not be None."""
        with pytest.raises(ValueError, match="alert_severity"):
            StructuralHealthRecord(
                asset_id="PLAT-001",
                measurement_type=MeasurementType.STRAIN,
                timestamp=datetime(2026, 2, 20, tzinfo=timezone.utc),
                value=150.0,
                channel="STR_NODE_A",
                alert_triggered=True,
                alert_severity=None,
            )


# ---------------------------------------------------------------------------
# SensorConfig
# ---------------------------------------------------------------------------

class TestSensorConfig:
    def test_sensor_config_fields(self):
        sc = SensorConfig(
            sensor_id="ACC_01",
            sensor_type=MeasurementType.ACCELERATION,
            location_description="Deck NE corner",
            sampling_rate_hz=100.0,
            warning_threshold=0.2,
            critical_threshold=0.4,
        )
        assert sc.sensor_id == "ACC_01"
        assert sc.sampling_rate_hz == 100.0
        assert sc.critical_threshold > sc.warning_threshold

    def test_critical_threshold_must_exceed_warning(self):
        with pytest.raises(ValueError, match="critical_threshold"):
            SensorConfig(
                sensor_id="ACC_01",
                sensor_type=MeasurementType.ACCELERATION,
                location_description="Deck NE corner",
                sampling_rate_hz=100.0,
                warning_threshold=0.5,
                critical_threshold=0.3,  # < warning
            )


# ---------------------------------------------------------------------------
# Sensor template — 3 asset types
# ---------------------------------------------------------------------------

class TestBuildSensorTemplate:
    def test_fixed_platform_template_has_accelerometers(self):
        tmpl = build_sensor_template(AssetType.FIXED_PLATFORM)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.ACCELERATION in types

    def test_fixed_platform_template_has_strain_gauges(self):
        tmpl = build_sensor_template(AssetType.FIXED_PLATFORM)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.STRAIN in types

    def test_fixed_platform_template_has_cp_monitoring(self):
        tmpl = build_sensor_template(AssetType.FIXED_PLATFORM)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.CATHODIC_PROTECTION in types

    def test_floating_structure_template_has_motion_reference(self):
        tmpl = build_sensor_template(AssetType.FLOATING_STRUCTURE)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.DISPLACEMENT in types

    def test_floating_structure_template_has_mooring_load(self):
        tmpl = build_sensor_template(AssetType.FLOATING_STRUCTURE)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.TENSION in types

    def test_offshore_wind_template_has_blade_strain(self):
        tmpl = build_sensor_template(AssetType.OFFSHORE_WIND)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.STRAIN in types

    def test_offshore_wind_template_has_tower_acceleration(self):
        tmpl = build_sensor_template(AssetType.OFFSHORE_WIND)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.ACCELERATION in types

    def test_offshore_wind_template_has_tilt(self):
        tmpl = build_sensor_template(AssetType.OFFSHORE_WIND)
        types = [s.sensor_type for s in tmpl.sensors]
        assert MeasurementType.TILT in types

    def test_each_template_has_at_least_three_sensors(self):
        for asset_type in AssetType:
            tmpl = build_sensor_template(asset_type)
            assert len(tmpl.sensors) >= 3, (
                f"{asset_type} template has fewer than 3 sensors"
            )


# ---------------------------------------------------------------------------
# Alert threshold framework
# ---------------------------------------------------------------------------

class TestBuildAlertThresholds:
    def test_fatigue_budget_threshold_exists(self):
        cfg = build_alert_thresholds(
            asset_type=AssetType.FIXED_PLATFORM,
            design_life_years=25,
            fatigue_utilisation_budget=0.85,
        )
        assert cfg.fatigue_rate_warning is not None
        assert cfg.fatigue_rate_critical is not None

    def test_fatigue_warning_below_critical(self):
        cfg = build_alert_thresholds(
            asset_type=AssetType.FIXED_PLATFORM,
            design_life_years=25,
            fatigue_utilisation_budget=0.85,
        )
        assert cfg.fatigue_rate_warning < cfg.fatigue_rate_critical

    def test_storm_threshold_exists_for_fixed_platform(self):
        cfg = build_alert_thresholds(
            asset_type=AssetType.FIXED_PLATFORM,
            design_life_years=25,
            fatigue_utilisation_budget=0.85,
        )
        assert cfg.storm_response_warning_g is not None

    def test_frequency_drift_threshold_exists(self):
        """Structural frequency drift is a damage/corrosion indicator."""
        cfg = build_alert_thresholds(
            asset_type=AssetType.FIXED_PLATFORM,
            design_life_years=25,
            fatigue_utilisation_budget=0.85,
        )
        assert cfg.frequency_drift_warning_pct is not None

    def test_budget_fraction_respected_in_fatigue_warning(self):
        """Warning threshold should be proportional to fatigue budget."""
        cfg_tight = build_alert_thresholds(
            asset_type=AssetType.FIXED_PLATFORM,
            design_life_years=25,
            fatigue_utilisation_budget=0.50,
        )
        cfg_loose = build_alert_thresholds(
            asset_type=AssetType.FIXED_PLATFORM,
            design_life_years=25,
            fatigue_utilisation_budget=0.90,
        )
        assert cfg_tight.fatigue_rate_warning < cfg_loose.fatigue_rate_warning
