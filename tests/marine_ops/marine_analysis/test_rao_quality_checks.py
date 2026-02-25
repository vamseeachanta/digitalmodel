"""Unit tests for Displacement RAO Quality Checks.

ABOUTME: Tests for long period phase angle validation and peak period detection.
ABOUTME: Validates vessel type auto-detection and quality report generation.
"""

import pytest
import numpy as np
from pathlib import Path
import tempfile
import yaml

from digitalmodel.marine_ops.marine_analysis.rao_validators import (
    RAODataValidators,
    ValidationReport,
    VesselType,
    VesselTypeCharacteristics,
    VESSEL_CHARACTERISTICS,
    LongPeriodExpectation,
    LONG_PERIOD_EXPECTATIONS,
    PhaseCheckResult,
    PeakDetectionResult,
    DisplacementRAOQualityReport,
)
from digitalmodel.marine_ops.marine_analysis.rao_quality_report import RAOQualityReportGenerator


def create_synthetic_orcaflex_rao_data(
    vessel_type: str = "ship",
    n_periods: int = 20,
    headings: list = None
) -> dict:
    """Create synthetic OrcaFlex-format RAO data for testing.

    Args:
        vessel_type: Type of vessel to simulate ('ship', 'semi', 'spar', 'barge')
        n_periods: Number of period points
        headings: List of headings (default: [0, 90, 180, 270])

    Returns:
        Dictionary in OrcaFlex YAML format
    """
    if headings is None:
        headings = [0.0, 90.0, 180.0, 270.0]

    # Set vessel-specific natural periods
    if vessel_type == "ship":
        heave_tn, pitch_tn, roll_tn = 10.0, 6.0, 15.0
    elif vessel_type == "semi":
        heave_tn, pitch_tn, roll_tn = 20.0, 18.0, 20.0
    elif vessel_type == "spar":
        heave_tn, pitch_tn, roll_tn = 25.0, 50.0, 50.0
    elif vessel_type == "barge":
        heave_tn, pitch_tn, roll_tn = 10.0, 7.0, 12.0
    else:
        heave_tn, pitch_tn, roll_tn = 10.0, 6.0, 15.0

    # Create periods from 3s to 40s
    periods = np.linspace(3, 40, n_periods)

    raos_list = []

    for heading in headings:
        # Determine active DOFs
        is_head_following = heading < 45 or heading > 315 or (135 < heading < 225)
        is_beam = (45 <= heading <= 135) or (225 <= heading <= 315)

        data_rows = []
        for period in periods:
            row = [period]  # First element is period

            # Generate RAO values for each DOF (amp, phase pairs)
            # SURGE
            if is_head_following:
                surge_amp = 1.0 + 0.5 * np.exp(-((period - pitch_tn) / 3) ** 2)
                surge_phase = -90.0 if heading > 90 else 90.0
                if period > 25:
                    surge_amp = 1.0
            else:
                surge_amp = 0.05
                surge_phase = 0.0
            row.extend([surge_amp, surge_phase])

            # SWAY
            if is_beam:
                sway_amp = 1.0 + 0.3 * np.exp(-((period - roll_tn) / 3) ** 2)
                sway_phase = 90.0 if heading < 180 else -90.0
                if period > 25:
                    sway_amp = 1.0
            else:
                sway_amp = 0.05
                sway_phase = 0.0
            row.extend([sway_amp, sway_phase])

            # HEAVE (always active)
            heave_amp = 1.0 + 0.7 * np.exp(-((period - heave_tn) / 2) ** 2)
            heave_phase = 0.0
            if period > 25:
                heave_amp = 1.0
            row.extend([heave_amp, heave_phase])

            # ROLL
            if is_beam:
                roll_amp = 1.0 + 3.0 * np.exp(-((period - roll_tn) / 2) ** 2)
                roll_phase = 90.0 if heading < 180 else -90.0
                if period > 25:
                    roll_amp = 1.0
            else:
                roll_amp = 0.02
                roll_phase = 0.0
            row.extend([roll_amp, roll_phase])

            # PITCH
            if is_head_following:
                pitch_amp = 1.0 + 1.0 * np.exp(-((period - pitch_tn) / 2) ** 2)
                pitch_phase = 90.0 if heading > 90 else -90.0
                if period > 25:
                    pitch_amp = 1.0
            else:
                pitch_amp = 0.02
                pitch_phase = 0.0
            row.extend([pitch_amp, pitch_phase])

            # YAW (typically small)
            row.extend([0.01, 0.0])

            data_rows.append(row)

        raos_list.append({
            'RAODirection': heading,
            'RAOPeriodOrFrequency, RAOSurgeAmp, RAOSurgePhase, RAOSwayAmp, RAOSwayPhase, RAOHeaveAmp, RAOHeavePhase, RAORollAmp, RAORollPhase, RAOPitchAmp, RAOPitchPhase, RAOYawAmp, RAOYawPhase': data_rows
        })

    return {
        'VesselTypes': [{
            'Name': f'Test {vessel_type.title()}',
            'Draughts': [{
                'Name': 'Draught1',
                'DisplacementRAOs': {
                    'RAOOrigin': [0, 0, 0],
                    'PhaseOrigin': [0, 0, 0],
                    'RAOs': raos_list
                }
            }]
        }]
    }


class TestVesselTypeDetection:
    """Tests for vessel type auto-detection."""

    def test_detect_ship_type(self):
        """Test detection of ship vessel type."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(
            rao_data, source_file="test_ship.yml"
        )

        assert report.vessel_type in [VesselType.SHIP, VesselType.FPSO, VesselType.BARGE]
        assert report.vessel_type_confidence > 0.3

    def test_detect_semi_type(self):
        """Test detection of semi-submersible vessel type."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="semi")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(
            rao_data, source_file="test_semi.yml"
        )

        assert report.vessel_type == VesselType.SEMI_SUBMERSIBLE
        assert report.vessel_type_confidence > 0.5

    def test_detect_spar_type(self):
        """Test detection of SPAR vessel type."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="spar")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(
            rao_data, source_file="test_spar.yml"
        )

        assert report.vessel_type in [VesselType.SPAR, VesselType.SEMI_SUBMERSIBLE]
        assert report.vessel_type_confidence > 0.3


class TestLongPeriodPhaseChecks:
    """Tests for long period phase angle validation."""

    def test_phase_checks_generated(self):
        """Test that phase checks are generated for all DOFs and headings."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(rao_data)

        assert len(report.phase_checks) > 0
        # Should have checks for multiple headings
        headings = set(c.heading for c in report.phase_checks)
        assert len(headings) >= 4

    def test_heave_phase_at_long_period(self):
        """Test heave phase approaches 0 degrees at long periods."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(rao_data)

        # Find heave checks
        heave_checks = [c for c in report.phase_checks if c.dof == 'heave']
        assert len(heave_checks) > 0

        # Heave at long periods should be near 0 degrees phase
        for check in heave_checks:
            if check.status == 'PASS':
                assert abs(check.actual_phase) < 30


class TestPeakDetection:
    """Tests for RAO peak period detection."""

    def test_peak_detection_generated(self):
        """Test that peak detection results are generated."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(rao_data)

        assert len(report.peak_checks) > 0

    def test_peak_period_positive(self):
        """Test that detected peak periods are positive."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(rao_data)

        for check in report.peak_checks:
            assert check.peak_period > 0
            assert check.peak_amplitude > 0


class TestComprehensiveQualityCheck:
    """Tests for comprehensive displacement RAO quality check."""

    def test_full_quality_check_ship(self):
        """Test full quality check for ship RAO data."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(
            rao_data, source_file="test_ship.yml"
        )

        assert isinstance(report, DisplacementRAOQualityReport)
        assert report.vessel_type in [VesselType.SHIP, VesselType.FPSO, VesselType.BARGE]
        assert report.total_checks > 0
        assert report.overall_status in ['PASS', 'WARNING', 'FAIL', 'UNKNOWN']
        assert 0 <= report.pass_rate <= 100

    def test_full_quality_check_semi(self):
        """Test full quality check for semi-submersible RAO data."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="semi")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(
            rao_data, source_file="test_semi.yml"
        )

        assert report.vessel_type == VesselType.SEMI_SUBMERSIBLE
        assert report.long_period_threshold == 30.0  # Semi threshold

    def test_quality_report_summary_statistics(self):
        """Test that summary statistics are calculated correctly."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(rao_data)

        # Check statistics consistency
        total = report.passed_checks + report.warning_checks + report.failed_checks
        assert total == report.total_checks

        # Check pass rate calculation
        if report.total_checks > 0:
            expected_rate = (report.passed_checks / report.total_checks) * 100
            assert report.pass_rate == pytest.approx(expected_rate, abs=0.1)


class TestActiveDOFDetermination:
    """Tests for active DOF determination by heading."""

    def test_head_seas_active_dofs(self):
        """Test active DOFs at head seas."""
        active = RAODataValidators.get_active_dofs_for_heading(180.0)

        assert 'surge' in active
        assert 'sway' not in active
        assert 'heave' in active
        assert 'roll' not in active
        assert 'pitch' in active
        assert 'yaw' not in active

    def test_beam_seas_active_dofs(self):
        """Test active DOFs at beam seas."""
        active = RAODataValidators.get_active_dofs_for_heading(90.0)

        assert 'surge' not in active
        assert 'sway' in active
        assert 'heave' in active
        assert 'roll' in active
        assert 'pitch' not in active
        assert 'yaw' not in active

    def test_quartering_seas_active_dofs(self):
        """Test all DOFs active at quartering seas."""
        active = RAODataValidators.get_active_dofs_for_heading(45.0)

        # All DOFs should be active at quartering seas
        assert len(active) == 6


class TestRAOQualityReportGenerator:
    """Tests for HTML report generation."""

    def test_html_report_generation(self):
        """Test that HTML report is generated correctly."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()
        report = validators.validate_displacement_rao_quality(rao_data)

        with tempfile.TemporaryDirectory() as tmpdir:
            generator = RAOQualityReportGenerator(output_dir=tmpdir)
            report_path = generator.generate_html_report(report, report_name="test_report")

            assert report_path.exists()
            assert report_path.suffix == '.html'

            # Check HTML content
            content = report_path.read_text()
            assert 'Displacement RAO Quality Report' in content
            assert 'Plotly' in content or 'plotly' in content

    def test_csv_export(self):
        """Test CSV export functionality."""
        rao_data = create_synthetic_orcaflex_rao_data(vessel_type="ship")
        validators = RAODataValidators()
        report = validators.validate_displacement_rao_quality(rao_data)

        with tempfile.TemporaryDirectory() as tmpdir:
            generator = RAOQualityReportGenerator(output_dir=tmpdir)
            csv_path = generator.export_csv_summary(report, report_name="test_summary")

            assert csv_path.exists()
            assert csv_path.suffix == '.csv'

            # Check CSV content
            content = csv_path.read_text()
            assert 'PHASE ANGLE CHECKS' in content
            assert 'PEAK PERIOD CHECKS' in content


class TestVesselCharacteristics:
    """Tests for vessel characteristics database."""

    def test_all_vessel_types_have_characteristics(self):
        """Test that all vessel types have defined characteristics."""
        for vessel_type in [VesselType.SHIP, VesselType.FPSO, VesselType.SEMI_SUBMERSIBLE,
                          VesselType.SPAR, VesselType.BARGE]:
            assert vessel_type in VESSEL_CHARACTERISTICS
            chars = VESSEL_CHARACTERISTICS[vessel_type]
            assert isinstance(chars, VesselTypeCharacteristics)
            assert chars.heave_tn_range[0] < chars.heave_tn_range[1]
            assert chars.pitch_tn_range[0] < chars.pitch_tn_range[1]
            assert chars.roll_tn_range[0] < chars.roll_tn_range[1]
            assert chars.long_period_threshold > 0

    def test_long_period_expectations_complete(self):
        """Test that long period expectations are defined for all DOFs."""
        dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        key_headings = [0.0, 90.0, 180.0, 270.0]

        for dof in dofs:
            assert dof in LONG_PERIOD_EXPECTATIONS
            for heading in key_headings:
                assert heading in LONG_PERIOD_EXPECTATIONS[dof]
                exp = LONG_PERIOD_EXPECTATIONS[dof][heading]
                assert isinstance(exp, LongPeriodExpectation)


class TestEdgeCases:
    """Tests for edge cases and error handling."""

    def test_empty_rao_data(self):
        """Test handling of empty RAO data."""
        rao_data = {}
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(rao_data)

        # Should return report with error
        assert report.total_checks == 0
        assert len(report.validation.errors) > 0

    def test_missing_vessel_types(self):
        """Test handling of missing VesselTypes."""
        rao_data = {'VesselTypes': []}
        validators = RAODataValidators()

        report = validators.validate_displacement_rao_quality(rao_data)

        assert report.total_checks == 0
        assert len(report.validation.errors) > 0

    def test_phase_normalization(self):
        """Test phase normalization utility."""
        assert RAODataValidators._normalize_phase(0.0) == 0.0
        assert RAODataValidators._normalize_phase(180.0) == 180.0
        assert RAODataValidators._normalize_phase(270.0) == -90.0
        # Note: -180 normalizes to 180 (equivalent angles)
        assert RAODataValidators._normalize_phase(-180.0) == 180.0
        assert RAODataValidators._normalize_phase(360.0) == 0.0
        assert RAODataValidators._normalize_phase(450.0) == 90.0


class TestIntegrationWithRealData:
    """Integration tests using real RAO data files from the repository."""

    @pytest.fixture
    def real_semi_rao_path(self):
        """Path to real semi-submersible RAO file."""
        path = Path(__file__).parent.parent.parent.parent / \
               "docs/domains/orcawave/examples/L02 OC4 Semi-sub/L02 OC4 Semi-sub_orcaflex.yml"
        if path.exists():
            return path
        pytest.skip("Real RAO data file not found")

    @pytest.fixture
    def real_ship_rao_path(self):
        """Path to real ship RAO file."""
        path = Path(__file__).parent.parent.parent.parent / \
               "docs/domains/orcawave/L01_aqwa_benchmark/orcawave_001_ship_raos_rev2_matched.yml"
        if path.exists():
            return path
        pytest.skip("Real ship RAO data file not found")

    def test_real_semi_rao_quality_check(self, real_semi_rao_path):
        """Test quality check on real semi-submersible RAO data."""
        with open(real_semi_rao_path, 'r', encoding='utf-8') as f:
            rao_data = yaml.safe_load(f)

        validators = RAODataValidators()
        report = validators.validate_displacement_rao_quality(
            rao_data, source_file=str(real_semi_rao_path)
        )

        # Should detect as semi-submersible or similar
        assert report.vessel_type in [VesselType.SEMI_SUBMERSIBLE, VesselType.SPAR]
        assert report.total_checks > 0

        # Generate HTML report
        with tempfile.TemporaryDirectory() as tmpdir:
            generator = RAOQualityReportGenerator(output_dir=tmpdir)
            report_path = generator.generate_html_report(
                report, report_name="OC4_semi_quality"
            )
            assert report_path.exists()

    def test_real_data_report_structure(self, real_semi_rao_path):
        """Test that quality report has correct structure with real data."""
        with open(real_semi_rao_path, 'r', encoding='utf-8') as f:
            rao_data = yaml.safe_load(f)

        validators = RAODataValidators()
        report = validators.validate_displacement_rao_quality(
            rao_data, source_file=str(real_semi_rao_path)
        )

        # Verify report structure
        assert isinstance(report.phase_checks, list)
        assert isinstance(report.peak_checks, list)
        assert report.timestamp is not None
        assert report.source_file == str(real_semi_rao_path)

        # All phase checks should have valid structure
        for check in report.phase_checks:
            assert isinstance(check, PhaseCheckResult)
            assert check.status in ['PASS', 'WARNING', 'FAIL']

        # All peak checks should have valid structure
        for check in report.peak_checks:
            assert isinstance(check, PeakDetectionResult)
            assert check.status in ['PASS', 'WARNING', 'FAIL']


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
