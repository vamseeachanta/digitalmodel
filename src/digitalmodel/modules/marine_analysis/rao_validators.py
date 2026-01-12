"""RAO Data Validation Suite.

ABOUTME: Comprehensive RAO data validation including displacement RAO quality checks.
ABOUTME: Validates frequency range, heading coverage, phase angles, and peak periods.

This module provides comprehensive validation for RAO data quality and physical reasonableness,
including frequency range checking, heading coverage validation, and physical limit verification.

Extended with displacement RAO quality checks for:
- Long period phase angle validation (Orcina convention: phase lag from wave crest)
- Peak period detection and natural frequency validation
- Vessel type auto-detection from RAO characteristics
- Active DOF determination by heading direction
"""

from typing import List, Optional, Dict, Any, Tuple
from enum import Enum
import numpy as np
from dataclasses import dataclass, field
from pathlib import Path
from datetime import datetime


@dataclass
class ValidationReport:
    """Container for validation results and suggestions."""
    is_valid: bool = True
    errors: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    suggestions: List[str] = field(default_factory=list)
    
    def add_error(self, message: str) -> None:
        """Add an error and mark report as invalid."""
        self.errors.append(message)
        self.is_valid = False
    
    def add_warning(self, message: str) -> None:
        """Add a warning without affecting validity."""
        self.warnings.append(message)
    
    def add_suggestion(self, message: str) -> None:
        """Add a suggestion for improvement."""
        self.suggestions.append(message)

    def merge(self, other: 'ValidationReport') -> None:
        """Merge another validation report into this one."""
        self.errors.extend(other.errors)
        self.warnings.extend(other.warnings)
        self.suggestions.extend(other.suggestions)
        if not other.is_valid:
            self.is_valid = False


class VesselType(Enum):
    """Vessel type classification for RAO quality checks."""
    SHIP = "ship"
    FPSO = "fpso"
    SEMI_SUBMERSIBLE = "semi_submersible"
    SPAR = "spar"
    BARGE = "barge"
    UNKNOWN = "unknown"


@dataclass
class VesselTypeCharacteristics:
    """Natural period ranges and thresholds for vessel types."""
    vessel_type: VesselType
    heave_tn_range: Tuple[float, float]  # seconds
    pitch_tn_range: Tuple[float, float]  # seconds
    roll_tn_range: Tuple[float, float]   # seconds
    long_period_threshold: float         # seconds - periods beyond this are "long"
    description: str


# Vessel type characteristics database
VESSEL_CHARACTERISTICS: Dict[VesselType, VesselTypeCharacteristics] = {
    VesselType.SHIP: VesselTypeCharacteristics(
        vessel_type=VesselType.SHIP,
        heave_tn_range=(8.0, 12.0),
        pitch_tn_range=(5.0, 8.0),
        roll_tn_range=(10.0, 25.0),
        long_period_threshold=20.0,
        description="Conventional ship hull form"
    ),
    VesselType.FPSO: VesselTypeCharacteristics(
        vessel_type=VesselType.FPSO,
        heave_tn_range=(9.0, 12.0),
        pitch_tn_range=(6.0, 8.0),
        roll_tn_range=(10.0, 20.0),
        long_period_threshold=20.0,
        description="Floating Production Storage and Offloading"
    ),
    VesselType.SEMI_SUBMERSIBLE: VesselTypeCharacteristics(
        vessel_type=VesselType.SEMI_SUBMERSIBLE,
        heave_tn_range=(17.0, 25.0),
        pitch_tn_range=(17.0, 20.0),
        roll_tn_range=(17.0, 24.0),
        long_period_threshold=30.0,
        description="Semi-submersible platform"
    ),
    VesselType.SPAR: VesselTypeCharacteristics(
        vessel_type=VesselType.SPAR,
        heave_tn_range=(20.0, 30.0),
        pitch_tn_range=(45.0, 60.0),
        roll_tn_range=(45.0, 60.0),
        long_period_threshold=35.0,
        description="SPAR platform with deep draft"
    ),
    VesselType.BARGE: VesselTypeCharacteristics(
        vessel_type=VesselType.BARGE,
        heave_tn_range=(8.0, 12.0),
        pitch_tn_range=(5.0, 10.0),
        roll_tn_range=(8.0, 15.0),
        long_period_threshold=20.0,
        description="Flat-bottomed barge"
    ),
}


@dataclass
class LongPeriodExpectation:
    """Expected RAO values at long period for a specific DOF and heading."""
    dof: str
    heading: float  # degrees
    expected_amplitude: float  # m/m or deg/m (normalized to wave slope for rotations)
    expected_phase: float  # degrees (Orcina convention: phase lag from wave crest)
    is_active: bool  # Whether this DOF should be active at this heading
    tolerance_amplitude: float = 0.05  # 5% tolerance
    tolerance_phase: float = 10.0  # +/-10 degrees


# Long period expectations by heading (Orcina convention)
# Phase convention: phase lag from wave crest until maximum positive excursion
LONG_PERIOD_EXPECTATIONS: Dict[str, Dict[float, LongPeriodExpectation]] = {
    # Head seas (180 deg): Surge, Heave, Pitch active
    'surge': {
        180.0: LongPeriodExpectation('surge', 180.0, 1.0, -90.0, True),
        0.0: LongPeriodExpectation('surge', 0.0, 1.0, 90.0, True),
        90.0: LongPeriodExpectation('surge', 90.0, 0.0, 0.0, False),
        270.0: LongPeriodExpectation('surge', 270.0, 0.0, 0.0, False),
    },
    'sway': {
        180.0: LongPeriodExpectation('sway', 180.0, 0.0, 0.0, False),
        0.0: LongPeriodExpectation('sway', 0.0, 0.0, 0.0, False),
        90.0: LongPeriodExpectation('sway', 90.0, 1.0, 90.0, True),
        270.0: LongPeriodExpectation('sway', 270.0, 1.0, -90.0, True),
    },
    'heave': {
        180.0: LongPeriodExpectation('heave', 180.0, 1.0, 0.0, True),
        0.0: LongPeriodExpectation('heave', 0.0, 1.0, 0.0, True),
        90.0: LongPeriodExpectation('heave', 90.0, 1.0, 0.0, True),
        270.0: LongPeriodExpectation('heave', 270.0, 1.0, 0.0, True),
    },
    'roll': {
        180.0: LongPeriodExpectation('roll', 180.0, 0.0, 0.0, False),
        0.0: LongPeriodExpectation('roll', 0.0, 0.0, 0.0, False),
        90.0: LongPeriodExpectation('roll', 90.0, 1.0, 90.0, True),
        270.0: LongPeriodExpectation('roll', 270.0, 1.0, -90.0, True),
    },
    'pitch': {
        180.0: LongPeriodExpectation('pitch', 180.0, 1.0, 90.0, True),
        0.0: LongPeriodExpectation('pitch', 0.0, 1.0, -90.0, True),
        90.0: LongPeriodExpectation('pitch', 90.0, 0.0, 0.0, False),
        270.0: LongPeriodExpectation('pitch', 270.0, 0.0, 0.0, False),
    },
    'yaw': {
        180.0: LongPeriodExpectation('yaw', 180.0, 0.0, 0.0, False),
        0.0: LongPeriodExpectation('yaw', 0.0, 0.0, 0.0, False),
        90.0: LongPeriodExpectation('yaw', 90.0, 0.0, 0.0, False),
        270.0: LongPeriodExpectation('yaw', 270.0, 0.0, 0.0, False),
    },
}


@dataclass
class PhaseCheckResult:
    """Result of a phase angle check."""
    dof: str
    heading: float
    period: float
    actual_amplitude: float
    actual_phase: float
    expected_amplitude: float
    expected_phase: float
    amplitude_error: float  # percentage
    phase_error: float  # degrees
    status: str  # 'PASS', 'WARNING', 'FAIL'
    message: str


@dataclass
class PeakDetectionResult:
    """Result of peak period detection for a DOF."""
    dof: str
    heading: float
    peak_period: float  # seconds
    peak_amplitude: float
    peak_frequency: float  # rad/s
    is_within_expected_range: bool
    expected_range: Tuple[float, float]
    amplification_factor: float  # peak amplitude / long period amplitude
    status: str  # 'PASS', 'WARNING', 'FAIL'
    message: str


@dataclass
class DisplacementRAOQualityReport:
    """Comprehensive quality report for displacement RAOs."""
    timestamp: datetime = field(default_factory=datetime.now)
    source_file: str = ""
    vessel_type: VesselType = VesselType.UNKNOWN
    vessel_type_confidence: float = 0.0
    long_period_threshold: float = 20.0

    # Check results
    phase_checks: List[PhaseCheckResult] = field(default_factory=list)
    peak_checks: List[PeakDetectionResult] = field(default_factory=list)

    # Summary statistics
    total_checks: int = 0
    passed_checks: int = 0
    warning_checks: int = 0
    failed_checks: int = 0

    # Validation report (for errors/warnings/suggestions)
    validation: ValidationReport = field(default_factory=ValidationReport)

    @property
    def overall_status(self) -> str:
        """Get overall quality status."""
        if self.failed_checks > 0:
            return "FAIL"
        elif self.warning_checks > 0:
            return "WARNING"
        elif self.passed_checks == self.total_checks and self.total_checks > 0:
            return "PASS"
        return "UNKNOWN"

    @property
    def pass_rate(self) -> float:
        """Calculate pass rate percentage."""
        if self.total_checks == 0:
            return 0.0
        return (self.passed_checks / self.total_checks) * 100


class RAODataValidators:
    """Validation suite for RAO data quality and physical reasonableness."""
    
    def __init__(self, config: Optional[dict] = None):
        """Initialize validators with configuration."""
        self.config = config or {}
        
        # Default validation limits
        self.frequency_limits = {
            'min_required': 0.1,    # rad/s
            'max_required': 2.0,    # rad/s
            'resolution_warning': 0.1  # rad/s
        }
        
        self.heading_limits = {
            'min_required': 0,      # degrees
            'max_required': 360,    # degrees (exclusive)
            'increment_warning': 30   # degrees
        }
        
        self.physical_limits = {
            'surge': {'max': 5.0, 'unit': 'm/m'},
            'sway': {'max': 5.0, 'unit': 'm/m'},
            'heave': {'max': 2.0, 'unit': 'm/m'},
            'roll': {'max': 10.0, 'unit': 'deg/m'},
            'pitch': {'max': 10.0, 'unit': 'deg/m'},
            'yaw': {'max': 10.0, 'unit': 'deg/m'}
        }
        
        # Update with user config if provided
        if 'validation' in self.config:
            val_config = self.config['validation']
            if 'frequency_range' in val_config:
                self.frequency_limits.update(val_config['frequency_range'])
            if 'heading_coverage' in val_config:
                self.heading_limits.update(val_config['heading_coverage'])
            if 'physical_limits' in val_config:
                for dof, limit in val_config['physical_limits'].items():
                    if dof in self.physical_limits:
                        self.physical_limits[dof]['max'] = limit
    
    def validate_rao_data(self, rao_data) -> ValidationReport:
        """Comprehensive RAO data validation."""
        report = ValidationReport()
        
        # Check data structure
        if not hasattr(rao_data, 'frequencies') or not hasattr(rao_data, 'headings'):
            report.add_error("RAO data missing required attributes (frequencies, headings)")
            return report
        
        # Check frequency range and resolution
        self._validate_frequency_range(rao_data.frequencies, report)
        
        # Check heading coverage
        self._validate_heading_coverage(rao_data.headings, report)
        
        # Check physical reasonableness
        self._validate_rao_magnitudes(rao_data, report)
        
        # Check data completeness
        self._validate_data_completeness(rao_data, report)
        
        # Add general suggestions if there were issues
        if report.errors or report.warnings:
            report.add_suggestion("Review the data import settings and source file")
            if report.errors:
                report.add_suggestion("Check that the RAO data file is complete and uncorrupted")
        
        return report
    
    def validate_experimental_data(self, experimental_data) -> ValidationReport:
        """Validate experimental RAO data with specific checks."""
        # First do general validation
        report = self.validate_rao_data(experimental_data)
        
        # Add experimental-specific checks
        self._validate_experimental_specifics(experimental_data, report)
        
        return report
    
    def _validate_frequency_range(self, frequencies: np.ndarray, report: ValidationReport) -> None:
        """Validate frequency range covers typical wave conditions."""
        if len(frequencies) == 0:
            report.add_error("No frequency data found")
            return
        
        min_freq, max_freq = frequencies.min(), frequencies.max()
        
        # Check minimum frequency
        if min_freq > self.frequency_limits['min_required']:
            report.add_warning(
                f"Minimum frequency {min_freq:.3f} rad/s may miss low-frequency waves. "
                f"Recommended minimum: {self.frequency_limits['min_required']} rad/s"
            )
            report.add_suggestion("Consider extending frequency range to lower values for complete analysis")
        
        # Check maximum frequency
        if max_freq < self.frequency_limits['max_required']:
            report.add_warning(
                f"Maximum frequency {max_freq:.3f} rad/s may miss high-frequency waves. "
                f"Recommended maximum: {self.frequency_limits['max_required']} rad/s"
            )
            report.add_suggestion("Consider extending frequency range to higher values")
        
        # Check resolution
        if len(frequencies) > 1:
            freq_resolution = np.mean(np.diff(frequencies))
            if freq_resolution > self.frequency_limits['resolution_warning']:
                report.add_warning(
                    f"Frequency resolution {freq_resolution:.3f} rad/s may be too coarse. "
                    f"Recommended maximum spacing: {self.frequency_limits['resolution_warning']} rad/s"
                )
                report.add_suggestion("Consider using finer frequency resolution for better accuracy")
            
            # Check for non-uniform spacing
            freq_diffs = np.diff(frequencies)
            if np.std(freq_diffs) > 0.01 * np.mean(freq_diffs):
                report.add_warning("Non-uniform frequency spacing detected")
    
    def _validate_heading_coverage(self, headings: np.ndarray, report: ValidationReport) -> None:
        """Validate heading coverage for complete directional analysis."""
        if len(headings) == 0:
            report.add_error("No heading data found")
            return
        
        min_heading, max_heading = headings.min(), headings.max()
        
        # Check coverage
        if min_heading > self.heading_limits['min_required'] + 1:  # Allow small tolerance
            report.add_error(f"Heading coverage should start at {self.heading_limits['min_required']}°")
        
        # Check if we have full circle coverage
        if max_heading < self.heading_limits['max_required'] - 15:  # Allow for 345° as max
            report.add_warning(
                f"Heading coverage up to {max_heading:.1f}° may be incomplete. "
                f"Full coverage to {self.heading_limits['max_required'] - 15}° recommended"
            )
        
        # Check resolution
        if len(headings) > 1:
            heading_increment = np.mean(np.diff(headings))
            if heading_increment > self.heading_limits['increment_warning']:
                report.add_warning(
                    f"Heading increment {heading_increment:.1f}° may be too coarse. "
                    f"Recommended maximum: {self.heading_limits['increment_warning']}°"
                )
                report.add_suggestion("Consider using finer heading resolution")
            
            # Check for common heading set (0, 15, 30, ..., 345)
            expected_headings = np.arange(0, 360, 15)
            if len(headings) == len(expected_headings):
                if not np.allclose(headings, expected_headings, atol=0.1):
                    report.add_warning("Non-standard heading values detected")
    
    def _validate_rao_magnitudes(self, rao_data, report: ValidationReport) -> None:
        """Check physical reasonableness of RAO magnitudes."""
        required_dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        for dof in required_dofs:
            if dof not in rao_data.raos:
                report.add_error(f"Missing RAO data for {dof}")
                continue
            
            # Check amplitude values
            amplitude = rao_data.raos[dof].get('amplitude', None)
            if amplitude is None:
                report.add_error(f"Missing amplitude data for {dof}")
                continue
            
            # Check for NaN or infinite values
            if np.any(np.isnan(amplitude)) or np.any(np.isinf(amplitude)):
                report.add_error(f"Invalid values (NaN or Inf) found in {dof} amplitude data")
                continue
            
            # Check physical limits
            max_amplitude = np.max(amplitude)
            if max_amplitude > self.physical_limits[dof]['max']:
                report.add_warning(
                    f"Maximum {dof} RAO amplitude {max_amplitude:.3f} {self.physical_limits[dof]['unit']} "
                    f"exceeds typical limit of {self.physical_limits[dof]['max']} {self.physical_limits[dof]['unit']}"
                )
                report.add_suggestion(f"Verify {dof} RAO values are reasonable for the vessel type")
            
            # Check for negative amplitudes and auto-fix them
            negative_count = np.sum(amplitude < 0)
            if negative_count > 0:
                # Auto-fix by taking absolute value (amplitudes should always be positive)
                rao_data.raos[dof]['amplitude'] = np.abs(amplitude)
                report.add_warning(
                    f"Fixed {negative_count} negative amplitude values in {dof} RAO data "
                    f"by taking absolute value (amplitudes must be non-negative)"
                )
                report.add_suggestion(
                    f"Review the source data parsing for {dof} - negative amplitudes "
                    "may indicate parsing errors in the input file"
                )
            
            # Check for extremely large amplitude values that suggest parsing errors
            amplitude = rao_data.raos[dof]['amplitude']  # Get updated amplitude after abs fix
            extreme_threshold = 1000.0  # Any amplitude > 1000 is likely a parsing error
            extreme_count = np.sum(amplitude > extreme_threshold)
            if extreme_count > 0:
                max_extreme = np.max(amplitude)
                # Cap extreme values at physical limits
                max_allowed = self.physical_limits[dof]['max'] * 10  # Allow 10x physical limit
                amplitude_capped = np.where(amplitude > max_allowed, max_allowed, amplitude)
                rao_data.raos[dof]['amplitude'] = amplitude_capped
                report.add_warning(
                    f"Fixed {extreme_count} extremely large amplitude values in {dof} RAO data "
                    f"(max was {max_extreme:.2e}, capped at {max_allowed:.1f})"
                )
                report.add_suggestion(
                    f"Review the source data parsing for {dof} - extremely large amplitudes "
                    "strongly indicate parsing errors in the input file"
                )
            
            # Check phase values
            phase = rao_data.raos[dof].get('phase', None)
            if phase is None:
                report.add_error(f"Missing phase data for {dof}")
                continue
            
            # Phase should be in degrees, typically -180 to 180 or 0 to 360
            if np.any(phase < -360) or np.any(phase > 360):
                report.add_warning(f"Phase values for {dof} outside expected range [-360, 360] degrees")
    
    def _validate_data_completeness(self, rao_data, report: ValidationReport) -> None:
        """Check data completeness across all frequencies and headings."""
        n_freq = len(rao_data.frequencies)
        n_head = len(rao_data.headings)
        
        required_dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        for dof in required_dofs:
            if dof in rao_data.raos:
                amplitude = rao_data.raos[dof].get('amplitude', None)
                phase = rao_data.raos[dof].get('phase', None)
                
                if amplitude is not None:
                    # Check shape
                    if amplitude.shape != (n_freq, n_head):
                        report.add_error(
                            f"{dof} amplitude array shape {amplitude.shape} doesn't match "
                            f"expected ({n_freq}, {n_head})"
                        )
                    
                    # Check for missing data (zeros might indicate missing)
                    zero_count = np.sum(amplitude == 0)
                    if zero_count > 0.1 * amplitude.size:  # More than 10% zeros
                        report.add_warning(
                            f"{dof} has {zero_count} zero values ({100*zero_count/amplitude.size:.1f}%), "
                            "which may indicate missing data"
                        )
                
                if phase is not None:
                    # Check shape
                    if phase.shape != (n_freq, n_head):
                        report.add_error(
                            f"{dof} phase array shape {phase.shape} doesn't match "
                            f"expected ({n_freq}, {n_head})"
                        )
    
    def _validate_experimental_specifics(self, experimental_data, report: ValidationReport) -> None:
        """Additional validation specific to experimental data."""
        # Check for measurement uncertainty info
        if not hasattr(experimental_data, 'metadata') or 'uncertainty' not in experimental_data.metadata:
            report.add_warning("Experimental data missing uncertainty information")
            report.add_suggestion("Consider adding measurement uncertainty estimates")
        
        # Check for test conditions
        if hasattr(experimental_data, 'metadata'):
            metadata = experimental_data.metadata
            recommended_metadata = ['test_date', 'facility', 'model_scale', 'water_depth']
            missing_metadata = [key for key in recommended_metadata if key not in metadata]
            
            if missing_metadata:
                report.add_warning(
                    f"Experimental data missing metadata: {', '.join(missing_metadata)}"
                )
                report.add_suggestion("Complete metadata for full traceability")
        
        # Check for repeatability data
        # Could add more experimental-specific checks here

    # =========================================================================
    # Displacement RAO Quality Check Methods
    # =========================================================================

    def validate_displacement_rao_quality(
        self,
        rao_data: Dict[str, Any],
        source_file: str = "",
        vessel_type: Optional[VesselType] = None,
        amplitude_tolerance: float = 0.05,
        phase_tolerance: float = 10.0
    ) -> DisplacementRAOQualityReport:
        """Validate displacement RAO quality including phase angles and peak periods.

        Args:
            rao_data: RAO data dictionary with periods, headings, and RAO values.
                      Expected structure (OrcaFlex format):
                      {
                          'VesselTypes': [{
                              'Draughts': [{
                                  'DisplacementRAOs': {
                                      'RAOs': [{'Period': [...], 'Amplitude': [...], 'Phase': [...], 'Direction': float}, ...]
                                  }
                              }]
                          }]
                      }
            source_file: Path to source file for reporting.
            vessel_type: Optional vessel type override. If None, auto-detect.
            amplitude_tolerance: Tolerance for amplitude checks (fraction, default 5%).
            phase_tolerance: Tolerance for phase checks (degrees, default 10°).

        Returns:
            DisplacementRAOQualityReport with all check results.
        """
        report = DisplacementRAOQualityReport(
            source_file=source_file,
            timestamp=datetime.now()
        )

        # Extract RAO data from OrcaFlex structure
        try:
            extracted = self._extract_orcaflex_raos(rao_data)
            if extracted is None:
                report.validation.add_error("Could not extract RAO data from input structure")
                return report

            periods, headings, rao_arrays = extracted
        except Exception as e:
            report.validation.add_error(f"Error extracting RAO data: {str(e)}")
            return report

        # Auto-detect vessel type if not specified
        if vessel_type is None:
            vessel_type, confidence = self._detect_vessel_type(periods, rao_arrays)
            report.vessel_type = vessel_type
            report.vessel_type_confidence = confidence
        else:
            report.vessel_type = vessel_type
            report.vessel_type_confidence = 1.0

        # Get vessel characteristics
        characteristics = VESSEL_CHARACTERISTICS.get(
            vessel_type,
            VESSEL_CHARACTERISTICS[VesselType.SHIP]
        )
        report.long_period_threshold = characteristics.long_period_threshold

        # Validate long period phase angles
        phase_results = self._validate_long_period_phases(
            periods, headings, rao_arrays, characteristics,
            amplitude_tolerance, phase_tolerance
        )
        report.phase_checks.extend(phase_results)

        # Detect and validate peak periods
        peak_results = self._detect_and_validate_peaks(
            periods, headings, rao_arrays, characteristics
        )
        report.peak_checks.extend(peak_results)

        # Calculate summary statistics
        all_results = phase_results + peak_results
        report.total_checks = len(all_results)
        report.passed_checks = sum(1 for r in all_results if r.status == 'PASS')
        report.warning_checks = sum(1 for r in all_results if r.status == 'WARNING')
        report.failed_checks = sum(1 for r in all_results if r.status == 'FAIL')

        return report

    def _extract_orcaflex_raos(
        self,
        rao_data: Dict[str, Any]
    ) -> Optional[Tuple[np.ndarray, np.ndarray, Dict[str, Dict[float, Tuple[np.ndarray, np.ndarray]]]]]:
        """Extract RAO arrays from OrcaFlex YAML structure.

        Supports two OrcaFlex formats:
        1. Standard format: RAODirection, RAOPeriodOrFrequency, RAOSurgeAmp, RAOSurgePhase, ...
           where data rows are: [period, surge_amp, surge_phase, sway_amp, ...]
        2. Alternative format: Direction, Period, Amplitude (nested), Phase (nested)

        Returns:
            Tuple of (periods, headings, rao_arrays) where rao_arrays is:
            {dof: {heading: (amplitude_array, phase_array)}}
        """
        try:
            # Navigate to DisplacementRAOs
            vessel_types = rao_data.get('VesselTypes', [])
            if not vessel_types:
                return None

            draughts = vessel_types[0].get('Draughts', [])
            if not draughts:
                return None

            disp_raos = draughts[0].get('DisplacementRAOs', {})
            raos_list = disp_raos.get('RAOs', [])
            if not raos_list:
                return None

            # DOF mapping (OrcaFlex order)
            dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

            # Build RAO arrays by DOF and heading
            rao_arrays: Dict[str, Dict[float, Tuple[np.ndarray, np.ndarray]]] = {
                dof: {} for dof in dof_names
            }

            # Detect format by checking first RAO entry keys
            first_rao = raos_list[0]

            # Check for standard OrcaFlex format (combined column header)
            combined_key = None
            for key in first_rao.keys():
                if 'RAOPeriodOrFrequency' in key and 'RAOSurgeAmp' in key:
                    combined_key = key
                    break

            if combined_key:
                # Standard OrcaFlex format: combined column header
                # Rows are: [period, surge_amp, surge_phase, sway_amp, sway_phase, ...]
                all_headings = set()
                periods_set = set()

                for rao_entry in raos_list:
                    direction = float(rao_entry.get('RAODirection', 0.0))
                    all_headings.add(direction)
                    data_rows = rao_entry.get(combined_key, [])

                    if not data_rows:
                        continue

                    # Extract periods and RAO values
                    periods_list = []
                    dof_data = {dof: {'amp': [], 'phase': []} for dof in dof_names}

                    for row in data_rows:
                        if len(row) >= 13:  # period + 6 DOFs * 2 (amp, phase)
                            periods_list.append(row[0])
                            periods_set.add(row[0])
                            # Order: surge_amp, surge_phase, sway_amp, sway_phase, heave_amp, heave_phase, ...
                            for i, dof in enumerate(dof_names):
                                amp_idx = 1 + i * 2
                                phase_idx = 2 + i * 2
                                dof_data[dof]['amp'].append(row[amp_idx])
                                dof_data[dof]['phase'].append(row[phase_idx])

                    # Store for this heading
                    for dof in dof_names:
                        if dof_data[dof]['amp']:
                            rao_arrays[dof][direction] = (
                                np.array(dof_data[dof]['amp'], dtype=float),
                                np.array(dof_data[dof]['phase'], dtype=float)
                            )

                periods = np.array(sorted(periods_set), dtype=float)
                headings = np.array(sorted(all_headings), dtype=float)

            else:
                # Alternative format: Direction, Period, Amplitude (nested), Phase (nested)
                periods = np.array(first_rao.get('Period', []), dtype=float)
                headings = np.array(sorted(set(r.get('Direction', 0.0) for r in raos_list)), dtype=float)

                for rao_entry in raos_list:
                    direction = float(rao_entry.get('Direction', 0.0))
                    amplitude_data = rao_entry.get('Amplitude', [])
                    phase_data = rao_entry.get('Phase', [])

                    # Handle nested structure (6 DOFs x N periods)
                    if amplitude_data and isinstance(amplitude_data[0], list):
                        for i, dof in enumerate(dof_names):
                            if i < len(amplitude_data):
                                amp = np.array(amplitude_data[i], dtype=float)
                                phs = np.array(phase_data[i], dtype=float) if i < len(phase_data) else np.zeros_like(amp)
                                rao_arrays[dof][direction] = (amp, phs)
                    else:
                        # Single DOF format (assume heave for backward compatibility)
                        amp = np.array(amplitude_data, dtype=float)
                        phs = np.array(phase_data, dtype=float) if phase_data else np.zeros_like(amp)
                        rao_arrays['heave'][direction] = (amp, phs)

            return periods, headings, rao_arrays

        except Exception as e:
            return None

    def _detect_vessel_type(
        self,
        periods: np.ndarray,
        rao_arrays: Dict[str, Dict[float, Tuple[np.ndarray, np.ndarray]]]
    ) -> Tuple[VesselType, float]:
        """Auto-detect vessel type from RAO peak characteristics.

        Returns:
            Tuple of (VesselType, confidence) where confidence is 0-1.
        """
        # Find heave peak period (most distinctive characteristic)
        heave_data = rao_arrays.get('heave', {})
        if not heave_data:
            return VesselType.UNKNOWN, 0.0

        # Find peak across all headings
        max_amplitude = 0.0
        peak_period = 0.0

        for heading, (amp, phs) in heave_data.items():
            if len(amp) > 0:
                idx = np.argmax(amp)
                if amp[idx] > max_amplitude:
                    max_amplitude = amp[idx]
                    peak_period = periods[idx] if idx < len(periods) else 0.0

        if peak_period == 0.0:
            return VesselType.UNKNOWN, 0.0

        # Match to vessel type based on heave natural period
        best_match = VesselType.UNKNOWN
        best_confidence = 0.0

        for vessel_type, chars in VESSEL_CHARACTERISTICS.items():
            tn_min, tn_max = chars.heave_tn_range
            if tn_min <= peak_period <= tn_max:
                # Perfect match
                confidence = 1.0
                if confidence > best_confidence:
                    best_confidence = confidence
                    best_match = vessel_type
            elif peak_period < tn_min:
                # Below range - calculate proximity
                confidence = max(0, 1 - (tn_min - peak_period) / tn_min)
                if confidence > best_confidence:
                    best_confidence = confidence
                    best_match = vessel_type
            elif peak_period > tn_max:
                # Above range - calculate proximity
                confidence = max(0, 1 - (peak_period - tn_max) / tn_max)
                if confidence > best_confidence:
                    best_confidence = confidence
                    best_match = vessel_type

        return best_match, best_confidence

    def _validate_long_period_phases(
        self,
        periods: np.ndarray,
        headings: np.ndarray,
        rao_arrays: Dict[str, Dict[float, Tuple[np.ndarray, np.ndarray]]],
        characteristics: VesselTypeCharacteristics,
        amplitude_tolerance: float,
        phase_tolerance: float
    ) -> List[PhaseCheckResult]:
        """Validate phase angles at long periods against expected values.

        Returns:
            List of PhaseCheckResult for each DOF/heading combination.
        """
        results = []
        long_period_threshold = characteristics.long_period_threshold

        # Find indices of long period data
        long_period_mask = periods >= long_period_threshold
        if not np.any(long_period_mask):
            # No long period data available - use longest available period
            longest_idx = np.argmax(periods)
            long_period_mask = np.zeros_like(periods, dtype=bool)
            long_period_mask[longest_idx] = True

        long_period_indices = np.where(long_period_mask)[0]

        for dof, dof_data in rao_arrays.items():
            if dof not in LONG_PERIOD_EXPECTATIONS:
                continue

            dof_expectations = LONG_PERIOD_EXPECTATIONS[dof]

            for heading in headings:
                if heading not in dof_data:
                    continue

                amp_array, phase_array = dof_data[heading]

                # Find nearest cardinal heading for expectations
                nearest_cardinal = self._find_nearest_heading(heading, list(dof_expectations.keys()))
                if nearest_cardinal is None:
                    continue

                expectation = dof_expectations[nearest_cardinal]

                # Get values at longest available period
                idx = long_period_indices[-1]  # Use longest period
                if idx >= len(amp_array):
                    continue

                actual_amplitude = amp_array[idx]
                actual_phase = self._normalize_phase(phase_array[idx])

                expected_amplitude = expectation.expected_amplitude
                expected_phase = expectation.expected_phase

                # Calculate errors
                if expected_amplitude > 0:
                    amplitude_error = abs(actual_amplitude - expected_amplitude) / expected_amplitude * 100
                else:
                    amplitude_error = actual_amplitude * 100  # Error relative to expected 0

                phase_error = abs(self._normalize_phase(actual_phase - expected_phase))

                # Determine status
                if expectation.is_active:
                    if amplitude_error <= amplitude_tolerance * 100 and phase_error <= phase_tolerance:
                        status = 'PASS'
                        message = f"Long period {dof} at {heading}° within tolerance"
                    elif amplitude_error <= amplitude_tolerance * 200 or phase_error <= phase_tolerance * 2:
                        status = 'WARNING'
                        message = f"Long period {dof} at {heading}° marginal (amp err: {amplitude_error:.1f}%, phase err: {phase_error:.1f}°)"
                    else:
                        status = 'FAIL'
                        message = f"Long period {dof} at {heading}° out of tolerance (amp err: {amplitude_error:.1f}%, phase err: {phase_error:.1f}°)"
                else:
                    # Inactive DOF - should have zero amplitude
                    if actual_amplitude < 0.1:
                        status = 'PASS'
                        message = f"Inactive {dof} at {heading}° correctly near zero"
                    elif actual_amplitude < 0.3:
                        status = 'WARNING'
                        message = f"Inactive {dof} at {heading}° has small residual amplitude {actual_amplitude:.3f}"
                    else:
                        status = 'FAIL'
                        message = f"Inactive {dof} at {heading}° has unexpected amplitude {actual_amplitude:.3f}"

                results.append(PhaseCheckResult(
                    dof=dof,
                    heading=heading,
                    period=periods[idx],
                    actual_amplitude=actual_amplitude,
                    actual_phase=actual_phase,
                    expected_amplitude=expected_amplitude,
                    expected_phase=expected_phase,
                    amplitude_error=amplitude_error,
                    phase_error=phase_error,
                    status=status,
                    message=message
                ))

        return results

    def _detect_and_validate_peaks(
        self,
        periods: np.ndarray,
        headings: np.ndarray,
        rao_arrays: Dict[str, Dict[float, Tuple[np.ndarray, np.ndarray]]],
        characteristics: VesselTypeCharacteristics
    ) -> List[PeakDetectionResult]:
        """Detect RAO peaks and validate against expected natural period ranges.

        Returns:
            List of PeakDetectionResult for each DOF/heading with detectable peaks.
        """
        results = []

        # Map DOF to expected range attribute
        dof_range_map = {
            'heave': characteristics.heave_tn_range,
            'pitch': characteristics.pitch_tn_range,
            'roll': characteristics.roll_tn_range,
        }

        for dof, dof_data in rao_arrays.items():
            expected_range = dof_range_map.get(dof)
            if expected_range is None:
                # Skip DOFs without defined natural period ranges (surge, sway, yaw)
                continue

            for heading in headings:
                if heading not in dof_data:
                    continue

                amp_array, phase_array = dof_data[heading]

                if len(amp_array) < 3:
                    continue

                # Find peaks using simple local maximum detection
                peak_indices = self._find_peaks(amp_array)

                if not peak_indices:
                    continue

                # Get the dominant peak (highest amplitude)
                dominant_idx = max(peak_indices, key=lambda i: amp_array[i])
                peak_period = periods[dominant_idx]
                peak_amplitude = amp_array[dominant_idx]

                # Calculate frequency
                peak_frequency = 2 * np.pi / peak_period if peak_period > 0 else 0.0

                # Get long period amplitude for amplification factor
                long_period_amp = amp_array[-1] if len(amp_array) > 0 else 1.0
                amplification_factor = peak_amplitude / long_period_amp if long_period_amp > 0 else float('inf')

                # Check if within expected range
                is_within = expected_range[0] <= peak_period <= expected_range[1]

                # Determine status
                if is_within:
                    status = 'PASS'
                    message = f"{dof.capitalize()} peak at {peak_period:.1f}s within expected range [{expected_range[0]:.1f}-{expected_range[1]:.1f}]s"
                else:
                    distance = min(
                        abs(peak_period - expected_range[0]),
                        abs(peak_period - expected_range[1])
                    )
                    if distance < 2.0:  # Within 2s of expected range
                        status = 'WARNING'
                        message = f"{dof.capitalize()} peak at {peak_period:.1f}s near expected range [{expected_range[0]:.1f}-{expected_range[1]:.1f}]s"
                    else:
                        status = 'FAIL'
                        message = f"{dof.capitalize()} peak at {peak_period:.1f}s outside expected range [{expected_range[0]:.1f}-{expected_range[1]:.1f}]s"

                results.append(PeakDetectionResult(
                    dof=dof,
                    heading=heading,
                    peak_period=peak_period,
                    peak_amplitude=peak_amplitude,
                    peak_frequency=peak_frequency,
                    is_within_expected_range=is_within,
                    expected_range=expected_range,
                    amplification_factor=amplification_factor,
                    status=status,
                    message=message
                ))

        return results

    @staticmethod
    def get_active_dofs_for_heading(heading: float) -> List[str]:
        """Determine which DOFs should be active for a given wave heading.

        Args:
            heading: Wave heading in degrees (0 = following, 90 = beam, 180 = head).

        Returns:
            List of DOF names that should be active at this heading.
        """
        # Normalize heading to 0-360
        heading = heading % 360

        # Define active DOFs by heading sector
        if heading < 22.5 or heading >= 337.5:
            # Following seas (0°): Surge, Heave, Pitch
            return ['surge', 'heave', 'pitch']
        elif 22.5 <= heading < 67.5 or 292.5 <= heading < 337.5:
            # Quartering following: All DOFs potentially active
            return ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        elif 67.5 <= heading < 112.5 or 247.5 <= heading < 292.5:
            # Beam seas (90°, 270°): Sway, Heave, Roll
            return ['sway', 'heave', 'roll']
        elif 112.5 <= heading < 157.5 or 202.5 <= heading < 247.5:
            # Quartering head: All DOFs potentially active
            return ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        elif 157.5 <= heading < 202.5:
            # Head seas (180°): Surge, Heave, Pitch
            return ['surge', 'heave', 'pitch']
        else:
            return ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']

    def _find_nearest_heading(
        self,
        heading: float,
        cardinal_headings: List[float]
    ) -> Optional[float]:
        """Find the nearest cardinal heading for expectations lookup."""
        if not cardinal_headings:
            return None

        # Normalize input heading
        heading = heading % 360

        min_diff = float('inf')
        nearest = None

        for cardinal in cardinal_headings:
            diff = abs(heading - cardinal)
            # Handle wraparound
            diff = min(diff, 360 - diff)
            if diff < min_diff:
                min_diff = diff
                nearest = cardinal

        return nearest

    @staticmethod
    def _normalize_phase(phase: float) -> float:
        """Normalize phase angle to -180 to 180 degrees."""
        phase = phase % 360
        if phase > 180:
            phase -= 360
        return phase

    @staticmethod
    def _find_peaks(arr: np.ndarray) -> List[int]:
        """Simple peak detection for 1D array.

        Returns indices of local maxima.
        """
        peaks = []
        for i in range(1, len(arr) - 1):
            if arr[i] > arr[i-1] and arr[i] > arr[i+1]:
                peaks.append(i)
        return peaks