"""RAO Data Validation Suite.

This module provides comprehensive validation for RAO data quality and physical reasonableness,
including frequency range checking, heading coverage validation, and physical limit verification.
"""

from typing import List, Optional
import numpy as np
from dataclasses import dataclass, field


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
            
            # Check for negative amplitudes
            if np.any(amplitude < 0):
                report.add_error(f"Negative amplitude values found in {dof} RAO data")
            
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