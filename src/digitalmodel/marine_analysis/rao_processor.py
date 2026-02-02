"""RAO Data Processing Core Implementation.

This module provides the main RAO data processing functionality including:
- Multi-format data import (AQWA, OrcaFlex, experimental)
- Data validation and quality checking
- DataFrame conversion with manual verification support
- Interpolation and standardization
"""

from typing import Dict, List, Tuple, Any, Optional
from dataclasses import dataclass, field
import numpy as np
import pandas as pd
from pathlib import Path
import random
from datetime import datetime

from .aqwa_reader import AQWAReader
from .orcaflex_reader import OrcaFlexReader
from .rao_validators import RAODataValidators, ValidationReport
from .rao_interpolator import RAOInterpolator


@dataclass
class RAOData:
    """Container for RAO data with metadata."""
    frequencies: np.ndarray  # rad/s
    headings: np.ndarray     # degrees
    raos: Dict[str, Dict[str, np.ndarray]]  # {dof: {'amplitude': array, 'phase': array}}
    units: Dict[str, str] = field(default_factory=lambda: {
        'frequency': 'rad/s',
        'heading': 'deg',
        'surge': 'm/m',
        'sway': 'm/m',
        'heave': 'm/m',
        'roll': 'deg/m',
        'pitch': 'deg/m',
        'yaw': 'deg/m'
    })
    source_file: str = ""
    vessel_name: str = ""
    analysis_date: str = ""
    metadata: Dict[str, Any] = field(default_factory=dict)

    def is_valid(self) -> bool:
        """Check if RAO data is valid and complete."""
        required_dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        return all(dof in self.raos for dof in required_dofs)


@dataclass
class VerificationReport:
    """Manual verification report for RAO data."""
    amplitude_check_required: bool = False
    phase_continuity_check_required: bool = False
    spot_check_locations: List[Dict] = field(default_factory=list)
    symmetry_pairs: List[Tuple[str, str]] = field(default_factory=list)
    phase_discontinuities: List[Dict] = field(default_factory=list)
    suspicious_amplitudes: List[Dict] = field(default_factory=list)
    timestamp: datetime = field(default_factory=datetime.now)


class RAOImportError(Exception):
    """User-friendly RAO import error with suggested solutions."""
    
    def __init__(self, message: str, suggestions: List[str] = None):
        self.message = message
        self.suggestions = suggestions or []
        super().__init__(self.message)
    
    def user_message(self) -> str:
        """Format error message for user interface."""
        msg = f"RAO Import Error: {self.message}\n"
        if self.suggestions:
            msg += "\nSuggested solutions:\n"
            for i, suggestion in enumerate(self.suggestions, 1):
                msg += f"{i}. {suggestion}\n"
        return msg


class RAODataProcessor:
    """Process Response Amplitude Operator data from various sources."""
    
    def __init__(self, config: Optional[Dict[str, Any]] = None):
        """Initialize RAO processor with configuration."""
        self.config = config or {}
        self.validators = RAODataValidators()
        self.interpolator = RAOInterpolator()
        self.aqwa_reader = AQWAReader()
        self.orcaflex_reader = OrcaFlexReader()
    
    def import_aqwa_lis_file(self, file_path: str, use_enhanced_parser: bool = True) -> RAOData:
        """Import RAO data from ANSYS AQWA .lis file.
        
        Args:
            file_path: Path to ANSYS AQWA .lis output file
            use_enhanced_parser: If True, uses enhanced parser with data interpretation
            
        Returns:
            RAOData object with 6-DOF displacement RAOs
            
        Raises:
            RAOImportError: If file format is invalid or unsupported
        """
        try:
            # Parse AQWA output format with enhanced data interpretation
            raw_data = self.aqwa_reader.parse_lis_file(file_path, use_enhanced_parser=use_enhanced_parser)
            
            # Create RAOData object
            rao_data = RAOData(
                frequencies=raw_data['frequencies'],
                headings=raw_data['headings'],
                raos=raw_data['raos'],
                source_file=file_path,
                analysis_date=datetime.now().isoformat(),
                metadata={
                    'enhanced_parser_used': use_enhanced_parser,
                    'data_interpretation_applied': use_enhanced_parser
                }
            )
            
            # Validate data completeness and physical reasonableness
            validation_report = self.validators.validate_rao_data(rao_data)
            if not validation_report.is_valid:
                raise RAOImportError(
                    f"RAO validation failed: {', '.join(validation_report.errors)}",
                    validation_report.suggestions
                )
            
            return rao_data
            
        except Exception as e:
            suggestions = [
                "Verify the file is a valid ANSYS AQWA .lis output file",
                "Check that displacement RAO analysis was performed",
                "Ensure file is not corrupted and completely written",
                "Verify the file contains the last set of RAOs",
                "Try disabling enhanced parser if data format issues persist"
            ]
            raise RAOImportError(
                f"Could not parse ANSYS AQWA file: {str(e)}", 
                suggestions
            ) from e
    
    def import_orcaflex_yml_file(self, file_path: str) -> RAOData:
        """Import RAO data from OrcaFlex YAML file.
        
        Args:
            file_path: Path to OrcaFlex YAML file
            
        Returns:
            RAOData object with 6-DOF displacement RAOs
            
        Raises:
            RAOImportError: If file format is invalid or unsupported
        """
        try:
            # Parse OrcaFlex YAML format
            raw_data = self.orcaflex_reader.parse_yml_file(file_path)
            
            # Create RAOData object
            rao_data = RAOData(
                frequencies=raw_data['frequencies'],
                headings=raw_data['headings'],
                raos=raw_data['raos'],
                source_file=file_path,
                vessel_name=raw_data.get('vessel_name', ''),
                analysis_date=datetime.now().isoformat()
            )
            
            # Validate data completeness and physical reasonableness
            validation_report = self.validators.validate_rao_data(rao_data)
            if not validation_report.is_valid:
                raise RAOImportError(
                    f"RAO validation failed: {', '.join(validation_report.errors)}",
                    validation_report.suggestions
                )
            
            return rao_data
            
        except Exception as e:
            suggestions = [
                "Verify the file is a valid OrcaFlex YAML file",
                "Check the YAML structure follows: VesselTypes > Vessel Type1 > Draughts > Draught1 > DisplacementRAOs",
                "Ensure the file contains RAO data in the expected format",
                "Verify YAML syntax is correct"
            ]
            raise RAOImportError(
                f"Could not parse OrcaFlex file: {str(e)}", 
                suggestions
            ) from e
    
    def get_rao_dataframes(self, rao_data: RAOData) -> Dict[str, Any]:
        """Convert RAO data to pandas DataFrames.
        
        Returns:
            Dictionary with 'amplitude' and 'phase' DataFrames:
            - Each DataFrame has frequency/period as index
            - Columns are multi-level: (DOF, Heading)
            - DOF: surge, sway, heave, roll, pitch, yaw
            - Heading: all available headings (0-360 degrees)
            - Phase values in degrees
        """
        # Create amplitude and phase data dictionaries
        amplitude_data = {}
        phase_data = {}
        
        dof_names = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        
        for dof in dof_names:
            for heading_idx, heading in enumerate(rao_data.headings):
                # Create column name as (DOF, Heading)
                col_name = (dof, f"{heading:.1f}deg")
                
                # Extract amplitude and phase for this DOF and heading
                amplitude_data[col_name] = rao_data.raos[dof]['amplitude'][:, heading_idx]
                phase_data[col_name] = rao_data.raos[dof]['phase'][:, heading_idx]  # in degrees
        
        # Create DataFrames with frequency/period as index
        freq_index = pd.Index(rao_data.frequencies, name='Frequency (rad/s)')
        
        amplitude_df = pd.DataFrame(amplitude_data, index=freq_index)
        amplitude_df.columns = pd.MultiIndex.from_tuples(
            amplitude_df.columns, names=['DOF', 'Heading']
        )
        
        phase_df = pd.DataFrame(phase_data, index=freq_index)
        phase_df.columns = pd.MultiIndex.from_tuples(
            phase_df.columns, names=['DOF', 'Heading']
        )
        
        # Add manual verification metadata
        verification_info = {
            'generated_timestamp': pd.Timestamp.now(),
            'source_file': rao_data.source_file,
            'requires_manual_verification': True,
            'verification_checklist': [
                'Amplitude values physical reasonableness',
                'Phase continuity across frequencies',
                'Spot check against source values',
                'Symmetry verification for appropriate headings'
            ]
        }
        
        return {
            'amplitude': amplitude_df,
            'phase': phase_df,
            'verification_info': verification_info
        }
    
    def create_verification_report(self, df_dict: Dict[str, Any]) -> VerificationReport:
        """Create manual verification report for DataFrame outputs.
        
        Returns:
            VerificationReport with checks and recommendations
        """
        report = VerificationReport()
        
        # Analyze amplitude values for reasonableness
        amp_df = df_dict['amplitude']
        report.amplitude_check_required, report.suspicious_amplitudes = \
            self._check_amplitude_ranges(amp_df)
        
        # Check phase continuity
        phase_df = df_dict['phase']
        report.phase_continuity_check_required, report.phase_discontinuities = \
            self._check_phase_continuity(phase_df)
        
        # Generate spot check locations
        report.spot_check_locations = self._generate_spot_check_locations(amp_df)
        
        # Identify symmetry pairs for verification
        report.symmetry_pairs = self._identify_symmetry_pairs(amp_df.columns)
        
        return report
    
    def generate_spot_check_values(self, df: pd.DataFrame, n_samples: int = 10) -> List[Dict]:
        """Generate random spot check values for manual verification.
        
        Returns:
            List of dictionaries with location and value for manual checking
        """
        spot_checks = []
        frequencies = df.index.tolist()
        columns = df.columns.tolist()
        
        # Ensure we don't request more samples than available data points
        max_samples = min(n_samples, len(frequencies) * len(columns))
        
        # Generate unique random samples
        sampled_points = set()
        while len(spot_checks) < max_samples:
            freq = random.choice(frequencies)
            col = random.choice(columns)
            
            # Ensure unique sampling
            point_key = (freq, col)
            if point_key not in sampled_points:
                sampled_points.add(point_key)
                value = df.loc[freq, col]
                
                spot_checks.append({
                    'frequency': freq,
                    'dof': col[0],
                    'heading': col[1],
                    'value': value,
                    'location_in_source': f"Freq={freq:.3f}, {col[0]} @ {col[1]}"
                })
        
        return spot_checks
    
    def check_phase_continuity(self, phase_df: pd.DataFrame, threshold: float = 180.0) -> List[Dict]:
        """Check for phase discontinuities that may indicate errors.
        
        Args:
            phase_df: DataFrame with phase values in degrees
            threshold: Maximum allowed phase jump between frequencies
            
        Returns:
            List of locations with potential phase discontinuities
        """
        discontinuities = []
        
        for col in phase_df.columns:
            phase_values = phase_df[col].values
            phase_diff = np.diff(phase_values)
            
            # Adjust for phase wrapping (e.g., 350° to 10° should be 20°, not -340°)
            phase_diff = np.where(phase_diff > 180, phase_diff - 360, phase_diff)
            phase_diff = np.where(phase_diff < -180, phase_diff + 360, phase_diff)
            
            # Find jumps larger than threshold
            jump_indices = np.where(np.abs(phase_diff) > threshold)[0]
            
            for idx in jump_indices:
                discontinuities.append({
                    'dof': col[0],
                    'heading': col[1],
                    'frequency_index': idx,
                    'frequency': phase_df.index[idx],
                    'phase_jump': phase_diff[idx],
                    'requires_manual_check': True
                })
        
        return discontinuities
    
    def interpolate_rao_data(self, rao_data: RAOData, 
                           target_frequencies: np.ndarray,
                           target_headings: np.ndarray) -> RAOData:
        """Interpolate RAO data to target frequency and heading grids.
        
        Args:
            rao_data: Source RAO data
            target_frequencies: Target frequency array (rad/s)
            target_headings: Target heading array (degrees)
            
        Returns:
            Interpolated RAOData on target grid
        """
        return self.interpolator.interpolate_2d(
            rao_data, target_frequencies, target_headings
        )
    
    # Private helper methods
    def _check_amplitude_ranges(self, amp_df: pd.DataFrame) -> Tuple[bool, List[Dict]]:
        """Check amplitude values for physical reasonableness."""
        suspicious = []
        check_required = False
        
        # Define reasonable limits for each DOF
        limits = {
            'surge': (0, 5.0),    # m/m
            'sway': (0, 5.0),     # m/m
            'heave': (0, 2.0),    # m/m
            'roll': (0, 10.0),    # deg/m
            'pitch': (0, 10.0),   # deg/m
            'yaw': (0, 10.0)      # deg/m
        }
        
        for col in amp_df.columns:
            dof = col[0]
            if dof in limits:
                min_limit, max_limit = limits[dof]
                col_values = amp_df[col]
                
                # Check for values outside limits
                mask = (col_values < min_limit) | (col_values > max_limit)
                if mask.any():
                    check_required = True
                    suspicious_indices = col_values[mask].index
                    for idx in suspicious_indices:
                        suspicious.append({
                            'dof': dof,
                            'heading': col[1],
                            'frequency': idx,
                            'value': col_values[idx],
                            'expected_range': limits[dof]
                        })
        
        return check_required, suspicious
    
    def _check_phase_continuity(self, phase_df: pd.DataFrame) -> Tuple[bool, List[Dict]]:
        """Check phase continuity and return discontinuities."""
        discontinuities = self.check_phase_continuity(phase_df)
        check_required = len(discontinuities) > 0
        return check_required, discontinuities
    
    def _generate_spot_check_locations(self, df: pd.DataFrame, n_spots: int = 20) -> List[Dict]:
        """Generate strategic spot check locations."""
        spots = []
        
        # Include extremes (min/max frequencies)
        freq_indices = [0, len(df.index) // 2, -1]
        
        # Include key headings (0°, 90°, 180°, 270°)
        key_headings = ['0.0deg', '90.0deg', '180.0deg', '270.0deg']
        
        for freq_idx in freq_indices:
            freq = df.index[freq_idx]
            for col in df.columns:
                if any(heading in col[1] for heading in key_headings):
                    spots.append({
                        'frequency': freq,
                        'dof': col[0],
                        'heading': col[1],
                        'reason': 'Key frequency/heading combination'
                    })
        
        # Add some random spots
        random_spots = self.generate_spot_check_values(df, n_samples=n_spots - len(spots))
        spots.extend(random_spots)
        
        return spots
    
    def _identify_symmetry_pairs(self, columns: pd.MultiIndex) -> List[Tuple[str, str]]:
        """Identify heading pairs that should be symmetric."""
        pairs = []
        
        # For symmetric vessels, certain heading pairs should have similar RAOs
        # e.g., port/starboard symmetry
        symmetry_pairs = [
            (30, 330),   # 30° and 330°
            (60, 300),   # 60° and 300°
            (120, 240),  # 120° and 240°
            (150, 210),  # 150° and 210°
        ]
        
        for dof in ['sway', 'roll', 'yaw']:  # Anti-symmetric DOFs
            for h1, h2 in symmetry_pairs:
                col1 = (dof, f"{h1:.1f}deg")
                col2 = (dof, f"{h2:.1f}deg")
                if col1 in columns and col2 in columns:
                    pairs.append((str(col1), str(col2)))
        
        return pairs