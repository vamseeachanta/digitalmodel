"""
OrcaFlex CSV Parser Service

Handles parsing of OrcaFlex CSV files with robust error handling and memory efficiency.
Supports dm_* summary files and time trace files with polar heading data processing.
"""

import pandas as pd
import numpy as np
import logging
from typing import Dict, List, Optional, Union, Tuple, Iterator
from pathlib import Path
import re
from dataclasses import dataclass
from enum import Enum
import math
import warnings

# Suppress pandas warnings for cleaner logs
warnings.filterwarnings('ignore', category=pd.errors.ParserWarning)

logger = logging.getLogger(__name__)


class FileType(Enum):
    """OrcaFlex CSV file types"""
    DM_SUMMARY = "dm_summary"
    DM_INPUTS = "dm_inputs"
    TIME_TRACE = "time_trace"
    UNKNOWN = "unknown"


@dataclass
class ParsedColumn:
    """Represents a parsed column with metadata"""
    name: str
    unit: Optional[str]
    component: Optional[str]
    parameter: Optional[str]
    original_name: str


@dataclass
class PolarData:
    """Polar coordinate data structure"""
    headings: np.ndarray  # Degrees [0, 15, 30, ..., 345]
    values: np.ndarray    # Corresponding values
    units: Optional[str] = None
    parameter: Optional[str] = None


class CSVParser:
    """
    High-performance CSV parser for OrcaFlex output files.
    
    Features:
    - Memory-efficient chunked processing for large files
    - Polar heading data extraction (0° to 345° in 15° increments)
    - Engineering unit parsing and validation
    - Component and parameter classification
    - Robust error handling with detailed diagnostics
    """
    
    # Standard polar headings in OrcaFlex (24 points)
    STANDARD_HEADINGS = np.arange(0, 360, 15)  # [0, 15, 30, ..., 345]
    
    # Engineering unit patterns
    UNIT_PATTERNS = {
        'force': r'\b(kN|N|MN|lbf)\b',
        'moment': r'\b(kN\.?m|kNm|N\.?m|Nm|MN\.?m|MNm)\b',
        'displacement': r'\b(m|mm|cm|ft|in)\b',
        'angle': r'\b(deg|rad|°)\b',
        'velocity': r'\b(m/s|ft/s|knots)\b',
        'acceleration': r'\b(m/s2|m/s²|ft/s2|ft/s²)\b'
    }
    
    # Column name patterns for component extraction
    COMPONENT_PATTERNS = {
        'fst1': r'fst1|float.*1|floater.*1',
        'fst2': r'fst2|float.*2|floater.*2', 
        'strut': r'strut|connector',
        'jacket': r'jacket|foundation',
        'lngc': r'lngc|vessel|ship'
    }
    
    def __init__(self, chunk_size: int = 10000):
        """
        Initialize CSV parser.
        
        Args:
            chunk_size: Number of rows to process at once for memory efficiency
        """
        self.chunk_size = chunk_size
        self.logger = logging.getLogger(f"{__name__}.{self.__class__.__name__}")
        
    def identify_file_type(self, file_path: Path) -> FileType:
        """
        Identify the type of OrcaFlex CSV file.
        
        Args:
            file_path: Path to CSV file
            
        Returns:
            FileType enum value
        """
        try:
            filename = file_path.name.lower()
            
            if filename.startswith('dm_') and filename.endswith('.csv'):
                if '_inputs' in filename:
                    return FileType.DM_INPUTS
                else:
                    return FileType.DM_SUMMARY
            elif any(trace_pattern in filename for trace_pattern in ['trace', 'time', 'ts_']):
                return FileType.TIME_TRACE
            else:
                # Try to identify by examining file structure
                return self._identify_by_structure(file_path)
                
        except Exception as e:
            self.logger.warning(f"Could not identify file type for {file_path}: {e}")
            return FileType.UNKNOWN
    
    def _identify_by_structure(self, file_path: Path) -> FileType:
        """Identify file type by examining first few rows"""
        try:
            # Read just the header
            df_sample = pd.read_csv(file_path, nrows=5)
            columns = [col.lower() for col in df_sample.columns]
            
            # Check for polar heading patterns
            heading_count = sum(1 for col in columns if re.search(r'\b\d{1,3}\.?\d*\s*deg\b', col))
            if heading_count >= 20:  # Likely has polar data
                return FileType.DM_SUMMARY
                
            # Check for time column
            if any('time' in col for col in columns):
                return FileType.TIME_TRACE
                
            return FileType.UNKNOWN
            
        except Exception as e:
            self.logger.warning(f"Structure analysis failed for {file_path}: {e}")
            return FileType.UNKNOWN
    
    def parse_column_metadata(self, column_name: str) -> ParsedColumn:
        """
        Extract metadata from column name including units, component, and parameter.
        
        Args:
            column_name: Original column name from CSV
            
        Returns:
            ParsedColumn with extracted metadata
        """
        # Extract units using regex
        unit = None
        for unit_type, pattern in self.UNIT_PATTERNS.items():
            match = re.search(pattern, column_name, re.IGNORECASE)
            if match:
                unit = match.group(0)
                break
        
        # Extract component
        component = None
        for comp_type, pattern in self.COMPONENT_PATTERNS.items():
            if re.search(pattern, column_name, re.IGNORECASE):
                component = comp_type
                break
        
        # Extract parameter (simplified approach)
        parameter = None
        param_patterns = {
            'force_x': r'fx|force.*x',
            'force_y': r'fy|force.*y', 
            'force_z': r'fz|force.*z',
            'moment_x': r'mx|moment.*x',
            'moment_y': r'my|moment.*y',
            'moment_z': r'mz|moment.*z',
            'displacement_x': r'x|disp.*x|pos.*x',
            'displacement_y': r'y|disp.*y|pos.*y',
            'displacement_z': r'z|disp.*z|pos.*z'
        }
        
        for param_name, pattern in param_patterns.items():
            if re.search(pattern, column_name, re.IGNORECASE):
                parameter = param_name
                break
        
        # Clean name for display
        clean_name = re.sub(r'[^\w\s-]', ' ', column_name).strip()
        clean_name = re.sub(r'\s+', ' ', clean_name)
        
        return ParsedColumn(
            name=clean_name,
            unit=unit,
            component=component,
            parameter=parameter,
            original_name=column_name
        )
    
    def extract_polar_data(self, df: pd.DataFrame) -> Dict[str, PolarData]:
        """
        Extract polar heading data from DataFrame.
        
        Args:
            df: DataFrame with polar columns
            
        Returns:
            Dictionary mapping parameter names to PolarData objects
        """
        polar_data = {}
        
        try:
            # Find columns with heading information
            heading_columns = []
            for col in df.columns:
                # Look for degree patterns: "123 deg", "123.0°", etc.
                match = re.search(r'(\d{1,3}(?:\.\d+)?)\s*(?:deg|°)', col, re.IGNORECASE)
                if match:
                    heading = float(match.group(1))
                    if 0 <= heading < 360:
                        heading_columns.append((col, heading))
            
            if not heading_columns:
                self.logger.debug("No polar heading columns found")
                return polar_data
            
            # Sort by heading
            heading_columns.sort(key=lambda x: x[1])
            
            # Group columns by parameter (everything before the heading)
            param_groups = {}
            for col, heading in heading_columns:
                # Extract parameter name by removing heading part
                param_match = re.search(r'^(.+?)\s+\d{1,3}(?:\.\d+)?\s*(?:deg|°)', col, re.IGNORECASE)
                if param_match:
                    param_name = param_match.group(1).strip()
                    if param_name not in param_groups:
                        param_groups[param_name] = []
                    param_groups[param_name].append((col, heading))
            
            # Process each parameter group
            for param_name, columns in param_groups.items():
                if len(columns) < 12:  # Need reasonable number of points
                    continue
                    
                headings = []
                values = []
                
                for col, heading in columns:
                    try:
                        # Use the last non-NaN value for summary data
                        col_data = df[col].dropna()
                        if len(col_data) > 0:
                            value = col_data.iloc[-1]  # Last value for summary
                            headings.append(heading)
                            values.append(float(value))
                    except (ValueError, IndexError) as e:
                        self.logger.debug(f"Skipping column {col}: {e}")
                        continue
                
                if len(headings) >= 12:  # Minimum for meaningful polar data
                    # Parse metadata from first column
                    parsed_col = self.parse_column_metadata(columns[0][0])
                    
                    polar_data[param_name] = PolarData(
                        headings=np.array(headings),
                        values=np.array(values),
                        units=parsed_col.unit,
                        parameter=parsed_col.parameter
                    )
                    
                    self.logger.debug(f"Extracted polar data for {param_name}: {len(headings)} points")
        
        except Exception as e:
            self.logger.error(f"Error extracting polar data: {e}")
            
        return polar_data
    
    def parse_file(self, file_path: Path, **kwargs) -> Dict:
        """
        Parse OrcaFlex CSV file with automatic type detection.
        
        Args:
            file_path: Path to CSV file
            **kwargs: Additional pandas.read_csv arguments
            
        Returns:
            Dictionary with parsed data and metadata
        """
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")
        
        file_type = self.identify_file_type(file_path)
        self.logger.info(f"Parsing {file_type.value} file: {file_path.name}")
        
        try:
            # Configure pandas options for robust parsing
            parse_options = {
                'low_memory': False,
                'encoding': 'utf-8',
                'skip_blank_lines': True,
                'na_values': ['', 'N/A', 'NaN', 'NULL', '#N/A', '#DIV/0!'],
                **kwargs
            }
            
            # Try different encoding if UTF-8 fails
            try:
                df = pd.read_csv(file_path, **parse_options)
            except UnicodeDecodeError:
                self.logger.warning(f"UTF-8 failed, trying latin-1 for {file_path}")
                parse_options['encoding'] = 'latin-1'
                df = pd.read_csv(file_path, **parse_options)
            
            # Parse column metadata
            column_metadata = {}
            for col in df.columns:
                column_metadata[col] = self.parse_column_metadata(col)
            
            result = {
                'file_path': str(file_path),
                'file_type': file_type,
                'dataframe': df,
                'column_metadata': column_metadata,
                'shape': df.shape,
                'memory_usage_mb': df.memory_usage(deep=True).sum() / 1024**2
            }
            
            # Extract polar data for summary files
            if file_type == FileType.DM_SUMMARY:
                result['polar_data'] = self.extract_polar_data(df)
            
            # Add basic statistics
            numeric_cols = df.select_dtypes(include=[np.number]).columns
            if len(numeric_cols) > 0:
                result['statistics'] = {
                    'numeric_columns': len(numeric_cols),
                    'total_rows': len(df),
                    'memory_usage_mb': result['memory_usage_mb']
                }
            
            self.logger.info(f"Successfully parsed {file_path.name}: "
                           f"{result['shape'][0]} rows, {result['shape'][1]} columns, "
                           f"{result['memory_usage_mb']:.1f} MB")
            
            return result
            
        except Exception as e:
            self.logger.error(f"Failed to parse {file_path}: {str(e)}")
            raise
    
    def parse_chunked(self, file_path: Path, **kwargs) -> Iterator[Dict]:
        """
        Parse large CSV files in chunks for memory efficiency.
        
        Args:
            file_path: Path to CSV file
            **kwargs: Additional pandas.read_csv arguments
            
        Yields:
            Dictionary with chunk data and metadata
        """
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {file_path}")
        
        file_type = self.identify_file_type(file_path)
        self.logger.info(f"Parsing {file_type.value} file in chunks: {file_path.name}")
        
        try:
            parse_options = {
                'chunksize': self.chunk_size,
                'low_memory': False,
                'encoding': 'utf-8',
                'skip_blank_lines': True,
                'na_values': ['', 'N/A', 'NaN', 'NULL', '#N/A', '#DIV/0!'],
                **kwargs
            }
            
            # Try different encoding if UTF-8 fails
            try:
                chunk_reader = pd.read_csv(file_path, **parse_options)
            except UnicodeDecodeError:
                self.logger.warning(f"UTF-8 failed, trying latin-1 for {file_path}")
                parse_options['encoding'] = 'latin-1'
                chunk_reader = pd.read_csv(file_path, **parse_options)
            
            chunk_num = 0
            column_metadata = None
            
            for chunk_df in chunk_reader:
                if column_metadata is None:
                    # Parse metadata only once
                    column_metadata = {}
                    for col in chunk_df.columns:
                        column_metadata[col] = self.parse_column_metadata(col)
                
                result = {
                    'file_path': str(file_path),
                    'file_type': file_type,
                    'chunk_number': chunk_num,
                    'dataframe': chunk_df,
                    'column_metadata': column_metadata,
                    'shape': chunk_df.shape,
                    'memory_usage_mb': chunk_df.memory_usage(deep=True).sum() / 1024**2
                }
                
                self.logger.debug(f"Chunk {chunk_num}: {chunk_df.shape[0]} rows, "
                                f"{result['memory_usage_mb']:.1f} MB")
                
                chunk_num += 1
                yield result
                
        except Exception as e:
            self.logger.error(f"Failed to parse chunks from {file_path}: {str(e)}")
            raise
    
    def validate_polar_data(self, polar_data: PolarData, 
                          expected_headings: Optional[np.ndarray] = None) -> Dict[str, any]:
        """
        Validate polar data completeness and consistency.
        
        Args:
            polar_data: PolarData object to validate
            expected_headings: Expected heading values (defaults to standard 0-345° in 15° increments)
            
        Returns:
            Dictionary with validation results
        """
        if expected_headings is None:
            expected_headings = self.STANDARD_HEADINGS
        
        validation = {
            'is_valid': True,
            'issues': [],
            'completeness': 0.0,
            'heading_coverage': 0.0,
            'data_quality': {}
        }
        
        try:
            # Check completeness
            expected_set = set(expected_headings)
            actual_set = set(np.round(polar_data.headings))
            missing_headings = expected_set - actual_set
            extra_headings = actual_set - expected_set
            
            validation['completeness'] = len(actual_set & expected_set) / len(expected_set)
            validation['heading_coverage'] = len(actual_set) / len(expected_headings)
            
            if missing_headings:
                validation['issues'].append(f"Missing headings: {sorted(missing_headings)}")
                validation['is_valid'] = False
            
            if extra_headings:
                validation['issues'].append(f"Extra headings: {sorted(extra_headings)}")
            
            # Check data quality
            values = polar_data.values
            validation['data_quality'] = {
                'has_nan': np.isnan(values).any(),
                'has_inf': np.isinf(values).any(),
                'range': [float(np.min(values)), float(np.max(values))],
                'mean': float(np.mean(values)),
                'std': float(np.std(values))
            }
            
            if validation['data_quality']['has_nan']:
                validation['issues'].append("Contains NaN values")
                validation['is_valid'] = False
            
            if validation['data_quality']['has_inf']:
                validation['issues'].append("Contains infinite values")
                validation['is_valid'] = False
                
        except Exception as e:
            validation['is_valid'] = False
            validation['issues'].append(f"Validation error: {str(e)}")
        
        return validation


# Utility functions for common parsing tasks
def quick_parse(file_path: Union[str, Path], chunk_size: int = 10000) -> Dict:
    """Quick parse with default settings"""
    parser = CSVParser(chunk_size=chunk_size)
    return parser.parse_file(Path(file_path))


def extract_time_series_stats(df: pd.DataFrame, time_col: str = 'time') -> Dict:
    """
    Calculate time series statistics including RMS values.
    
    Args:
        df: DataFrame with time series data
        time_col: Name of time column
        
    Returns:
        Dictionary with statistical measures
    """
    stats = {}
    
    if time_col not in df.columns:
        # Try to find time column
        time_candidates = [col for col in df.columns 
                          if 'time' in col.lower() or 't' == col.lower()]
        if time_candidates:
            time_col = time_candidates[0]
        else:
            return {'error': 'No time column found'}
    
    time_data = df[time_col].dropna()
    if len(time_data) < 2:
        return {'error': 'Insufficient time data'}
    
    dt = np.mean(np.diff(time_data))  # Average time step
    T = time_data.iloc[-1] - time_data.iloc[0]  # Total time
    
    numeric_cols = df.select_dtypes(include=[np.number]).columns
    numeric_cols = [col for col in numeric_cols if col != time_col]
    
    for col in numeric_cols:
        try:
            data = df[col].dropna()
            if len(data) > 0:
                # Basic statistics
                col_stats = {
                    'mean': float(np.mean(data)),
                    'std': float(np.std(data)),
                    'min': float(np.min(data)),
                    'max': float(np.max(data)),
                    'rms': float(np.sqrt(np.mean(data**2))),  # RMS calculation
                    'range': float(np.max(data) - np.min(data))
                }
                
                # Additional statistics for time series
                if len(data) == len(time_data):
                    # Can calculate proper RMS over time
                    col_stats['time_rms'] = float(np.sqrt(np.trapz(data**2, time_data) / T))
                
                stats[col] = col_stats
                
        except Exception as e:
            logger.warning(f"Could not calculate stats for column {col}: {e}")
    
    return stats