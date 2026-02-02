"""
Generic Time Series Reader for flexible file processing

Supports multiple input modes:
- Single file
- Pattern matching (wildcards)
- Directory scanning
- File lists

Features dynamic column detection and mapping.
"""

import re
import glob
import pandas as pd
import numpy as np
from pathlib import Path
from typing import List, Dict, Union, Optional, Tuple
import logging

logger = logging.getLogger(__name__)


class GenericTimeSeriesReader:
    """Flexible time series data reader with auto-detection capabilities"""
    
    # Common time column patterns
    TIME_PATTERNS = [
        r'^time$', r'^Time$', r'^TIME$', r'^t$', r'^T$',
        r'^Time\s*\(s\)$', r'^Time\s*\[s\]$', r'^Time_s$',
        r'^Simulation\s*Time$', r'^elapsed[_\s]*time$',
        r'^timestamp$', r'^Timestamp$'
    ]
    
    # Common data column patterns for different domains
    DATA_PATTERNS = {
        'tension': [r'.*[Tt]ension.*', r'.*TENSION.*'],
        'force': [r'.*[Ff]orce.*', r'.*FORCE.*', r'.*[Ff][xyz].*'],
        'stress': [r'.*[Ss]tress.*', r'.*STRESS.*'],
        'moment': [r'.*[Mm]oment.*', r'.*MOMENT.*', r'.*[Mm][xyz].*'],
        'pressure': [r'.*[Pp]ressure.*', r'.*PRESSURE.*'],
        'displacement': [r'.*[Dd]isplacement.*', r'.*[Pp]osition.*', r'.*[Xx].*', r'.*[Yy].*', r'.*[Zz].*'],
        'velocity': [r'.*[Vv]elocity.*', r'.*[Vv][xyz].*'],
        'acceleration': [r'.*[Aa]cceleration.*', r'.*[Aa][xyz].*']
    }
    
    # Patterns to exclude
    EXCLUDE_PATTERNS = [
        r'.*_flag$', r'.*_quality$', r'.*_status$', r'.*_comment$',
        r'^index$', r'^row_id$', r'^id$', r'^ID$'
    ]
    
    def __init__(self, config: Optional[Dict] = None):
        """
        Initialize reader with optional configuration
        
        Args:
            config: Configuration dictionary with column mapping settings
        """
        self.config = config or {}
        self.detected_mappings = {}
        
    def discover_files(self, 
                      pattern: Optional[str] = None,
                      directory: Optional[Union[str, Path]] = None,
                      recursive: bool = False,
                      file_list: Optional[List[str]] = None) -> List[Path]:
        """
        Discover files based on various input modes
        
        Args:
            pattern: File pattern with wildcards (e.g., "*.csv", "fat*.csv")
            directory: Directory to search in
            recursive: Whether to search recursively
            file_list: Explicit list of file paths
            
        Returns:
            List of Path objects for discovered files
        """
        files = []
        
        # Mode 1: Explicit file list
        if file_list:
            files = [Path(f) for f in file_list]
            
        # Mode 2: Pattern matching
        elif pattern:
            if directory:
                directory = Path(directory)
                if recursive:
                    pattern_path = directory / "**" / pattern
                else:
                    pattern_path = directory / pattern
            else:
                pattern_path = pattern
                
            files = [Path(f) for f in glob.glob(str(pattern_path), recursive=recursive)]
            
        # Mode 3: Directory scanning
        elif directory:
            directory = Path(directory)
            extensions = self.config.get('file_extensions', ['.csv', '.txt', '.dat'])
            
            if recursive:
                for ext in extensions:
                    files.extend(directory.rglob(f"*{ext}"))
            else:
                for ext in extensions:
                    files.extend(directory.glob(f"*{ext}"))
        
        # Apply filters
        files = self._apply_filters(files)
        
        logger.info(f"Discovered {len(files)} files")
        return sorted(files)
    
    def _apply_filters(self, files: List[Path]) -> List[Path]:
        """Apply size and pattern filters to file list"""
        filtered = []
        
        min_size = self.config.get('min_file_size', 0)
        max_size = self.config.get('max_file_size', float('inf'))
        exclude_patterns = self.config.get('exclude_patterns', [])
        
        for file in files:
            if not file.exists():
                continue
                
            # Size filter
            size = file.stat().st_size
            if size < min_size or size > max_size:
                continue
                
            # Pattern filter
            excluded = False
            for pattern in exclude_patterns:
                if re.match(pattern, file.name):
                    excluded = True
                    break
                    
            if not excluded:
                filtered.append(file)
                
        return filtered
    
    def auto_detect_columns(self, df: pd.DataFrame) -> Dict[str, Union[str, List[str]]]:
        """
        Automatically detect time and data columns
        
        Args:
            df: DataFrame to analyze
            
        Returns:
            Dictionary with 'time' and 'data_columns' keys
        """
        mapping = {'time': None, 'data_columns': []}
        
        # Detect time column
        for col in df.columns:
            col_str = str(col)
            for pattern in self.TIME_PATTERNS:
                if re.match(pattern, col_str, re.IGNORECASE):
                    mapping['time'] = col
                    logger.info(f"Auto-detected time column: {col}")
                    break
            if mapping['time']:
                break
        
        # If no time column found, check if first column is numeric and monotonic
        if not mapping['time'] and len(df.columns) > 0:
            first_col = df.columns[0]
            if pd.api.types.is_numeric_dtype(df[first_col]):
                if df[first_col].is_monotonic_increasing:
                    mapping['time'] = first_col
                    logger.info(f"Using first column as time: {first_col}")
        
        # Detect data columns
        data_patterns = self.config.get('data_patterns', ['.*'])
        
        for col in df.columns:
            # Skip time column
            if col == mapping['time']:
                continue
                
            col_str = str(col)
            
            # Check exclusion patterns first
            excluded = False
            for pattern in self.EXCLUDE_PATTERNS:
                if re.match(pattern, col_str, re.IGNORECASE):
                    excluded = True
                    break
            
            if excluded:
                continue
            
            # Check if numeric
            if not pd.api.types.is_numeric_dtype(df[col]):
                continue
            
            # Check data patterns
            for pattern_type, patterns in self.DATA_PATTERNS.items():
                for pattern in patterns:
                    if re.match(pattern, col_str, re.IGNORECASE):
                        mapping['data_columns'].append({
                            'name': col,
                            'type': pattern_type,
                            'original_name': col
                        })
                        break
        
        # If no specific patterns matched, include all numeric columns
        if not mapping['data_columns']:
            for col in df.columns:
                if col != mapping['time'] and pd.api.types.is_numeric_dtype(df[col]):
                    mapping['data_columns'].append({
                        'name': col,
                        'type': 'generic',
                        'original_name': col
                    })
        
        logger.info(f"Auto-detected {len(mapping['data_columns'])} data columns")
        return mapping
    
    def read_file(self, 
                  filepath: Union[str, Path],
                  column_mapping: Optional[Dict] = None,
                  auto_detect: bool = True) -> Tuple[pd.DataFrame, Dict]:
        """
        Read file with flexible column mapping
        
        Args:
            filepath: Path to file
            column_mapping: Manual column mapping
            auto_detect: Whether to auto-detect columns
            
        Returns:
            Tuple of (DataFrame, column mapping used)
        """
        filepath = Path(filepath)
        
        # Determine file type and read
        if filepath.suffix.lower() == '.csv':
            df = self._read_csv(filepath)
        elif filepath.suffix.lower() in ['.xlsx', '.xls']:
            df = pd.read_excel(filepath)
        elif filepath.suffix.lower() == '.h5':
            df = pd.read_hdf(filepath)
        else:
            # Try CSV reader as default
            df = self._read_csv(filepath)
        
        # Determine column mapping
        if column_mapping:
            mapping = column_mapping
        elif auto_detect:
            mapping = self.auto_detect_columns(df)
        else:
            # Use configuration mapping
            mapping = self._get_config_mapping(df)
        
        # Validate mapping
        if not self.validate_mapping(df, mapping):
            raise ValueError(f"Invalid column mapping for {filepath}")
        
        # Store detected mapping
        self.detected_mappings[str(filepath)] = mapping
        
        return df, mapping
    
    def _read_csv(self, filepath: Path) -> pd.DataFrame:
        """Read CSV file with configured parameters"""
        params = {
            'encoding': self.config.get('encoding', 'utf-8'),
            'delimiter': self.config.get('delimiter', ','),
            'decimal': self.config.get('decimal', '.'),
            'skiprows': self.config.get('skip_rows', 0),
            'header': self.config.get('header_row', 0)
        }
        
        try:
            df = pd.read_csv(filepath, **params)
        except Exception as e:
            logger.warning(f"Failed to read with config params: {e}")
            # Try with default parameters
            df = pd.read_csv(filepath)
            
        return df
    
    def _get_config_mapping(self, df: pd.DataFrame) -> Dict:
        """Get column mapping from configuration"""
        if 'column_mapping' not in self.config:
            return self.auto_detect_columns(df)
            
        mapping_config = self.config['column_mapping']
        
        # Check for profile
        if 'active_profile' in mapping_config and 'profiles' in mapping_config:
            profile_name = mapping_config['active_profile']
            if profile_name in mapping_config['profiles']:
                profile = mapping_config['profiles'][profile_name]
                return self._apply_profile(df, profile)
        
        # Use manual mapping
        if 'manual' in mapping_config:
            return mapping_config['manual']
            
        # Fall back to auto-detection
        return self.auto_detect_columns(df)
    
    def _apply_profile(self, df: pd.DataFrame, profile: Dict) -> Dict:
        """Apply a column mapping profile"""
        mapping = {'time': None, 'data_columns': []}
        
        # Get time column
        if 'time' in profile:
            if profile['time'] in df.columns:
                mapping['time'] = profile['time']
            elif isinstance(profile['time'], int) and profile['time'] < len(df.columns):
                mapping['time'] = df.columns[profile['time']]
        
        # Get data columns
        if 'data_pattern' in profile:
            pattern = profile['data_pattern']
            for col in df.columns:
                if col != mapping['time'] and re.match(pattern, str(col)):
                    mapping['data_columns'].append({
                        'name': col,
                        'original_name': col,
                        'units': profile.get('units', '')
                    })
        elif 'data_columns' in profile:
            for col_spec in profile['data_columns']:
                if isinstance(col_spec, str):
                    if col_spec in df.columns:
                        mapping['data_columns'].append({
                            'name': col_spec,
                            'original_name': col_spec,
                            'units': profile.get('units', '')
                        })
                elif isinstance(col_spec, dict):
                    if col_spec.get('name') in df.columns:
                        mapping['data_columns'].append(col_spec)
        
        return mapping
    
    def validate_mapping(self, df: pd.DataFrame, mapping: Dict) -> bool:
        """
        Validate that mapping is applicable to DataFrame
        
        Args:
            df: DataFrame to validate against
            mapping: Column mapping to validate
            
        Returns:
            True if valid, False otherwise
        """
        # Check time column
        if mapping.get('time'):
            if mapping['time'] not in df.columns:
                logger.error(f"Time column '{mapping['time']}' not found")
                return False
                
        # Check data columns
        if not mapping.get('data_columns'):
            logger.warning("No data columns specified")
            return True  # Allow empty data columns
            
        for col_spec in mapping['data_columns']:
            if isinstance(col_spec, dict):
                col_name = col_spec.get('name') or col_spec.get('original_name')
            else:
                col_name = col_spec
                
            if col_name not in df.columns:
                # Check if optional
                if isinstance(col_spec, dict) and col_spec.get('optional'):
                    continue
                logger.error(f"Data column '{col_name}' not found")
                return False
                
        return True
    
    def validate_data(self, df: pd.DataFrame, mapping: Dict) -> Dict:
        """
        Validate data integrity and quality
        
        Args:
            df: DataFrame to validate
            mapping: Column mapping
            
        Returns:
            Dictionary with validation results
        """
        results = {
            'valid': True,
            'warnings': [],
            'errors': [],
            'statistics': {}
        }
        
        # Check for time column monotonicity
        if mapping.get('time'):
            time_col = df[mapping['time']]
            
            if not time_col.is_monotonic_increasing:
                results['warnings'].append("Time column is not monotonic")
                
            # Check for gaps
            if len(time_col) > 1:
                dt = np.diff(time_col)
                dt_median = np.median(dt)
                gaps = np.where(dt > 2 * dt_median)[0]
                if len(gaps) > 0:
                    results['warnings'].append(f"Found {len(gaps)} time gaps")
                    
            results['statistics']['sampling_rate'] = 1.0 / dt_median if dt_median > 0 else None
            results['statistics']['duration'] = time_col.iloc[-1] - time_col.iloc[0]
        
        # Check data columns
        for col_spec in mapping.get('data_columns', []):
            if isinstance(col_spec, dict):
                col_name = col_spec.get('name')
            else:
                col_name = col_spec
                
            if col_name not in df.columns:
                continue
                
            col_data = df[col_name]
            
            # Check for NaN values
            nan_count = col_data.isna().sum()
            if nan_count > 0:
                results['warnings'].append(f"Column '{col_name}' has {nan_count} NaN values")
                
            # Basic statistics
            results['statistics'][col_name] = {
                'mean': col_data.mean(),
                'std': col_data.std(),
                'min': col_data.min(),
                'max': col_data.max(),
                'nan_count': nan_count
            }
        
        return results
    
    def preprocess(self, 
                   df: pd.DataFrame,
                   mapping: Dict,
                   fill_gaps: bool = True,
                   remove_outliers: bool = False,
                   detrend: bool = False) -> pd.DataFrame:
        """
        Preprocess data for analysis
        
        Args:
            df: DataFrame to preprocess
            mapping: Column mapping
            fill_gaps: Whether to interpolate gaps
            remove_outliers: Whether to remove outliers
            detrend: Whether to detrend data
            
        Returns:
            Preprocessed DataFrame
        """
        df = df.copy()
        
        # Fill gaps
        if fill_gaps:
            method = self.config.get('fill_na_method', 'interpolate')
            if method == 'interpolate':
                df = df.interpolate(method='linear')
            elif method == 'forward':
                df = df.fillna(method='ffill')
            elif method == 'backward':
                df = df.fillna(method='bfill')
        
        # Remove outliers
        if remove_outliers:
            threshold = self.config.get('outlier_threshold', 5)
            for col_spec in mapping.get('data_columns', []):
                if isinstance(col_spec, dict):
                    col_name = col_spec.get('name')
                else:
                    col_name = col_spec
                    
                if col_name in df.columns:
                    col_data = df[col_name]
                    z_scores = np.abs((col_data - col_data.mean()) / col_data.std())
                    df.loc[z_scores > threshold, col_name] = np.nan
                    df[col_name] = df[col_name].interpolate(method='linear')
        
        # Detrend
        if detrend:
            from scipy import signal
            for col_spec in mapping.get('data_columns', []):
                if isinstance(col_spec, dict):
                    col_name = col_spec.get('name')
                else:
                    col_name = col_spec
                    
                if col_name in df.columns:
                    df[col_name] = signal.detrend(df[col_name])
        
        return df