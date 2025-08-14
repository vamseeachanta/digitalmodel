"""
Maximum Force Finder for Auto-Max Mode
Automatically identifies configuration with maximum strut forces from OrcaFlex summary files
"""

import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any, Callable
from dataclasses import dataclass
import logging
from concurrent.futures import ProcessPoolExecutor, as_completed
import re

logger = logging.getLogger(__name__)


@dataclass
class MaxForceResult:
    """Result from maximum force identification"""
    max_force: float
    force_column: str
    fe_filename: str
    configuration: Dict[str, str]
    source_file: Path
    row_index: int
    all_forces: Dict[str, float]


class MaxForceFinder:
    """Finds maximum forces from OrcaFlex summary files"""
    
    # Priority order for summary files
    SUMMARY_FILE_PRIORITY = [
        'dm*strut_dyn.csv',
        'dm*jacket_dyn.csv',
        'dm*mooring_dyn.csv',
        '*strut*.csv',
        '*jacket*.csv'
    ]
    
    # Force column patterns to search for
    FORCE_COLUMN_PATTERNS = [
        r'Strut\d+_Body_eff_tension_max',
        r'Jacket\d+_.*_max',
        r'Mooring\d+_.*_max',
        r'.*_tension_max',
        r'.*_force_max'
    ]
    
    def __init__(self, base_path: Optional[Path] = None):
        """
        Initialize max force finder
        
        Args:
            base_path: Base directory path for OrcaFlex files
        """
        self.base_path = base_path or Path("D:/1522/ctr7/orcaflex/rev_a08")
        self.max_result_cache: Optional[MaxForceResult] = None
        
    def find_summary_files(self, folder: Path) -> List[Path]:
        """
        Find summary files in priority order
        
        Args:
            folder: Folder to search in
            
        Returns:
            List of summary file paths in priority order
        """
        summary_files = []
        
        if not folder.exists():
            logger.warning(f"Folder does not exist: {folder}")
            return summary_files
        
        # Search for files in priority order
        for pattern in self.SUMMARY_FILE_PRIORITY:
            matches = list(folder.glob(pattern))
            if matches:
                summary_files.extend(matches)
                logger.info(f"Found {len(matches)} files matching pattern: {pattern}")
                # Return after finding first matching pattern (highest priority)
                break
        
        # If no priority files found, get all CSV files
        if not summary_files:
            summary_files = list(folder.glob("*.csv"))
            logger.info(f"Using all {len(summary_files)} CSV files in folder")
        
        return summary_files
    
    def _process_summary_file(self, file_path: Path) -> Optional[MaxForceResult]:
        """
        Process a single summary file to find maximum forces
        
        Args:
            file_path: Path to summary file
            
        Returns:
            MaxForceResult if successful, None otherwise
        """
        try:
            # Read CSV file
            df = pd.read_csv(file_path, low_memory=False)
            
            # Find force columns
            force_columns = []
            for col in df.columns:
                for pattern in self.FORCE_COLUMN_PATTERNS:
                    if re.match(pattern, col):
                        force_columns.append(col)
                        break
            
            if not force_columns:
                logger.debug(f"No force columns found in {file_path.name}")
                return None
            
            # Find maximum force across all force columns
            max_force = -np.inf
            max_col = None
            max_row = None
            
            for col in force_columns:
                try:
                    # Convert to numeric, handling any non-numeric values
                    numeric_col = pd.to_numeric(df[col], errors='coerce')
                    
                    # Find max value and its index
                    col_max = numeric_col.max()
                    if not np.isnan(col_max) and col_max > max_force:
                        max_force = col_max
                        max_col = col
                        max_row = numeric_col.idxmax()
                except Exception as e:
                    logger.debug(f"Error processing column {col}: {e}")
                    continue
            
            if max_col is None or max_row is None:
                return None
            
            # Extract fe_filename for configuration
            fe_filename = None
            if 'fe_filename' in df.columns:
                fe_filename = df.loc[max_row, 'fe_filename']
            elif 'filename' in df.columns:
                fe_filename = df.loc[max_row, 'filename']
            elif 'basename' in df.columns:
                fe_filename = df.loc[max_row, 'basename']
            
            if not fe_filename:
                # Try to extract from file name itself
                fe_filename = file_path.stem
            
            # Parse configuration from filename
            configuration = self._parse_configuration(fe_filename)
            
            # Collect all forces from this row
            all_forces = {}
            for col in force_columns:
                try:
                    value = pd.to_numeric(df.loc[max_row, col], errors='coerce')
                    if not np.isnan(value):
                        all_forces[col] = float(value)
                except:
                    pass
            
            return MaxForceResult(
                max_force=float(max_force),
                force_column=max_col,
                fe_filename=fe_filename,
                configuration=configuration,
                source_file=file_path,
                row_index=int(max_row),
                all_forces=all_forces
            )
            
        except Exception as e:
            logger.error(f"Error processing file {file_path}: {e}")
            return None
    
    def _parse_configuration(self, filename: str) -> Dict[str, str]:
        """
        Parse configuration parameters from filename
        
        Args:
            filename: Filename to parse
            
        Returns:
            Dictionary of configuration parameters
        """
        # Remove extension if present
        basename = Path(filename).stem
        
        # Remove 'dm_' prefix if present
        if basename.startswith('dm_'):
            basename = basename[3:]
        
        config = {
            'vessel_type': '',
            'analysis_type': '',
            'return_period': '',
            'loading_condition': '',
            'tide_level': '',
            'wave_direction': '000deg'  # Default
        }
        
        # Parse vessel type
        for vessel in ['fsts', 'flng', 'lngc']:
            if vessel in basename.lower():
                config['vessel_type'] = vessel
                break
        
        # Parse analysis type (e.g., 03c, 04a)
        match = re.search(r'(\d{2}[a-z])', basename.lower())
        if match:
            config['analysis_type'] = match.group(1)
        
        # Parse return period
        match = re.search(r'(\d{4}yr)', basename.lower())
        if match:
            config['return_period'] = match.group(1)
        
        # Parse loading condition
        match = re.search(r'(l\d{3})', basename.lower())
        if match:
            config['loading_condition'] = match.group(1)
        
        # Parse tide level
        for tide in ['hwl', 'mwl', 'lwl']:
            if tide in basename.lower():
                config['tide_level'] = tide
                break
        
        # Parse wave direction
        match = re.search(r'(\d{3}deg)', basename.lower())
        if match:
            config['wave_direction'] = match.group(1)
        
        return config
    
    def find_maximum_force_configuration(
        self, 
        folder: Optional[Path] = None,
        use_parallel: bool = True
    ) -> Optional[MaxForceResult]:
        """
        Find configuration with maximum strut forces
        
        Args:
            folder: Folder to search (uses default if None)
            use_parallel: Use parallel processing for multiple files
            
        Returns:
            MaxForceResult with maximum force configuration
        """
        # Use cached result if available
        if self.max_result_cache:
            logger.info("Using cached maximum force result")
            return self.max_result_cache
        
        # Default folder
        if folder is None:
            folder = self.base_path / "output" / "csv"
        
        # Find summary files
        summary_files = self.find_summary_files(folder)
        
        if not summary_files:
            logger.warning("No summary files found")
            return None
        
        logger.info(f"Processing {len(summary_files)} summary files")
        
        # Process files to find maximum
        max_result = None
        
        if use_parallel and len(summary_files) > 1:
            # Parallel processing for multiple files
            with ProcessPoolExecutor(max_workers=min(4, len(summary_files))) as executor:
                future_to_file = {
                    executor.submit(self._process_summary_file, file_path): file_path
                    for file_path in summary_files
                }
                
                for future in as_completed(future_to_file):
                    file_path = future_to_file[future]
                    try:
                        result = future.result()
                        if result:
                            if max_result is None or result.max_force > max_result.max_force:
                                max_result = result
                                logger.info(
                                    f"New maximum found: {result.max_force:.2f} "
                                    f"in {file_path.name}"
                                )
                    except Exception as e:
                        logger.error(f"Error processing {file_path}: {e}")
        else:
            # Sequential processing
            for file_path in summary_files:
                result = self._process_summary_file(file_path)
                if result:
                    if max_result is None or result.max_force > max_result.max_force:
                        max_result = result
                        logger.info(
                            f"New maximum found: {result.max_force:.2f} "
                            f"in {file_path.name}"
                        )
        
        # Cache the result
        self.max_result_cache = max_result
        
        if max_result:
            logger.info(
                f"Maximum force configuration found: "
                f"{max_result.fe_filename} with force {max_result.max_force:.2f}"
            )
        else:
            logger.warning("No maximum force configuration found")
        
        return max_result
    
    def clear_cache(self):
        """Clear cached maximum result"""
        self.max_result_cache = None
        logger.info("Maximum force cache cleared")
    
    def get_auto_max_configuration(self) -> Optional[Dict[str, Any]]:
        """
        Get configuration for auto-max mode
        
        Returns:
            Configuration dictionary for auto-max mode
        """
        max_result = self.find_maximum_force_configuration()
        
        if not max_result:
            return None
        
        return {
            'configuration': max_result.configuration,
            'max_force': max_result.max_force,
            'force_column': max_result.force_column,
            'fe_filename': max_result.fe_filename,
            'source_file': str(max_result.source_file),
            'auto_max': True
        }


class ModeController:
    """Controls switching between auto-max and manual modes"""
    
    def __init__(self, max_finder: Optional[MaxForceFinder] = None):
        """
        Initialize mode controller
        
        Args:
            max_finder: MaxForceFinder instance
        """
        self.max_finder = max_finder or MaxForceFinder()
        self.current_mode = 'auto'
        self.manual_config: Optional[Dict[str, str]] = None
        self.mode_listeners: List[Callable[[str, Dict], None]] = []
        
    def add_mode_listener(self, callback: Callable[[str, Dict], None]):
        """Add listener for mode changes"""
        self.mode_listeners.append(callback)
    
    def _notify_listeners(self, mode: str, config: Dict):
        """Notify listeners of mode change"""
        for listener in self.mode_listeners:
            try:
                listener(mode, config)
            except Exception as e:
                logger.error(f"Error notifying mode listener: {e}")
    
    def switch_to_auto(self) -> Dict[str, Any]:
        """
        Switch to auto-max mode
        
        Returns:
            Auto-max configuration
        """
        self.current_mode = 'auto'
        auto_config = self.max_finder.get_auto_max_configuration()
        
        if auto_config:
            logger.info("Switched to auto-max mode")
            self._notify_listeners('auto', auto_config)
            return auto_config
        else:
            logger.warning("No auto-max configuration available")
            return {'auto_max': True, 'configuration': {}}
    
    def switch_to_manual(self, config: Optional[Dict[str, str]] = None) -> Dict[str, Any]:
        """
        Switch to manual mode
        
        Args:
            config: Manual configuration to use
            
        Returns:
            Manual configuration
        """
        self.current_mode = 'manual'
        
        # If no config provided, use previous or auto-max as default
        if config is None:
            if self.manual_config:
                config = self.manual_config
            else:
                auto_config = self.max_finder.get_auto_max_configuration()
                if auto_config:
                    config = auto_config['configuration']
                else:
                    config = {
                        'vessel_type': 'fsts',
                        'loading_condition': 'l015',
                        'tide_level': 'hwl',
                        'return_period': '0100yr',
                        'wave_direction': '000deg',
                        'analysis_type': '03c'
                    }
        
        self.manual_config = config
        
        result = {
            'auto_max': False,
            'configuration': config
        }
        
        logger.info("Switched to manual mode")
        self._notify_listeners('manual', result)
        
        return result
    
    def get_current_configuration(self) -> Dict[str, Any]:
        """
        Get current configuration based on mode
        
        Returns:
            Current configuration
        """
        if self.current_mode == 'auto':
            return self.switch_to_auto()
        else:
            return self.switch_to_manual(self.manual_config)
    
    def update_manual_configuration(self, config: Dict[str, str]):
        """
        Update manual configuration
        
        Args:
            config: New manual configuration
        """
        if self.current_mode == 'manual':
            self.manual_config = config
            self._notify_listeners('manual', {'auto_max': False, 'configuration': config})
            logger.info("Manual configuration updated")
    
    def refresh_auto_max(self):
        """Refresh auto-max configuration by clearing cache"""
        self.max_finder.clear_cache()
        if self.current_mode == 'auto':
            self.switch_to_auto()


# Example usage
if __name__ == "__main__":
    # Configure logging
    logging.basicConfig(level=logging.INFO)
    
    # Create max force finder
    finder = MaxForceFinder()
    
    # Find maximum force configuration
    max_result = finder.find_maximum_force_configuration()
    
    if max_result:
        print(f"Maximum force: {max_result.max_force}")
        print(f"Configuration: {max_result.configuration}")
        print(f"Source file: {max_result.source_file.name}")
        print(f"Force column: {max_result.force_column}")
    
    # Test mode controller
    controller = ModeController(finder)
    
    # Get auto-max configuration
    auto_config = controller.switch_to_auto()
    print(f"Auto-max config: {auto_config}")
    
    # Switch to manual
    manual_config = controller.switch_to_manual()
    print(f"Manual config: {manual_config}")
    
    # Update manual configuration
    controller.update_manual_configuration({
        'vessel_type': 'flng',
        'loading_condition': 'l095',
        'tide_level': 'mwl',
        'return_period': '0010yr',
        'wave_direction': '045deg',
        'analysis_type': '04a'
    })
    
    current = controller.get_current_configuration()
    print(f"Current config: {current}")