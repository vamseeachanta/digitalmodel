"""
Excel Collation Reader for OrcaFlex Dashboard
Reads wlng_dm_fsts*.xlsx files to extract configuration and file patterns
"""

import os
import re
import json
import logging
from typing import Dict, List, Optional, Any
from pathlib import Path
import pandas as pd
import openpyxl
from datetime import datetime

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


class ExcelCollationReader:
    """Reads and parses Excel collation files for dynamic UI configuration"""
    
    def __init__(self, base_path: str = r"D:\1522\ctr7\orcaflex\rev_a08"):
        self.base_path = Path(base_path)
        self.collated_path = self.base_path / "postproc" / "collated"
        self.config = {}
        self.cache = {}
        self.last_refresh = None
        
    def find_collation_files(self) -> List[Path]:
        """Find all wlng_dm_fsts*.xlsx files in the collated directory"""
        pattern = "wlng_dm_fsts*.xlsx"
        files = []
        
        if self.collated_path.exists():
            files = list(self.collated_path.glob(pattern.lower())) + \
                   list(self.collated_path.glob(pattern.upper()))
        
        logger.info(f"Found {len(files)} collation files")
        return files
    
    def parse_filename_pattern(self, filename: str) -> Dict[str, str]:
        """Extract metadata from Excel filename"""
        # Pattern: WLNG_dm_fsts_lngc_fsts_l015_hwl_125km3_l100_pb.xlsx
        # Components: prefix_vessel1_vessel2_loading_tide_capacity_loading2_berthing
        
        name = Path(filename).stem.upper()
        parts = name.split('_')
        
        metadata = {
            'filename': filename,
            'vessel_type': 'UNKNOWN',
            'fst1_loading': None,
            'fst2_loading': None,
            'tide': None,
            'lngc_capacity': None,
            'lngc_loading': None,
            'berthing': None
        }
        
        # Detect vessel type - these files contain both LNGC and FSTS configurations
        if 'LNGC' in name and 'FSTS' in name:
            metadata['vessel_type'] = 'COMBINED'  # Files contain both vessel types
        elif 'FSTS' in name:
            metadata['vessel_type'] = 'FST'
        elif 'LNGC' in name:
            metadata['vessel_type'] = 'LNGC'
            
        # Extract FST loading (L015 = 15%, L095 = 95%)
        fst_pattern = r'L(\d{3})'
        fst_matches = re.findall(fst_pattern, name)
        if fst_matches:
            metadata['fst1_loading'] = f"{int(fst_matches[0])}%"
            if len(fst_matches) > 1:
                metadata['fst2_loading'] = f"{int(fst_matches[1])}%"
        
        # Extract tide (HWL/LWL)
        if 'HWL' in name:
            metadata['tide'] = 'HWL'
        elif 'LWL' in name:
            metadata['tide'] = 'LWL'
            
        # Extract LNGC capacity
        capacity_pattern = r'(\d+)KM3'
        capacity_match = re.search(capacity_pattern, name)
        if capacity_match:
            metadata['lngc_capacity'] = f"{capacity_match.group(1)},000 m³"
            
        # Extract LNGC loading
        if 'L000' in name:
            metadata['lngc_loading'] = 'Ballast'
        elif 'L100' in name:
            metadata['lngc_loading'] = 'Laden'
        elif 'L050' in name:
            metadata['lngc_loading'] = 'Partial'
            
        # Extract berthing (PB/SB)
        if name.endswith('_PB'):
            metadata['berthing'] = 'Port'
        elif name.endswith('_SB'):
            metadata['berthing'] = 'Starboard'
            
        return metadata
    
    def parse_excel_config(self, excel_path: Path) -> Dict[str, Any]:
        """Parse Excel file to extract configuration"""
        config = {
            'file_patterns': [],
            'parameters': {},
            'case_matrix': [],
            'metadata': self.parse_filename_pattern(excel_path.name)
        }
        
        try:
            # Read Excel file
            excel_file = pd.ExcelFile(excel_path)
            
            # Parse inputs worksheet if exists
            if 'inputs' in excel_file.sheet_names:
                inputs_df = pd.read_excel(excel_file, sheet_name='inputs')
                config['parameters'] = self._parse_inputs_worksheet(inputs_df)
                
            # Parse r_inputs worksheet if exists
            if 'r_inputs' in excel_file.sheet_names:
                r_inputs_df = pd.read_excel(excel_file, sheet_name='r_inputs')
                config['file_patterns'] = self._parse_r_inputs_worksheet(r_inputs_df)
                
            # Parse case matrix if exists
            if 'case_matrix' in excel_file.sheet_names:
                matrix_df = pd.read_excel(excel_file, sheet_name='case_matrix')
                config['case_matrix'] = self._parse_case_matrix(matrix_df)
                
        except Exception as e:
            logger.error(f"Error parsing {excel_path}: {e}")
            
        return config
    
    def _parse_inputs_worksheet(self, df: pd.DataFrame) -> Dict[str, Any]:
        """Extract UI parameters from inputs worksheet"""
        parameters = {
            'vessel_options': [],
            'loading_options': [],
            'environment_options': [],
            'mooring_options': []
        }
        
        try:
            # Look for specific columns that define options
            if 'Parameter' in df.columns and 'Value' in df.columns:
                for _, row in df.iterrows():
                    param = str(row['Parameter']).strip()
                    value = str(row['Value']).strip()
                    
                    if 'vessel' in param.lower():
                        parameters['vessel_options'].append(value)
                    elif 'loading' in param.lower():
                        parameters['loading_options'].append(value)
                    elif 'environment' in param.lower():
                        parameters['environment_options'].append(value)
                    elif 'mooring' in param.lower():
                        parameters['mooring_options'].append(value)
                        
        except Exception as e:
            logger.warning(f"Could not parse inputs worksheet: {e}")
            
        return parameters
    
    def _parse_r_inputs_worksheet(self, df: pd.DataFrame) -> List[str]:
        """Extract file patterns from r_inputs worksheet"""
        patterns = []
        
        try:
            # Look for file paths in the worksheet
            for col in df.columns:
                if 'file' in col.lower() or 'path' in col.lower():
                    for value in df[col].dropna():
                        if isinstance(value, str) and '.csv' in value.lower():
                            # Extract just the filename pattern
                            filename = Path(value).name
                            pattern = self._create_pattern_from_filename(filename)
                            if pattern and pattern not in patterns:
                                patterns.append(pattern)
                                
        except Exception as e:
            logger.warning(f"Could not parse r_inputs worksheet: {e}")
            
        return patterns
    
    def _parse_case_matrix(self, df: pd.DataFrame) -> List[Dict]:
        """Parse case matrix to understand analysis combinations"""
        cases = []
        
        try:
            for _, row in df.iterrows():
                case = row.to_dict()
                # Clean up NaN values
                case = {k: v for k, v in case.items() if pd.notna(v)}
                if case:
                    cases.append(case)
                    
        except Exception as e:
            logger.warning(f"Could not parse case matrix: {e}")
            
        return cases
    
    def _create_pattern_from_filename(self, filename: str) -> Optional[str]:
        """Create a regex pattern from a filename"""
        # Convert specific filename to pattern
        # e.g., FST1_E_FST2_E_env01_tide01_hdg000.csv -> FST1_[EF]_FST2_[EF]_env\d+_tide\d+_hdg\d+\.csv
        
        pattern = filename
        
        # Replace loading indicators
        pattern = re.sub(r'_E_', r'_[EF]_', pattern)
        pattern = re.sub(r'_F_', r'_[EF]_', pattern)
        
        # Replace environment numbers
        pattern = re.sub(r'env\d+', r'env\\d+', pattern)
        pattern = re.sub(r'tide\d+', r'tide\\d+', pattern)
        pattern = re.sub(r'hdg\d+', r'hdg\\d+', pattern)
        
        return pattern
    
    def get_vessel_types(self) -> List[Dict[str, str]]:
        """Get available vessel types from all Excel files"""
        vessel_types = set()
        
        for file in self.find_collation_files():
            metadata = self.parse_filename_pattern(file.name)
            vessel_types.add(metadata['vessel_type'])
            
        # Convert to structured format
        types = []
        
        # Since all files contain both LNGC and FSTS data, we provide both options
        if 'COMBINED' in vessel_types or 'FST' in vessel_types or 'FSTS' in str(vessel_types):
            types.append({
                'value': 'FST',
                'label': 'FST (Floating Storage Tank)',
                'description': 'Twin FST configuration with 15% or 95% LNG loading'
            })
        
        if 'COMBINED' in vessel_types or 'LNGC' in vessel_types:
            types.append({
                'value': 'LNGC',
                'label': 'LNGC (LNG Carrier)',
                'description': 'LNG Carrier with 125,000 or 180,000 m³ capacity'
            })
            
        # If no specific types found, provide both by default
        if not types:
            types = [
                {
                    'value': 'FST',
                    'label': 'FST (Floating Storage Tank)',
                    'description': 'Twin FST configuration with 15% or 95% LNG loading'
                },
                {
                    'value': 'LNGC',
                    'label': 'LNGC (LNG Carrier)',
                    'description': 'LNG Carrier with 125,000 or 180,000 m³ capacity'
                }
            ]
            
        if len(types) > 1:
            types.append({
                'value': 'CUSTOM',
                'label': 'Custom',
                'description': 'Custom vessel configuration from Excel'
            })
            
        return types
    
    def get_configuration_for_vessel(self, vessel_type: str) -> Dict[str, Any]:
        """Get UI configuration for specific vessel type"""
        configs = []
        
        for file in self.find_collation_files():
            metadata = self.parse_filename_pattern(file.name)
            # Handle COMBINED type files that contain both vessel configurations
            if metadata['vessel_type'] == 'COMBINED' or vessel_type in metadata['vessel_type']:
                config = self.parse_excel_config(file)
                configs.append(config)
                
        # Merge configurations
        merged = {
            'vessel_type': vessel_type,
            'fst_loadings': [],
            'lngc_capacities': [],
            'lngc_loadings': [],
            'berthings': [],
            'tides': [],
            'file_patterns': []
        }
        
        for config in configs:
            meta = config['metadata']
            
            if meta['fst1_loading'] and meta['fst1_loading'] not in merged['fst_loadings']:
                merged['fst_loadings'].append(meta['fst1_loading'])
                
            if meta['lngc_capacity'] and meta['lngc_capacity'] not in merged['lngc_capacities']:
                merged['lngc_capacities'].append(meta['lngc_capacity'])
                
            if meta['lngc_loading'] and meta['lngc_loading'] not in merged['lngc_loadings']:
                merged['lngc_loadings'].append(meta['lngc_loading'])
                
            if meta['berthing'] and meta['berthing'] not in merged['berthings']:
                merged['berthings'].append(meta['berthing'])
                
            if meta['tide'] and meta['tide'] not in merged['tides']:
                merged['tides'].append(meta['tide'])
                
            merged['file_patterns'].extend(config.get('file_patterns', []))
            
        return merged
    
    def refresh_cache(self):
        """Refresh the configuration cache"""
        self.cache = {}
        self.last_refresh = datetime.now()
        
        # Pre-load configurations for all vessel types
        for vessel_type in ['FST', 'LNGC', 'LNGC_FST']:
            self.cache[vessel_type] = self.get_configuration_for_vessel(vessel_type)
            
        logger.info(f"Cache refreshed at {self.last_refresh}")
        
    def get_all_configurations(self) -> Dict[str, Any]:
        """Get complete configuration for all vessel types"""
        if not self.cache or (self.last_refresh and 
                            (datetime.now() - self.last_refresh).seconds > 900):  # 15 min cache
            self.refresh_cache()
            
        return {
            'vessel_types': self.get_vessel_types(),
            'configurations': self.cache,
            'last_refresh': self.last_refresh.isoformat() if self.last_refresh else None
        }


# Default configuration fallback
DEFAULT_CONFIG = {
    'vessel_types': [
        {'value': 'FST', 'label': 'FST (Floating Storage Tank)'},
        {'value': 'LNGC', 'label': 'LNGC (LNG Carrier)'}
    ],
    'fst': {
        'loadings': ['15% LNG', '95% LNG'],
        'mooring': ['Intact', 'Damaged']
    },
    'lngc': {
        'capacities': ['125,000 m³', '180,000 m³'],
        'loadings': ['Ballast (10%)', 'Partial (50%)', 'Laden (95%)'],
        'berthings': ['Port', 'Starboard']
    },
    'environment': {
        'types': ['Colinear', 'Non-colinear'],
        'return_periods': ['5yr', '10yr', '100yr', '1000yr']
    }
}


def get_excel_config(base_path: Optional[str] = None) -> Dict[str, Any]:
    """Main entry point to get Excel-based configuration"""
    try:
        reader = ExcelCollationReader(base_path) if base_path else ExcelCollationReader()
        config = reader.get_all_configurations()
        
        if not config['configurations']:
            logger.warning("No Excel configurations found, using defaults")
            return DEFAULT_CONFIG
            
        return config
        
    except Exception as e:
        logger.error(f"Failed to read Excel configuration: {e}")
        return DEFAULT_CONFIG


if __name__ == "__main__":
    # Test the Excel reader
    config = get_excel_config()
    print(json.dumps(config, indent=2, default=str))