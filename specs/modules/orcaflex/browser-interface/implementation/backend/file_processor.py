"""
OrcaFlex CSV File Processor
Handles file discovery, reading, and parallel processing
"""

import os
import glob
import pandas as pd
from pathlib import Path
from typing import List, Dict, Any, Optional, Tuple
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor
import json
from datetime import datetime
import re


class FileProcessor:
    """Processes OrcaFlex CSV files with pattern matching and parallel processing"""
    
    def __init__(self, base_path: str):
        self.base_path = Path(base_path)
        self.cache = {}
        
    def search_files(self, pattern: str = "dm_*_strut_dyn.csv") -> List[Path]:
        """Search for files matching the given pattern"""
        search_path = self.base_path / pattern
        files = glob.glob(str(search_path))
        return [Path(f) for f in sorted(files)]
    
    def read_csv_file(self, filepath: Path) -> pd.DataFrame:
        """Read a single CSV file into a DataFrame"""
        try:
            df = pd.read_csv(filepath)
            # Add source file information
            df['source_file'] = filepath.name
            df['source_path'] = str(filepath)
            return df
        except Exception as e:
            print(f"Error reading {filepath}: {e}")
            return pd.DataFrame()
    
    def read_multiple_csvs_parallel(self, filepaths: List[Path], max_workers: int = 4) -> Dict[str, pd.DataFrame]:
        """Read multiple CSV files in parallel"""
        dataframes = {}
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {executor.submit(self.read_csv_file, fp): fp for fp in filepaths}
            
            for future in futures:
                filepath = futures[future]
                try:
                    df = future.result()
                    if not df.empty:
                        # Use filename without extension as key to ensure uniqueness
                        key = filepath.stem  # This gives filename without .csv
                        dataframes[key] = df
                except Exception as e:
                    print(f"Error processing {filepath}: {e}")
                    
        return dataframes
    
    def extract_strut_number(self, filename: str) -> str:
        """Extract strut number from filename"""
        # Pattern: look for 'strut' followed by numbers or 'Strut' followed by numbers
        pattern = r'[Ss]trut(\d+)'
        match = re.search(pattern, filename)
        if match:
            return match.group(1)
        
        # Try pattern like "090deg_strut_dyn" - extract the degree as strut identifier
        pattern = r'(\d{3})deg_strut'
        match = re.search(pattern, filename)
        if match:
            return f"deg_{match.group(1)}"
        
        # Fallback: try to extract any number after underscore
        pattern = r'_(\d+)_'
        match = re.search(pattern, filename)
        if match:
            return match.group(1)
        
        # Final fallback: use part of filename as identifier
        parts = filename.replace('.csv', '').split('_')
        if len(parts) > 3:
            return '_'.join(parts[-3:-1])  # Use last parts before 'dyn'
        
        return "unknown"


class TensionAnalyzer:
    """Analyzes effective tension data from strut CSV files"""
    
    def __init__(self):
        self.results = {}
        
    def analyze_tensions(self, dataframes: Dict[str, pd.DataFrame]) -> Dict[str, Any]:
        """Analyze effective tensions across all struts"""
        analysis_results = {
            'strut_analysis': {},
            'absolute_maximum': None,
            'absolute_minimum': None,
            'summary': {}
        }
        
        all_max_values = []
        all_min_values = []
        
        for strut_name, df in dataframes.items():
            if 'eff_tension' in df.columns:
                min_tension = df['eff_tension'].min()
                max_tension = df['eff_tension'].max()
                
                # Find row with max tension
                max_row_idx = df['eff_tension'].idxmax()
                max_row = df.loc[max_row_idx].to_dict()
                
                # Find row with min tension
                min_row_idx = df['eff_tension'].idxmin()
                min_row = df.loc[min_row_idx].to_dict()
                
                analysis_results['strut_analysis'][strut_name] = {
                    'min_tension': min_tension,
                    'max_tension': max_tension,
                    'min_row': min_row,
                    'max_row': max_row,
                    'source_file': df['source_file'].iloc[0] if 'source_file' in df.columns else 'unknown'
                }
                
                all_max_values.append((max_tension, strut_name, max_row))
                all_min_values.append((min_tension, strut_name, min_row))
        
        # Find absolute maximum and minimum
        if all_max_values:
            abs_max = max(all_max_values, key=lambda x: x[0])
            analysis_results['absolute_maximum'] = {
                'value': abs_max[0],
                'strut': abs_max[1],
                'row_data': abs_max[2],
                'fe_filename': abs_max[2].get('fe_filename', ''),
                'fe_filename_stem': abs_max[2].get('fe_filename_stem', '')
            }
            
        if all_min_values:
            abs_min = min(all_min_values, key=lambda x: x[0])
            analysis_results['absolute_minimum'] = {
                'value': abs_min[0],
                'strut': abs_min[1],
                'row_data': abs_min[2]
            }
            
        # Create summary
        analysis_results['summary'] = {
            'total_struts_analyzed': len(dataframes),
            'timestamp': datetime.now().isoformat(),
            'absolute_max_tension': abs_max[0] if all_max_values else None,
            'absolute_min_tension': abs_min[0] if all_min_values else None
        }
        
        return analysis_results


class MetadataExtractor:
    """Extracts metadata from OrcaFlex filenames"""
    
    def __init__(self):
        self.patterns = {
            'lng_loading': {
                'fsts_l015': '15% LNG',
                'fsts_l095': '95% LNG',
                'fsts_03c_l015': '03C 15% LNG',
                'fsts_03c_l095': '03C 95% LNG'
            },
            'tide_level': {
                'hwl': 'HHWL (Highest High Water Level)',
                'lwl': 'LLWL (Lowest Low Water Level)',
                'mwl': 'MWL (Mean Water Level)'
            },
            'environment_type': {
                'ncl': 'Non-colinear',
                'cl': 'Colinear'
            }
        }
        
    def extract_metadata(self, filename: str, full_path: str = '') -> Dict[str, Any]:
        """Extract all metadata from filename and path"""
        metadata = {
            'filename': filename,
            'full_path': full_path,
            'loading_condition': None,
            'lng_loading': None,
            'tide_level': None,
            'environment_type': None,
            'direction': None,
            'parsed_components': {}
        }
        
        # Extract loading condition from path
        if full_path:
            path_parts = Path(full_path).parts
            if 'csv' in path_parts:
                csv_idx = path_parts.index('csv')
                if csv_idx + 1 < len(path_parts):
                    metadata['loading_condition'] = path_parts[csv_idx + 1]
        
        # Convert filename to lowercase for matching
        filename_lower = filename.lower()
        
        # Extract LNG loading
        for pattern, description in self.patterns['lng_loading'].items():
            if pattern in filename_lower:
                metadata['lng_loading'] = description
                metadata['parsed_components']['lng'] = pattern
                break
        
        # Extract tide level
        for pattern, description in self.patterns['tide_level'].items():
            if pattern in filename_lower:
                metadata['tide_level'] = description
                metadata['parsed_components']['tide'] = pattern
                break
        
        # Extract environment type
        for pattern, description in self.patterns['environment_type'].items():
            # Look for pattern with word boundaries
            if f'_{pattern}_' in filename_lower or f'_{pattern}.' in filename_lower:
                metadata['environment_type'] = description
                metadata['parsed_components']['env'] = pattern
                break
        
        # Extract direction
        direction_match = re.search(r'(\d{3})deg', filename_lower)
        if direction_match:
            metadata['direction'] = f"{direction_match.group(1)} degrees"
            metadata['parsed_components']['direction'] = direction_match.group(0)
        
        return metadata
    
    def extract_from_fe_filename(self, fe_filename: str) -> Dict[str, Any]:
        """Extract metadata specifically from fe_filename field"""
        # fe_filename typically contains the full path
        filename = Path(fe_filename).name if fe_filename else ''
        return self.extract_metadata(filename, fe_filename)


class VerificationLogger:
    """Logs verification results with timestamps for AI learning"""
    
    def __init__(self, verification_dir: str = "verification"):
        self.verification_dir = Path(verification_dir)
        self.verification_dir.mkdir(exist_ok=True)
        
    def log_verification(self, results: Dict[str, Any], user_feedback: Optional[Dict] = None) -> str:
        """Log verification results to timestamped JSON file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"verification_{timestamp}.json"
        filepath = self.verification_dir / filename
        
        verification_data = {
            'timestamp': datetime.now().isoformat(),
            'results': results,
            'user_feedback': user_feedback or {},
            'metadata': {
                'version': '1.0',
                'processor': 'OrcaFlexBrowserBackend'
            }
        }
        
        with open(filepath, 'w') as f:
            json.dump(verification_data, f, indent=2, default=str)
            
        return str(filepath)
    
    def load_verification(self, filepath: str) -> Dict[str, Any]:
        """Load verification data from JSON file"""
        with open(filepath, 'r') as f:
            return json.load(f)
    
    def get_latest_verification(self) -> Optional[Dict[str, Any]]:
        """Get the most recent verification file"""
        files = list(self.verification_dir.glob("verification_*.json"))
        if files:
            latest = max(files, key=lambda f: f.stat().st_mtime)
            return self.load_verification(str(latest))
        return None