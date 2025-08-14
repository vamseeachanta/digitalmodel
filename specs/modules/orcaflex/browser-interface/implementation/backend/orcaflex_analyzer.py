"""
Enhanced OrcaFlex Data Analyzer
Handles both time-series and summary CSV formats
"""

import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, Any, List, Optional, Tuple
import re
from datetime import datetime


class OrcaFlexAnalyzer:
    """Analyzes OrcaFlex CSV files with multiple format support"""
    
    def __init__(self):
        self.results = {}
        self.format_type = None
        
    def detect_format(self, df: pd.DataFrame) -> str:
        """Detect the CSV format type"""
        columns = df.columns.tolist()
        
        # Check for summary format (multiple strut columns with min/max)
        if any('Strut' in col and ('_min' in col or '_max' in col) for col in columns):
            return 'summary'
        
        # Check for time-series format (single eff_tension column)
        if 'eff_tension' in columns:
            return 'timeseries'
        
        # Check for other tension-related columns
        tension_cols = [col for col in columns if 'tension' in col.lower()]
        if tension_cols:
            return 'mixed'
        
        return 'unknown'
    
    def extract_strut_tensions_summary(self, df: pd.DataFrame) -> Dict[str, Dict[str, float]]:
        """Extract tensions from summary format (StrutN_Body_eff_tension_min/max)"""
        strut_data = {}
        
        # Find all strut-related columns
        strut_pattern = r'(Strut\d+).*eff_tension_(min|max)'
        
        for col in df.columns:
            match = re.match(strut_pattern, col)
            if match:
                strut_name = match.group(1).lower()  # e.g., 'strut1'
                metric = match.group(2)  # 'min' or 'max'
                
                if strut_name not in strut_data:
                    strut_data[strut_name] = {
                        'min_tension': float('inf'),
                        'max_tension': float('-inf')
                    }
                
                # Get the actual min/max across ALL rows
                values = df[col].dropna()
                if not values.empty:
                    if metric == 'min':
                        # For min columns, find the minimum value
                        strut_data[strut_name]['min_tension'] = min(
                            strut_data[strut_name]['min_tension'],
                            float(values.min())
                        )
                    else:  # max
                        # For max columns, find the maximum value
                        strut_data[strut_name]['max_tension'] = max(
                            strut_data[strut_name]['max_tension'],
                            float(values.max())
                        )
        
        return strut_data
    
    def extract_strut_tensions_timeseries(self, df: pd.DataFrame) -> Dict[str, Dict[str, float]]:
        """Extract tensions from time-series format (eff_tension column)"""
        if 'eff_tension' not in df.columns:
            return {}
        
        # Extract strut identifier from source file if available
        strut_name = 'strut_unknown'
        if 'source_file' in df.columns:
            filename = df['source_file'].iloc[0]
            # Try to extract strut number
            match = re.search(r'strut(\d+)', filename, re.IGNORECASE)
            if match:
                strut_name = f"strut{match.group(1)}"
        
        return {
            strut_name: {
                'min_tension': float(df['eff_tension'].min()),
                'max_tension': float(df['eff_tension'].max())
            }
        }
    
    def analyze_dataframe(self, df: pd.DataFrame, source_name: str = '') -> Dict[str, Any]:
        """Analyze a single dataframe regardless of format"""
        format_type = self.detect_format(df)
        
        result = {
            'format': format_type,
            'source': source_name,
            'rows': len(df),
            'columns': len(df.columns),
            'strut_data': {}
        }
        
        if format_type == 'summary':
            result['strut_data'] = self.extract_strut_tensions_summary(df)
        elif format_type == 'timeseries':
            result['strut_data'] = self.extract_strut_tensions_timeseries(df)
        elif format_type == 'mixed':
            # Try both approaches
            summary_data = self.extract_strut_tensions_summary(df)
            timeseries_data = self.extract_strut_tensions_timeseries(df)
            result['strut_data'] = {**summary_data, **timeseries_data}
        
        # Add metadata - but don't add here, we'll add it when we find the max
        # We need to track which row has the maximum value
        result['df_reference'] = df  # Keep reference to dataframe for later
        
        return result
    
    def analyze_multiple_dataframes(self, dataframes: Dict[str, pd.DataFrame]) -> Dict[str, Any]:
        """Analyze multiple dataframes and combine results"""
        all_strut_data = {}
        file_analyses = {}
        
        for name, df in dataframes.items():
            analysis = self.analyze_dataframe(df, name)
            file_analyses[name] = analysis
            
            # Merge strut data
            for strut_name, strut_tensions in analysis['strut_data'].items():
                if strut_name not in all_strut_data:
                    all_strut_data[strut_name] = {
                        'min_tension': float('inf'),
                        'max_tension': float('-inf'),
                        'source_files': []
                    }
                
                if 'min_tension' in strut_tensions:
                    all_strut_data[strut_name]['min_tension'] = min(
                        all_strut_data[strut_name]['min_tension'],
                        strut_tensions['min_tension']
                    )
                
                if 'max_tension' in strut_tensions:
                    all_strut_data[strut_name]['max_tension'] = max(
                        all_strut_data[strut_name]['max_tension'],
                        strut_tensions['max_tension']
                    )
                
                all_strut_data[strut_name]['source_files'].append(name)
        
        # Find absolute maximum and minimum
        abs_max = None
        abs_min = None
        
        for strut_name, data in all_strut_data.items():
            if data['max_tension'] != float('-inf'):
                if abs_max is None or data['max_tension'] > abs_max['value']:
                    abs_max = {
                        'value': data['max_tension'],
                        'strut': strut_name,
                        'source_files': data['source_files']
                    }
            
            if data['min_tension'] != float('inf'):
                if abs_min is None or data['min_tension'] < abs_min['value']:
                    abs_min = {
                        'value': data['min_tension'],
                        'strut': strut_name,
                        'source_files': data['source_files']
                    }
        
        # Add fe_filename from the actual row with the maximum value
        if abs_max and abs_max['source_files']:
            # Find which file and strut has the maximum
            for source in abs_max['source_files']:
                if source in file_analyses:
                    df_ref = file_analyses[source].get('df_reference')
                    if df_ref is not None and 'fe_filename' in df_ref.columns:
                        # Find the column that contains this maximum
                        critical_strut = abs_max['strut']
                        strut_num = critical_strut.replace('strut', '')
                        max_col = f'Strut{strut_num}_Body_eff_tension_max'
                        
                        if max_col in df_ref.columns:
                            # Find the row with the maximum value
                            max_val = df_ref[max_col].max()
                            if abs(max_val - abs_max['value']) < 0.01:  # Match found
                                max_row = df_ref[df_ref[max_col] == max_val]
                                if not max_row.empty:
                                    abs_max['fe_filename'] = max_row['fe_filename'].iloc[0]
                                    if 'fe_filename_stem' in max_row.columns:
                                        abs_max['fe_filename_stem'] = max_row['fe_filename_stem'].iloc[0]
                                    break
        
        return {
            'strut_analysis': all_strut_data,
            'file_analyses': file_analyses,
            'absolute_maximum': abs_max,
            'absolute_minimum': abs_min,
            'summary': {
                'total_files_analyzed': len(dataframes),
                'total_struts_found': len(all_strut_data),
                'timestamp': datetime.now().isoformat(),
                'absolute_max_tension': abs_max['value'] if abs_max else None,
                'absolute_min_tension': abs_min['value'] if abs_min else None,
                'formats_detected': list(set(fa['format'] for fa in file_analyses.values()))
            }
        }
    
    def get_strut_summary_table(self, results: Dict[str, Any]) -> pd.DataFrame:
        """Create a summary dataframe of all strut tensions"""
        rows = []
        
        for strut_name, data in results['strut_analysis'].items():
            row = {
                'strut': strut_name,
                'min_tension': data.get('min_tension', np.nan),
                'max_tension': data.get('max_tension', np.nan),
                'range': data.get('max_tension', 0) - data.get('min_tension', 0),
                'source_count': len(data.get('source_files', []))
            }
            rows.append(row)
        
        df = pd.DataFrame(rows)
        if not df.empty:
            df = df.sort_values('max_tension', ascending=False)
        
        return df