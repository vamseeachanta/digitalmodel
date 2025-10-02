"""
Export utilities for passing ship forces calculation results.

Provides functionality to export calculation results to various formats
including JSON, CSV, and structured data formats for integration with
other marine analysis tools.
"""

import json
import csv
from pathlib import Path
from typing import Dict, Any, List, Union, Optional
import numpy as np
from datetime import datetime
import pandas as pd


class ResultExporter:
    """Handle export of calculation results to various formats."""
    
    def __init__(self, results: Union[Dict[str, Any], List[Dict[str, Any]]]):
        """
        Initialize exporter with results.
        
        Args:
            results: Calculation results (single or multiple)
        """
        self.results = results
        self.timestamp = datetime.now().isoformat()
    
    def to_json(self, filepath: Union[str, Path], indent: int = 2) -> None:
        """
        Export results to JSON format.
        
        Args:
            filepath: Output file path
            indent: JSON indentation level
        """
        output_data = {
            'metadata': {
                'timestamp': self.timestamp,
                'version': '1.0.0',
                'module': 'passing_ship_forces'
            },
            'results': self.results
        }
        
        # Convert numpy arrays to lists for JSON serialization
        def convert_numpy(obj):
            if isinstance(obj, np.ndarray):
                return obj.tolist()
            elif isinstance(obj, (np.integer, np.int32, np.int64)):
                return int(obj)
            elif isinstance(obj, (np.floating, np.float32, np.float64)):
                return float(obj)
            elif isinstance(obj, dict):
                return {k: convert_numpy(v) for k, v in obj.items()}
            elif isinstance(obj, list):
                return [convert_numpy(item) for item in obj]
            return obj
        
        output_data = convert_numpy(output_data)
        
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        with open(filepath, 'w') as f:
            json.dump(output_data, f, indent=indent)
    
    def to_csv(self, filepath: Union[str, Path]) -> None:
        """
        Export results to CSV format.
        
        Args:
            filepath: Output file path
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        # Flatten results for CSV export
        if isinstance(self.results, dict):
            # Single result
            rows = [self._flatten_dict(self.results)]
        elif isinstance(self.results, list):
            # Multiple results
            rows = [self._flatten_dict(r) for r in self.results]
        else:
            raise ValueError("Results must be dict or list of dicts")
        
        if rows:
            # Write CSV
            with open(filepath, 'w', newline='') as f:
                writer = csv.DictWriter(f, fieldnames=rows[0].keys())
                writer.writeheader()
                writer.writerows(rows)
    
    def to_dataframe(self) -> pd.DataFrame:
        """
        Convert results to pandas DataFrame.
        
        Returns:
            DataFrame containing results
        """
        if isinstance(self.results, dict):
            # Single result
            df = pd.DataFrame([self._flatten_dict(self.results)])
        elif isinstance(self.results, list):
            # Multiple results
            df = pd.DataFrame([self._flatten_dict(r) for r in self.results])
        else:
            raise ValueError("Results must be dict or list of dicts")
        
        return df
    
    def to_excel(self, filepath: Union[str, Path], sheet_name: str = 'Results') -> None:
        """
        Export results to Excel format.
        
        Args:
            filepath: Output file path
            sheet_name: Excel sheet name
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        df = self.to_dataframe()
        
        with pd.ExcelWriter(filepath, engine='openpyxl') as writer:
            df.to_excel(writer, sheet_name=sheet_name, index=False)
            
            # Add metadata sheet
            metadata_df = pd.DataFrame([
                {'Property': 'Timestamp', 'Value': self.timestamp},
                {'Property': 'Version', 'Value': '1.0.0'},
                {'Property': 'Module', 'Value': 'passing_ship_forces'},
                {'Property': 'Records', 'Value': len(df)}
            ])
            metadata_df.to_excel(writer, sheet_name='Metadata', index=False)
    
    def to_orcaflex_format(self, filepath: Union[str, Path]) -> None:
        """
        Export results in OrcaFlex-compatible format.
        
        Args:
            filepath: Output file path
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        # Format for OrcaFlex constraint forces
        orcaflex_data = {
            'ForceType': 'PassingShip',
            'TimeHistories': self._create_time_histories(),
            'Units': {
                'Force': 'N',
                'Moment': 'N.m',
                'Distance': 'm'
            }
        }
        
        with open(filepath, 'w') as f:
            json.dump(orcaflex_data, f, indent=2)
    
    def to_aqwa_format(self, filepath: Union[str, Path]) -> None:
        """
        Export results in AQWA-compatible format.
        
        Args:
            filepath: Output file path
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        # Format for AQWA external forces
        with open(filepath, 'w') as f:
            f.write("* AQWA External Forces - Passing Ship Effects\n")
            f.write(f"* Generated: {self.timestamp}\n")
            f.write("*\n")
            
            if isinstance(self.results, dict):
                results_list = [self.results]
            else:
                results_list = self.results
            
            for i, result in enumerate(results_list, 1):
                f.write(f"* Load Case {i}\n")
                f.write(f"FORC {result.get('surge_force', 0):.6E} ")
                f.write(f"{result.get('sway_force', 0):.6E} ")
                f.write(f"{result.get('heave_force', 0):.6E}\n")
                f.write(f"MOMT {result.get('roll_moment', 0):.6E} ")
                f.write(f"{result.get('pitch_moment', 0):.6E} ")
                f.write(f"{result.get('yaw_moment', 0):.6E}\n")
                f.write("*\n")
    
    def to_summary_report(self, filepath: Union[str, Path]) -> None:
        """
        Generate a summary report in markdown format.
        
        Args:
            filepath: Output file path
        """
        filepath = Path(filepath)
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        with open(filepath, 'w') as f:
            f.write("# Passing Ship Forces Calculation Report\n\n")
            f.write(f"**Generated:** {self.timestamp}\n\n")
            
            if isinstance(self.results, dict):
                # Single result
                self._write_single_result(f, self.results)
            elif isinstance(self.results, list):
                # Multiple results
                f.write(f"## Summary of {len(self.results)} Calculations\n\n")
                
                # Statistics
                surge_forces = [r.get('surge_force', 0) for r in self.results]
                sway_forces = [r.get('sway_force', 0) for r in self.results]
                yaw_moments = [r.get('yaw_moment', 0) for r in self.results]
                
                f.write("### Force Statistics\n\n")
                f.write("| Force Component | Min | Max | Mean | Std Dev |\n")
                f.write("|-----------------|-----|-----|------|--------|\n")
                
                for name, values in [
                    ('Surge Force (N)', surge_forces),
                    ('Sway Force (N)', sway_forces),
                    ('Yaw Moment (N·m)', yaw_moments)
                ]:
                    if values:
                        f.write(f"| {name} | {min(values):.2f} | {max(values):.2f} | ")
                        f.write(f"{np.mean(values):.2f} | {np.std(values):.2f} |\n")
                
                f.write("\n### Individual Results\n\n")
                for i, result in enumerate(self.results, 1):
                    f.write(f"#### Calculation {i}\n")
                    self._write_single_result(f, result)
    
    def _flatten_dict(self, d: Dict[str, Any], parent_key: str = '') -> Dict[str, Any]:
        """
        Flatten nested dictionary for CSV export.
        
        Args:
            d: Dictionary to flatten
            parent_key: Parent key for nested items
            
        Returns:
            Flattened dictionary
        """
        items = []
        for k, v in d.items():
            new_key = f"{parent_key}_{k}" if parent_key else k
            
            if isinstance(v, dict):
                items.extend(self._flatten_dict(v, new_key).items())
            elif isinstance(v, (list, np.ndarray)):
                # Convert arrays to string representation
                items.append((new_key, str(v)))
            else:
                items.append((new_key, v))
        
        return dict(items)
    
    def _create_time_histories(self) -> Dict[str, List[float]]:
        """
        Create time history data from results.
        
        Returns:
            Time history dictionary
        """
        if isinstance(self.results, dict):
            # Single point - create constant time history
            time = [0.0, 1.0]
            surge = [self.results.get('surge_force', 0)] * 2
            sway = [self.results.get('sway_force', 0)] * 2
            yaw = [self.results.get('yaw_moment', 0)] * 2
        elif isinstance(self.results, list):
            # Multiple points - use as time series
            time = list(range(len(self.results)))
            surge = [r.get('surge_force', 0) for r in self.results]
            sway = [r.get('sway_force', 0) for r in self.results]
            yaw = [r.get('yaw_moment', 0) for r in self.results]
        else:
            time = surge = sway = yaw = []
        
        return {
            'Time': time,
            'SurgeForce': surge,
            'SwayForce': sway,
            'YawMoment': yaw
        }
    
    def _write_single_result(self, f, result: Dict[str, Any]) -> None:
        """
        Write a single result to file.
        
        Args:
            f: File object
            result: Result dictionary
        """
        f.write("\n**Forces and Moments:**\n\n")
        f.write(f"- Surge Force: {result.get('surge_force', 0):.2f} N\n")
        f.write(f"- Sway Force: {result.get('sway_force', 0):.2f} N\n")
        f.write(f"- Yaw Moment: {result.get('yaw_moment', 0):.2f} N·m\n")
        
        if 'metadata' in result:
            f.write("\n**Configuration:**\n\n")
            meta = result['metadata']
            if 'vessel_1' in meta:
                f.write(f"- Vessel 1: {meta['vessel_1']}\n")
            if 'vessel_2' in meta:
                f.write(f"- Vessel 2: {meta['vessel_2']}\n")
            if 'separation' in meta:
                f.write(f"- Separation: {meta['separation']} m\n")
            if 'water_depth' in meta:
                f.write(f"- Water Depth: {meta['water_depth']} m\n")
        
        f.write("\n---\n")


def export_batch_results(
    results: Dict[str, Any],
    output_dir: Union[str, Path],
    formats: List[str] = ['json', 'csv', 'summary']
) -> None:
    """
    Export batch processing results to multiple formats.
    
    Args:
        results: Batch processing results
        output_dir: Output directory
        formats: List of export formats
    """
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    
    timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
    
    # Flatten batch results
    all_results = []
    for config_file, result in results.items():
        if isinstance(result, dict):
            result['config_file'] = config_file
            all_results.append(result)
    
    if not all_results:
        print("No results to export")
        return
    
    exporter = ResultExporter(all_results)
    
    for format_type in formats:
        if format_type == 'json':
            filepath = output_dir / f"batch_results_{timestamp}.json"
            exporter.to_json(filepath)
            print(f"Exported JSON: {filepath}")
        
        elif format_type == 'csv':
            filepath = output_dir / f"batch_results_{timestamp}.csv"
            exporter.to_csv(filepath)
            print(f"Exported CSV: {filepath}")
        
        elif format_type == 'excel':
            filepath = output_dir / f"batch_results_{timestamp}.xlsx"
            exporter.to_excel(filepath)
            print(f"Exported Excel: {filepath}")
        
        elif format_type == 'summary':
            filepath = output_dir / f"batch_summary_{timestamp}.md"
            exporter.to_summary_report(filepath)
            print(f"Exported Summary: {filepath}")
        
        elif format_type == 'orcaflex':
            filepath = output_dir / f"orcaflex_forces_{timestamp}.json"
            exporter.to_orcaflex_format(filepath)
            print(f"Exported OrcaFlex: {filepath}")
        
        elif format_type == 'aqwa':
            filepath = output_dir / f"aqwa_forces_{timestamp}.dat"
            exporter.to_aqwa_format(filepath)
            print(f"Exported AQWA: {filepath}")


if __name__ == "__main__":
    # Example usage
    sample_results = {
        'surge_force': 150000.0,
        'sway_force': 75000.0,
        'yaw_moment': 250000.0,
        'metadata': {
            'vessel_1': 'Tanker',
            'vessel_2': 'Supply',
            'separation': 50.0,
            'water_depth': 100.0
        }
    }
    
    exporter = ResultExporter(sample_results)
    
    # Export to various formats
    exporter.to_json('sample_output.json')
    exporter.to_csv('sample_output.csv')
    exporter.to_summary_report('sample_report.md')
    
    print("Sample exports completed")