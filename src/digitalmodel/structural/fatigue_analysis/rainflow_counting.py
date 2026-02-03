#!/usr/bin/env python
"""
Rainflow Counting Module for Fatigue Analysis
=============================================

This module implements the rainflow counting algorithm to extract load cycles
from time series data. It processes the scaled effective tension data to identify
tension ranges and cycle counts for fatigue damage calculation.

Key Features:
- ASTM E1049-85 compliant rainflow counting algorithm
- Half-cycle and full-cycle extraction
- Gate filtering for small cycles
- Cycle range binning and statistics
- Support for multiple time series formats

Input Files Required:
- scaled_loads/FC###_scaled.npz: Scaled load time series from load_scaling module
- Or direct time series CSV files with time and load columns
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional, Union
import logging
from datetime import datetime
import json

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class RainflowCounter:
    """Implements rainflow counting algorithm for fatigue analysis"""
    
    def __init__(self, 
                 gate_value: float = 0.0,
                 bin_width: Optional[float] = None,
                 output_dir: str = "output/rainflow_cycles"):
        """
        Initialize the rainflow counter
        
        Args:
            gate_value: Minimum range to consider (percentage of max range if <1, absolute if >=1)
            bin_width: Width of bins for cycle range histogram (optional)
            output_dir: Directory for cycle count outputs
        """
        self.gate_value = gate_value
        self.bin_width = bin_width
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Statistics storage
        self.processing_stats = []
    
    def find_peaks_and_valleys(self, time_series: np.ndarray) -> np.ndarray:
        """
        Find peaks and valleys (turning points) in the time series
        
        Args:
            time_series: Input time series data
        
        Returns:
            Array of turning points
        """
        if len(time_series) < 3:
            return time_series
        
        turning_points = []
        
        # Start with first point if it's a turning point
        if time_series[0] != time_series[1]:
            turning_points.append(time_series[0])
        
        # Find interior turning points
        for i in range(1, len(time_series) - 1):
            # Check if it's a peak
            if time_series[i] > time_series[i-1] and time_series[i] > time_series[i+1]:
                turning_points.append(time_series[i])
            # Check if it's a valley
            elif time_series[i] < time_series[i-1] and time_series[i] < time_series[i+1]:
                turning_points.append(time_series[i])
            # Check for equal values that change direction
            elif time_series[i] != time_series[i-1] and time_series[i] != time_series[i+1]:
                if (time_series[i] - time_series[i-1]) * (time_series[i+1] - time_series[i]) < 0:
                    turning_points.append(time_series[i])
        
        # Add last point if it's a turning point
        if time_series[-1] != time_series[-2]:
            turning_points.append(time_series[-1])
        
        return np.array(turning_points)
    
    def apply_gate_filter(self, ranges: np.ndarray, max_range: float) -> np.ndarray:
        """
        Apply gate filter to remove small cycles
        
        Args:
            ranges: Cycle ranges
            max_range: Maximum range in the data
        
        Returns:
            Filtered ranges
        """
        if self.gate_value == 0:
            return ranges
        
        # If gate_value < 1, treat as percentage
        if self.gate_value < 1:
            threshold = self.gate_value * max_range
        else:
            threshold = self.gate_value
        
        filtered = ranges[ranges >= threshold]
        
        if len(filtered) < len(ranges):
            logger.debug(f"Gate filter removed {len(ranges) - len(filtered)} small cycles")
        
        return filtered
    
    def rainflow_counting_astm(self, turning_points: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Perform rainflow counting using ASTM E1049-85 method
        
        Args:
            turning_points: Array of turning points
        
        Returns:
            Tuple of (ranges, counts) arrays
        """
        if len(turning_points) < 2:
            return np.array([]), np.array([])
        
        # Stack for rainflow counting
        stack = []
        cycles = []
        
        # Process each turning point
        for i, point in enumerate(turning_points):
            stack.append(point)
            
            # Check for extractable cycles
            while len(stack) >= 3:
                # Get last three points
                X = abs(stack[-3] - stack[-2])
                Y = abs(stack[-2] - stack[-1])
                
                if X <= Y:
                    # Extract cycle
                    if len(stack) >= 4:
                        # Full cycle
                        range_val = abs(stack[-3] - stack[-2])
                        cycles.append(('full', range_val))
                        # Remove the extracted points
                        del stack[-3:-1]
                    else:
                        break
                else:
                    break
        
        # Extract remaining half cycles
        for i in range(len(stack) - 1):
            range_val = abs(stack[i] - stack[i + 1])
            cycles.append(('half', range_val))
        
        # Separate full and half cycles
        full_cycles = [c[1] for c in cycles if c[0] == 'full']
        half_cycles = [c[1] for c in cycles if c[0] == 'half']
        
        # Combine results (count half cycles as 0.5)
        if full_cycles or half_cycles:
            all_ranges = full_cycles + half_cycles
            all_counts = [1.0] * len(full_cycles) + [0.5] * len(half_cycles)
            
            # Aggregate same ranges
            unique_ranges = {}
            for r, c in zip(all_ranges, all_counts):
                if r in unique_ranges:
                    unique_ranges[r] += c
                else:
                    unique_ranges[r] = c
            
            ranges = np.array(list(unique_ranges.keys()))
            counts = np.array(list(unique_ranges.values()))
            
            # Sort by range
            sort_idx = np.argsort(ranges)
            return ranges[sort_idx], counts[sort_idx]
        else:
            return np.array([]), np.array([])
    
    def process_time_series(self, 
                           time_series: np.ndarray,
                           time: Optional[np.ndarray] = None,
                           label: str = "timeseries") -> Dict:
        """
        Process a complete time series through rainflow counting
        
        Args:
            time_series: Load time series data
            time: Time array (optional)
            label: Label for this time series
        
        Returns:
            Dictionary with rainflow counting results
        """
        logger.debug(f"Processing {label}: {len(time_series)} points")
        
        # Find turning points
        turning_points = self.find_peaks_and_valleys(time_series)
        logger.debug(f"Found {len(turning_points)} turning points")
        
        # Perform rainflow counting
        ranges, counts = self.rainflow_counting_astm(turning_points)
        
        if len(ranges) == 0:
            logger.warning(f"No cycles found in {label}")
            return {
                'label': label,
                'ranges': np.array([]),
                'counts': np.array([]),
                'statistics': {}
            }
        
        # Apply gate filter
        max_range = np.max(ranges) if len(ranges) > 0 else 0
        filtered_mask = self.apply_gate_filter(ranges, max_range) if self.gate_value > 0 else ranges
        
        # Calculate statistics
        statistics = {
            'total_cycles': float(np.sum(counts)),
            'unique_ranges': len(ranges),
            'max_range': float(np.max(ranges)),
            'min_range': float(np.min(ranges)),
            'mean_range': float(np.mean(ranges)),
            'weighted_mean_range': float(np.average(ranges, weights=counts)),
            'turning_points': len(turning_points),
            'original_points': len(time_series)
        }
        
        result = {
            'label': label,
            'ranges': ranges,
            'counts': counts,
            'statistics': statistics
        }
        
        return result
    
    def create_cycle_histogram(self, ranges: np.ndarray, counts: np.ndarray, 
                             n_bins: int = 50) -> Tuple[np.ndarray, np.ndarray]:
        """
        Create histogram of cycle ranges
        
        Args:
            ranges: Cycle ranges
            counts: Cycle counts
            n_bins: Number of histogram bins
        
        Returns:
            Tuple of (bin_edges, bin_counts)
        """
        if len(ranges) == 0:
            return np.array([]), np.array([])
        
        # Create bins
        if self.bin_width:
            bin_edges = np.arange(0, np.max(ranges) + self.bin_width, self.bin_width)
        else:
            bin_edges = np.linspace(0, np.max(ranges), n_bins + 1)
        
        # Accumulate counts in bins
        bin_counts = np.zeros(len(bin_edges) - 1)
        
        for r, c in zip(ranges, counts):
            bin_idx = np.searchsorted(bin_edges[1:], r)
            if bin_idx < len(bin_counts):
                bin_counts[bin_idx] += c
        
        return bin_edges, bin_counts
    
    def process_scaled_load_file(self, file_path: Union[str, Path]) -> Dict:
        """
        Process a scaled load file from the load_scaling module
        
        Args:
            file_path: Path to the scaled load file (.npz format)
        
        Returns:
            Rainflow counting results
        """
        file_path = Path(file_path)
        
        if file_path.suffix == '.npz':
            # Load numpy archive
            data = np.load(file_path, allow_pickle=True)
            combined_loads = data['combined_loads']
            
            # Extract metadata if available
            if 'metadata' in data:
                metadata = json.loads(str(data['metadata']))
                label = f"FC{metadata.get('condition_id', 'unknown'):03d}"
            else:
                label = file_path.stem
        elif file_path.suffix == '.csv':
            # Load CSV file
            df = pd.read_csv(file_path)
            
            # Try to find load column
            load_columns = [c for c in df.columns if 'load' in c.lower() or 'tension' in c.lower()]
            if load_columns:
                combined_loads = df[load_columns[0]].values
            else:
                # Assume second column is loads
                combined_loads = df.iloc[:, 1].values
            
            label = file_path.stem
        else:
            raise ValueError(f"Unsupported file format: {file_path.suffix}")
        
        # Process through rainflow counting
        result = self.process_time_series(combined_loads, label=label)
        
        return result
    
    def process_batch(self, input_dir: Union[str, Path], 
                     pattern: str = "*.npz") -> pd.DataFrame:
        """
        Process multiple scaled load files in batch
        
        Args:
            input_dir: Directory containing scaled load files
            pattern: File pattern to match
        
        Returns:
            Summary DataFrame with all results
        """
        input_dir = Path(input_dir)
        files = sorted(input_dir.glob(pattern))
        
        logger.info(f"Processing {len(files)} files from {input_dir}")
        
        results = []
        
        for i, file_path in enumerate(files):
            # Process file
            result = self.process_scaled_load_file(file_path)
            
            # Save individual results
            output_file = self.output_dir / f"{result['label']}_cycles.csv"
            self.save_cycle_data(result, output_file)
            
            # Add to summary
            results.append({
                'file': file_path.name,
                'label': result['label'],
                'total_cycles': result['statistics']['total_cycles'],
                'unique_ranges': result['statistics']['unique_ranges'],
                'max_range': result['statistics']['max_range'],
                'mean_range': result['statistics']['mean_range'],
                'weighted_mean': result['statistics']['weighted_mean_range']
            })
            
            if (i + 1) % 10 == 0:
                logger.info(f"Processed {i + 1}/{len(files)} files")
        
        # Create summary DataFrame
        summary_df = pd.DataFrame(results)
        
        # Save summary
        summary_path = self.output_dir / "rainflow_summary.csv"
        summary_df.to_csv(summary_path, index=False)
        logger.info(f"Saved summary to: {summary_path}")
        
        # Generate report
        self._generate_report(summary_df)
        
        return summary_df
    
    def save_cycle_data(self, result: Dict, output_path: Union[str, Path]):
        """
        Save rainflow counting results to file
        
        Args:
            result: Rainflow counting results
            output_path: Path to save the results
        """
        output_path = Path(output_path)
        
        # Create DataFrame with results
        if len(result['ranges']) > 0:
            df = pd.DataFrame({
                'range': result['ranges'],
                'cycles': result['counts']
            })
        else:
            df = pd.DataFrame(columns=['range', 'cycles'])
        
        # Add metadata as comments
        with open(output_path, 'w') as f:
            f.write(f"# Rainflow Cycle Counting Results\n")
            f.write(f"# Label: {result['label']}\n")
            f.write(f"# Total Cycles: {result['statistics'].get('total_cycles', 0):.2f}\n")
            f.write(f"# Max Range: {result['statistics'].get('max_range', 0):.4f}\n")
            f.write(f"# Mean Range: {result['statistics'].get('mean_range', 0):.4f}\n")
            f.write(f"# Turning Points: {result['statistics'].get('turning_points', 0)}\n")
            df.to_csv(f, index=False)
    
    def _generate_report(self, summary_df: pd.DataFrame):
        """Generate processing report"""
        report_path = self.output_dir / "rainflow_report.txt"
        
        with open(report_path, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("RAINFLOW COUNTING REPORT\n")
            f.write("=" * 80 + "\n\n")
            
            f.write(f"Generated: {datetime.now().isoformat()}\n")
            f.write(f"Total Files Processed: {len(summary_df)}\n\n")
            
            f.write("CYCLE STATISTICS:\n")
            f.write(f"  Total Cycles: {summary_df['total_cycles'].sum():.0f}\n")
            f.write(f"  Average Cycles per File: {summary_df['total_cycles'].mean():.1f}\n")
            f.write(f"  Maximum Range: {summary_df['max_range'].max():.2f}\n")
            f.write(f"  Average Range: {summary_df['mean_range'].mean():.2f}\n\n")
            
            f.write("FILE STATISTICS:\n")
            f.write(f"  Files with Most Cycles: {summary_df.nlargest(5, 'total_cycles')['label'].tolist()}\n")
            f.write(f"  Files with Largest Range: {summary_df.nlargest(5, 'max_range')['label'].tolist()}\n\n")
            
            f.write("GATE FILTER:\n")
            f.write(f"  Gate Value: {self.gate_value}\n")
            f.write(f"  Bin Width: {self.bin_width if self.bin_width else 'Auto'}\n")
        
        logger.info(f"Generated report: {report_path}")


def main():
    """Main execution function"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Rainflow Counting for Fatigue Analysis")
    parser.add_argument('--input-dir', default='output/scaled_loads',
                       help='Directory containing scaled load files')
    parser.add_argument('--output-dir', default='output/rainflow_cycles',
                       help='Output directory for cycle counts')
    parser.add_argument('--pattern', default='*.npz',
                       help='File pattern to match (e.g., *.npz, *.csv)')
    parser.add_argument('--gate-value', type=float, default=0.0,
                       help='Gate filter value (0 for no filter)')
    parser.add_argument('--bin-width', type=float, default=None,
                       help='Bin width for cycle histogram')
    parser.add_argument('--single-file', default=None,
                       help='Process a single file instead of batch')
    
    args = parser.parse_args()
    
    # Create rainflow counter
    counter = RainflowCounter(
        gate_value=args.gate_value,
        bin_width=args.bin_width,
        output_dir=args.output_dir
    )
    
    if args.single_file:
        # Process single file
        result = counter.process_scaled_load_file(args.single_file)
        output_path = Path(args.output_dir) / f"{result['label']}_cycles.csv"
        counter.save_cycle_data(result, output_path)
        logger.info(f"Processed {args.single_file}")
        logger.info(f"Results saved to: {output_path}")
    else:
        # Process batch
        summary = counter.process_batch(args.input_dir, args.pattern)
        logger.info("Rainflow counting complete!")
        logger.info(f"Processed {len(summary)} files")
        logger.info(f"Results saved to: {args.output_dir}")


if __name__ == "__main__":
    main()