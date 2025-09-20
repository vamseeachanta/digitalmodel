#!/usr/bin/env python3
"""
Rainflow Counting Algorithm for Fatigue Analysis

This module implements the ASTM E1049 rainflow counting method for fatigue analysis.
The algorithm identifies stress/load cycles from time series data and counts their
occurrences for use in fatigue damage calculations.

References:
    - ASTM E1049-85: Standard Practices for Cycle Counting in Fatigue Analysis
    - Downing, S.D. and Socie, D.F., "Simple rainflow counting algorithms"
"""

import numpy as np
import pandas as pd
from typing import Tuple, List, Optional, Dict, Any
from dataclasses import dataclass
import logging

logger = logging.getLogger(__name__)


@dataclass
class RainflowCycle:
    """Represents a single rainflow cycle"""
    range_value: float  # Stress/load range
    mean_value: float   # Mean stress/load
    count: float        # Cycle count (0.5 for half cycles, 1.0 for full)
    start_index: int    # Starting index in time series
    end_index: int      # Ending index in time series


class RainflowCounter:
    """
    Implements ASTM E1049 rainflow counting algorithm for fatigue analysis
    """
    
    def __init__(self, gate_value: Optional[float] = None):
        """
        Initialize the rainflow counter
        
        Args:
            gate_value: Optional threshold below which cycles are ignored (noise filter)
        """
        self.gate_value = gate_value
        self.cycles: List[RainflowCycle] = []
        
    def count_cycles(self, time_series: np.ndarray, 
                    time: Optional[np.ndarray] = None) -> Tuple[np.ndarray, np.ndarray]:
        """
        Perform rainflow counting on a time series
        
        Args:
            time_series: Array of stress/load values
            time: Optional array of time values
            
        Returns:
            Tuple of (ranges, counts) arrays
        """
        logger.info(f"Starting rainflow counting on {len(time_series)} data points")
        
        # Extract peaks and valleys (reversals)
        reversals, reversal_indices = self._extract_reversals(time_series)
        logger.debug(f"Extracted {len(reversals)} reversal points")
        
        # Apply gate filter if specified
        if self.gate_value is not None:
            reversals, reversal_indices = self._apply_gate_filter(
                reversals, reversal_indices
            )
            logger.debug(f"After gate filter: {len(reversals)} reversals remain")
        
        # Perform rainflow counting
        self.cycles = self._rainflow_algorithm(reversals, reversal_indices)
        logger.info(f"Identified {len(self.cycles)} cycles")
        
        # Convert to range-count format
        ranges, counts = self._aggregate_cycles(self.cycles)
        
        return ranges, counts
    
    def _extract_reversals(self, data: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Extract peaks and valleys from time series
        
        Args:
            data: Time series data
            
        Returns:
            Tuple of (reversal_values, reversal_indices)
        """
        reversals = []
        indices = []
        
        # Ensure we start with a reversal point
        reversals.append(data[0])
        indices.append(0)
        
        # Find local maxima and minima
        for i in range(1, len(data) - 1):
            # Check if peak (local maximum)
            if data[i] > data[i-1] and data[i] > data[i+1]:
                reversals.append(data[i])
                indices.append(i)
            # Check if valley (local minimum)
            elif data[i] < data[i-1] and data[i] < data[i+1]:
                reversals.append(data[i])
                indices.append(i)
        
        # Ensure we end with a reversal point
        if len(data) > 1:
            reversals.append(data[-1])
            indices.append(len(data) - 1)
        
        # Remove consecutive duplicate values
        cleaned_reversals = []
        cleaned_indices = []
        
        for i in range(len(reversals)):
            if i == 0 or abs(reversals[i] - reversals[i-1]) > 1e-10:
                cleaned_reversals.append(reversals[i])
                cleaned_indices.append(indices[i])
        
        return np.array(cleaned_reversals), np.array(cleaned_indices)
    
    def _apply_gate_filter(self, reversals: np.ndarray, 
                          indices: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Apply gate filter to remove small fluctuations
        
        Args:
            reversals: Reversal values
            indices: Reversal indices
            
        Returns:
            Filtered reversals and indices
        """
        if len(reversals) < 3:
            return reversals, indices
        
        filtered_reversals = [reversals[0]]
        filtered_indices = [indices[0]]
        
        i = 1
        while i < len(reversals) - 1:
            # Check range with previous point
            range_prev = abs(reversals[i] - filtered_reversals[-1])
            
            if range_prev >= self.gate_value:
                filtered_reversals.append(reversals[i])
                filtered_indices.append(indices[i])
            
            i += 1
        
        # Always include last point
        filtered_reversals.append(reversals[-1])
        filtered_indices.append(indices[-1])
        
        return np.array(filtered_reversals), np.array(filtered_indices)
    
    def _rainflow_algorithm(self, reversals: np.ndarray, 
                           indices: np.ndarray) -> List[RainflowCycle]:
        """
        Core rainflow counting algorithm (ASTM E1049)
        
        Args:
            reversals: Array of reversal values
            indices: Array of reversal indices
            
        Returns:
            List of identified cycles
        """
        cycles = []
        stack = []
        
        # Push first two reversals onto stack
        for i in range(min(2, len(reversals))):
            stack.append((reversals[i], indices[i]))
        
        # Process remaining reversals
        for i in range(2, len(reversals)):
            stack.append((reversals[i], indices[i]))
            
            # Check for cycles
            while len(stack) >= 3:
                # Get last three points
                X = abs(stack[-2][0] - stack[-1][0])  # Range X-Y
                Y = abs(stack[-3][0] - stack[-2][0])  # Range S-X
                
                if X >= Y:
                    # Found a cycle
                    if len(stack) == 3:
                        # Half cycle
                        cycle_range = Y
                        cycle_mean = (stack[-3][0] + stack[-2][0]) / 2
                        cycles.append(RainflowCycle(
                            range_value=cycle_range,
                            mean_value=cycle_mean,
                            count=0.5,
                            start_index=stack[-3][1],
                            end_index=stack[-2][1]
                        ))
                        # Remove middle point
                        del stack[-2]
                        break
                    else:
                        # Full cycle
                        cycle_range = Y
                        cycle_mean = (stack[-3][0] + stack[-2][0]) / 2
                        cycles.append(RainflowCycle(
                            range_value=cycle_range,
                            mean_value=cycle_mean,
                            count=1.0,
                            start_index=stack[-3][1],
                            end_index=stack[-2][1]
                        ))
                        # Remove the two points that formed the cycle
                        del stack[-3:-1]  # Remove S and X, keep Y
                else:
                    break
        
        # Process remaining points in stack as half cycles
        while len(stack) >= 2:
            cycle_range = abs(stack[0][0] - stack[1][0])
            cycle_mean = (stack[0][0] + stack[1][0]) / 2
            cycles.append(RainflowCycle(
                range_value=cycle_range,
                mean_value=cycle_mean,
                count=0.5,
                start_index=stack[0][1],
                end_index=stack[1][1]
            ))
            stack.pop(0)
        
        return cycles
    
    def _aggregate_cycles(self, cycles: List[RainflowCycle]) -> Tuple[np.ndarray, np.ndarray]:
        """
        Aggregate cycles into range-count format
        
        Args:
            cycles: List of rainflow cycles
            
        Returns:
            Tuple of (ranges, counts) arrays
        """
        if not cycles:
            return np.array([]), np.array([])
        
        # Create dictionary to aggregate counts by range
        range_counts: Dict[float, float] = {}
        
        for cycle in cycles:
            # Round range to reasonable precision to group similar values
            range_key = round(cycle.range_value, 6)
            
            if range_key in range_counts:
                range_counts[range_key] += cycle.count
            else:
                range_counts[range_key] = cycle.count
        
        # Sort by range value
        sorted_items = sorted(range_counts.items())
        
        ranges = np.array([item[0] for item in sorted_items])
        counts = np.array([item[1] for item in sorted_items])
        
        return ranges, counts
    
    def get_cycle_statistics(self) -> Dict[str, Any]:
        """
        Calculate statistics about the identified cycles
        
        Returns:
            Dictionary of cycle statistics
        """
        if not self.cycles:
            return {
                'total_cycles': 0,
                'full_cycles': 0,
                'half_cycles': 0,
                'max_range': 0,
                'min_range': 0,
                'mean_range': 0,
                'max_mean': 0,
                'min_mean': 0
            }
        
        ranges = [c.range_value for c in self.cycles]
        means = [c.mean_value for c in self.cycles]
        full_cycles = sum(1 for c in self.cycles if c.count == 1.0)
        half_cycles = sum(1 for c in self.cycles if c.count == 0.5)
        
        return {
            'total_cycles': len(self.cycles),
            'full_cycles': full_cycles,
            'half_cycles': half_cycles,
            'max_range': max(ranges),
            'min_range': min(ranges),
            'mean_range': np.mean(ranges),
            'std_range': np.std(ranges),
            'max_mean': max(means),
            'min_mean': min(means),
            'mean_mean': np.mean(means)
        }
    
    def export_results(self, filename: str, include_stats: bool = True):
        """
        Export rainflow counting results to CSV file
        
        Args:
            filename: Output CSV filename
            include_stats: Whether to include statistics in header
        """
        ranges, counts = self._aggregate_cycles(self.cycles)
        
        # Create DataFrame
        df = pd.DataFrame({
            'Range': ranges,
            'Counts': counts
        })
        
        # Write to CSV with optional statistics in header
        with open(filename, 'w') as f:
            if include_stats:
                stats = self.get_cycle_statistics()
                f.write("# Rainflow Counting Results\n")
                f.write(f"# Total Cycles: {stats['total_cycles']}\n")
                f.write(f"# Full Cycles: {stats['full_cycles']}\n")
                f.write(f"# Half Cycles: {stats['half_cycles']}\n")
                f.write(f"# Max Range: {stats['max_range']:.6f}\n")
                f.write(f"# Min Range: {stats['min_range']:.6f}\n")
                f.write(f"# Mean Range: {stats['mean_range']:.6f}\n")
            
            df.to_csv(f, index=False)
        
        logger.info(f"Rainflow results exported to {filename}")


def rainflow_count(time_series: np.ndarray, 
                   gate_value: Optional[float] = None) -> Tuple[np.ndarray, np.ndarray]:
    """
    Convenience function for rainflow counting
    
    Args:
        time_series: Array of stress/load values
        gate_value: Optional threshold for filtering small cycles
        
    Returns:
        Tuple of (ranges, counts) arrays
    """
    counter = RainflowCounter(gate_value=gate_value)
    return counter.count_cycles(time_series)


# Example usage
if __name__ == "__main__":
    # Create example time series
    t = np.linspace(0, 10, 1000)
    stress = 100 + 50*np.sin(2*np.pi*t) + 20*np.sin(6*np.pi*t)
    
    # Perform rainflow counting
    counter = RainflowCounter(gate_value=5.0)
    ranges, counts = counter.count_cycles(stress)
    
    # Display results
    print("Rainflow Counting Results:")
    print(f"Number of unique ranges: {len(ranges)}")
    print(f"Total counts: {np.sum(counts)}")
    
    stats = counter.get_cycle_statistics()
    print(f"\nStatistics:")
    for key, value in stats.items():
        print(f"  {key}: {value}")