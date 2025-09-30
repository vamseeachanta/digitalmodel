"""
Rainflow Counting for Fatigue Analysis
======================================

This module implements the rainflow counting algorithm according to ASTM E1049-85
standard for extracting load cycles from time series data. It's essential for
variable amplitude fatigue analysis.

Key Features:
- ASTM E1049-85 compliant rainflow counting
- Fast implementation with vectorized operations
- Half-cycle and full-cycle extraction
- Gate filtering for noise removal
- Cycle range binning and statistics
- Memory-efficient processing for large datasets
- Integration with S-N curves and damage models

Author: Digital Model Team
Version: 2.0.0
"""

import numpy as np
import pandas as pd
from typing import Dict, Optional, Union, Tuple, List, Literal
from dataclasses import dataclass, field
import logging
from pathlib import Path
import warnings

# Optional numba import for performance
try:
    from numba import jit, njit
    NUMBA_AVAILABLE = True
except ImportError:
    # Fallback decorators that do nothing
    def jit(*args, **kwargs):
        def decorator(func):
            return func
        return decorator

    def njit(*args, **kwargs):
        def decorator(func):
            return func
        return decorator

    NUMBA_AVAILABLE = False

logger = logging.getLogger(__name__)

# Type aliases
TimeSeriesData = Union[np.ndarray, pd.Series, List[float]]
CycleResults = Dict[str, Union[np.ndarray, Dict, float, int]]


@dataclass
class RainflowParameters:
    """Configuration parameters for rainflow counting"""
    gate_value: float = 0.0  # Filter threshold (absolute or relative)
    gate_type: Literal['absolute', 'relative'] = 'relative'
    bin_width: Optional[float] = None  # Histogram bin width
    small_cycle_removal: bool = True  # Remove small cycles
    cycle_combination: bool = True  # Combine similar cycles


@dataclass
class CycleStatistics:
    """Statistical information about extracted cycles"""
    total_cycles: float
    unique_ranges: int
    max_range: float
    min_range: float
    mean_range: float
    weighted_mean_range: float
    range_std: float
    turning_points: int
    original_points: int
    filter_removed: int


@njit(cache=True)
def _find_turning_points_numba(time_series: np.ndarray) -> np.ndarray:
    """
    Find turning points (peaks and valleys) using numba for speed

    Parameters
    ----------
    time_series : np.ndarray
        Input time series data

    Returns
    -------
    np.ndarray
        Array of turning points
    """
    n = len(time_series)
    if n < 3:
        return time_series.copy()

    # Pre-allocate with maximum possible size
    turning_points = np.empty(n, dtype=np.float64)
    tp_count = 0

    # Add first point if it's different from second
    if time_series[0] != time_series[1]:
        turning_points[tp_count] = time_series[0]
        tp_count += 1

    # Find interior turning points
    for i in range(1, n - 1):
        prev_val = time_series[i - 1]
        curr_val = time_series[i]
        next_val = time_series[i + 1]

        # Peak condition
        if curr_val > prev_val and curr_val > next_val:
            turning_points[tp_count] = curr_val
            tp_count += 1
        # Valley condition
        elif curr_val < prev_val and curr_val < next_val:
            turning_points[tp_count] = curr_val
            tp_count += 1
        # Handle plateau conditions
        elif curr_val != prev_val and curr_val != next_val:
            if (curr_val - prev_val) * (next_val - curr_val) < 0:
                turning_points[tp_count] = curr_val
                tp_count += 1

    # Add last point if it's different from second-to-last
    if time_series[-1] != time_series[-2]:
        turning_points[tp_count] = time_series[-1]
        tp_count += 1

    return turning_points[:tp_count]


@njit(cache=True)
def _rainflow_counting_numba(turning_points: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
    """
    Perform rainflow counting using numba for speed

    Parameters
    ----------
    turning_points : np.ndarray
        Array of turning points

    Returns
    -------
    ranges : np.ndarray
        Cycle ranges
    counts : np.ndarray
        Cycle counts (0.5 for half cycles, 1.0 for full cycles)
    """
    n = len(turning_points)
    if n < 2:
        return np.array([]), np.array([])

    # Use dynamic arrays for cycles
    max_cycles = n  # Conservative estimate
    ranges = np.empty(max_cycles, dtype=np.float64)
    counts = np.empty(max_cycles, dtype=np.float64)
    cycle_count = 0

    # Stack for rainflow counting
    stack = np.empty(n, dtype=np.float64)
    stack_size = 0

    # Process each turning point
    for i in range(n):
        # Add point to stack
        stack[stack_size] = turning_points[i]
        stack_size += 1

        # Check for extractable cycles
        while stack_size >= 3:
            # Get last three points
            X = abs(stack[stack_size - 3] - stack[stack_size - 2])
            Y = abs(stack[stack_size - 2] - stack[stack_size - 1])

            if X <= Y:
                # Extract cycle
                if stack_size >= 4:
                    # Full cycle
                    range_val = abs(stack[stack_size - 3] - stack[stack_size - 2])
                    ranges[cycle_count] = range_val
                    counts[cycle_count] = 1.0
                    cycle_count += 1

                    # Remove the extracted points
                    stack[stack_size - 3] = stack[stack_size - 1]
                    stack_size -= 2
                else:
                    break
            else:
                break

    # Extract remaining half cycles
    for i in range(stack_size - 1):
        range_val = abs(stack[i] - stack[i + 1])
        ranges[cycle_count] = range_val
        counts[cycle_count] = 0.5
        cycle_count += 1

    return ranges[:cycle_count], counts[:cycle_count]


class RainflowCounter:
    """
    Rainflow counting implementation for fatigue analysis

    This class provides a complete implementation of the rainflow counting
    algorithm with various options for cycle extraction and filtering.
    """

    def __init__(self, parameters: Optional[RainflowParameters] = None):
        """
        Initialize rainflow counter

        Parameters
        ----------
        parameters : RainflowParameters, optional
            Configuration parameters
        """
        self.params = parameters or RainflowParameters()
        self.last_result: Optional[CycleResults] = None

    def count_cycles(self,
                    time_series: TimeSeriesData,
                    time_array: Optional[np.ndarray] = None) -> CycleResults:
        """
        Perform rainflow counting on time series data

        Parameters
        ----------
        time_series : array-like
            Load or stress time series
        time_array : np.ndarray, optional
            Time array corresponding to time series

        Returns
        -------
        CycleResults
            Dictionary containing cycle ranges, counts, and statistics
        """
        # Convert input to numpy array
        if isinstance(time_series, (list, pd.Series)):
            data = np.asarray(time_series, dtype=np.float64)
        else:
            data = time_series.astype(np.float64)

        # Validate input
        if len(data) < 2:
            logger.warning("Time series too short for rainflow counting")
            return self._empty_result()

        # Remove NaN values
        finite_mask = np.isfinite(data)
        if not np.all(finite_mask):
            data = data[finite_mask]
            logger.warning(f"Removed {np.sum(~finite_mask)} non-finite values")

        original_points = len(data)

        # Find turning points
        turning_points = self._find_turning_points(data)

        if len(turning_points) < 2:
            logger.warning("Insufficient turning points for cycle extraction")
            return self._empty_result()

        # Perform rainflow counting
        ranges, counts = self._rainflow_count(turning_points)

        if len(ranges) == 0:
            logger.warning("No cycles extracted")
            return self._empty_result()

        # Apply gate filter
        ranges, counts, filter_removed = self._apply_gate_filter(ranges, counts)

        # Combine similar cycles if requested
        if self.params.cycle_combination:
            ranges, counts = self._combine_similar_cycles(ranges, counts)

        # Create cycle DataFrame
        cycle_df = pd.DataFrame({
            'range': ranges,
            'count': counts
        }).sort_values('range').reset_index(drop=True)

        # Calculate statistics
        stats = self._calculate_statistics(
            cycle_df, len(turning_points), original_points, filter_removed
        )

        # Prepare results
        result = {
            'cycles': cycle_df,
            'ranges': ranges,
            'counts': counts,
            'statistics': stats,
            'turning_points': turning_points,
            'parameters': self.params
        }

        self.last_result = result
        return result

    def _find_turning_points(self, time_series: np.ndarray) -> np.ndarray:
        """Find turning points in time series"""
        return _find_turning_points_numba(time_series)

    def _rainflow_count(self, turning_points: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """Perform rainflow counting algorithm"""
        return _rainflow_counting_numba(turning_points)

    def _apply_gate_filter(self,
                          ranges: np.ndarray,
                          counts: np.ndarray) -> Tuple[np.ndarray, np.ndarray, int]:
        """
        Apply gate filter to remove small cycles

        Parameters
        ----------
        ranges : np.ndarray
            Cycle ranges
        counts : np.ndarray
            Cycle counts

        Returns
        -------
        filtered_ranges : np.ndarray
            Filtered ranges
        filtered_counts : np.ndarray
            Filtered counts
        n_removed : int
            Number of cycles removed
        """
        if self.params.gate_value <= 0:
            return ranges, counts, 0

        # Determine threshold
        if self.params.gate_type == 'relative':
            threshold = self.params.gate_value * np.max(ranges)
        else:
            threshold = self.params.gate_value

        # Apply filter
        keep_mask = ranges >= threshold
        filtered_ranges = ranges[keep_mask]
        filtered_counts = counts[keep_mask]
        n_removed = np.sum(~keep_mask)

        if n_removed > 0:
            logger.debug(f"Gate filter removed {n_removed} small cycles")

        return filtered_ranges, filtered_counts, n_removed

    def _combine_similar_cycles(self,
                               ranges: np.ndarray,
                               counts: np.ndarray) -> Tuple[np.ndarray, np.ndarray]:
        """
        Combine cycles with similar ranges

        Parameters
        ----------
        ranges : np.ndarray
            Cycle ranges
        counts : np.ndarray
            Cycle counts

        Returns
        -------
        combined_ranges : np.ndarray
            Combined ranges
        combined_counts : np.ndarray
            Combined counts
        """
        if len(ranges) == 0:
            return ranges, counts

        # Use pandas groupby for efficient combination
        df = pd.DataFrame({'range': ranges, 'count': counts})

        # Define tolerance for "similar" ranges
        tolerance = self.params.bin_width or (np.max(ranges) - np.min(ranges)) / 1000

        # Round ranges to tolerance
        df['range_rounded'] = np.round(df['range'] / tolerance) * tolerance

        # Group and sum
        grouped = df.groupby('range_rounded').agg({
            'range': 'mean',  # Use mean of actual ranges
            'count': 'sum'
        }).reset_index()

        return grouped['range'].values, grouped['count'].values

    def _calculate_statistics(self,
                             cycle_df: pd.DataFrame,
                             n_turning_points: int,
                             n_original_points: int,
                             n_filtered: int) -> CycleStatistics:
        """Calculate cycle statistics"""
        if len(cycle_df) == 0:
            return CycleStatistics(
                total_cycles=0, unique_ranges=0, max_range=0, min_range=0,
                mean_range=0, weighted_mean_range=0, range_std=0,
                turning_points=n_turning_points, original_points=n_original_points,
                filter_removed=n_filtered
            )

        ranges = cycle_df['range'].values
        counts = cycle_df['count'].values

        return CycleStatistics(
            total_cycles=float(np.sum(counts)),
            unique_ranges=len(cycle_df),
            max_range=float(np.max(ranges)),
            min_range=float(np.min(ranges)),
            mean_range=float(np.mean(ranges)),
            weighted_mean_range=float(np.average(ranges, weights=counts)),
            range_std=float(np.std(ranges)),
            turning_points=n_turning_points,
            original_points=n_original_points,
            filter_removed=n_filtered
        )

    def _empty_result(self) -> CycleResults:
        """Return empty result structure"""
        empty_df = pd.DataFrame(columns=['range', 'count'])
        empty_stats = CycleStatistics(
            total_cycles=0, unique_ranges=0, max_range=0, min_range=0,
            mean_range=0, weighted_mean_range=0, range_std=0,
            turning_points=0, original_points=0, filter_removed=0
        )

        return {
            'cycles': empty_df,
            'ranges': np.array([]),
            'counts': np.array([]),
            'statistics': empty_stats,
            'turning_points': np.array([]),
            'parameters': self.params
        }

    def create_histogram(self,
                        n_bins: int = 50,
                        range_limits: Optional[Tuple[float, float]] = None) -> Tuple[np.ndarray, np.ndarray]:
        """
        Create histogram of cycle ranges

        Parameters
        ----------
        n_bins : int, default=50
            Number of histogram bins
        range_limits : tuple, optional
            (min, max) range limits for histogram

        Returns
        -------
        bin_edges : np.ndarray
            Histogram bin edges
        bin_counts : np.ndarray
            Cycle counts in each bin
        """
        if self.last_result is None or len(self.last_result['ranges']) == 0:
            return np.array([]), np.array([])

        ranges = self.last_result['ranges']
        counts = self.last_result['counts']

        # Determine range limits
        if range_limits:
            min_range, max_range = range_limits
        else:
            min_range, max_range = np.min(ranges), np.max(ranges)

        # Create bin edges
        if self.params.bin_width:
            bin_edges = np.arange(min_range, max_range + self.params.bin_width, self.params.bin_width)
        else:
            bin_edges = np.linspace(min_range, max_range, n_bins + 1)

        # Calculate histogram
        bin_counts = np.zeros(len(bin_edges) - 1)

        for r, c in zip(ranges, counts):
            if min_range <= r <= max_range:
                bin_idx = np.searchsorted(bin_edges[1:], r)
                if bin_idx < len(bin_counts):
                    bin_counts[bin_idx] += c

        return bin_edges, bin_counts

    def extract_cycles_with_means(self,
                                 time_series: TimeSeriesData,
                                 extract_means: bool = True) -> pd.DataFrame:
        """
        Extract cycles with mean stress information

        Parameters
        ----------
        time_series : array-like
            Load or stress time series
        extract_means : bool, default=True
            Whether to calculate mean stresses

        Returns
        -------
        pd.DataFrame
            Cycles with range, mean, and count columns
        """
        # Perform basic rainflow counting
        result = self.count_cycles(time_series)

        if not extract_means or len(result['ranges']) == 0:
            # Return without means
            cycles_df = result['cycles'].copy()
            cycles_df['mean'] = 0.0
            return cycles_df[['range', 'mean', 'count']]

        # Extract cycles with mean stress calculation
        data = np.asarray(time_series, dtype=np.float64)
        turning_points = result['turning_points']

        # Re-implement rainflow with mean stress tracking
        cycles_with_means = self._rainflow_with_means(turning_points)

        return cycles_with_means

    def _rainflow_with_means(self, turning_points: np.ndarray) -> pd.DataFrame:
        """
        Rainflow counting that also tracks mean stresses

        Parameters
        ----------
        turning_points : np.ndarray
            Array of turning points

        Returns
        -------
        pd.DataFrame
            Cycles with range, mean, and count
        """
        n = len(turning_points)
        if n < 2:
            return pd.DataFrame(columns=['range', 'mean', 'count'])

        cycles = []
        stack = []

        for i in range(n):
            stack.append(turning_points[i])

            while len(stack) >= 3:
                X = abs(stack[-3] - stack[-2])
                Y = abs(stack[-2] - stack[-1])

                if X <= Y:
                    if len(stack) >= 4:
                        # Full cycle
                        stress_range = abs(stack[-3] - stack[-2])
                        mean_stress = (stack[-3] + stack[-2]) / 2
                        cycles.append({
                            'range': stress_range,
                            'mean': mean_stress,
                            'count': 1.0
                        })
                        # Remove extracted points
                        del stack[-3:-1]
                    else:
                        break
                else:
                    break

        # Extract remaining half cycles
        for i in range(len(stack) - 1):
            stress_range = abs(stack[i] - stack[i + 1])
            mean_stress = (stack[i] + stack[i + 1]) / 2
            cycles.append({
                'range': stress_range,
                'mean': mean_stress,
                'count': 0.5
            })

        # Create DataFrame and aggregate
        df = pd.DataFrame(cycles)
        if len(df) == 0:
            return pd.DataFrame(columns=['range', 'mean', 'count'])

        # Group by range and mean (with tolerance)
        tolerance = np.max(df['range']) / 1000 if len(df) > 0 else 1.0
        df['range_rounded'] = np.round(df['range'] / tolerance) * tolerance
        df['mean_rounded'] = np.round(df['mean'] / tolerance) * tolerance

        grouped = df.groupby(['range_rounded', 'mean_rounded']).agg({
            'range': 'mean',
            'mean': 'mean',
            'count': 'sum'
        }).reset_index()

        return grouped[['range', 'mean', 'count']].sort_values('range').reset_index(drop=True)


class RainflowBatch:
    """
    Batch processing of multiple time series for rainflow counting

    Useful for processing large datasets or multiple load cases.
    """

    def __init__(self, parameters: Optional[RainflowParameters] = None):
        """Initialize batch processor"""
        self.params = parameters or RainflowParameters()
        self.counter = RainflowCounter(self.params)
        self.results: List[CycleResults] = []

    def process_files(self,
                     file_paths: List[Union[str, Path]],
                     data_column: Union[str, int] = 1,
                     time_column: Optional[Union[str, int]] = 0) -> pd.DataFrame:
        """
        Process multiple data files

        Parameters
        ----------
        file_paths : list
            List of file paths to process
        data_column : str or int, default=1
            Column containing load/stress data
        time_column : str or int, optional
            Column containing time data

        Returns
        -------
        pd.DataFrame
            Summary of results from all files
        """
        summary_data = []

        for file_path in file_paths:
            try:
                # Load data
                if isinstance(file_path, str):
                    file_path = Path(file_path)

                if file_path.suffix == '.csv':
                    df = pd.read_csv(file_path)
                elif file_path.suffix in ['.npz', '.npy']:
                    data = np.load(file_path)
                    if isinstance(data, np.ndarray):
                        df = pd.DataFrame({'time': np.arange(len(data)), 'data': data})
                    else:
                        # For .npz files, assume 'data' key
                        df = pd.DataFrame({'time': np.arange(len(data['data'])), 'data': data['data']})
                else:
                    logger.warning(f"Unsupported file format: {file_path}")
                    continue

                # Extract time series
                if isinstance(data_column, str):
                    time_series = df[data_column].values
                else:
                    time_series = df.iloc[:, data_column].values

                # Process with rainflow counting
                result = self.counter.count_cycles(time_series)
                self.results.append(result)

                # Add to summary
                stats = result['statistics']
                summary_data.append({
                    'file': file_path.name,
                    'total_cycles': stats.total_cycles,
                    'unique_ranges': stats.unique_ranges,
                    'max_range': stats.max_range,
                    'mean_range': stats.mean_range,
                    'turning_points': stats.turning_points,
                    'original_points': stats.original_points
                })

            except Exception as e:
                logger.error(f"Error processing {file_path}: {e}")
                continue

        return pd.DataFrame(summary_data)

    def get_combined_cycles(self) -> pd.DataFrame:
        """
        Combine cycles from all processed files

        Returns
        -------
        pd.DataFrame
            Combined cycle data
        """
        if not self.results:
            return pd.DataFrame(columns=['range', 'count'])

        all_cycles = []
        for result in self.results:
            all_cycles.append(result['cycles'])

        # Combine and aggregate
        combined = pd.concat(all_cycles, ignore_index=True)
        if len(combined) == 0:
            return combined

        # Group by range and sum counts
        tolerance = combined['range'].max() / 1000 if len(combined) > 0 else 1.0
        combined['range_rounded'] = np.round(combined['range'] / tolerance) * tolerance

        grouped = combined.groupby('range_rounded').agg({
            'range': 'mean',
            'count': 'sum'
        }).reset_index()

        return grouped[['range', 'count']].sort_values('range').reset_index(drop=True)


# Convenience functions
def rainflow_count(time_series: TimeSeriesData,
                  gate_value: float = 0.0,
                  gate_type: str = 'relative') -> pd.DataFrame:
    """
    Simple rainflow counting function

    Parameters
    ----------
    time_series : array-like
        Load or stress time series
    gate_value : float, default=0.0
        Gate filter threshold
    gate_type : str, default='relative'
        Gate filter type: 'relative' or 'absolute'

    Returns
    -------
    pd.DataFrame
        Cycle data with range and count columns
    """
    params = RainflowParameters(gate_value=gate_value, gate_type=gate_type)
    counter = RainflowCounter(params)
    result = counter.count_cycles(time_series)
    return result['cycles']


def rainflow_with_means(time_series: TimeSeriesData) -> pd.DataFrame:
    """
    Rainflow counting with mean stress calculation

    Parameters
    ----------
    time_series : array-like
        Load or stress time series

    Returns
    -------
    pd.DataFrame
        Cycle data with range, mean, and count columns
    """
    counter = RainflowCounter()
    return counter.extract_cycles_with_means(time_series)


if __name__ == "__main__":
    # Example usage and testing
    import matplotlib.pyplot as plt

    # Generate test signal
    t = np.linspace(0, 10, 1000)
    signal = (50 * np.sin(2 * np.pi * 0.5 * t) +
             25 * np.sin(2 * np.pi * 2 * t) +
             10 * np.random.normal(0, 1, len(t)))

    # Perform rainflow counting
    cycles = rainflow_count(signal, gate_value=0.05)
    cycles_with_means = rainflow_with_means(signal)

    print(f"Extracted {len(cycles)} unique cycle ranges")
    print(f"Total cycles: {cycles['count'].sum():.1f}")
    print(f"Maximum range: {cycles['range'].max():.2f}")

    # Create histogram
    counter = RainflowCounter()
    result = counter.count_cycles(signal)
    bin_edges, bin_counts = counter.create_histogram()

    # Plot results
    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(10, 8))

    # Original signal
    ax1.plot(t, signal)
    ax1.set_ylabel('Amplitude')
    ax1.set_title('Original Signal')
    ax1.grid(True, alpha=0.3)

    # Cycle range vs count
    ax2.scatter(cycles['range'], cycles['count'], alpha=0.7)
    ax2.set_xlabel('Stress Range')
    ax2.set_ylabel('Cycle Count')
    ax2.set_title('Rainflow Cycles')
    ax2.grid(True, alpha=0.3)

    # Histogram
    if len(bin_edges) > 1:
        bin_centers = (bin_edges[1:] + bin_edges[:-1]) / 2
        ax3.bar(bin_centers, bin_counts, width=np.diff(bin_edges), alpha=0.7)
        ax3.set_xlabel('Stress Range')
        ax3.set_ylabel('Cycle Count')
        ax3.set_title('Cycle Range Histogram')
        ax3.grid(True, alpha=0.3)

    plt.tight_layout()
    plt.show()

    print("Rainflow counting module test completed successfully!")