#!/usr/bin/env python
"""
Load Scaling Module for Fatigue Analysis
=========================================

This module handles the scaling of reference seastate time series data to match
target fatigue conditions. It processes reference loads (wind @ 10m/s, wave @ Hs=0.5m)
and scales them according to the fatigue seastate conditions.

Key Features:
- Wind load scaling: (V/10)²
- Wave load scaling: Hs/0.5
- Direction-based reference selection
- Combined effective tension calculation
- Support for multiple vessel configurations

Input Files Required:
- reference_seastates.csv: Definition of reference conditions
- fatigue_conditions.csv: Target fatigue conditions with scaling parameters
- configuration_weights.csv: Vessel configuration operational weights
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
import logging
from datetime import datetime
import json

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


class LoadScaler:
    """Scales reference seastate loads to match fatigue conditions"""
    
    def __init__(self, 
                 reference_seastates_path: str = "input/reference_seastates.csv",
                 fatigue_conditions_path: str = "input/fatigue_conditions.csv",
                 output_dir: str = "output/scaled_loads"):
        """
        Initialize the load scaler
        
        Args:
            reference_seastates_path: Path to reference seastate definitions
            fatigue_conditions_path: Path to fatigue conditions file
            output_dir: Directory for scaled load outputs
        """
        self.reference_seastates_path = reference_seastates_path
        self.fatigue_conditions_path = fatigue_conditions_path
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        
        # Reference conditions
        self.base_wind_speed = 10.0  # m/s
        self.base_hs = 0.5  # m
        
        # Load data
        self.reference_seastates = None
        self.fatigue_conditions = None
        self.scaling_factors = None
        
        self._load_configurations()
        self._calculate_scaling_factors()
    
    def _load_configurations(self):
        """Load reference seastates and fatigue conditions"""
        logger.info("Loading configuration files...")
        
        # Load reference seastates
        if Path(self.reference_seastates_path).exists():
            self.reference_seastates = pd.read_csv(self.reference_seastates_path)
            logger.info(f"Loaded {len(self.reference_seastates)} reference seastates")
        else:
            logger.warning(f"Reference seastates file not found: {self.reference_seastates_path}")
            self._create_sample_reference_seastates()
        
        # Load fatigue conditions
        if Path(self.fatigue_conditions_path).exists():
            self.fatigue_conditions = pd.read_csv(self.fatigue_conditions_path)
            logger.info(f"Loaded {len(self.fatigue_conditions)} fatigue conditions")
        else:
            logger.warning(f"Fatigue conditions file not found: {self.fatigue_conditions_path}")
            self._create_sample_fatigue_conditions()
    
    def _create_sample_reference_seastates(self):
        """Create sample reference seastates for demonstration"""
        data = []
        
        # Wind reference seastates (10 m/s at various directions)
        wind_directions = [0, 45, 70, 90, 110, 125, 135, 150, 160, 180, 200, 225, 270, 290, 315, 335]
        for i, direction in enumerate(wind_directions, 1):
            data.append({
                'reference_id': f'WD{i:02d}',
                'type': 'wind',
                'wind_speed': 10.0,
                'wind_direction': direction,
                'hs': 0.0,
                'tp': 0.0,
                'wave_direction': 0.0
            })
        
        # Wave reference seastates (Hs=0.5m at various directions and periods)
        wave_data = [
            (0, 1.93), (45, 1.93), (70, 2.70), (90, 2.70),
            (110, 3.47), (125, 3.47), (135, 1.93), (150, 2.70),
            (160, 3.47), (180, 1.93), (200, 2.70), (225, 3.47),
            (270, 1.93), (290, 2.70), (315, 3.47), (335, 1.93)
        ]
        
        for i, (direction, tp) in enumerate(wave_data, 1):
            data.append({
                'reference_id': f'W{i:02d}',
                'type': 'wave',
                'wind_speed': 0.0,
                'wind_direction': 0.0,
                'hs': 0.5,
                'tp': tp,
                'wave_direction': direction
            })
        
        self.reference_seastates = pd.DataFrame(data)
        
        # Save sample file
        self.reference_seastates.to_csv(self.reference_seastates_path, index=False)
        logger.info(f"Created sample reference seastates: {self.reference_seastates_path}")
    
    def _create_sample_fatigue_conditions(self):
        """Create sample fatigue conditions for demonstration"""
        data = []
        
        # Create subset of 81 fatigue conditions
        wind_speeds = [3, 5, 7, 10, 12, 15]
        wave_heights = [0.09, 0.15, 0.25, 0.5, 0.75, 1.0, 1.5]
        
        condition_id = 1
        for wind_speed in wind_speeds:
            for hs in wave_heights:
                if condition_id > 81:
                    break
                
                # Vary directions
                wind_dir = (condition_id * 45) % 360
                wave_dir = (condition_id * 30) % 360
                
                # Calculate occurrence (example distribution)
                occurrence = 100.0 / 81  # Uniform for simplicity
                
                # Estimate Tp based on Hs
                tp = 3.86 * np.sqrt(hs) if hs > 0 else 0
                
                data.append({
                    'condition_id': condition_id,
                    'wind_speed': wind_speed,
                    'wind_direction': wind_dir,
                    'hs': hs,
                    'tp': round(tp, 2),
                    'wave_direction': wave_dir,
                    'occurrence_pct': round(occurrence, 3)
                })
                
                condition_id += 1
        
        self.fatigue_conditions = pd.DataFrame(data[:81])  # Ensure exactly 81 conditions
        
        # Save sample file
        self.fatigue_conditions.to_csv(self.fatigue_conditions_path, index=False)
        logger.info(f"Created sample fatigue conditions: {self.fatigue_conditions_path}")
    
    def _calculate_scaling_factors(self):
        """Calculate and store all scaling factors"""
        logger.info("Calculating scaling factors...")
        
        scaling_data = []
        
        for idx, row in self.fatigue_conditions.iterrows():
            # Calculate wind scaling factor
            wind_speed = row['wind_speed']
            wind_scale = (wind_speed / self.base_wind_speed) ** 2
            
            # Calculate wave scaling factor
            hs = row['hs']
            wave_scale = hs / self.base_hs
            
            scaling_data.append({
                'condition_id': row['condition_id'],
                'wind_scale_factor': round(wind_scale, 6),
                'wave_scale_factor': round(wave_scale, 6),
                'wind_formula': f"({wind_speed}/{self.base_wind_speed})^2",
                'wave_formula': f"{hs}/{self.base_hs}",
                'occurrence_pct': row['occurrence_pct']
            })
        
        self.scaling_factors = pd.DataFrame(scaling_data)
        
        # Save scaling factors
        scaling_path = self.output_dir / "scaling_factors.csv"
        self.scaling_factors.to_csv(scaling_path, index=False)
        logger.info(f"Saved scaling factors to: {scaling_path}")
        
        # Log statistics
        logger.info(f"Wind scaling range: {self.scaling_factors['wind_scale_factor'].min():.4f} to "
                   f"{self.scaling_factors['wind_scale_factor'].max():.4f}")
        logger.info(f"Wave scaling range: {self.scaling_factors['wave_scale_factor'].min():.4f} to "
                   f"{self.scaling_factors['wave_scale_factor'].max():.4f}")
    
    def select_reference_seastate(self, seastate_type: str, direction: float, tp: Optional[float] = None) -> str:
        """
        Select the closest reference seastate based on type and direction
        
        Args:
            seastate_type: 'wind' or 'wave'
            direction: Target direction in degrees
            tp: Target peak period (for wave selection)
        
        Returns:
            Reference seastate ID
        """
        # Filter by type
        refs = self.reference_seastates[self.reference_seastates['type'] == seastate_type]
        
        if seastate_type == 'wind':
            # Select by closest direction
            dir_column = 'wind_direction'
        else:
            dir_column = 'wave_direction'
            
            # If Tp provided, also consider period
            if tp is not None:
                # Find closest Tp first
                refs['tp_diff'] = abs(refs['tp'] - tp)
                min_tp_diff = refs['tp_diff'].min()
                refs = refs[refs['tp_diff'] <= min_tp_diff + 0.1]
        
        # Find closest direction (accounting for circular nature)
        refs['dir_diff'] = refs[dir_column].apply(
            lambda x: min(abs(x - direction), 360 - abs(x - direction))
        )
        
        closest_ref = refs.loc[refs['dir_diff'].idxmin()]
        return closest_ref['reference_id']
    
    def scale_time_series(self, time_series: np.ndarray, scale_factor: float) -> np.ndarray:
        """
        Apply scaling factor to time series
        
        Args:
            time_series: Original time series data
            scale_factor: Scaling factor to apply
        
        Returns:
            Scaled time series
        """
        return time_series * scale_factor
    
    def process_fatigue_condition(self, condition_id: int, time_series_data: Optional[Dict] = None) -> Dict:
        """
        Process a single fatigue condition and generate scaled loads
        
        Args:
            condition_id: Fatigue condition ID
            time_series_data: Optional pre-loaded time series data
        
        Returns:
            Dictionary containing scaled load data and metadata
        """
        # Get fatigue condition parameters
        condition = self.fatigue_conditions[
            self.fatigue_conditions['condition_id'] == condition_id
        ].iloc[0]
        
        # Get scaling factors
        scaling = self.scaling_factors[
            self.scaling_factors['condition_id'] == condition_id
        ].iloc[0]
        
        # Select reference seastates
        wind_ref = self.select_reference_seastate(
            'wind', condition['wind_direction']
        )
        wave_ref = self.select_reference_seastate(
            'wave', condition['wave_direction'], condition['tp']
        )
        
        # Generate sample time series if not provided
        if time_series_data is None:
            time_series_data = self._generate_sample_time_series()
        
        # Scale the loads
        wind_loads = self.scale_time_series(
            time_series_data.get(wind_ref, np.zeros(2000)),
            scaling['wind_scale_factor']
        )
        
        wave_loads = self.scale_time_series(
            time_series_data.get(wave_ref, np.zeros(2000)),
            scaling['wave_scale_factor']
        )
        
        # Combine loads (simple addition for effective tension)
        combined_loads = wind_loads + wave_loads
        
        result = {
            'condition_id': condition_id,
            'wind_reference': wind_ref,
            'wave_reference': wave_ref,
            'wind_scale_factor': scaling['wind_scale_factor'],
            'wave_scale_factor': scaling['wave_scale_factor'],
            'occurrence_pct': scaling['occurrence_pct'],
            'wind_loads': wind_loads,
            'wave_loads': wave_loads,
            'combined_loads': combined_loads,
            'statistics': {
                'max_load': float(np.max(combined_loads)),
                'min_load': float(np.min(combined_loads)),
                'mean_load': float(np.mean(combined_loads)),
                'std_load': float(np.std(combined_loads))
            }
        }
        
        return result
    
    def _generate_sample_time_series(self, duration: float = 200.0, dt: float = 0.1) -> Dict:
        """Generate sample time series for demonstration"""
        n_points = int(duration / dt)
        time_series = {}
        
        # Generate for all reference seastates
        for _, ref in self.reference_seastates.iterrows():
            ref_id = ref['reference_id']
            
            if ref['type'] == 'wind':
                # Wind load pattern (more turbulent)
                base_load = 1000 * (ref['wind_speed'] / 10)
                fluctuation = 0.2 * base_load
                loads = base_load + fluctuation * np.random.randn(n_points)
            else:
                # Wave load pattern (sinusoidal with randomness)
                frequency = 1.0 / ref['tp'] if ref['tp'] > 0 else 0.5
                t = np.arange(n_points) * dt
                base_load = 500 * ref['hs'] / 0.5
                loads = base_load * (np.sin(2 * np.pi * frequency * t) + 
                                    0.1 * np.random.randn(n_points))
            
            time_series[ref_id] = loads
        
        return time_series
    
    def process_all_conditions(self, save_outputs: bool = True) -> pd.DataFrame:
        """
        Process all fatigue conditions and generate scaled loads
        
        Args:
            save_outputs: Whether to save individual output files
        
        Returns:
            Summary DataFrame with all processing results
        """
        logger.info(f"Processing {len(self.fatigue_conditions)} fatigue conditions...")
        
        results = []
        time_series_data = self._generate_sample_time_series()
        
        for idx, row in self.fatigue_conditions.iterrows():
            condition_id = row['condition_id']
            
            # Process condition
            result = self.process_fatigue_condition(condition_id, time_series_data)
            
            # Save individual results if requested
            if save_outputs:
                output_path = self.output_dir / f"FC{condition_id:03d}_scaled.npz"
                np.savez_compressed(
                    output_path,
                    wind_loads=result['wind_loads'],
                    wave_loads=result['wave_loads'],
                    combined_loads=result['combined_loads'],
                    metadata=json.dumps({
                        k: v for k, v in result.items()
                        if k not in ['wind_loads', 'wave_loads', 'combined_loads']
                    })
                )
            
            # Add to summary
            results.append({
                'condition_id': condition_id,
                'wind_ref': result['wind_reference'],
                'wave_ref': result['wave_reference'],
                'wind_scale': result['wind_scale_factor'],
                'wave_scale': result['wave_scale_factor'],
                'occurrence_pct': result['occurrence_pct'],
                'max_load': result['statistics']['max_load'],
                'mean_load': result['statistics']['mean_load'],
                'std_load': result['statistics']['std_load']
            })
            
            if (idx + 1) % 10 == 0:
                logger.info(f"Processed {idx + 1}/{len(self.fatigue_conditions)} conditions")
        
        # Create summary DataFrame
        summary_df = pd.DataFrame(results)
        
        # Save summary
        summary_path = self.output_dir / "load_scaling_summary.csv"
        summary_df.to_csv(summary_path, index=False)
        logger.info(f"Saved summary to: {summary_path}")
        
        # Generate report
        self._generate_report(summary_df)
        
        return summary_df
    
    def _generate_report(self, summary_df: pd.DataFrame):
        """Generate processing report"""
        report_path = self.output_dir / "load_scaling_report.txt"
        
        with open(report_path, 'w') as f:
            f.write("=" * 80 + "\n")
            f.write("LOAD SCALING REPORT\n")
            f.write("=" * 80 + "\n\n")
            
            f.write(f"Generated: {datetime.now().isoformat()}\n")
            f.write(f"Total Conditions Processed: {len(summary_df)}\n\n")
            
            f.write("SCALING FACTOR STATISTICS:\n")
            f.write(f"  Wind Scale Range: {summary_df['wind_scale'].min():.4f} - {summary_df['wind_scale'].max():.4f}\n")
            f.write(f"  Wave Scale Range: {summary_df['wave_scale'].min():.4f} - {summary_df['wave_scale'].max():.4f}\n\n")
            
            f.write("LOAD STATISTICS:\n")
            f.write(f"  Maximum Load: {summary_df['max_load'].max():.2f} kN\n")
            f.write(f"  Mean Load Range: {summary_df['mean_load'].min():.2f} - {summary_df['mean_load'].max():.2f} kN\n")
            f.write(f"  Total Occurrence Check: {summary_df['occurrence_pct'].sum():.2f}%\n\n")
            
            f.write("OUTPUT FILES:\n")
            f.write(f"  Scaling Factors: scaling_factors.csv\n")
            f.write(f"  Load Summary: load_scaling_summary.csv\n")
            f.write(f"  Individual Conditions: FC###_scaled.npz\n")
        
        logger.info(f"Generated report: {report_path}")


def main():
    """Main execution function"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Load Scaling for Fatigue Analysis")
    parser.add_argument('--reference-seastates', default='input/reference_seastates.csv',
                       help='Path to reference seastates file')
    parser.add_argument('--fatigue-conditions', default='input/fatigue_conditions.csv',
                       help='Path to fatigue conditions file')
    parser.add_argument('--output-dir', default='output/scaled_loads',
                       help='Output directory for scaled loads')
    parser.add_argument('--save-individual', action='store_true',
                       help='Save individual scaled load files')
    
    args = parser.parse_args()
    
    # Create load scaler
    scaler = LoadScaler(
        reference_seastates_path=args.reference_seastates,
        fatigue_conditions_path=args.fatigue_conditions,
        output_dir=args.output_dir
    )
    
    # Process all conditions
    summary = scaler.process_all_conditions(save_outputs=args.save_individual)
    
    logger.info("Load scaling complete!")
    logger.info(f"Processed {len(summary)} fatigue conditions")
    logger.info(f"Results saved to: {args.output_dir}")


if __name__ == "__main__":
    main()