#!/usr/bin/env python
"""
Production Data Processor with Progress Tracking
Enhanced version with progress bars for long operations
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
from typing import Dict, List, Tuple, Optional
import json
from dataclasses import dataclass, asdict
import logging
from datetime import datetime
from tqdm import tqdm

# Import base processor
from .strut_foundation_processor import (
    Configuration, FatigueCondition, ProductionDataHandler, LoadScaler
)

# Set up logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class ProductionDataHandlerWithProgress(ProductionDataHandler):
    """Enhanced handler with progress tracking"""
    
    def process_all_configurations(self, fatigue_conditions: List[FatigueCondition],
                                  struts: List[int] = None,
                                  output_dir: Path = None) -> Dict:
        """Process all configurations with progress tracking"""
        
        if struts is None:
            struts = list(range(1, 9))  # All 8 struts
        
        if output_dir is None:
            output_dir = Path("output")
        output_dir.mkdir(exist_ok=True)
        
        results = {}
        
        # Calculate total operations for progress bar
        total_ops = len(self.configurations) * len(fatigue_conditions) * len(struts)
        
        with tqdm(total=total_ops, desc="Processing", unit="analysis") as pbar:
            for config_name, config in self.configurations.items():
                pbar.set_description(f"Config: {config_name}")
                
                # Check if configuration has files
                pattern = f"{config_name}_mwl_*_Strut*.csv"
                config_files = list(self.base_path.glob(pattern))
                if not config_files:
                    logger.warning(f"Skipping {config_name} - no files found")
                    pbar.update(len(fatigue_conditions) * len(struts))
                    continue
                
                config_results = []
                config_output = output_dir / config_name
                config_output.mkdir(exist_ok=True)
                
                for fc in fatigue_conditions:
                    for strut_num in struts:
                        pbar.set_postfix(FC=f"{fc.id:03d}", Strut=strut_num)
                        
                        # Process this combination
                        result = self.process_single_analysis(
                            config_name, fc, strut_num, config_output
                        )
                        if result:
                            config_results.append(result)
                        
                        pbar.update(1)
                
                results[config_name] = config_results
        
        return results
    
    def process_single_analysis(self, config_name: str, 
                               condition: FatigueCondition,
                               strut_num: int,
                               output_dir: Path) -> Optional[Dict]:
        """Process a single fatigue analysis"""
        
        # Initialize scaler
        scaler = LoadScaler(self)
        
        # Generate combined effective tension
        effective_tension, metadata = scaler.process_fatigue_condition(
            config_name, condition, strut_num
        )
        
        if len(effective_tension) == 0:
            return None
        
        # Save combined tension
        output_file = output_dir / f"FC{condition.id:03d}_Strut{strut_num}_combined.csv"
        
        df_output = pd.DataFrame({
            'time_s': np.arange(len(effective_tension)) * 0.1,
            'effective_tension_kN': effective_tension
        })
        df_output.to_csv(output_file, index=False)
        
        # Return statistics
        return {
            'config': config_name,
            'fc_id': condition.id,
            'strut': strut_num,
            'max_tension': float(np.max(effective_tension)),
            'min_tension': float(np.min(effective_tension)),
            'mean_tension': float(np.mean(effective_tension)),
            'std_tension': float(np.std(effective_tension)),
            'samples': len(effective_tension),
            **metadata
        }

class BatchProcessor:
    """Batch processing with progress tracking"""
    
    def __init__(self, base_path: str = None):
        self.handler = ProductionDataHandlerWithProgress(base_path=base_path)
        self.scaler = LoadScaler(self.handler)
    
    def run_batch_analysis(self, config_file: str = None) -> None:
        """Run batch fatigue analysis with progress tracking"""
        
        print("=" * 60)
        print("FATIGUE ANALYSIS - BATCH PROCESSING")
        print("=" * 60)
        
        # Load configuration
        if config_file and Path(config_file).exists():
            print(f"Loading configuration from: {config_file}")
            with open(config_file, 'r') as f:
                batch_config = json.load(f)
            
            fatigue_conditions = [
                FatigueCondition(**fc) for fc in batch_config['fatigue_conditions']
            ]
            struts = batch_config.get('struts', list(range(1, 9)))
            output_dir = Path(batch_config.get('output_dir', 'output'))
        else:
            print("Using default configuration (demo mode)")
            fatigue_conditions = self.scaler.fatigue_conditions[:3]  # First 3 for demo
            struts = [1, 2]  # First 2 struts for demo
            output_dir = Path('output')
        
        print(f"\nConfiguration Summary:")
        print(f"  - Vessel configs: {len(self.handler.configurations)}")
        print(f"  - Fatigue conditions: {len(fatigue_conditions)}")
        print(f"  - Struts: {struts}")
        print(f"  - Output directory: {output_dir}")
        
        # Run analysis with progress tracking
        print("\nStarting batch processing...")
        results = self.handler.process_all_configurations(
            fatigue_conditions, struts, output_dir
        )
        
        # Save summary
        self.save_summary(results, output_dir)
        
        print("\n" + "=" * 60)
        print("BATCH PROCESSING COMPLETE")
        print("=" * 60)
        
        # Print summary statistics
        total_analyses = sum(len(r) for r in results.values())
        print(f"\nSummary:")
        print(f"  - Total analyses completed: {total_analyses}")
        print(f"  - Output saved to: {output_dir.absolute()}")
        
        if total_analyses > 0:
            all_results = []
            for config_results in results.values():
                all_results.extend(config_results)
            
            df_summary = pd.DataFrame(all_results)
            print(f"  - Max tension observed: {df_summary['max_tension'].max():.2f} kN")
            print(f"  - Min tension observed: {df_summary['min_tension'].min():.2f} kN")
            print(f"  - Mean tension overall: {df_summary['mean_tension'].mean():.2f} kN")
    
    def save_summary(self, results: Dict, output_dir: Path) -> None:
        """Save processing summary with progress bar"""
        
        all_results = []
        for config_results in results.values():
            all_results.extend(config_results)
        
        if all_results:
            print("\nSaving summary...")
            df_summary = pd.DataFrame(all_results)
            summary_file = output_dir / "processing_summary.csv"
            df_summary.to_csv(summary_file, index=False)
            
            # Also save as JSON for easier reading
            json_file = output_dir / "processing_summary.json"
            with open(json_file, 'w') as f:
                json.dump(all_results, f, indent=2)
            
            print(f"  - Summary saved to: {summary_file}")
            print(f"  - JSON saved to: {json_file}")

def main(data_path=None, config_file=None):
    """Main execution with progress tracking"""
    
    processor = BatchProcessor(base_path=data_path)
    processor.run_batch_analysis(config_file=config_file)

if __name__ == "__main__":
    import sys
    
    # Parse command line arguments
    data_path = None
    config_file = None
    
    for i, arg in enumerate(sys.argv[1:]):
        if arg == '--data' and i + 1 < len(sys.argv) - 1:
            data_path = sys.argv[i + 2]
        elif arg == '--config' and i + 1 < len(sys.argv) - 1:
            config_file = sys.argv[i + 2]
    
    main(data_path=data_path, config_file=config_file)