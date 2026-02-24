#!/usr/bin/env python
"""
Production Transformation Runner
Processes 2,592 rainflow files from CTR9 fatigue analysis
Converts tension ranges to stress ranges using lookup table interpolation
"""

import os
import sys
import glob
import pandas as pd
import numpy as np
from scipy import interpolate
import yaml
from pathlib import Path
from datetime import datetime
import logging
from concurrent.futures import ProcessPoolExecutor, as_completed
from tqdm import tqdm
import warnings
warnings.filterwarnings('ignore')

class ProductionTransformer:
    """Production-grade transformer with batch processing and error handling."""
    
    def __init__(self, config_file='production_transformation_config.yaml'):
        """Initialize with production configuration."""
        self.config_file = config_file
        self.load_config()
        self.setup_logging()
        self.interpolators = {}  # Cache for interpolators
        self.stats = {
            'files_processed': 0,
            'files_skipped': 0,
            'total_rows': 0,
            'start_time': datetime.now()
        }
        
    def load_config(self):
        """Load production configuration."""
        with open(self.config_file, 'r') as f:
            self.config = yaml.safe_load(f)
        
        # Extract key paths
        self.input_folder = self.config['input']['folder']
        self.output_folder = self.config['output']['folder']
        self.lookup_file = self.config['lookup_table']['file']
        
    def setup_logging(self):
        """Setup production logging."""
        log_format = '%(asctime)s - %(levelname)s - %(message)s'
        logging.basicConfig(
            level=getattr(logging, self.config['processing']['log_level']),
            format=log_format,
            handlers=[
                logging.FileHandler('production_transformation.log'),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
        
    def load_lookup_table(self):
        """Load and validate lookup table."""
        self.logger.info(f"Loading lookup table from {self.lookup_file}")
        
        try:
            self.lookup_table = pd.read_csv(self.lookup_file)
            
            # Clean column names and values
            self.lookup_table.columns = self.lookup_table.columns.str.strip()
            config_col = self.config['lookup_table']['columns']['config']
            self.lookup_table[config_col] = self.lookup_table[config_col].str.strip()
            
            # Get unique locations
            self.location_ids = sorted(self.lookup_table[
                self.config['lookup_table']['columns']['location_id']
            ].unique())
            
            self.logger.info(f"Loaded {len(self.lookup_table)} lookup records")
            self.logger.info(f"Location IDs: {self.location_ids}")
            
            # Validate monotonicity if required
            if self.config['validation']['check_monotonicity']:
                self.validate_monotonicity()
                
        except Exception as e:
            self.logger.error(f"Failed to load lookup table: {e}")
            raise
            
    def validate_monotonicity(self):
        """Validate lookup table monotonicity."""
        self.logger.info("Validating monotonicity...")
        
        location_col = self.config['lookup_table']['columns']['location_id']
        config_col = self.config['lookup_table']['columns']['config']
        tension_col = self.config['lookup_table']['columns']['tension']
        stress_col = self.config['lookup_table']['columns']['stress']
        
        for location_id in self.location_ids:
            for config in self.lookup_table[config_col].unique():
                subset = self.lookup_table[
                    (self.lookup_table[location_col] == location_id) &
                    (self.lookup_table[config_col] == config)
                ].sort_values(tension_col)
                
                if len(subset) > 2:
                    x = subset[tension_col].values
                    y = subset[stress_col].values
                    
                    # Check R-squared
                    correlation = np.corrcoef(x, y)[0, 1]
                    r_squared = correlation ** 2
                    
                    if r_squared < self.config['validation']['min_r_squared']:
                        self.logger.warning(
                            f"Low R² ({r_squared:.4f}) for location {location_id}, config {config}"
                        )
                        
    def get_or_create_interpolator(self, location_id, config):
        """Get cached interpolator or create new one."""
        cache_key = f"{location_id}_{config}"
        
        if cache_key in self.interpolators:
            return self.interpolators[cache_key]
            
        # Try exact config match
        location_col = self.config['lookup_table']['columns']['location_id']
        config_col = self.config['lookup_table']['columns']['config']
        tension_col = self.config['lookup_table']['columns']['tension']
        stress_col = self.config['lookup_table']['columns']['stress']
        
        lookup_subset = self.lookup_table[
            (self.lookup_table[location_col] == location_id) &
            (self.lookup_table[config_col] == config)
        ].sort_values(tension_col)
        
        # Try config mapping if no exact match
        if len(lookup_subset) == 0:
            config_mapping = self.config['transformation'].get('config_mapping', {})
            for key, mapped_config in config_mapping.items():
                if key in config:
                    lookup_subset = self.lookup_table[
                        (self.lookup_table[location_col] == location_id) &
                        (self.lookup_table[config_col] == mapped_config)
                    ].sort_values(tension_col)
                    if len(lookup_subset) > 0:
                        break
                        
        if len(lookup_subset) == 0:
            return None
            
        # Create interpolator
        x = lookup_subset[tension_col].values
        y = lookup_subset[stress_col].values
        
        interpolator = interpolate.interp1d(
            x, y,
            kind=self.config['transformation']['method'].replace('_interpolation', ''),
            bounds_error=self.config['transformation']['bounds_error'],
            fill_value=self.config['transformation']['fill_value']
        )
        
        # Cache it
        if self.config['optimization']['cache_interpolators']:
            self.interpolators[cache_key] = interpolator
            
        return interpolator
        
    def process_file(self, input_file):
        """Process single rainflow file."""
        try:
            # Extract metadata from filename
            filename = Path(input_file).stem
            parts = filename.split('_')
            
            # Find FC and Strut numbers
            fc_number = None
            strut_number = None
            config_parts = []
            
            for i, part in enumerate(parts):
                if part.startswith('FC') and len(part) > 2:
                    fc_number = part[2:]
                elif part.startswith('Strut') and len(part) > 5:
                    strut_number = part[5:]
                elif part != 'rainflow':
                    config_parts.append(part)
                    
            config = '_'.join(config_parts)
            
            if not fc_number or not strut_number:
                self.logger.warning(f"Could not parse filename: {filename}")
                return False
                
            # Load input data
            df = pd.read_csv(input_file)
            tension_col = self.config['input']['data_columns']['tension']
            cycles_col = self.config['input']['data_columns']['cycles']
            
            if tension_col not in df.columns or cycles_col not in df.columns:
                self.logger.warning(f"Required columns not found in {filename}")
                return False
                
            # Process for each location
            outputs_created = 0
            for location_id in self.location_ids:
                interpolator = self.get_or_create_interpolator(location_id, config)
                
                if interpolator is None:
                    continue
                    
                # Transform data
                df_output = pd.DataFrame()
                df_output['stress range (Mpa)'] = df[tension_col].apply(
                    lambda x: float(interpolator(x)) if x > 0 else 0
                )
                df_output['Cycles_Annual'] = df[cycles_col]
                
                # Create output filename
                output_pattern = self.config['output']['file_pattern']
                output_filename = output_pattern.format(
                    config=config,
                    fc_number=fc_number,
                    strut_number=strut_number,
                    location_id=int(location_id)
                )
                
                output_path = Path(self.output_folder) / output_filename
                output_path.parent.mkdir(parents=True, exist_ok=True)
                
                # Save output
                df_output.to_csv(output_path, index=False)
                outputs_created += 1
                
            self.stats['files_processed'] += 1
            self.stats['total_rows'] += len(df)
            
            return outputs_created > 0
            
        except Exception as e:
            self.logger.error(f"Error processing {input_file}: {e}")
            self.stats['files_skipped'] += 1
            
            if not self.config['processing']['skip_errors']:
                raise
                
            return False
            
    def process_batch(self, file_batch):
        """Process batch of files."""
        results = []
        for file in file_batch:
            result = self.process_file(file)
            results.append(result)
        return results
        
    def run_production(self, sample_size=None):
        """Run production transformation on all files."""
        self.logger.info("=" * 70)
        self.logger.info("STARTING PRODUCTION TRANSFORMATION")
        self.logger.info(f"Input: {self.input_folder}")
        self.logger.info(f"Output: {self.output_folder}")
        self.logger.info("=" * 70)
        
        # Load lookup table
        self.load_lookup_table()
        
        # Find all input files
        pattern = os.path.join(self.input_folder, self.config['input']['file_pattern'])
        input_files = glob.glob(pattern)
        
        # Apply sample size if specified
        if sample_size:
            input_files = input_files[:sample_size]
            self.logger.info(f"Processing sample of {len(input_files)} files (from {len(glob.glob(pattern))} total)")
        else:
            self.logger.info(f"Found {len(input_files)} input files")
        
        if len(input_files) == 0:
            self.logger.error("No input files found!")
            return
            
        # Create output directory
        Path(self.output_folder).mkdir(parents=True, exist_ok=True)
        
        # Process files - use sequential for now to avoid multiprocessing issues
        self.process_sequential(input_files)
            
        # Final report
        self.print_summary()
        
    def process_sequential(self, input_files):
        """Process files sequentially with progress bar."""
        self.logger.info("Processing files sequentially...")
        
        with tqdm(total=len(input_files), desc="Processing") as pbar:
            for input_file in input_files:
                self.process_file(input_file)
                pbar.update(1)
                
    def process_parallel(self, input_files):
        """Process files in parallel."""
        self.logger.info(f"Processing files in parallel ({self.config['processing']['max_workers']} workers)...")
        
        # Split into batches
        batch_size = self.config['processing']['batch_size']
        batches = [input_files[i:i+batch_size] for i in range(0, len(input_files), batch_size)]
        
        with ProcessPoolExecutor(max_workers=self.config['processing']['max_workers']) as executor:
            futures = {executor.submit(self.process_batch, batch): batch for batch in batches}
            
            with tqdm(total=len(input_files), desc="Processing") as pbar:
                for future in as_completed(futures):
                    batch = futures[future]
                    try:
                        results = future.result()
                        pbar.update(len(batch))
                    except Exception as e:
                        self.logger.error(f"Batch processing failed: {e}")
                        
    def print_summary(self):
        """Print processing summary."""
        elapsed = datetime.now() - self.stats['start_time']
        
        self.logger.info("=" * 70)
        self.logger.info("PRODUCTION TRANSFORMATION COMPLETE")
        self.logger.info("-" * 70)
        self.logger.info(f"Files processed: {self.stats['files_processed']}")
        self.logger.info(f"Files skipped: {self.stats['files_skipped']}")
        self.logger.info(f"Total rows: {self.stats['total_rows']:,}")
        self.logger.info(f"Output files created: {self.stats['files_processed'] * len(self.location_ids)}")
        self.logger.info(f"Processing time: {elapsed}")
        self.logger.info(f"Average time per file: {elapsed.total_seconds() / max(self.stats['files_processed'], 1):.2f}s")
        self.logger.info("=" * 70)
        
        # Save summary to file
        summary = {
            'timestamp': datetime.now().isoformat(),
            'stats': self.stats,
            'elapsed_seconds': elapsed.total_seconds(),
            'config_file': self.config_file
        }
        
        with open('production_summary.yaml', 'w') as f:
            yaml.dump(summary, f, default_flow_style=False)
            

def main():
    """Main entry point."""
    import argparse
    
    parser = argparse.ArgumentParser(description='Production Tension-to-Stress Transformation')
    parser.add_argument('--config', default='production_transformation_config.yaml',
                       help='Configuration file path')
    parser.add_argument('--dry-run', action='store_true',
                       help='Preview without processing')
    parser.add_argument('--sample', type=int,
                       help='Process only N sample files')
    
    args = parser.parse_args()
    
    # Initialize transformer
    transformer = ProductionTransformer(args.config)
    
    if args.dry_run:
        print("DRY RUN MODE - Preview only")
        transformer.load_lookup_table()
        
        pattern = os.path.join(transformer.input_folder, transformer.config['input']['file_pattern'])
        input_files = glob.glob(pattern)[:args.sample if args.sample else 10]
        
        print(f"\nWould process {len(glob.glob(pattern))} files")
        print(f"Sample files:")
        for f in input_files[:5]:
            print(f"  - {Path(f).name}")
        print(f"\nOutput to: {transformer.output_folder}")
        print(f"Locations: {transformer.location_ids}")
        print(f"Expected output files: {len(glob.glob(pattern)) * len(transformer.location_ids)}")
        
    else:
        # Run production transformation
        transformer.run_production(sample_size=args.sample)
        

if __name__ == "__main__":
    main()