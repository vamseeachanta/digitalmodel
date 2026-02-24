"""
Data Transformation Script
Converts tension ranges to stress ranges using lookup table with linear interpolation.
Creates separate output files for each location ID found in the lookup table.
"""

import pandas as pd
import numpy as np
from scipy import interpolate
import json
import yaml
import os
import glob
import re
from pathlib import Path
import logging

class TensionToStressTransformer:
    def __init__(self, config_file):
        """Initialize transformer with configuration file."""
        self.config = self.load_config(config_file)
        self.setup_logging()
        self.lookup_table = None
        self.interpolators = {}
        
    def load_config(self, config_file):
        """Load configuration from YAML or JSON file."""
        with open(config_file, 'r') as f:
            if config_file.endswith('.yaml') or config_file.endswith('.yml'):
                return yaml.safe_load(f)
            else:
                return json.load(f)
    
    def setup_logging(self):
        """Configure logging based on config settings."""
        log_config = self.config.get('logging', {})
        level = getattr(logging, log_config.get('level', 'INFO'))
        log_file = log_config.get('file', 'transformation_log.txt')
        
        logging.basicConfig(
            level=level,
            format='%(asctime)s - %(levelname)s - %(message)s',
            handlers=[
                logging.FileHandler(log_file),
                logging.StreamHandler()
            ]
        )
        self.logger = logging.getLogger(__name__)
    
    def load_lookup_table(self):
        """Load and process the lookup table."""
        lookup_config = self.config['transformation']['lookup_table']
        self.lookup_table = pd.read_csv(lookup_config['file'])
        
        # Clean column names (remove leading/trailing spaces)
        self.lookup_table.columns = self.lookup_table.columns.str.strip()
        
        # Get unique location IDs
        location_col = lookup_config['location_id_column']
        self.location_ids = self.lookup_table[location_col].unique()
        self.logger.info(f"Found location IDs: {self.location_ids}")
        
        # Get unique configs (strip spaces)
        config_col = lookup_config['config_column']
        self.lookup_table[config_col] = self.lookup_table[config_col].str.strip()
        self.unique_configs = self.lookup_table[config_col].unique()
        self.logger.info(f"Found configs in lookup table: {self.unique_configs}")
        
    def create_interpolators(self):
        """Create interpolation functions for each location ID and config combination."""
        lookup_config = self.config['transformation']['lookup_table']
        x_col = lookup_config['x_column']
        y_col = lookup_config['y_column']
        location_col = lookup_config['location_id_column']
        config_col = lookup_config['config_column']
        
        interp_method = self.config['transformation']['interpolation']['method']
        extrapolate = self.config['transformation']['interpolation']['extrapolate']
        
        for location_id in self.location_ids:
            self.interpolators[location_id] = {}
            
            for config in self.unique_configs:
                # Filter lookup table for this location and config
                mask = (self.lookup_table[location_col] == location_id) & \
                       (self.lookup_table[config_col] == config)
                subset = self.lookup_table[mask].copy()
                
                if len(subset) > 0:
                    # Sort by x values
                    subset = subset.sort_values(x_col)
                    x_values = subset[x_col].values
                    y_values = subset[y_col].values
                    
                    # Create interpolator
                    if extrapolate:
                        fill_value = 'extrapolate'
                    else:
                        fill_value = (y_values[0], y_values[-1])
                    
                    interpolator = interpolate.interp1d(
                        x_values, y_values, 
                        kind=interp_method,
                        fill_value=fill_value,
                        bounds_error=False
                    )
                    
                    # Store with cleaned config key
                    self.interpolators[location_id][config.strip() if isinstance(config, str) else config] = interpolator
                    self.logger.debug(f"Created interpolator for location {location_id}, config {config}")
    
    def map_config(self, filename_config):
        """Map filename config to lookup table config."""
        config_mapping = self.config['transformation']['lookup_table'].get('config_mapping', {})
        
        # Try exact match first
        if filename_config in config_mapping:
            return config_mapping[filename_config]
        
        # Try partial matches
        for pattern, mapped_config in config_mapping.items():
            if pattern in filename_config:
                return mapped_config
        
        # Return original if no mapping found
        return filename_config
    
    def process_file(self, input_file):
        """Process a single input file for all location IDs."""
        self.logger.info(f"Processing file: {input_file}")
        
        # Extract metadata from filename
        filename = os.path.basename(input_file)
        pattern = self.config['raw_data']['naming_pattern']['regex']
        match = re.match(pattern, filename)
        
        if not match:
            self.logger.warning(f"Filename doesn't match pattern: {filename}")
            return
        
        groups = self.config['raw_data']['naming_pattern']['groups']
        metadata = {
            'config': match.group(groups['config']),
            'fc_number': match.group(groups['fc_number']),
            'strut_number': match.group(groups['strut_number'])
        }
        
        # Map config to lookup table config
        lookup_config = self.map_config(metadata['config'])
        self.logger.debug(f"Mapped config '{metadata['config']}' to '{lookup_config}'")
        
        # Load input data
        df = pd.read_csv(input_file)
        data_col = self.config['raw_data']['data_column']
        
        # Process for each location ID
        for location_id in self.location_ids:
            if lookup_config not in self.interpolators[location_id]:
                self.logger.warning(f"No interpolator for location {location_id}, config {lookup_config}")
                continue
            
            # Create output dataframe
            output_df = pd.DataFrame()
            
            # Apply transformation
            interpolator = self.interpolators[location_id][lookup_config]
            tension_values = df[data_col].values
            
            # Skip empty ranges if configured
            if self.config['processing']['skip_empty_ranges']:
                mask = (df[data_col] > 0) | (df.get('Cycles_Annual', 0) > 0)
                output_df = df[mask].copy()
                tension_values = output_df[data_col].values
            else:
                output_df = df.copy()
            
            # Calculate stress values
            stress_values = interpolator(tension_values)
            
            # Build output dataframe with specified columns
            output_data = {}
            for col_config in self.config['output']['columns']:
                if col_config['source'] == 'transformation':
                    output_data[col_config['name']] = stress_values
                elif col_config['source'] == 'input':
                    if col_config['name'] in output_df.columns:
                        output_data[col_config['name']] = output_df[col_config['name']].values
            
            result_df = pd.DataFrame(output_data)
            
            # Generate output filename with zero-padded location ID
            output_pattern = self.config['output']['file_pattern']
            # Handle the location_id formatting
            if '{location_id:02d}' in output_pattern:
                # Format string expects zero-padded integer
                output_filename = output_pattern.format(
                    config=metadata['config'],
                    fc_number=metadata['fc_number'],
                    strut_number=metadata['strut_number'],
                    location_id=int(location_id)
                )
            else:
                # Fallback for backward compatibility
                output_filename = output_pattern.format(
                    config=metadata['config'],
                    fc_number=metadata['fc_number'],
                    strut_number=metadata['strut_number'],
                    location_id=f"{int(location_id):02d}"
                )
            
            # Create output path
            output_folder = self.config['output']['folder']
            os.makedirs(output_folder, exist_ok=True)
            output_path = os.path.join(output_folder, output_filename)
            
            # Save output
            result_df.to_csv(output_path, index=False)
            self.logger.info(f"Saved output to: {output_path}")
    
    def process_all_files(self):
        """Process all files matching the pattern."""
        input_folder = self.config['raw_data']['folder']
        file_pattern = self.config['raw_data']['file_pattern']
        
        # Find all matching files
        search_pattern = os.path.join(input_folder, file_pattern)
        input_files = glob.glob(search_pattern)
        
        self.logger.info(f"Found {len(input_files)} files to process")
        
        # Process each file
        for input_file in input_files:
            try:
                self.process_file(input_file)
            except Exception as e:
                error_handling = self.config['processing']['error_handling']
                if error_handling == 'fail':
                    raise
                elif error_handling == 'log':
                    self.logger.error(f"Error processing {input_file}: {str(e)}")
                    continue
                elif error_handling == 'skip':
                    continue
    
    def run(self):
        """Execute the complete transformation process."""
        self.logger.info("Starting tension to stress transformation")
        
        # Load lookup table
        self.load_lookup_table()
        
        # Create interpolators
        self.create_interpolators()
        
        # Process all files
        if self.config['processing']['process_all_files']:
            self.process_all_files()
        
        self.logger.info("Transformation complete")


if __name__ == "__main__":
    import sys
    
    # Default config file - updated to test version
    config_file = "specs/modules/data-transformation/lookup-table/inputs/test_transformation_config.yaml"
    
    # Allow config file to be specified as command line argument
    if len(sys.argv) > 1:
        config_file = sys.argv[1]
    
    # Create and run transformer
    transformer = TensionToStressTransformer(config_file)
    transformer.run()