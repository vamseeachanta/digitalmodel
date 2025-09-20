#!/usr/bin/env python
"""
Extract representative sample data from production for testing
Creates a local sample_data directory with:
- All 4 configurations
- 2-3 reference seastates per configuration  
- 2 struts per reference
- 1000 timesteps (100 seconds) per file
"""

import os
import pandas as pd
import numpy as np
from pathlib import Path
import shutil
import logging

logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

def extract_sample_data():
    """Extract sample data from production"""
    
    # Production data path
    prod_base = Path("D:/1522/ctr9/fatigue_wsp_method/07c_fatigue/csv")
    
    # Local sample data path
    sample_base = Path("sample_data")
    
    # Clear and recreate sample_data directory
    if sample_base.exists():
        shutil.rmtree(sample_base)
    sample_base.mkdir()
    
    # Configurations to sample
    configs = [
        'fsts_l015',
        'fsts_l095', 
        'fsts_l015_125km3_l100_pb',
        'fsts_l095_125km3_l000_pb'
    ]
    
    # Statistics collector
    extraction_stats = []
    
    for config in configs:
        logger.info(f"\nProcessing configuration: {config}")
        
        config_prod = prod_base / config
        config_sample = sample_base / config
        
        if not config_prod.exists():
            logger.warning(f"  Configuration {config} does not exist in production")
            # Create synthetic data for this configuration
            config_sample.mkdir(parents=True)
            create_synthetic_config_data(config_sample, config)
            continue
        
        config_sample.mkdir(parents=True)
        
        # Get all reference directories
        ref_dirs = [d for d in config_prod.iterdir() if d.is_dir()]
        
        # Select subset of references (max 3: 2 wind, 1 wave)
        wind_refs = [d for d in ref_dirs if d.name.startswith('wind')][:2]
        wave_refs = [d for d in ref_dirs if d.name.startswith('wave')][:1]
        selected_refs = wind_refs + wave_refs
        
        if not selected_refs:
            logger.warning(f"  No reference directories found for {config}")
            continue
        
        for ref_dir in selected_refs:
            logger.info(f"  Extracting: {ref_dir.name}")
            
            # Create reference directory in sample
            ref_sample = config_sample / ref_dir.name
            ref_sample.mkdir()
            
            # Extract data for struts 1 and 2 only
            for strut_num in [1, 2]:
                strut_file = ref_dir / f"Strut{strut_num}.csv"
                
                if not strut_file.exists():
                    logger.warning(f"    Strut{strut_num}.csv not found")
                    continue
                
                try:
                    # Read original file
                    df = pd.read_csv(strut_file, encoding='latin-1', nrows=1001)  # 1000 timesteps + header
                    
                    # Save to sample directory
                    sample_file = ref_sample / f"Strut{strut_num}.csv"
                    df.to_csv(sample_file, index=False)
                    
                    # Collect stats
                    stats = {
                        'config': config,
                        'reference': ref_dir.name,
                        'strut': strut_num,
                        'rows': len(df),
                        'columns': len(df.columns),
                        'file_size_kb': sample_file.stat().st_size / 1024
                    }
                    extraction_stats.append(stats)
                    
                    logger.info(f"    Extracted Strut{strut_num}: {len(df)} rows, {len(df.columns)} columns")
                    
                except Exception as e:
                    logger.error(f"    Error extracting Strut{strut_num}: {e}")
    
    # Save extraction statistics
    if extraction_stats:
        stats_df = pd.DataFrame(extraction_stats)
        stats_df.to_csv(sample_base / "extraction_stats.csv", index=False)
        
        logger.info("\n" + "="*60)
        logger.info("EXTRACTION SUMMARY")
        logger.info("="*60)
        logger.info(f"Configurations: {stats_df['config'].nunique()}")
        logger.info(f"Reference seastates: {stats_df['reference'].nunique()}")
        logger.info(f"Total files: {len(stats_df)}")
        logger.info(f"Total size: {stats_df['file_size_kb'].sum():.1f} KB")
        logger.info(f"Sample data saved to: {sample_base.absolute()}")
    
    return extraction_stats

def create_synthetic_config_data(config_path: Path, config_name: str):
    """Create synthetic data for missing configurations"""
    logger.info(f"  Creating synthetic data for {config_name}")
    
    # Create sample reference directories
    ref_dirs = [
        'wind_000deg',
        'wind_090deg', 
        'wave_000deg_Hs050cm_Tp270cs'
    ]
    
    for ref_dir_name in ref_dirs:
        ref_path = config_path / ref_dir_name
        ref_path.mkdir()
        
        # Create synthetic strut data
        for strut_num in [1, 2]:
            # Generate synthetic time series
            time = np.arange(0, 100, 0.1)  # 100 seconds at 0.1s intervals
            
            # Base tension with some variation based on config
            if 'l015' in config_name:
                base_tension = 500  # Lower tension for light config
            else:
                base_tension = 800  # Higher tension for full config
            
            # Add sinusoidal variation
            if 'wind' in ref_dir_name:
                # Wind: higher frequency, lower amplitude
                tension = base_tension + 50 * np.sin(2 * np.pi * 0.5 * time) + \
                         20 * np.sin(2 * np.pi * 1.5 * time)
            else:
                # Wave: lower frequency, higher amplitude  
                tension = base_tension + 100 * np.sin(2 * np.pi * 0.2 * time) + \
                         30 * np.sin(2 * np.pi * 0.7 * time)
            
            # Add some noise
            tension += np.random.normal(0, 5, len(time))
            
            # Create DataFrame with multiple columns (simulating production format)
            df = pd.DataFrame({
                'Time (s)': time,
                'Effective Tension at FST Vessel End (kN)': tension,
                'Effective Tension at FST Jacket End (kN)': tension * 0.95,
                'Effective Tension at Jacket End (kN)': tension * 0.90,
                'Dummy Column 1': np.zeros(len(time)),
                'Dummy Column 2': np.zeros(len(time))
            })
            
            # Save file
            output_file = ref_path / f"Strut{strut_num}.csv"
            df.to_csv(output_file, index=False)
            
            logger.info(f"    Created synthetic {ref_dir_name}/Strut{strut_num}.csv")

if __name__ == "__main__":
    extract_sample_data()