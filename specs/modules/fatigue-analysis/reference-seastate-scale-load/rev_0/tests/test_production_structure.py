#!/usr/bin/env python
"""
Test the updated strut_foundation_processor with production-style data structure
"""

import sys
import os
from pathlib import Path

# Add the src directory to the path
src_path = Path(__file__).parent.parent.parent.parent.parent / "src"
sys.path.insert(0, str(src_path))

from digitalmodel.fatigue_analysis.strut_foundation_processor import (
    ProductionDataHandler, LoadScaler
)

def test_production_structure():
    """Test that the processor can handle production-style flat structure"""
    
    print("=" * 60)
    print("TESTING PRODUCTION-STYLE DATA STRUCTURE")
    print("=" * 60)
    
    # Use the sample data in spec folder
    sample_path = Path(__file__).parent / "sample_data"
    print(f"\nUsing sample data path: {sample_path}")
    
    # Initialize handler
    handler = ProductionDataHandler(base_path=str(sample_path), sample_timesteps=100)
    
    # Test for each configuration
    for config_name in handler.configurations.keys():
        print(f"\n{'='*50}")
        print(f"Testing configuration: {config_name}")
        print(f"{'='*50}")
        
        # Get reference files
        ref_files = handler.get_reference_files(config_name)
        print(f"Wind references found: {ref_files.get('wind', [])}")
        print(f"Wave references found: {ref_files.get('wave', [])}")
        
        # Test loading a strut file if references exist
        if ref_files.get('wind'):
            wind_ref = ref_files['wind'][0]
            print(f"\nTesting load for {wind_ref}, Strut1:")
            time_data, tension_data = handler.load_strut_data(config_name, wind_ref, 1)
            if len(tension_data) > 0:
                print(f"  - Successfully loaded {len(tension_data)} timesteps")
                print(f"  - Time range: {time_data[0]:.1f}s to {time_data[-1]:.1f}s")
                print(f"  - Tension range: {tension_data.min():.2f} to {tension_data.max():.2f} kN")
            else:
                print(f"  - Failed to load data")
        
        if ref_files.get('wave'):
            wave_ref = ref_files['wave'][0]
            print(f"\nTesting load for {wave_ref}, Strut1:")
            time_data, tension_data = handler.load_strut_data(config_name, wave_ref, 1)
            if len(tension_data) > 0:
                print(f"  - Successfully loaded {len(tension_data)} timesteps")
                print(f"  - Time range: {time_data[0]:.1f}s to {time_data[-1]:.1f}s")
                print(f"  - Tension range: {tension_data.min():.2f} to {tension_data.max():.2f} kN")
            else:
                print(f"  - Failed to load data")
    
    # Test the scaler with production structure
    print("\n" + "=" * 60)
    print("TESTING LOAD SCALER WITH PRODUCTION STRUCTURE")
    print("=" * 60)
    
    scaler = LoadScaler(handler)
    
    # Test selecting closest references
    test_conditions = [
        {'wind_dir': 0, 'wave_dir': 0, 'tp': 2.7},
        {'wind_dir': 45, 'wave_dir': 90, 'tp': 3.0},
        {'wind_dir': 180, 'wave_dir': 180, 'tp': 2.5},
    ]
    
    for i, cond in enumerate(test_conditions, 1):
        print(f"\nTest condition {i}:")
        print(f"  Wind dir: {cond['wind_dir']}°, Wave dir: {cond['wave_dir']}°, Tp: {cond['tp']}s")
        
        config_name = 'fsts_l015'  # Use first config for testing
        wind_ref = scaler.select_closest_reference(config_name, 'wind', cond['wind_dir'])
        wave_ref = scaler.select_closest_reference(config_name, 'wave', cond['wave_dir'], cond['tp'])
        
        print(f"  Selected wind reference: {wind_ref}")
        print(f"  Selected wave reference: {wave_ref}")
    
    print("\n" + "=" * 60)
    print("PRODUCTION STRUCTURE TEST COMPLETE")
    print("=" * 60)
    
    return True

if __name__ == "__main__":
    success = test_production_structure()
    sys.exit(0 if success else 1)