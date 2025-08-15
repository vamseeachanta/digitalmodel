#!/usr/bin/env python
"""
Test script to run OrcaFlex analysis on a single DAT file.
Verifies that static analysis is performed and a SIM file is generated.
"""

import os
import sys
import yaml
import logging
from pathlib import Path

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parents[4] / 'src'))

from digitalmodel.engine import engine

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def test_dat_file_analysis():
    """Test OrcaFlex analysis on a single DAT file."""
    
    print("="*60)
    print("OrcaFlex DAT File Analysis Test")
    print("="*60)
    
    # Setup paths
    test_dir = Path(__file__).parent
    config_file = test_dir / 'run_single_dat.yml'
    dat_file = test_dir / 'orcaflex_test2.dat'
    
    # Verify input file exists
    if dat_file.exists():
        print(f"[OK] Input file found: {dat_file.name}")
    else:
        print(f"[ERROR] Input file missing: {dat_file}")
        return False
    
    # Load and display configuration
    with open(config_file, 'r') as f:
        cfg = yaml.safe_load(f)
    
    print(f"\nConfiguration:")
    print(f"  Library: {cfg['meta']['library']}")
    print(f"  Basename: {cfg['meta']['basename']}")
    print(f"  Description: {cfg['meta']['description']}")
    print(f"  Static Analysis: {cfg['orcaflex']['analysis']['static']}")
    print(f"  Dynamic Analysis: {cfg['orcaflex']['analysis'].get('dynamic', False)}")
    print(f"  Save SIM: {cfg['orcaflex']['analysis']['save_sim']}")
    
    # Run analysis
    print("\n" + "-"*40)
    print("Running OrcaFlex Analysis...")
    print("-"*40)
    
    try:
        # Change to test directory for relative paths
        original_dir = os.getcwd()
        os.chdir(test_dir)
        
        # Run the engine
        logger.info(f"Calling engine with config: {config_file.name}")
        result = engine(str(config_file.name))
        
        print("[SUCCESS] Engine execution completed!")
        
        # Check for output files
        output_dir = Path('results_dat_test')
        sim_files_found = []
        
        # Check output directory
        if output_dir.exists():
            files = list(output_dir.iterdir())
            print(f"\nOutput directory created: {output_dir}")
            print(f"  Files generated: {len(files)}")
            for f in files:
                print(f"    - {f.name}")
                if f.suffix == '.sim':
                    sim_files_found.append(f)
        
        # Also check current directory
        for f in Path('.').glob('*.sim'):
            if f.name.startswith('orcaflex_test2'):
                sim_files_found.append(f)
                print(f"\nSIM file found in current directory: {f.name}")
        
        # Verify SIM file generation
        if sim_files_found:
            print(f"\n[SUCCESS] {len(sim_files_found)} SIM file(s) generated:")
            for sim_file in sim_files_found:
                print(f"  - {sim_file}")
                # Check file size
                size = sim_file.stat().st_size
                print(f"    Size: {size:,} bytes")
            return True
        else:
            print("\n[WARNING] No SIM files generated")
            print("This might be due to:")
            print("  1. OrcFxAPI not installed")
            print("  2. OrcaFlex license not available")
            print("  3. Analysis running in mock mode")
            return False
            
    except Exception as e:
        print(f"\n[ERROR] Analysis failed: {e}")
        logger.exception("Detailed error:")
        
        # Try to understand the error
        if "OrcFxAPI" in str(e):
            print("\n[INFO] OrcFxAPI module not found.")
            print("To run actual OrcaFlex analysis, install OrcFxAPI:")
            print("  pip install OrcFxAPI")
        elif "license" in str(e).lower():
            print("\n[INFO] OrcaFlex license not available.")
        else:
            print("\n[INFO] Check the log for details.")
        
        return False
        
    finally:
        # Restore original directory
        os.chdir(original_dir)


def check_orcaflex_availability():
    """Check if OrcaFlex is available."""
    try:
        import OrcFxAPI
        print("[OK] OrcFxAPI module found")
        
        # Try to create a model to check license
        try:
            model = OrcFxAPI.Model()
            print("[OK] OrcaFlex license available")
            return True
        except Exception as e:
            print(f"[WARNING] OrcaFlex license check failed: {e}")
            return False
            
    except ImportError:
        print("[INFO] OrcFxAPI not installed")
        print("The analysis will run in mock/configuration mode only")
        return False


if __name__ == "__main__":
    print("OrcaFlex Analysis Test Suite")
    print("="*60)
    
    # Check OrcaFlex availability
    print("\n1. Checking OrcaFlex availability...")
    orcaflex_available = check_orcaflex_availability()
    
    # Run DAT file test
    print("\n2. Testing DAT file analysis...")
    success = test_dat_file_analysis()
    
    # Summary
    print("\n" + "="*60)
    print("Test Summary:")
    print("-"*40)
    
    if orcaflex_available:
        if success:
            print("[SUCCESS] All tests passed!")
            print("  - OrcaFlex is available")
            print("  - DAT file loaded successfully")
            print("  - Static analysis completed")
            print("  - SIM file generated")
        else:
            print("[PARTIAL] Configuration valid but analysis incomplete")
    else:
        print("[INFO] Running in configuration-only mode")
        print("  - Configuration files are valid")
        print("  - Install OrcFxAPI for full functionality")
    
    print("="*60)
    
    sys.exit(0 if success else 1)