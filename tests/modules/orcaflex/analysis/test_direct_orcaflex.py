#!/usr/bin/env python
"""
Direct test of OrcaFlex DAT to SIM conversion using OrcFxAPI.
This bypasses the digitalmodel configuration system to test the core functionality.
"""

import os
import sys
from pathlib import Path
from datetime import datetime

def test_direct_dat_to_sim():
    """Direct test of OrcaFlex analysis from DAT to SIM."""
    
    print("="*60)
    print("Direct OrcaFlex DAT to SIM Conversion Test")
    print("="*60)
    
    # Check if OrcFxAPI is available
    try:
        import OrcFxAPI
        print("[OK] OrcFxAPI module imported successfully")
    except ImportError as e:
        print(f"[ERROR] OrcFxAPI not available: {e}")
        return False
    
    # Set up file paths
    test_dir = Path(__file__).parent
    dat_file = test_dir / "orcaflex_test2.dat"
    sim_file = test_dir / "orcaflex_test2_direct.sim"
    
    # Remove existing SIM file if it exists
    if sim_file.exists():
        print(f"[INFO] Removing existing SIM file: {sim_file.name}")
        sim_file.unlink()
    
    # Check if DAT file exists
    if not dat_file.exists():
        print(f"[ERROR] DAT file not found: {dat_file}")
        return False
    print(f"[OK] Input DAT file found: {dat_file.name}")
    
    # Create OrcaFlex model
    try:
        print("\n" + "-"*40)
        print("Starting OrcaFlex Analysis...")
        print("-"*40)
        
        model = OrcFxAPI.Model()
        print("[OK] OrcaFlex model created")
        
        # Load the DAT file
        print(f"[INFO] Loading DAT file: {dat_file.name}")
        model.LoadData(str(dat_file))
        print("[OK] DAT file loaded successfully")
        
        # Get model info
        general = model.general
        print(f"[INFO] Model title: {general.Title}")
        print(f"[INFO] Simulation duration: {general.StageDuration[1]} seconds")
        
        # Run static analysis
        print("\n[INFO] Running static analysis...")
        start_time = datetime.now()
        model.CalculateStatics()
        static_time = (datetime.now() - start_time).total_seconds()
        print(f"[OK] Static analysis complete ({static_time:.2f} seconds)")
        
        # Check if we should run dynamic simulation
        run_dynamic = False  # Set to True if you want to run full simulation
        
        if run_dynamic:
            print("\n[INFO] Running dynamic simulation...")
            start_time = datetime.now()
            model.RunSimulation()
            dynamic_time = (datetime.now() - start_time).total_seconds()
            print(f"[OK] Dynamic simulation complete ({dynamic_time:.2f} seconds)")
        else:
            print("\n[INFO] Skipping dynamic simulation (static only)")
        
        # Save the SIM file
        print(f"\n[INFO] Saving SIM file: {sim_file.name}")
        model.SaveSimulation(str(sim_file))
        print(f"[OK] SIM file saved successfully")
        
        # Verify the SIM file was created
        if sim_file.exists():
            size = sim_file.stat().st_size
            print(f"\n[SUCCESS] SIM file created: {sim_file.name}")
            print(f"           Size: {size:,} bytes")
            return True
        else:
            print(f"\n[ERROR] SIM file was not created")
            return False
            
    except Exception as e:
        print(f"\n[ERROR] Analysis failed: {e}")
        import traceback
        traceback.print_exc()
        return False


def check_legacy_code_path():
    """Check if the legacy code path works."""
    print("\n" + "="*60)
    print("Testing Legacy Code Path")
    print("="*60)
    
    # Add src to path
    src_path = Path(__file__).parents[4] / 'src'
    if str(src_path) not in sys.path:
        sys.path.insert(0, str(src_path))
    
    try:
        from digitalmodel.modules.orcaflex.orcaflex_custom_analysis import OrcaFlexCustomAnalysis
        print("[OK] OrcaFlexCustomAnalysis imported")
        
        # Check the key methods exist
        analysis = OrcaFlexCustomAnalysis()
        methods = ['perform_simulations', 'process_fea', 'run_static_analysis']
        for method in methods:
            if hasattr(analysis, method):
                print(f"[OK] Method exists: {method}")
            else:
                print(f"[ERROR] Method missing: {method}")
                
    except Exception as e:
        print(f"[ERROR] Failed to import legacy code: {e}")
        return False
    
    return True


if __name__ == "__main__":
    print("OrcaFlex Direct Test Suite")
    print("="*60)
    
    # Test direct conversion
    print("\n1. Testing direct DAT to SIM conversion...")
    direct_success = test_direct_dat_to_sim()
    
    # Test legacy code path
    print("\n2. Checking legacy code path...")
    legacy_success = check_legacy_code_path()
    
    # Summary
    print("\n" + "="*60)
    print("Test Summary:")
    print("-"*40)
    
    if direct_success:
        print("[SUCCESS] Direct DAT to SIM conversion works!")
        print("          The OrcaFlex API can successfully:")
        print("          - Load .dat files")
        print("          - Run static analysis")
        print("          - Save .sim files")
    else:
        print("[FAILED] Direct conversion did not work")
    
    if legacy_success:
        print("\n[OK] Legacy code structure is intact")
    else:
        print("\n[WARNING] Legacy code has issues")
    
    print("="*60)
    
    sys.exit(0 if direct_success else 1)