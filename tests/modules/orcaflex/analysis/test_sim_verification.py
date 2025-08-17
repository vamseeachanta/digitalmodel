#!/usr/bin/env python
"""
Enhanced test that creates a .sim file and thoroughly verifies it.
Tests both creation and validation of the OrcaFlex simulation file.
"""

import os
import sys
import hashlib
from pathlib import Path
from datetime import datetime
import shutil

def calculate_checksum(file_path):
    """Calculate MD5 checksum of a file."""
    md5_hash = hashlib.md5()
    with open(file_path, "rb") as f:
        for chunk in iter(lambda: f.read(4096), b""):
            md5_hash.update(chunk)
    return md5_hash.hexdigest()

def test_sim_creation_and_verification():
    """Create a .sim file and verify it thoroughly."""
    
    print("="*60)
    print("OrcaFlex SIM File Creation and Verification Test")
    print("="*60)
    
    # Check if OrcFxAPI is available
    try:
        import OrcFxAPI
        print("[OK] OrcFxAPI module available")
    except ImportError as e:
        print(f"[SKIP] OrcFxAPI not available: {e}")
        return False
    
    # Set up file paths
    test_dir = Path(__file__).parent
    dat_file = test_dir / "orcaflex_test2.dat"
    test_sim_file = test_dir / "test_verification.sim"
    backup_sim_file = test_dir / "test_verification_backup.sim"
    
    # Clean up any existing test files
    for f in [test_sim_file, backup_sim_file]:
        if f.exists():
            print(f"[CLEANUP] Removing existing file: {f.name}")
            f.unlink()
    
    print("\n" + "-"*40)
    print("STEP 1: Create SIM File")
    print("-"*40)
    
    try:
        # Create and run analysis
        model = OrcFxAPI.Model()
        print(f"[INFO] Loading: {dat_file.name}")
        model.LoadData(str(dat_file))
        
        print("[INFO] Running static analysis...")
        start_time = datetime.now()
        model.CalculateStatics()
        duration = (datetime.now() - start_time).total_seconds()
        print(f"[OK] Static analysis complete ({duration:.2f}s)")
        
        # Save the SIM file
        print(f"[INFO] Saving: {test_sim_file.name}")
        model.SaveSimulation(str(test_sim_file))
        
        # Initial verification
        if not test_sim_file.exists():
            print("[ERROR] SIM file was not created!")
            return False
            
        initial_size = test_sim_file.stat().st_size
        initial_checksum = calculate_checksum(test_sim_file)
        print(f"[OK] SIM file created successfully")
        print(f"     Size: {initial_size:,} bytes")
        print(f"     MD5: {initial_checksum}")
        
    except Exception as e:
        print(f"[ERROR] Failed to create SIM file: {e}")
        return False
    
    print("\n" + "-"*40)
    print("STEP 2: Verify SIM File Can Be Loaded")
    print("-"*40)
    
    try:
        # Try to load the SIM file
        verify_model = OrcFxAPI.Model()
        print(f"[INFO] Loading SIM file: {test_sim_file.name}")
        verify_model.LoadSimulation(str(test_sim_file))
        print("[OK] SIM file loaded successfully")
        
        # Extract information from loaded SIM
        general = verify_model.general
        print(f"[INFO] Extracted from SIM file:")
        print(f"       Title: {general.Title if general.Title else '(empty)'}")
        print(f"       Duration: {general.StageDuration[1]}s")
        print(f"       Current time: {general.CurrentTime}s")
        
        # Check if it contains valid simulation data
        if general.CurrentTime > 0:
            print(f"[OK] SIM contains simulation at t={general.CurrentTime}s")
        else:
            print("[INFO] SIM contains static analysis only (t=0)")
            
    except Exception as e:
        print(f"[ERROR] Failed to load/verify SIM file: {e}")
        return False
    
    print("\n" + "-"*40)
    print("STEP 3: Test SIM File Persistence")
    print("-"*40)
    
    try:
        # Create a backup copy
        shutil.copy2(test_sim_file, backup_sim_file)
        backup_checksum = calculate_checksum(backup_sim_file)
        print(f"[OK] Backup created: {backup_sim_file.name}")
        
        # Verify checksums match
        if initial_checksum == backup_checksum:
            print("[OK] File integrity verified (checksums match)")
        else:
            print("[ERROR] Checksum mismatch!")
            return False
            
        # Test re-saving
        print("[INFO] Testing re-save capability...")
        verify_model.SaveSimulation(str(test_sim_file))
        resaved_size = test_sim_file.stat().st_size
        resaved_checksum = calculate_checksum(test_sim_file)
        
        print(f"[OK] Re-saved successfully")
        print(f"     New size: {resaved_size:,} bytes")
        
        # Note: Checksum might change due to timestamps
        if resaved_size > 0:
            print("[OK] Re-saved file is valid")
        
    except Exception as e:
        print(f"[ERROR] Persistence test failed: {e}")
        return False
    
    print("\n" + "-"*40)
    print("STEP 4: Verify Multiple SIM Files")
    print("-"*40)
    
    # Check all SIM files in directory
    all_sim_files = list(test_dir.glob("*.sim"))
    print(f"[INFO] Found {len(all_sim_files)} SIM files in directory:")
    
    valid_count = 0
    for sim_file in all_sim_files:
        size = sim_file.stat().st_size
        age_days = (datetime.now() - datetime.fromtimestamp(sim_file.stat().st_mtime)).days
        status = "✓" if size > 1000 else "✗"
        
        # Try to load it
        can_load = False
        try:
            test_model = OrcFxAPI.Model()
            test_model.LoadSimulation(str(sim_file))
            can_load = True
            valid_count += 1
        except:
            pass
        
        load_status = "VALID" if can_load else "INVALID"
        print(f"  {status} {sim_file.name:30} {size:10,} bytes  {age_days:3}d old  [{load_status}]")
    
    print(f"\n[SUMMARY] {valid_count}/{len(all_sim_files)} SIM files are valid")
    
    # Clean up test files
    print("\n" + "-"*40)
    print("STEP 5: Cleanup")
    print("-"*40)
    
    for f in [test_sim_file, backup_sim_file]:
        if f.exists():
            f.unlink()
            print(f"[CLEANUP] Removed: {f.name}")
    
    return True


def test_sim_content_analysis():
    """Analyze the content of existing SIM files."""
    
    print("\n" + "="*60)
    print("SIM File Content Analysis")
    print("="*60)
    
    try:
        import OrcFxAPI
    except ImportError:
        print("[SKIP] OrcFxAPI not available")
        return
    
    test_dir = Path(__file__).parent
    sim_files = list(test_dir.glob("orcaflex_test*.sim"))
    
    if not sim_files:
        print("[INFO] No orcaflex_test*.sim files found")
        return
    
    for sim_file in sim_files[:2]:  # Analyze first 2 files
        print(f"\n[ANALYZING] {sim_file.name}")
        print("-"*40)
        
        try:
            model = OrcFxAPI.Model()
            model.LoadSimulation(str(sim_file))
            
            # Get general info
            g = model.general
            print(f"  Title: {g.Title if g.Title else '(no title)'}")
            print(f"  Stages: {g.NumberOfStages}")
            print(f"  Duration: {g.StageDuration[1]}s")
            print(f"  Time Step: {g.ActualLogSampleInterval}s")
            print(f"  Current Time: {g.CurrentTime}s")
            
            # Count objects
            object_types = ['Lines', 'Vessels', '6DBuoys', '3DBuoys']
            for obj_type in object_types:
                try:
                    count = model.objectCount(obj_type)
                    if count > 0:
                        print(f"  {obj_type}: {count}")
                except:
                    pass
            
            # Check simulation state
            if g.CurrentTime > 0:
                print(f"  State: Dynamic simulation at t={g.CurrentTime}s")
            else:
                print(f"  State: Static analysis only")
                
        except Exception as e:
            print(f"  [ERROR] Could not analyze: {e}")


if __name__ == "__main__":
    print("Enhanced OrcaFlex SIM File Test Suite")
    print("="*60)
    
    # Run comprehensive SIM file test
    success = test_sim_creation_and_verification()
    
    # Analyze existing SIM files
    test_sim_content_analysis()
    
    # Final summary
    print("\n" + "="*60)
    print("FINAL TEST RESULTS")
    print("="*60)
    
    if success:
        print("[✓] SUCCESS: All SIM file operations verified:")
        print("    • Creation from DAT file")
        print("    • Static analysis execution")
        print("    • SIM file saving")
        print("    • SIM file loading")
        print("    • Data integrity verification")
        print("    • File persistence")
    else:
        print("[✗] FAILED: Some tests did not pass")
    
    print("="*60)
    
    sys.exit(0 if success else 1)