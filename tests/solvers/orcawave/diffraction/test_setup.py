#!/usr/bin/env python3
"""
Test script to verify OrcaWave diffraction module setup
"""

import sys
from pathlib import Path

def test_setup():
    """Test module setup and paths"""
    
    print("=" * 60)
    print("OrcaWave Diffraction Module Setup Test")
    print("=" * 60)
    
    # Check module structure
    module_dir = Path(__file__).parent
    repo_root = module_dir.parent.parent.parent.parent
    
    print(f"\n1. Module Directory: {module_dir}")
    print(f"   Exists: {module_dir.exists()}")
    
    # Check configs
    configs_dir = module_dir / "configs"
    print(f"\n2. Configs Directory: {configs_dir}")
    print(f"   Exists: {configs_dir.exists()}")
    
    base_config = configs_dir / "base_diffraction_config.yml"
    print(f"   Base config exists: {base_config.exists()}")
    
    vessels_dir = configs_dir / "vessels"
    print(f"   Vessels dir exists: {vessels_dir.exists()}")
    
    if vessels_dir.exists():
        vessels = list(vessels_dir.glob("*.yml"))
        print(f"   Available vessels: {[v.stem for v in vessels]}")
    
    # Check scripts
    scripts_dir = module_dir / "scripts"
    print(f"\n3. Scripts Directory: {scripts_dir}")
    print(f"   Exists: {scripts_dir.exists()}")
    
    required_scripts = [
        "orchestrator.py",
        "validate_geometry.py",
        "convert_to_orcaflex.py",
        "run_diffraction_analysis.bat"
    ]
    
    for script in required_scripts:
        script_path = scripts_dir / script
        status = "OK" if script_path.exists() else "MISSING"
        print(f"   {script}: {status}")
    
    # Check geometry path
    geometry_path = repo_root / "specs" / "modules" / "orcawave" / "sea-cypress-diffraction-analysis" / "inputs" / "geometry"
    print(f"\n4. Geometry Path: {geometry_path}")
    print(f"   Exists: {geometry_path.exists()}")
    
    if geometry_path.exists():
        geometry_files = list(geometry_path.glob("*"))
        print(f"   Files found: {len(geometry_files)}")
        for file in geometry_files:
            print(f"     - {file.name}")
    
    # Check results directory
    results_dir = module_dir / "results"
    print(f"\n5. Results Directory: {results_dir}")
    print(f"   Exists: {results_dir.exists()}")
    
    # Test imports
    print(f"\n6. Testing Python imports:")
    try:
        import yaml
        print("   yaml: OK")
    except ImportError:
        print("   yaml: MISSING")
    
    try:
        import numpy
        print("   numpy: OK")
    except ImportError:
        print("   numpy: MISSING")
    
    try:
        import pandas
        print("   pandas: OK")
    except ImportError:
        print("   pandas: MISSING")
    
    try:
        import h5py
        print("   h5py: OK")
    except ImportError:
        print("   h5py: MISSING")
    
    print("\n" + "=" * 60)
    print("Setup test complete!")
    print("=" * 60)
    
    return 0

if __name__ == "__main__":
    sys.exit(test_setup())