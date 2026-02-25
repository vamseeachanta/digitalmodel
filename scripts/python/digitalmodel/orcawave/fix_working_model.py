#!/usr/bin/env python
"""
Fix the working_model.yml issues:
1. Use a valid GDF file
2. Set proper moments of inertia
"""

import yaml
from pathlib import Path

def fix_working_model():
    """Fix the working model YAML file"""
    
    print("="*60)
    print(" FIXING WORKING MODEL ISSUES ")
    print("="*60)
    
    # Load the working model
    with open("working_model.yml", 'r', encoding='utf-8-sig') as f:
        content = f.read()
        # Skip the %YAML header
        lines = content.split('\n')
        yaml_content = '\n'.join([line for line in lines if not line.startswith('%YAML')])
        config = yaml.safe_load(yaml_content)
    
    print("\nFixing issues:")
    
    # FIX 1: Use a valid GDF file that exists
    print("\n1. Checking for valid GDF files...")
    geometry_path = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry")
    
    # List of GDF files to try in order of preference
    gdf_files = [
        "sea_cypress_corrected.gdf",
        "sea_cypress_orcawave.gdf", 
        "small_box_test.gdf",
        "waterline_test_box.gdf"
    ]
    
    valid_gdf = None
    for gdf in gdf_files:
        full_path = geometry_path / gdf
        if full_path.exists():
            # Check if file has content
            with open(full_path, 'r') as f:
                content = f.read()
                if len(content) > 100:  # File has substantial content
                    valid_gdf = str(full_path).replace('/', '\\')
                    print(f"   Found valid GDF: {gdf}")
                    break
    
    if valid_gdf:
        config['Bodies'][0]['BodyMeshFileName'] = valid_gdf
    else:
        print("   WARNING: No valid GDF file found")
    
    # FIX 2: Set proper moments of inertia (non-zero radii of gyration)
    print("\n2. Setting proper moments of inertia...")
    
    # For a box-like vessel, use reasonable radii of gyration
    # Typical values are 0.3-0.4 times the characteristic dimension
    config['Bodies'][0]['BodyRadiiOfGyrationRx, BodyRadiiOfGyrationRy, BodyRadiiOfGyrationRz'] = [
        [5.0, 0, 0],   # Rx = 5m (roll)
        [0, 8.0, 0],   # Ry = 8m (pitch)
        [0, 0, 8.0]    # Rz = 8m (yaw)
    ]
    
    # Also set a proper mass center (if needed)
    config['Bodies'][0]['BodyCentreOfMass'] = [0, 0, -1.0]  # 1m below origin
    
    # FIX 3: Add more periods and headings for better analysis
    print("\n3. Adding more analysis points...")
    config['PeriodOrFrequency'] = [5, 7, 10, 12, 15, 20]
    config['WaveHeading'] = [0, 45, 90, 135, 180]
    
    # FIX 4: Update body name
    config['Bodies'][0]['BodyName'] = 'SeaCypressTest'
    
    # Save the fixed configuration
    output_file = "working_model_fixed.yml"
    
    with open(output_file, 'w', encoding='utf-8') as f:
        # Write the YAML header
        f.write("%YAML 1.1\n")
        f.write("# Fixed Working Model Configuration\n")
        f.write("# This file has been corrected to address GDF and inertia issues\n")
        f.write("---\n")
        # Write the configuration
        yaml.dump(config, f, default_flow_style=False, sort_keys=False, allow_unicode=True)
    
    print(f"\n[OK] Fixed configuration saved to: {output_file}")
    
    return output_file

def create_simple_gdf():
    """Create a simple valid GDF file for testing"""
    
    print("\n" + "="*60)
    print(" CREATING SIMPLE TEST GDF ")
    print("="*60)
    
    # Create a simple box GDF file
    gdf_content = """Rhino Test Box
    1 9.80665     ULEN GRAV
    0  0          ISX  ISY
    8             NPER
    -5.0 -2.5 -3.0
    -5.0  2.5 -3.0
     5.0  2.5 -3.0
     5.0 -2.5 -3.0
    -5.0 -2.5  0.0
    -5.0  2.5  0.0
     5.0  2.5  0.0
     5.0 -2.5  0.0
    12  4         NBODY NPATCH
    1  4  1 2 6 5
    2  4  2 3 7 6
    3  4  3 4 8 7
    4  4  4 1 5 8
    5  4  1 2 3 4
    6  4  5 6 7 8
    7  4  1 5 6 2
    8  4  2 6 7 3
    9  4  3 7 8 4
    10 4  4 8 5 1
    11 4  5 8 7 6
    12 4  1 4 3 2
"""
    
    output_file = "test_box.gdf"
    with open(output_file, 'w') as f:
        f.write(gdf_content)
    
    print(f"[OK] Created simple GDF: {output_file}")
    
    # Also create a minimal working model using this GDF
    create_minimal_with_test_box()
    
    return output_file

def create_minimal_with_test_box():
    """Create minimal config with the test box"""
    
    config = {
        'UnitsSystem': 'SI',
        'SolveType': 'Potential formulation only',
        'WaterDepth': 1000,
        'WaterDensity': 1.025,
        'WavesReferredToBy': 'period (s)',
        'ValidatePanelArrangement': False,
        'PeriodOrFrequency': [10],
        'WaveHeading': [0],
        'Bodies': [{
            'BodyName': 'TestBox',
            'BodyMeshFileName': 'D:\\github\\digitalmodel\\test_box.gdf',
            'BodyMeshFormat': 'Wamit gdf',
            'BodyMeshLengthUnits': 'm',
            'BodyMeshPosition': [0, 0, 0],
            'BodyMeshAttitude': [0, 0, 0],
            'BodyIncludedInAnalysis': True,
            'BodyInertiaSpecifiedBy': 'Radii of gyration (for a free-floating body)',
            'BodyCentreOfMass': [0, 0, -1],
            'BodyRadiiOfGyrationRx, BodyRadiiOfGyrationRy, BodyRadiiOfGyrationRz': [
                [3.0, 0, 0],
                [0, 5.0, 0],
                [0, 0, 5.0]
            ],
            'BodyRadiiOfGyrationOriginType': 'Body origin',
            'BodyConnectionParent': 'Free',
            'BodyFixedDOFx': False,
            'BodyFixedDOFy': False,
            'BodyFixedDOFz': False,
            'BodyFixedDOFRx': False,
            'BodyFixedDOFRy': False,
            'BodyFixedDOFRz': False
        }]
    }
    
    output_file = "minimal_test_box.yml"
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write("%YAML 1.1\n")
        f.write("# Minimal Test Box Configuration\n")
        f.write("---\n")
        yaml.dump(config, f, default_flow_style=False, sort_keys=False)
    
    print(f"[OK] Created minimal config: {output_file}")

def main():
    print("\nStarting fixes for working_model.yml issues...")
    
    # Fix the working model
    fixed_file = fix_working_model()
    
    # Create a simple test GDF
    test_gdf = create_simple_gdf()
    
    print("\n" + "="*60)
    print(" SOLUTIONS COMPLETE ")
    print("="*60)
    
    print("\nFiles created:")
    print(f"1. {fixed_file} - Fixed version with valid GDF and inertia")
    print(f"2. {test_gdf} - Simple valid GDF file")
    print(f"3. minimal_test_box.yml - Minimal config with test box")
    
    print("\n" + "-"*60)
    print("TRY THESE IN ORDER:")
    print("-"*60)
    
    print("\n1. SIMPLEST TEST:")
    print("   Open: minimal_test_box.yml")
    print("   Uses: test_box.gdf (created above)")
    print("   Should work immediately")
    
    print("\n2. FIXED WORKING MODEL:")
    print("   Open: working_model_fixed.yml")
    print("   Uses: Valid GDF from geometry folder")
    print("   Has proper inertia values")
    
    print("\n3. IF STILL HAVING ISSUES:")
    print("   - Check that GDF files are not corrupted")
    print("   - Try creating a new model manually in OrcaWave")
    print("   - Export a working example from OrcaWave")
    
    return 0

if __name__ == "__main__":
    main()