#!/usr/bin/env python
"""
Create a proper OrcaWave .owd file instead of YAML
OrcaWave might be having issues with YAML import
"""

def create_minimal_owd():
    """Create a minimal .owd file in OrcaWave native format"""
    
    print("Creating OrcaWave native .owd file...")
    
    # OrcaWave .owd files use a specific format
    # This is based on OrcaWave's native format structure
    owd_content = """% OrcaWave version 11.5
% Minimal test configuration
% Units: SI

General Data
    UnitsSystem = SI
    WaterDepth = 1000.0
    WaterDensity = 1025.0
    g = 9.80665
    WavesReferredToBy = "period (s)"
    SolveType = "Potentials only"
    LinearSolverMethod = "Direct LU"
End General Data

Environment
    WaterDepth = 1000.0
    WaterDensity = 1025.0
End Environment

Periods
    NumberOfPeriods = 3
    Period(1) = 5.0
    Period(2) = 10.0
    Period(3) = 15.0
End Periods

Directions
    NumberOfDirections = 3
    Direction(1) = 0.0
    Direction(2) = 90.0
    Direction(3) = 180.0
End Directions

Bodies
    NumberOfBodies = 1
    
    Body 1
        Name = "TestBox"
        MeshFile = "D:\\github\\digitalmodel\\specs\\modules\\orcawave\\diffraction-analysis\\inputs\\geometry\\simple_box_test.gdf"
        MeshFormat = "Wamit gdf"
        Mass = 1.0e5
        CentreOfMass = 0.0, 0.0, -1.0
        RadiusOfGyrationX = 5.0
        RadiusOfGyrationY = 10.0
        RadiusOfGyrationZ = 10.0
        IncludedInAnalysis = Yes
    End Body 1
End Bodies
"""
    
    with open("minimal_test.owd", "w") as f:
        f.write(owd_content)
    
    print("[OK] Created: minimal_test.owd")
    return "minimal_test.owd"

def create_alternative_yml():
    """Create alternative YAML with different mass format"""
    
    print("Creating alternative YAML format...")
    
    # Try different formatting approaches
    yml_content = """# OrcaWave Configuration
# Minimal working example

UnitsSystem: SI
WaterDepth: 1000.0
WaterDensity: 1025.0

PeriodOrFrequency: [5.0, 10.0, 15.0]
WaveHeading: [0.0, 90.0, 180.0]

Bodies:
  - BodyName: TestBox
    BodyMeshFileName: simple_box_test.gdf
    BodyMeshFormat: Wamit gdf
    BodyMass: 1.0e5
    BodyCentreOfMass: [0.0, 0.0, -1.0]
    BodyInertiaSpecifiedBy: Radii of gyration
    BodyRadiusOfGyrationRx: 5.0
    BodyRadiusOfGyrationRy: 10.0
    BodyRadiusOfGyrationRz: 10.0
"""
    
    with open("alternative_format.yml", "w") as f:
        f.write(yml_content)
    
    print("[OK] Created: alternative_format.yml")
    return "alternative_format.yml"

def check_gdf_file():
    """Check if the GDF file exists and show its path"""
    from pathlib import Path
    
    gdf_path = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry/simple_box_test.gdf")
    
    if gdf_path.exists():
        print(f"[OK] GDF file exists: {gdf_path}")
        
        # Read first few lines to check format
        with open(gdf_path, 'r') as f:
            lines = f.readlines()[:5]
            print("\nFirst few lines of GDF file:")
            for line in lines:
                print(f"  {line.strip()}")
    else:
        print(f"[WARNING] GDF file not found: {gdf_path}")
        print("\nAvailable GDF files in geometry folder:")
        geometry_dir = gdf_path.parent
        for gdf in geometry_dir.glob("*.gdf"):
            print(f"  - {gdf.name}")

def create_diagnostic_script():
    """Create a script to help diagnose the issue"""
    
    diagnostic = """# OrcaWave Diagnostic Steps
# Run these checks in order

1. CHECK ORCAWAVE VERSION
   - Open OrcaWave
   - Help -> About
   - Verify version is 11.5 or compatible

2. CHECK MESH FILE
   - File -> Open
   - Navigate to: D:\\github\\digitalmodel\\specs\\modules\\orcawave\\diffraction-analysis\\inputs\\geometry\\
   - Try opening: simple_box_test.gdf directly
   - If it opens, the mesh file is valid

3. TRY MANUAL ENTRY
   - File -> New
   - Manually set:
     * Water depth: 1000 m
     * Add one period: 10 s
     * Add one direction: 0 deg
   - Bodies -> Add Body
     * Name: TestBox
     * Mass: 100000 kg (or try 1.0e5)
     * Browse to mesh file
   - Try to run

4. CHECK ERROR LOG
   - View -> Message Log
   - Look for detailed error messages

5. ALTERNATIVE APPROACH
   - Use an existing OrcaWave example file
   - Modify it with your parameters
   - Save as new file
"""
    
    with open("DIAGNOSTIC_STEPS.txt", "w") as f:
        f.write(diagnostic)
    
    print("[OK] Created: DIAGNOSTIC_STEPS.txt")

def main():
    print("="*60)
    print(" FIXING ORCAWAVE BODY MASS ERROR ")
    print("="*60)
    print()
    print("Error: 'BodyMass=100000 (Change not allowed)'")
    print("This suggests OrcaWave has specific requirements for mass values.")
    print()
    
    # Check GDF file
    check_gdf_file()
    print()
    
    # Create alternative files
    owd_file = create_minimal_owd()
    yml_file = create_alternative_yml()
    create_diagnostic_script()
    
    print()
    print("="*60)
    print(" SOLUTIONS TO TRY ")
    print("="*60)
    print()
    print("1. TRY NATIVE OWD FORMAT:")
    print(f"   Open: {owd_file}")
    print("   This uses OrcaWave's native format instead of YAML")
    print()
    print("2. TRY ALTERNATIVE YAML:")
    print(f"   Open: {yml_file}")
    print("   Uses scientific notation for mass (1.0e5)")
    print()
    print("3. MANUAL DIAGNOSTIC:")
    print("   Follow steps in: DIAGNOSTIC_STEPS.txt")
    print()
    print("4. WORKAROUND:")
    print("   - Open OrcaWave")
    print("   - File -> New")
    print("   - Manually create a simple model")
    print("   - Save it as .owd")
    print("   - Then we can modify that file")
    print()
    print("The error suggests OrcaWave might need:")
    print("- Different mass units or format")
    print("- Specific body property combinations")
    print("- Mesh file to be loaded first")
    
    return 0

if __name__ == "__main__":
    main()