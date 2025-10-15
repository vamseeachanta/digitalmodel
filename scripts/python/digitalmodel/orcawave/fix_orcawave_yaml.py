#!/usr/bin/env python
"""
Fix OrcaWave YAML parsing error
Creates properly formatted OrcaWave configuration files
"""

def create_orcawave_minimal_config():
    """Create a minimal OrcaWave configuration file in proper format"""
    
    print("Creating properly formatted OrcaWave configuration...")
    
    # Write the configuration manually to ensure proper formatting
    config_content = """UnitsSystem: SI
SolveType: Potentials only
WaterDepth: 1000
WaterDensity: 1025
WavesReferredToBy: period (s)
LinearSolverMethod: Direct LU
ValidatePanelArrangement: Yes
OutputIntermediateResults: Yes

PeriodOrFrequency:
  - 5
  - 10
  - 15

WaveHeading:
  - 0
  - 90
  - 180

Bodies:
  - BodyName: TestBox
    BodyMeshFileName: D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry/simple_box_test.gdf
    BodyMeshFormat: Wamit gdf
    BodyMeshLengthUnits: m
    BodyMass: 100000
    BodyCentreOfMass: [0, 0, -1]
    BodyInertiaSpecifiedBy: Radii of gyration
    BodyRadiusOfGyrationRx: 5
    BodyRadiusOfGyrationRy: 10
    BodyRadiusOfGyrationRz: 10
    BodyIncludedInAnalysis: Yes
    BodyConnectionParent: Free
    BodyFixedDOFx: No
    BodyFixedDOFy: No
    BodyFixedDOFz: No
    BodyFixedDOFRx: No
    BodyFixedDOFRy: No
    BodyFixedDOFRz: No
"""
    
    # Save the configuration
    with open("orcawave_minimal_working.yml", "w") as f:
        f.write(config_content)
    
    print("[OK] Created: orcawave_minimal_working.yml")
    return "orcawave_minimal_working.yml"

def create_sea_cypress_working():
    """Create a working Sea Cypress configuration"""
    
    config_content = """UnitsSystem: SI
SolveType: Potentials only
LoadRAOCalculationMethod: Pressure integration
PreferredLoadRAOCalculationMethod: Pressure integration
WaterDepth: 100
WaterDensity: 1025
WavesReferredToBy: period (s)
LinearSolverMethod: Direct LU
ValidatePanelArrangement: Yes
OutputIntermediateResults: Yes
OutputBodyWireFrames: Yes
PanelAspectRatioWarningLevel: 20
PanelsPerWavelengthWarningLevel: 5
WaterlineGapTolerance: 0.05
WaterlineZTolerance: 0.001

PeriodOrFrequency:
  - 3
  - 5
  - 7
  - 10
  - 12
  - 15
  - 20
  - 25

WaveHeading:
  - 0
  - 45
  - 90
  - 135
  - 180

Bodies:
  - BodyName: SeaCypress
    BodyMeshFileName: D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry/sea_cypress_corrected.gdf
    BodyMeshFormat: Wamit gdf
    BodyMeshLengthUnits: m
    BodyMeshSymmetry: None
    BodyMeshBodyNumber: 1
    BodyIncludedInAnalysis: Yes
    BodyMeshPosition: [0, 0, 0]
    BodyMeshAttitude: [0, 0, 0]
    BodyConnectionParent: Free
    BodyMass: 400000
    BodyCentreOfMass: [0, 0, -1.5]
    BodyInertiaSpecifiedBy: Matrix (for a general body)
    BodyInertiaTensorRx: [[44100000, 0, 0], [0, 0, 0], [0, 0, 0]]
    BodyInertiaTensorRy: [[0, 0, 0], [0, 22500000, 0], [0, 0, 0]]
    BodyInertiaTensorRz: [[0, 0, 0], [0, 0, 0], [0, 0, 22500000]]
    BodyInertiaTensorOriginType: Centre of mass
    BodyFixedDOFx: No
    BodyFixedDOFy: No
    BodyFixedDOFz: No
    BodyFixedDOFRx: No
    BodyFixedDOFRy: No
    BodyFixedDOFRz: No
    BodyOrcaFlexImportLength: 30
    BodyOrcaFlexImportSymmetry: Use global mesh symmetry
"""
    
    with open("sea_cypress_working.yml", "w") as f:
        f.write(config_content)
    
    print("[OK] Created: sea_cypress_working.yml")
    return "sea_cypress_working.yml"

def create_simplest_test():
    """Create the absolute simplest possible test"""
    
    config_content = """UnitsSystem: SI
WaterDepth: 1000
WaterDensity: 1025
WavesReferredToBy: period (s)

PeriodOrFrequency:
  - 10

WaveHeading:
  - 0

Bodies:
  - BodyName: Box
    BodyMeshFileName: D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/inputs/geometry/simple_box_test.gdf
    BodyMeshFormat: Wamit gdf
    BodyMass: 100000
    BodyCentreOfMass: [0, 0, 0]
    BodyRadiusOfGyrationRx: 5
    BodyRadiusOfGyrationRy: 5
    BodyRadiusOfGyrationRz: 5
"""
    
    with open("simplest_test.yml", "w") as f:
        f.write(config_content)
    
    print("[OK] Created: simplest_test.yml")
    return "simplest_test.yml"

def main():
    print("="*60)
    print(" FIXING ORCAWAVE YAML PARSING ERROR ")
    print("="*60)
    print()
    
    print("The error 'Potentials only' near line 4 indicates")
    print("OrcaWave is having trouble with the YAML format.")
    print()
    print("Creating properly formatted configuration files...")
    print()
    
    # Create all versions
    simplest = create_simplest_test()
    minimal = create_orcawave_minimal_config()
    sea_cypress = create_sea_cypress_working()
    
    print()
    print("="*60)
    print(" FILES CREATED ")
    print("="*60)
    print()
    print("Try these files in ORDER:")
    print()
    print("1. SIMPLEST TEST (most likely to work):")
    print(f"   {simplest}")
    print("   - Just 1 period, 1 heading")
    print("   - Absolute minimum configuration")
    print()
    print("2. MINIMAL WORKING:")
    print(f"   {minimal}")
    print("   - 3 periods, 3 headings")
    print("   - Basic but complete configuration")
    print()
    print("3. SEA CYPRESS WORKING:")
    print(f"   {sea_cypress}")
    print("   - Your actual vessel")
    print("   - All parameters properly set")
    print()
    print("-"*60)
    print("HOW TO USE:")
    print("-"*60)
    print("1. Open OrcaWave")
    print("2. File -> Open")
    print("3. Select 'simplest_test.yml' FIRST")
    print("4. If it loads, click Calculate (F5)")
    print()
    print("If simplest_test.yml works, then try the others.")
    print()
    
    return 0

if __name__ == "__main__":
    main()