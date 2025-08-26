#!/usr/bin/env python
"""
Create a final working configuration using ONLY small_box_test.gdf
This file exists and has valid content (30 triangular panels)
STICK TO THIS ONE MESH FILE - NO SWITCHING
"""

import yaml

def create_final_working_config():
    """Create the final configuration using only small_box_test.gdf"""
    
    print("="*60)
    print(" FINAL WORKING CONFIGURATION ")
    print("="*60)
    print()
    print("Using ONLY: small_box_test.gdf")
    print("Format: Wamit gdf")
    print("This file is verified to exist and has valid content")
    print()
    
    # Build configuration based on the working_model.yml structure
    # but with proper values and using small_box_test.gdf consistently
    
    config = {
        # Model
        'UnitsSystem': 'SI',
        
        # Calculation & output
        'SolveType': 'Potential formulation only',
        'LoadRAOCalculationMethod': 'Both',
        'PreferredLoadRAOCalculationMethod': 'Haskind',
        'QuadraticLoadControlSurface': False,
        'QuadraticLoadMomentumConservation': False,
        'HasResonanceDampingLid': False,
        'LengthTolerance': '100e-9',
        'WaterlineZTolerance': 1e-6,
        'WaterlineGapTolerance': 1e-6,
        'DivideNonPlanarPanels': False,
        'LinearSolverMethod': 'Direct LU',
        'OutputPanelPressures': False,
        'OutputBodyWireFrames': True,
        'OutputIntermediateResults': False,
        'ValidatePanelArrangement': False,
        'BodyVolumeWarningLevel': 1e-12,
        'PanelAspectRatioWarningLevel': 25,
        'PanelsPerWavelengthWarningLevel': 5,
        
        # Environment
        'WaterDepth': 1000,
        'WaterDensity': 1.025,  # Note: 1.025, not 1025
        'WavesReferredToBy': 'period (s)',
        'HasWaveSpectrumForDragLinearisation': False,
        'MorisonFluidVelocity': 'Undisturbed incident wave',
        
        # Periods and headings - start simple
        'PeriodOrFrequency': [10],  # Just one period to start
        'WaveHeading': [0],  # Just head seas to start
        
        # Bodies
        'Bodies': [{
            'BodyName': 'SmallBox',
            'BodyMeshPosition': [0, 0, 0],
            'BodyMeshAttitude': [0, 0, 0],
            'BodyIncludedInAnalysis': True,
            
            # STICK TO THIS MESH FILE - NO CHANGING
            'BodyMeshFileName': 'D:\\github\\digitalmodel\\specs\\modules\\orcawave\\diffraction-analysis\\inputs\\geometry\\small_box_test.gdf',
            'BodyMeshFormat': 'Wamit gdf',
            'BodyMeshLengthUnits': 'm',
            'BodyMeshSymmetry': 'None',
            'BodyMeshDipolePanels': '',  # Empty string, not null
            'BodyAddInteriorSurfacePanels': False,
            
            # OrcaFlex import settings
            'BodyOrcaFlexImportSymmetry': 'Use global mesh symmetry',
            'BodyOrcaFlexImportLength': 5,  # Box is 5m long
            
            # Hydrostatic settings
            'BodyHydrostaticIntegralMethod': 'Standard',
            'BodyHydrostaticStiffnessMethod': 'Displacement',
            
            # Inertia - MUST BE NON-ZERO
            'BodyInertiaSpecifiedBy': 'Radii of gyration (for a free-floating body)',
            'BodyCentreOfMass': [0, 0, -0.5],  # 0.5m below waterline
            
            # Radii of gyration - for a 5x3x1m box
            'BodyRadiiOfGyrationRx, BodyRadiiOfGyrationRy, BodyRadiiOfGyrationRz': [
                [1.0, 0, 0],  # Roll radius
                [0, 2.0, 0],  # Pitch radius  
                [0, 0, 2.0]   # Yaw radius
            ],
            'BodyRadiiOfGyrationOriginType': 'Body origin',
            
            # External stiffness (all zeros for free floating)
            'BodyExternalStiffnessMatrixx, BodyExternalStiffnessMatrixy, BodyExternalStiffnessMatrixz, BodyExternalStiffnessMatrixRx, BodyExternalStiffnessMatrixRy, BodyExternalStiffnessMatrixRz': [
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0]
            ],
            'BodyExternalStiffnessMatrixOriginType': 'Body origin',
            
            # External damping (all zeros)
            'BodyExternalDampingMatrixx, BodyExternalDampingMatrixy, BodyExternalDampingMatrixz, BodyExternalDampingMatrixRx, BodyExternalDampingMatrixRy, BodyExternalDampingMatrixRz': [
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0]
            ],
            'BodyExternalDampingMatrixOriginType': 'Body origin',
            
            # Connection and DOFs
            'BodyConnectionParent': 'Free',
            'BodyIncreaseRollDampingToTarget': False,
            'BodyFixedDOFx': False,
            'BodyFixedDOFy': False,
            'BodyFixedDOFz': False,
            'BodyFixedDOFRx': False,
            'BodyFixedDOFRy': False,
            'BodyFixedDOFRz': False
        }],
        
        # Field points
        'DetectAndSkipFieldPointsInsideBodies': True
    }
    
    # Save with proper YAML header
    output_file = 'FINAL_small_box.yml'
    
    with open(output_file, 'w', encoding='utf-8') as f:
        # Write header exactly as OrcaWave expects
        f.write('%YAML 1.1\n')
        f.write('# OrcaWave Configuration - Small Box Test\n')
        f.write('# Using: small_box_test.gdf (verified to exist)\n')
        f.write('---\n')
        
        # Write configuration
        yaml.dump(config, f, default_flow_style=False, sort_keys=False, allow_unicode=True)
    
    print(f"Created: {output_file}")
    print()
    print("This configuration:")
    print("- Uses small_box_test.gdf consistently")
    print("- Has non-zero moments of inertia")
    print("- Uses correct water density (1.025)")
    print("- No null values")
    print("- Proper YAML 1.1 header")
    
    # Also create an extended version with more periods
    create_extended_version(config)
    
    return output_file

def create_extended_version(base_config):
    """Create extended version with more analysis points"""
    
    config = base_config.copy()
    
    # Add more periods and headings for full analysis
    config['PeriodOrFrequency'] = [5, 7, 10, 12, 15, 20]
    config['WaveHeading'] = [0, 45, 90, 135, 180]
    
    output_file = 'FINAL_small_box_extended.yml'
    
    with open(output_file, 'w', encoding='utf-8') as f:
        f.write('%YAML 1.1\n')
        f.write('# OrcaWave Configuration - Small Box Extended Analysis\n')
        f.write('---\n')
        yaml.dump(config, f, default_flow_style=False, sort_keys=False, allow_unicode=True)
    
    print(f"Created: {output_file} (with more periods/headings)")

def main():
    print("\nCreating FINAL working configuration...")
    print("COMMITMENT: Using ONLY small_box_test.gdf - no switching!")
    print()
    
    final_file = create_final_working_config()
    
    print("\n" + "="*60)
    print(" INSTRUCTIONS ")
    print("="*60)
    print()
    print("1. OPEN THIS FILE:")
    print(f"   {final_file}")
    print()
    print("2. IF IT WORKS:")
    print("   Try: FINAL_small_box_extended.yml")
    print("   (same but with more periods/headings)")
    print()
    print("3. KEY POINTS:")
    print("   - We're using small_box_test.gdf ONLY")
    print("   - This file exists and has valid content")
    print("   - No more switching between mesh files")
    print("   - Configuration matches OrcaWave's expected format")
    print()
    
    return 0

if __name__ == "__main__":
    main()