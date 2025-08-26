#!/usr/bin/env python3
"""
Use OrcaFlex API to create/export geometry for OrcaWave
This leverages OrcaFlex's built-in geometry handling
"""

import sys
import os
from pathlib import Path
import logging

# Add OrcaFlex to path
sys.path.append(r'C:\Program Files (x86)\Orcina\OrcaFlex\11.5')

try:
    import OrcFxAPI as ofx
    print("OrcaFlex API loaded successfully")
except ImportError as e:
    print(f"Failed to import OrcaFlex API: {e}")
    print("Make sure OrcaFlex is installed and licensed")
    sys.exit(1)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def create_vessel_in_orcaflex(stl_file=None):
    """
    Create a vessel in OrcaFlex and export for OrcaWave
    """
    
    logger.info("Creating new OrcaFlex model...")
    
    # Create new model
    model = ofx.Model()
    
    # Set environment
    env = model.environment
    env.WaterDepth = 100  # 100m water depth
    env.Density = 1025    # Seawater density
    
    # Create vessel
    vessel = model.CreateObject(ofx.otVessel, "SeaCypress")
    
    # Vessel type setup
    vessel_type = model.CreateObject(ofx.otVesselType, "SeaCypressType")
    
    # Set basic dimensions (Sea Cypress tug)
    vessel_type.Length = 30.0  # 30m length
    vessel_type.Breadth = 9.0  # 9m beam
    
    # Set mass properties
    vessel_type.Mass = 400  # 400 tonnes
    vessel_type.CentreOfMassX = 0
    vessel_type.CentreOfMassY = 0
    vessel_type.CentreOfMassZ = -1.5  # CoG below waterline
    
    # Inertias (estimates for tug)
    vessel_type.MomentOfInertiaX = 1.6e6
    vessel_type.MomentOfInertiaY = 1.5e7
    vessel_type.MomentOfInertiaZ = 1.5e7
    
    # Set draughts
    vessel_type.Draughts = "2.0, 2.5, 3.0, 3.5"  # Various loading conditions
    
    # Link vessel to type
    vessel.VesselType = vessel_type
    
    logger.info("Vessel created in OrcaFlex")
    
    # Save OrcaFlex data file
    orcaflex_file = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/models/sea_cypress.dat")
    orcaflex_file.parent.mkdir(exist_ok=True)
    
    model.SaveData(str(orcaflex_file))
    logger.info(f"OrcaFlex model saved: {orcaflex_file}")
    
    return model, vessel

def export_for_orcawave(model, vessel):
    """
    Export vessel data in format suitable for OrcaWave
    """
    
    logger.info("Exporting vessel data for OrcaWave...")
    
    # Get vessel properties
    vessel_type = vessel.VesselType
    
    # Create OrcaWave input file
    orcawave_config = f"""# OrcaWave Configuration
# Generated from OrcaFlex vessel data

# Vessel Properties
Length: {vessel_type.Length} m
Beam: {vessel_type.Breadth} m
Mass: {vessel_type.Mass} tonnes
CoG: ({vessel_type.CentreOfMassX}, {vessel_type.CentreOfMassY}, {vessel_type.CentreOfMassZ}) m

# Inertias
Ixx: {vessel_type.MomentOfInertiaX} kg.m²
Iyy: {vessel_type.MomentOfInertiaY} kg.m²
Izz: {vessel_type.MomentOfInertiaZ} kg.m²

# Draughts
Draughts: {vessel_type.Draughts}

# Mesh
Note: Use OrcaFlex's vessel drawing to generate mesh
Or import external mesh file
"""
    
    output_file = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/models/vessel_properties.txt")
    output_file.write_text(orcawave_config)
    
    logger.info(f"Vessel properties exported: {output_file}")
    
    return output_file

def create_orcawave_batch():
    """
    Create a batch script to run OrcaWave from OrcaFlex
    """
    
    batch_script = """@echo off
REM Launch OrcaWave from OrcaFlex model

echo Starting OrcaWave analysis from OrcaFlex...

set ORCAFLEX="C:\\Program Files (x86)\\Orcina\\OrcaFlex\\11.5\\OrcaFlex.exe"
set ORCAWAVE="C:\\Program Files (x86)\\Orcina\\OrcaFlex\\11.5\\OrcaWave.exe"
set MODEL=..\\models\\sea_cypress.dat

echo Step 1: Open model in OrcaFlex
%ORCAFLEX% %MODEL%

echo.
echo In OrcaFlex:
echo 1. Go to Vessel object
echo 2. Right-click and select "Export to OrcaWave"
echo 3. Or use: Model -> Export -> OrcaWave data
echo.
pause

echo Step 2: Open in OrcaWave
%ORCAWAVE%

echo.
echo In OrcaWave:
echo 1. File -> Import -> From OrcaFlex
echo 2. Select the exported data
echo 3. Run diffraction analysis
echo.
pause
"""
    
    batch_file = Path("D:/github/digitalmodel/specs/modules/orcawave/diffraction-analysis/scripts/orcaflex_to_orcawave.bat")
    batch_file.write_text(batch_script)
    
    logger.info(f"Batch script created: {batch_file}")
    return batch_file

def test_orcaflex_api():
    """
    Test OrcaFlex API capabilities
    """
    
    logger.info("Testing OrcaFlex API...")
    
    try:
        # Create test model
        model = ofx.Model()
        
        # Check if we can access environment
        env = model.environment
        logger.info(f"Default water depth: {env.WaterDepth}m")
        logger.info(f"Default water density: {env.Density} kg/m³")
        
        # List available object types
        logger.info("OrcaFlex objects available:")
        object_types = [
            "Vessel", "Line", "Buoy", 
            "6DBuoy", "3DBuoy", "Shape"
        ]
        
        for obj_type in object_types:
            logger.info(f"  - {obj_type}")
        
        return True
        
    except Exception as e:
        logger.error(f"API test failed: {e}")
        logger.info("This might be a license issue - OrcaFlex needs a valid license")
        return False

def main():
    """
    Main execution
    """
    
    logger.info("="*60)
    logger.info("OrcaFlex to OrcaWave Geometry Transfer")
    logger.info("="*60)
    
    # Test API
    if not test_orcaflex_api():
        logger.error("OrcaFlex API test failed")
        return
    
    try:
        # Create vessel in OrcaFlex
        model, vessel = create_vessel_in_orcaflex()
        
        # Export properties
        export_for_orcawave(model, vessel)
        
        # Create batch script
        batch_file = create_orcawave_batch()
        
        logger.info("\n" + "="*60)
        logger.info("SUCCESS: OrcaFlex model created")
        logger.info("="*60)
        logger.info("\nNext steps:")
        logger.info("1. Run the batch file: orcaflex_to_orcawave.bat")
        logger.info("2. Follow the instructions to export to OrcaWave")
        logger.info("3. OrcaWave will handle the geometry correctly")
        
    except Exception as e:
        logger.error(f"Failed to create OrcaFlex model: {e}")
        logger.info("\nThis might be due to:")
        logger.info("- OrcaFlex license not available")
        logger.info("- Insufficient permissions")
        logger.info("- API version mismatch")

if __name__ == "__main__":
    main()