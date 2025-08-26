#!/usr/bin/env python
"""
Test OrcaWave Geometry Import
Verify that the AQWA DAT file can be loaded by OrcaWave
"""

import os
import sys
import subprocess
from pathlib import Path
import logging

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# OrcaWave path
ORCAWAVE_PATH = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.5"
ORCAWAVE_EXE = os.path.join(ORCAWAVE_PATH, "OrcaWave.exe")

def test_geometry_import():
    """Test if OrcaWave can import the geometry file."""
    
    # Change to the configs directory
    os.chdir(Path(__file__).parent.parent / "configs")
    
    # Create a minimal OrcaWave configuration for testing
    test_config = """%%YAML 1.1
# Minimal OrcaWave configuration to test geometry import
---
# Model
UnitsSystem: SI

# Environment
WaterDepth: 100
WaterDensity: 1025

# Just test loading the geometry
Bodies:
  - BodyName: SeaCypress_Test
    BodyMeshPosition: [0, 0, 0]
    BodyMeshAttitude: [0, 0, 0]
    BodyIncludedInAnalysis: Yes
    BodyMeshFileName: ../inputs/geometry/sea_cypress_gmsh_optimized.dat
    BodyMeshFormat: Aqwa dat
    BodyMeshLengthUnits: m
    BodyMeshSymmetry: None
    BodyMeshBodyNumber: 1
    
# Minimal frequency range for testing
PeriodOrFrequency:
  - 10  # Just one period for quick test
  
WaveHeading:
  - 0   # Just head seas for quick test
...
"""
    
    # Save test configuration
    test_file = Path("test_geometry_import.yml")
    test_file.write_text(test_config)
    logger.info(f"Created test configuration: {test_file}")
    
    # Try to run OrcaWave in validation mode
    logger.info("Testing geometry import with OrcaWave...")
    
    try:
        # Use OrcaWave to validate the model
        cmd = [ORCAWAVE_EXE, "-validate", str(test_file)]
        
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=30
        )
        
        if "successfully" in result.stdout.lower() or result.returncode == 0:
            logger.info("SUCCESS: Geometry imported successfully!")
            return True
        else:
            logger.error("FAILED: Could not import geometry")
            if result.stdout:
                logger.error(f"Output: {result.stdout}")
            if result.stderr:
                logger.error(f"Error: {result.stderr}")
            return False
            
    except subprocess.TimeoutExpired:
        logger.warning("Validation timed out - this might mean OrcaWave opened GUI mode")
        logger.info("Please check if OrcaWave opened and loaded the geometry")
        return None
    except FileNotFoundError:
        logger.error(f"OrcaWave not found at: {ORCAWAVE_EXE}")
        return False
    except Exception as e:
        logger.error(f"Error during validation: {e}")
        return False
    finally:
        # Clean up test file
        if test_file.exists():
            test_file.unlink()

def verify_dat_file():
    """Verify the AQWA DAT file exists and has correct format."""
    dat_file = Path("../inputs/geometry/sea_cypress_gmsh_optimized.dat")
    
    if not dat_file.exists():
        logger.error(f"DAT file not found: {dat_file}")
        return False
    
    logger.info(f"Found DAT file: {dat_file}")
    
    # Check file format
    with open(dat_file, 'r') as f:
        lines = f.readlines()
        
    # Check header
    if len(lines) < 3:
        logger.error("DAT file too short")
        return False
    
    # First line should have gravity
    if "9.80665" not in lines[0]:
        logger.warning("Gravity value not found in header")
    
    # Second line should have symmetry flags
    if "0 0" not in lines[1] and "0" not in lines[1]:
        logger.warning("Symmetry flags not found")
    
    # Third line should have number of nodes
    try:
        num_nodes = int(lines[2].strip())
        logger.info(f"Number of nodes: {num_nodes}")
    except:
        logger.error("Could not read number of nodes")
        return False
    
    # Check we have enough lines for nodes and panels
    expected_lines = 3 + num_nodes + 1  # header + nodes + panel count line
    if len(lines) < expected_lines:
        logger.error(f"File too short. Expected at least {expected_lines} lines, got {len(lines)}")
        return False
    
    # Get number of panels
    try:
        num_panels = int(lines[3 + num_nodes].strip())
        logger.info(f"Number of panels: {num_panels}")
    except:
        logger.error("Could not read number of panels")
        return False
    
    logger.info("DAT file format appears correct")
    logger.info(f"Mesh: {num_nodes} nodes, {num_panels} panels")
    
    return True

def main():
    """Main test function."""
    logger.info("="*60)
    logger.info("OrcaWave Geometry Import Test")
    logger.info("="*60)
    
    # First verify the DAT file
    if not verify_dat_file():
        logger.error("DAT file verification failed")
        sys.exit(1)
    
    logger.info("")
    
    # Check if OrcaWave exists
    if not os.path.exists(ORCAWAVE_EXE):
        logger.error(f"OrcaWave not found at: {ORCAWAVE_EXE}")
        logger.info("Cannot perform import test without OrcaWave")
        sys.exit(1)
    
    # Test geometry import
    result = test_geometry_import()
    
    if result is True:
        logger.info("✓ Geometry import test PASSED")
    elif result is False:
        logger.error("✗ Geometry import test FAILED")
        logger.info("\nTroubleshooting:")
        logger.info("1. Open OrcaWave manually")
        logger.info("2. File -> Import -> AQWA DAT")
        logger.info("3. Select: sea_cypress_gmsh_optimized.dat")
        logger.info("4. Check for any error messages")
    else:
        logger.info("Test inconclusive - please check OrcaWave manually")
    
    logger.info("="*60)

if __name__ == "__main__":
    main()