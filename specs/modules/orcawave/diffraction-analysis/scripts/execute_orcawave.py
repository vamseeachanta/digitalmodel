#!/usr/bin/env python
"""
Execute OrcaWave Analysis
Direct interface to OrcaWave for Sea Cypress diffraction analysis
"""

import os
import sys
import subprocess
from pathlib import Path
import time
import logging

# Setup logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

# OrcaWave configuration
ORCAWAVE_PATH = r"C:\Program Files (x86)\Orcina\OrcaFlex\11.5"
ORCAWAVE_EXE = os.path.join(ORCAWAVE_PATH, "OrcaWave.exe")

def check_orcawave():
    """Check if OrcaWave is available."""
    if not os.path.exists(ORCAWAVE_EXE):
        logger.error(f"OrcaWave not found at: {ORCAWAVE_EXE}")
        return False
    logger.info(f"OrcaWave found at: {ORCAWAVE_EXE}")
    return True

def prepare_orcawave_input():
    """Prepare OrcaWave input file in native format."""
    logger.info("Preparing OrcaWave input...")
    
    # Create OrcaWave native input file
    owp_content = """# OrcaWave Project File
# Sea Cypress Diffraction Analysis

[General]
Title = Sea Cypress Tug Diffraction Analysis
UnitsSystem = SI
WaterDepth = 100.0
WaterDensity = 1025.0

[Mesh]
InputFile = ../inputs/geometry/sea_cypress_trimesh.gdf
MeshFormat = GDF
Symmetry = None

[FrequencyRange]
Type = Periods
Values = 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 25

[Directions]
Type = Degrees
Values = 0, 22.5, 45, 67.5, 90, 112.5, 135, 157.5, 180

[Analysis]
CalculationType = Diffraction
IncludeRadiation = Yes
IncludeDiffraction = Yes
ComputeQTF = Yes
QTFMethod = Direct

[Output]
OutputDirectory = ../outputs
SaveRAOs = Yes
SaveAddedMass = Yes
SaveDamping = Yes
SaveExcitation = Yes
SaveQTF = Yes
OutputFormat = All

[Solver]
Method = DirectLU
Tolerance = 1e-6
MaxIterations = 1000
"""
    
    owp_file = Path("../configs/sea_cypress.owp")
    owp_file.parent.mkdir(exist_ok=True)
    owp_file.write_text(owp_content)
    logger.info(f"Created OrcaWave project file: {owp_file}")
    return owp_file

def run_orcawave_analysis(project_file):
    """Execute OrcaWave analysis."""
    logger.info("Starting OrcaWave analysis...")
    
    try:
        # Prepare command
        cmd = [ORCAWAVE_EXE, str(project_file)]
        
        logger.info(f"Executing: {' '.join(cmd)}")
        
        # Run OrcaWave
        start_time = time.time()
        process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )
        
        # Wait for completion
        stdout, stderr = process.communicate(timeout=14400)  # 4 hour timeout
        
        elapsed = time.time() - start_time
        
        if process.returncode == 0:
            logger.info(f"SUCCESS: Analysis completed in {elapsed/60:.1f} minutes")
            return True
        else:
            logger.error(f"FAILED: Return code {process.returncode}")
            if stderr:
                logger.error(f"Error output: {stderr}")
            return False
            
    except subprocess.TimeoutExpired:
        logger.error("Analysis timed out after 4 hours")
        process.kill()
        return False
    except Exception as e:
        logger.error(f"Execution failed: {e}")
        return False

def verify_outputs():
    """Check if output files were created."""
    output_dir = Path("../outputs")
    expected_files = [
        "AddedMass.txt",
        "Damping.txt", 
        "Excitation.txt",
        "RAOs.txt"
    ]
    
    found = []
    missing = []
    
    for file in expected_files:
        file_path = output_dir / file
        if file_path.exists():
            found.append(file)
            logger.info(f"  Found: {file}")
        else:
            missing.append(file)
    
    if missing:
        logger.warning(f"Missing files: {missing}")
    
    return len(missing) == 0

def main():
    """Main execution."""
    logger.info("="*60)
    logger.info("OrcaWave Sea Cypress Analysis")
    logger.info("="*60)
    
    # Change to script directory
    os.chdir(Path(__file__).parent)
    
    # Check OrcaWave
    if not check_orcawave():
        logger.error("OrcaWave not available. Please check installation.")
        sys.exit(1)
    
    # Prepare input
    project_file = prepare_orcawave_input()
    
    # Run analysis
    success = run_orcawave_analysis(project_file)
    
    if success:
        # Verify outputs
        if verify_outputs():
            logger.info("All output files generated successfully")
            
            # Process results
            logger.info("Processing results...")
            os.system("python process_orcawave_results.py --output-dir ../outputs")
        else:
            logger.warning("Some output files missing")
    else:
        logger.error("Analysis failed")
        sys.exit(1)
    
    logger.info("="*60)
    logger.info("Complete!")
    logger.info("="*60)

if __name__ == "__main__":
    main()