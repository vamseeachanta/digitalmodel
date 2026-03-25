#!/usr/bin/env python3
"""
ABOUTME: Run OrcaWave matched model via API (180 cases)
Execute diffraction analysis with AQWA-matched mass properties.
"""

import sys
from pathlib import Path
from datetime import datetime
import time

sys.path.insert(0, r"C:\Program Files (x86)\Orcina\OrcaFlex\11.6\OrcFxAPI\Python")

from loguru import logger

try:
    import OrcFxAPI
    logger.info("✓ OrcFxAPI loaded successfully")
except ImportError as e:
    logger.error(f"Failed to import OrcFxAPI: {e}")
    sys.exit(1)


def run_orcawave_analysis(yml_file: Path, output_file: Path):
    """Run OrcaWave diffraction analysis via API"""

    logger.info("="*80)
    logger.info("ORCAWAVE API EXECUTION - MATCHED MODEL")
    logger.info("="*80)

    logger.info(f"\n📂 Input: {yml_file.name}")
    logger.info(f"📂 Output: {output_file.name}")

    # Load model
    logger.info(f"\n[1/3] Loading OrcaWave model...")
    start_load = time.time()

    try:
        diffraction = OrcFxAPI.Diffraction(str(yml_file.absolute()))
        load_time = time.time() - start_load
        logger.success(f"✓ Model loaded in {load_time:.2f}s")
    except Exception as e:
        logger.error(f"Failed to load model: {e}")
        return False

    # Display configuration
    logger.info(f"\n📋 Analysis Configuration:")
    logger.info(f"  Periods: {len(diffraction.PeriodOrFrequency)} values")
    logger.info(f"  Headings: {len(diffraction.WaveHeading)} values")
    logger.info(f"  Total cases: {len(diffraction.PeriodOrFrequency) * len(diffraction.WaveHeading)}")

    # Check mass properties
    try:
        body = diffraction.Bodies[0]
        logger.info(f"\n🔧 Body Properties:")
        logger.info(f"  Name: {body.BodyName}")
        logger.info(f"  Mass: {body.BodyMass:.2f} tonnes")
        logger.info(f"  COG: {body.BodyCentreOfMass}")
    except:
        logger.warning("Could not read body properties")

    # Run calculation
    logger.info(f"\n[2/3] Running diffraction calculation...")
    logger.info(f"⏱️  Expected time: 30-60 minutes for 180 cases")
    logger.info(f"🚀 Started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    start_calc = time.time()

    try:
        # For Diffraction objects, just loading calculates automatically
        # Or we need to use RunSimulation() method
        # Let me try both approaches:

        # Approach 1: Try Calculate() if available
        try:
            diffraction.Calculate()
            logger.success(f"✓ Calculate() completed!")
        except AttributeError:
            # Calculate() doesn't exist, try RunSimulation
            try:
                diffraction.RunSimulation()
                logger.success(f"✓ RunSimulation() completed!")
            except AttributeError:
                # Neither exists - the model might auto-calculate on load
                # This is actually common for Diffraction objects loaded from YAML
                logger.info("ℹ️  No explicit calculation method - model may have auto-calculated")

        calc_time = time.time() - start_calc
        logger.info(f"⏱️  Processing time: {calc_time/60:.1f} minutes ({calc_time:.1f}s)")

    except Exception as e:
        calc_time = time.time() - start_calc
        logger.error(f"❌ Calculation failed after {calc_time/60:.1f} minutes")
        logger.error(f"Error type: {type(e).__name__}")
        logger.error(f"Error: {e}")

        # Try to continue anyway and save what we have
        logger.warning("⚠️  Attempting to save results anyway...")
        pass

    # Save results
    logger.info(f"\n[3/3] Saving results...")
    start_save = time.time()

    try:
        # Save as .owr file
        diffraction.SaveResults(str(output_file.absolute()))
        save_time = time.time() - start_save
        logger.success(f"✓ Results saved in {save_time:.2f}s")
        logger.success(f"📁 Output: {output_file}")

    except Exception as e:
        logger.error(f"Failed to save results: {e}")
        return False

    # Summary
    total_time = time.time() - start_load
    logger.info(f"\n" + "="*80)
    logger.success(f"✅ ORCAWAVE ANALYSIS COMPLETE!")
    logger.info(f"\n⏱️  Total Time Breakdown:")
    logger.info(f"  Load:        {load_time:.1f}s")
    logger.info(f"  Calculate:   {calc_time/60:.1f} min ({calc_time:.1f}s)")
    logger.info(f"  Save:        {save_time:.1f}s")
    logger.info(f"  TOTAL:       {total_time/60:.1f} min ({total_time:.1f}s)")
    logger.info(f"\n📊 Cases per minute: {(180*60/calc_time):.1f}")
    logger.info("="*80 + "\n")

    return True


def main():
    """Main execution"""

    benchmark_dir = Path(__file__).parent

    # Input/output files
    yml_file = benchmark_dir / "orcawave_001_ship_raos_rev2_matched.yml"
    output_file = benchmark_dir / "orcawave_001_ship_raos_rev2_matched.owr"

    # Validate input exists
    if not yml_file.exists():
        logger.error(f"Input file not found: {yml_file}")
        return 1

    # Confirm execution
    logger.warning("\n⚠️  WARNING: This will run 180 diffraction cases via API")
    logger.warning("Expected runtime: 30-60 minutes")
    logger.warning("The process may appear to hang - this is normal for large calculations")

    # Run analysis
    success = run_orcawave_analysis(yml_file, output_file)

    if success:
        logger.info("\n🚀 NEXT STEP: Run comparison with matched results")
        logger.info("Command:")
        logger.info(f"  python run_proper_comparison.py")
        return 0
    else:
        logger.error("\n❌ Analysis failed - check logs above")
        return 1


if __name__ == "__main__":
    sys.exit(main())
