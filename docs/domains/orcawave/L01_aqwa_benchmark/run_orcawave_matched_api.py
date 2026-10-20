#!/usr/bin/env python3
"""
ABOUTME: Run OrcaWave matched model via API (180 cases)
Execute diffraction analysis with AQWA-matched mass properties.
"""

import sys
from pathlib import Path
from datetime import datetime
import time

from loguru import logger


def run_orcawave_analysis(yml_file: Path, output_file: Path, *,
                         diffraction_factory=None):
    """Calculate and save; True is not native readback or qualification.

    The factory permits offline orchestration tests without importing a solver.
    Production uses the solver manager's configured API environment.
    """
    if diffraction_factory is None:
        try:
            import OrcFxAPI
        except ImportError as error:
            logger.error(f"Failed to import OrcFxAPI: {error}")
            return False
        diffraction_factory = OrcFxAPI.Diffraction

    logger.info("="*80)
    logger.info("ORCAWAVE API EXECUTION - MATCHED MODEL")
    logger.info("="*80)

    logger.info(f"\n📂 Input: {yml_file.name}")
    logger.info(f"📂 Output: {output_file.name}")

    # Load model
    logger.info("\n[1/3] Loading OrcaWave model...")
    start_load = time.time()

    try:
        diffraction = diffraction_factory(str(yml_file.absolute()))
        load_time = time.time() - start_load
        logger.success(f"✓ Model loaded in {load_time:.2f}s")
    except Exception as e:
        logger.error(f"Failed to load model: {e}")
        return False

    try:
        # Display configuration
        logger.info("\n📋 Analysis Configuration:")
        logger.info(f"  Periods: {len(diffraction.PeriodOrFrequency)} values")
        logger.info(f"  Headings: {len(diffraction.WaveHeading)} values")
        logger.info(f"  Total cases: {len(diffraction.PeriodOrFrequency) * len(diffraction.WaveHeading)}")

    except Exception as error:
        logger.error(f"Failed to read calculation configuration: {error}")
        return False

    # Check mass properties
    try:
        body = diffraction.Bodies[0]
        logger.info("\n🔧 Body Properties:")
        logger.info(f"  Name: {body.BodyName}")
        logger.info(f"  Mass: {body.BodyMass:.2f} tonnes")
        logger.info(f"  COG: {body.BodyCentreOfMass}")
    except (AttributeError, IndexError, TypeError):
        logger.warning("Could not read body properties")

    # Run calculation
    logger.info("\n[2/3] Running diffraction calculation...")
    logger.info("⏱️  Licensed runtime estimate: NOT_EVALUATED; use reviewed allocation")
    logger.info(f"🚀 Started at: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")

    start_calc = time.time()

    try:
        # Calculate is the recorded diffraction entry point in this script.
        # An unavailable method or calculation error must never imply completion.
        diffraction.Calculate()
        calc_time = time.time() - start_calc
        logger.info(f"⏱️  Processing time: {calc_time/60:.1f} minutes ({calc_time:.1f}s)")

    except Exception as e:
        calc_time = time.time() - start_calc
        logger.error(f"❌ Calculation failed after {calc_time/60:.1f} minutes")
        logger.error(f"Error type: {type(e).__name__}")
        logger.error(f"Error: {e}")

        return False

    # Save results
    logger.info("\n[3/3] Saving results...")
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
    logger.info("\n" + "="*80)
    logger.success("✅ CALCULATION SAVED - NATIVE READBACK AND QA STILL REQUIRED")
    logger.info("\n⏱️  Total Time Breakdown:")
    logger.info(f"  Load:        {load_time:.1f}s")
    logger.info(f"  Calculate:   {calc_time/60:.1f} min ({calc_time:.1f}s)")
    logger.info(f"  Save:        {save_time:.1f}s")
    logger.info(f"  TOTAL:       {total_time/60:.1f} min ({total_time:.1f}s)")
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
    logger.warning("Licensed runtime estimate: NOT_EVALUATED")
    logger.warning("Record native progress and resource evidence before diagnosing a timeout")

    # Run analysis
    success = run_orcawave_analysis(yml_file, output_file)

    if success:
        logger.info("\nNEXT STEP: Verify native readback and model equivalence")
        logger.info("Command:")
        logger.info("  See solver_request.html for full-grid export and QA requirements")
        return 0
    else:
        logger.error("\n❌ Analysis failed - check logs above")
        return 1


if __name__ == "__main__":
    sys.exit(main())
