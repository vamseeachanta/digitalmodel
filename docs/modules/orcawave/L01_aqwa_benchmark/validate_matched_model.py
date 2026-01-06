#!/usr/bin/env python3
"""
ABOUTME: Validate the matched OrcaWave model before running analysis
"""

import yaml
from pathlib import Path
from loguru import logger

def validate_model(yml_file: Path):
    """Validate updated model parameters"""

    logger.info("="*80)
    logger.info("VALIDATING UPDATED ORCAWAVE MODEL")
    logger.info("="*80)

    with yml_file.open('r', encoding='utf-8') as f:
        data = yaml.safe_load(f)

    body = data['Bodies'][0]

    # Expected AQWA values
    expected = {
        'mass': 44082.20,
        'cog_x': 108.88,
        'cog_z_approx': 8.0,
        'ixx_approx': 8.357e6,
        'iyy_approx': 1.130e8,
        'izz_approx': 1.222e8
    }

    # Actual values
    actual = {
        'mass': body['BodyMass'],
        'cog': body['BodyCentreOfMass'],
        'inertia': body['BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz']
    }

    logger.info(f"\n✓ Model file: {yml_file.name}")
    logger.info(f"\n📋 Mass Properties:")
    logger.info(f"  Mass: {actual['mass']:.2f} tonnes (expected: {expected['mass']:.2f})")
    logger.info(f"  COG: {actual['cog']}")
    logger.info(f"    X: {actual['cog'][0]:.2f} m (expected: {expected['cog_x']:.2f})")
    logger.info(f"    Z: {actual['cog'][2]:.2f} m (expected: ~{expected['cog_z_approx']:.1f})")

    # Convert inertia values to float (YAML may parse as string)
    ixx = float(actual['inertia'][0][0])
    iyy = float(actual['inertia'][1][1])
    izz = float(actual['inertia'][2][2])

    logger.info(f"\n📊 Inertia Tensor (tonne·m²):")
    logger.info(f"  Ixx: {ixx:.2e} (expected: ~{expected['ixx_approx']:.2e})")
    logger.info(f"  Iyy: {iyy:.2e} (expected: ~{expected['iyy_approx']:.2e})")
    logger.info(f"  Izz: {izz:.2e} (expected: ~{expected['izz_approx']:.2e})")

    # Validation checks
    errors = []

    if abs(actual['mass'] - expected['mass']) > 0.1:
        errors.append(f"Mass mismatch: {actual['mass']} vs {expected['mass']}")

    if abs(actual['cog'][0] - expected['cog_x']) > 0.1:
        errors.append(f"COG X mismatch: {actual['cog'][0]} vs {expected['cog_x']}")

    if abs(ixx - expected['ixx_approx']) / expected['ixx_approx'] > 0.01:
        errors.append(f"Ixx mismatch: {ixx} vs {expected['ixx_approx']}")

    logger.info(f"\n🔍 Analysis Configuration:")
    logger.info(f"  Periods: {len(data['PeriodOrFrequency'])} values")
    logger.info(f"  Headings: {len(data['WaveHeading'])} values")
    logger.info(f"  Total cases: {len(data['PeriodOrFrequency']) * len(data['WaveHeading'])}")

    if errors:
        logger.error(f"\n❌ VALIDATION FAILED:")
        for error in errors:
            logger.error(f"  - {error}")
        return False
    else:
        logger.success(f"\n✅ VALIDATION PASSED")
        logger.success(f"  Mass properties match AQWA model")
        logger.success(f"  Model ready for OrcaWave execution")
        return True

def main():
    benchmark_dir = Path(__file__).parent
    matched_yml = benchmark_dir / "orcawave_001_ship_raos_rev2_matched.yml"

    if not matched_yml.exists():
        logger.error(f"File not found: {matched_yml}")
        return 1

    result = validate_model(matched_yml)

    logger.info("\n" + "="*80)
    logger.info("NEXT STEPS:")
    logger.info("="*80)
    logger.info("\n1. Open OrcaWave GUI")
    logger.info(f"2. Load file: {matched_yml.name}")
    logger.info("3. Run diffraction analysis")
    logger.info("4. Save results as: orcawave_001_ship_raos_rev2_matched.owr")
    logger.info("\n⏱️  Expected runtime: ~1 hour (180 cases)")
    logger.info("="*80 + "\n")

    return 0 if result else 1

if __name__ == "__main__":
    import sys
    sys.exit(main())
