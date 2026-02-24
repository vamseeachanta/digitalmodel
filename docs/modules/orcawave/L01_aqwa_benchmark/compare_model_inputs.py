#!/usr/bin/env python3
"""
ABOUTME: Compare AQWA and OrcaWave model inputs (mass, COG, geometry)
to identify differences causing RAO mismatches.
"""

import sys
from pathlib import Path
import yaml
import re

sys.path.insert(0, str(Path(__file__).resolve().parents[4] / "src"))

from loguru import logger

def extract_aqwa_inputs(lis_file: Path):
    """Extract model inputs from AQWA .LIS file"""

    content = lis_file.read_text()

    inputs = {}

    # Extract mass displacement
    match = re.search(r'MASS BASED DISPLACEMENT.*?=\s+([\d.E+-]+)', content)
    if match:
        inputs['displacement'] = float(match.group(1))

    # Extract COG
    match = re.search(r'THE CENTRE OF GRAVITY\s+AT\s+\(\s+([\d.E+-]+),\s+([\d.E+-]+),\s+([\d.E+-]+)\)', content)
    if match:
        inputs['cog'] = [float(match.group(1)), float(match.group(2)), float(match.group(3))]

    # Extract centre of buoyancy
    match = re.search(r'POSITION OF THE CENTRE OF BUOYANCY\s+BX\s+=\s+([\d.E+-]+)', content)
    if match:
        inputs['centre_of_buoyancy_x'] = float(match.group(1))

    return inputs

def extract_orcawave_inputs(yml_file: Path):
    """Extract model inputs from OrcaWave .yml file"""

    with yml_file.open('r', encoding='utf-8') as f:
        data = yaml.safe_load(f)

    body = data['Bodies'][0]

    inputs = {
        'mass': body['BodyMass'],
        'cog': body['BodyCentreOfMass'],
        'mesh_position': body['BodyMeshPosition'],
        'mesh_attitude': body['BodyMeshAttitude'],
        'origin_type': body['BodyOriginType'],
        'inertia_tensor': body['BodyInertiaTensorRx, BodyInertiaTensorRy, BodyInertiaTensorRz'],
        'water_depth': data['WaterDepth'],
        'water_density': data['WaterDensity']
    }

    return inputs

def main():
    """Compare model inputs"""

    benchmark_dir = Path(__file__).parent

    logger.info("="*80)
    logger.info("AQWA vs OrcaWave MODEL INPUT COMPARISON")
    logger.info("="*80)

    # Extract AQWA inputs
    aqwa_lis = benchmark_dir / "001_SHIP_RAOS_REV2.LIS"
    aqwa_inputs = extract_aqwa_inputs(aqwa_lis)

    logger.info("\n📋 AQWA Model Inputs:")
    logger.info(f"  Displacement: {aqwa_inputs.get('displacement', 'N/A'):.2f} tonnes")
    logger.info(f"  COG: {aqwa_inputs.get('cog', 'N/A')}")
    logger.info(f"  Centre of Buoyancy X: {aqwa_inputs.get('centre_of_buoyancy_x', 'N/A')}")

    # Extract OrcaWave inputs
    orcawave_yml = benchmark_dir / "orcawave_001_ship_raos_rev2.yml"
    orcawave_inputs = extract_orcawave_inputs(orcawave_yml)

    logger.info("\n📋 OrcaWave Model Inputs:")
    logger.info(f"  Mass: {orcawave_inputs['mass']:.2f} tonnes")
    logger.info(f"  COG: {orcawave_inputs['cog']}")
    logger.info(f"  Origin Type: {orcawave_inputs['origin_type']}")
    logger.info(f"  Mesh Position: {orcawave_inputs['mesh_position']}")
    logger.info(f"  Mesh Attitude: {orcawave_inputs['mesh_attitude']}")
    logger.info(f"  Water Depth: {orcawave_inputs['water_depth']} m")
    logger.info(f"  Water Density: {orcawave_inputs['water_density']} t/m³")

    # Compare
    logger.info("\n⚠️  DIFFERENCES:")

    aqwa_disp = aqwa_inputs.get('displacement', 0)
    ow_mass = orcawave_inputs['mass']

    if abs(aqwa_disp - ow_mass) > 1.0:
        ratio = aqwa_disp / ow_mass if ow_mass > 0 else 0
        logger.warning(f"  Mass/Displacement mismatch:")
        logger.warning(f"    AQWA displacement: {aqwa_disp:.2f} tonnes")
        logger.warning(f"    OrcaWave mass: {ow_mass:.2f} tonnes")
        logger.warning(f"    Ratio: {ratio:.2f}x")
        logger.warning(f"  → This will cause different natural frequencies!")

    aqwa_cog = aqwa_inputs.get('cog', [0, 0, 0])
    ow_cog = orcawave_inputs['cog']

    if max(abs(a - b) for a, b in zip(aqwa_cog, ow_cog)) > 1.0:
        logger.warning(f"  COG position mismatch:")
        logger.warning(f"    AQWA COG: {aqwa_cog}")
        logger.warning(f"    OrcaWave COG: {ow_cog}")
        logger.warning(f"  → Different reference frames or coordinate systems!")

    logger.info("\n" + "="*80)
    logger.info("✓ Input comparison complete")
    logger.info("="*80)

    return 0

if __name__ == "__main__":
    sys.exit(main())
