#!/usr/bin/env python3
"""
DP Backup Mooring System Analysis

Description: Emergency mooring system for DP drilling vessel failure scenarios
Application: Quick deployment mooring for DP Class 2/3 vessel emergency
Standards: DNV-OS-E301, IMCA M 109, API RP 2SK
Author: Marine Engineering Team
Date: 2025-10-03
"""

import sys
from pathlib import Path
from dataclasses import dataclass
import numpy as np
import pandas as pd
import logging

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class DPVesselConfig:
    """DP vessel configuration."""
    vessel_type: str = "drillship"
    loa: float = 240.0                 # [m]
    beam: float = 42.0                 # [m]
    displacement: float = 75000.0      # [tonnes]
    dp_class: int = 2
    water_depth: float = 2000.0        # [m]


def analyze_dp_backup_mooring(vessel: DPVesselConfig, failure_mode: str, current_speed: float) -> Dict:
    """Analyze DP backup mooring emergency scenario."""

    # Environmental force (simplified)
    wind_force = 2000.0  # kN (moderate wind)
    current_force = 0.5 * 1025 * (vessel.loa * vessel.beam * 0.1) * current_speed**2 / 1000  # kN

    # Total force
    total_force = np.sqrt(wind_force**2 + current_force**2)

    # Failure scenarios
    scenarios = {
        'single_thruster': 1.0,      # Full mooring capacity needed
        'blackout': 1.0,             # Full mooring capacity needed
        'drive_off': 1.2,            # Include dynamic amplification
        'drift_off': 0.8             # Slower drift, lower dynamic loads
    }

    load_factor = scenarios.get(failure_mode, 1.0)
    design_force = total_force * load_factor

    # Emergency mooring (4 lines typically)
    num_lines = 4
    max_line_tension = design_force / num_lines * 1.3  # Unequal loading

    # Wire rope MBL (large capacity for emergency system)
    wire_mbl = 12000.0  # kN
    sf = wire_mbl / max_line_tension

    # Different SF requirements for emergency system
    required_sf = 1.25  # Lower SF acceptable for emergency
    sf_pass = sf >= required_sf

    # Deployment time (simplified)
    deployment_time = 45.0  # minutes (typical quick deployment system)

    return {
        'failure_mode': failure_mode,
        'current_speed': current_speed,
        'total_force': total_force,
        'design_force': design_force,
        'max_line_tension': max_line_tension,
        'safety_factor': sf,
        'deployment_time': deployment_time,
        'status': 'PASS' if sf_pass else 'FAIL'
    }


def main():
    """Main execution."""
    logger.info("DP BACKUP MOORING ANALYSIS")

    vessel = DPVesselConfig()

    # Analyze different failure modes
    results = []
    failure_modes = ['single_thruster', 'blackout', 'drive_off', 'drift_off']
    current_speeds = [0.5, 1.0, 1.5]

    for mode in failure_modes:
        for current in current_speeds:
            result = analyze_dp_backup_mooring(vessel, mode, current)
            results.append(result)

    output_dir = Path(__file__).parent / "outputs" / "10_dp_backup"
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pd.DataFrame(results)
    df.to_excel(output_dir / "dp_backup_analysis.xlsx", index=False)

    logger.info(f"Analysis complete. Outputs saved to: {output_dir}")


if __name__ == "__main__":
    main()
