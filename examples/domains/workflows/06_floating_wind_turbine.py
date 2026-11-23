#!/usr/bin/env python3
"""
Floating Wind Turbine Mooring Analysis

Description: Semi-submersible platform mooring for offshore wind turbine
Application: 15MW floating wind turbine in 200m water depth
Standards: DNV-ST-0119, IEC 61400-3-2, DNV-OS-E301
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
class FloatingWindTurbine:
    """Floating wind turbine platform data."""
    rated_power: float = 15.0          # [MW]
    rotor_diameter: float = 240.0      # [m]
    hub_height: float = 150.0          # [m]
    platform_type: str = "semi-submersible"
    platform_draft: float = 20.0       # [m]
    displacement: float = 20000.0      # [tonnes]
    water_depth: float = 200.0         # [m]


def analyze_floating_wind_mooring(turbine: FloatingWindTurbine, wind_speed: float) -> Dict:
    """Analyze floating wind turbine mooring."""

    # Wind thrust force (simplified)
    if wind_speed < 4.0:
        thrust = 0.0
    elif wind_speed < 11.0:
        thrust = 50.0 * wind_speed**2  # kN
    else:
        thrust = 6000.0  # Rated thrust, kN

    # Wave force (simplified)
    wave_force = 2000.0  # kN

    # Current force
    current_force = 500.0  # kN

    total_force = np.sqrt(thrust**2 + wave_force**2 + current_force**2)

    # 3-line catenary mooring (120° spacing)
    num_lines = 3
    max_line_tension = total_force / num_lines * 1.5  # Unequal loading factor

    # Chain MBL
    chain_mbl = 15000.0  # kN (for 127mm R4)
    sf = chain_mbl / max_line_tension
    sf_pass = sf >= 1.6  # Per DNV-ST-0119

    return {
        'wind_speed': wind_speed,
        'thrust': thrust,
        'total_force': total_force,
        'max_line_tension': max_line_tension,
        'safety_factor': sf,
        'status': 'PASS' if sf_pass else 'FAIL'
    }


def main():
    """Main execution."""
    logger.info("FLOATING WIND TURBINE MOORING ANALYSIS")

    turbine = FloatingWindTurbine()

    # Analyze wind speed range
    wind_speeds = np.linspace(0, 30, 15)
    results = [analyze_floating_wind_mooring(turbine, ws) for ws in wind_speeds]

    output_dir = Path(__file__).parent / "outputs" / "06_floating_wind"
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pd.DataFrame(results)
    df.to_excel(output_dir / "floating_wind_analysis.xlsx", index=False)

    logger.info(f"Analysis complete. Outputs saved to: {output_dir}")


if __name__ == "__main__":
    main()
