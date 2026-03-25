#!/usr/bin/env python3
"""
Single Point Mooring Terminal Analysis

Description: Offshore SPM buoy for crude oil tanker loading operations
Application: VLCC and Suezmax tanker mooring at offshore terminal
Standards: OCIMF MEG4, API RP 2SK, ISO 19901-7
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
class SPMConfig:
    """SPM terminal configuration."""
    water_depth: float = 60.0          # [m]
    num_chains: int = 6                # Ground chains
    chain_diameter: float = 127.0      # [mm]
    swivel_capacity: float = 15000.0   # [kN]


def analyze_spm_terminal(config: SPMConfig, tanker_size: str, wind_speed: float) -> Dict:
    """Analyze SPM terminal loading."""

    # Tanker particulars
    tanker_data = {
        'VLCC': {'loa': 330, 'beam': 60, 'wind_area': 8000},
        'Suezmax': {'loa': 270, 'beam': 48, 'wind_area': 6000},
        'Aframax': {'loa': 250, 'beam': 44, 'wind_area': 5000}
    }

    tanker = tanker_data.get(tanker_size, tanker_data['VLCC'])

    # OCIMF wind force (simplified)
    wind_force = 0.5 * 1.225 * tanker['wind_area'] * wind_speed**2 * 0.8 / 1000  # kN

    # Current force (simplified)
    current_force = 0.5 * 1025 * (tanker['loa'] * 20) * 1.0**2 * 0.5 / 1000  # kN

    # Breakout force (starting from rest)
    breakout_force = (wind_force + current_force) * 1.5

    # Swivel stack check
    swivel_sf = config.swivel_capacity / breakout_force
    swivel_pass = swivel_sf >= 2.0

    return {
        'tanker_size': tanker_size,
        'wind_speed': wind_speed,
        'wind_force': wind_force,
        'current_force': current_force,
        'breakout_force': breakout_force,
        'swivel_sf': swivel_sf,
        'status': 'PASS' if swivel_pass else 'FAIL'
    }


def main():
    """Main execution."""
    logger.info("SPM TERMINAL ANALYSIS")

    config = SPMConfig()

    # Analyze different tanker sizes and wind conditions
    results = []
    for tanker in ['VLCC', 'Suezmax', 'Aframax']:
        for wind in [15, 20, 25, 30]:
            result = analyze_spm_terminal(config, tanker, wind)
            results.append(result)

    output_dir = Path(__file__).parent / "outputs" / "09_spm"
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pd.DataFrame(results)
    df.to_excel(output_dir / "spm_analysis.xlsx", index=False)

    logger.info(f"Analysis complete. Outputs saved to: {output_dir}")


if __name__ == "__main__":
    main()
