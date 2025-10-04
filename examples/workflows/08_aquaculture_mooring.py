#!/usr/bin/env python3
"""
Aquaculture Mooring Grid Design

Description: Fish farm mooring grid system with current loading
Application: Offshore salmon farm mooring in exposed location
Standards: NS 9415, SINTEF recommendations, DNV-RP-C205
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
class FishFarmConfig:
    """Fish farm configuration."""
    num_cages: int = 12
    cage_circumference: float = 160.0  # [m]
    cage_depth: float = 25.0           # [m]
    grid_spacing: float = 50.0         # [m]
    water_depth: float = 80.0          # [m]


def analyze_aquaculture_mooring(farm: FishFarmConfig, current_speed: float) -> Dict:
    """Analyze aquaculture mooring grid."""

    # Current force on net (dominant loading)
    net_area = farm.cage_circumference * farm.cage_depth
    cd = 0.8  # Drag coefficient for netting
    current_force = 0.5 * 1025 * cd * net_area * current_speed**2 / 1000  # kN per cage

    # Total force on grid
    total_force = current_force * farm.num_cages * 0.6  # Sheltering effect

    # Anchor points (corner anchors for grid)
    num_anchors = 8
    max_anchor_load = total_force / num_anchors * 1.5  # Unequal distribution

    # Drag embedment anchor capacity
    anchor_capacity = 800.0  # kN (typical 10-tonne anchor)
    sf = anchor_capacity / max_anchor_load
    sf_pass = sf >= 1.5  # NS 9415 requirement

    return {
        'current_speed': current_speed,
        'force_per_cage': current_force,
        'total_force': total_force,
        'max_anchor_load': max_anchor_load,
        'safety_factor': sf,
        'status': 'PASS' if sf_pass else 'FAIL'
    }


def main():
    """Main execution."""
    logger.info("AQUACULTURE MOORING GRID ANALYSIS")

    farm = FishFarmConfig()

    # Analyze current speed range
    current_speeds = np.linspace(0.2, 1.5, 10)
    results = [analyze_aquaculture_mooring(farm, cs) for cs in current_speeds]

    output_dir = Path(__file__).parent / "outputs" / "08_aquaculture"
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pd.DataFrame(results)
    df.to_excel(output_dir / "aquaculture_analysis.xlsx", index=False)

    logger.info(f"Analysis complete. Outputs saved to: {output_dir}")


if __name__ == "__main__":
    main()
