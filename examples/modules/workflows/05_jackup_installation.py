#!/usr/bin/env python3
"""
Jack-Up Installation Mooring Analysis

Description: Temporary mooring for jack-up vessel during offshore installation
Application: Jack-up rig mooring prior to leg deployment
Standards: SNAME 5-5A, API RP 2A, ISO 19905-1
Author: Marine Engineering Team
Date: 2025-10-03
"""

import sys
from pathlib import Path
from dataclasses import dataclass
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import logging

sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class JackUpData:
    """Jack-up vessel parameters."""
    hull_length: float = 80.0          # [m]
    hull_width: float = 80.0           # [m]
    operating_draft: float = 8.0       # [m]
    displacement: float = 25000.0      # [tonnes]
    num_legs: int = 3
    water_depth: float = 120.0         # [m]


def analyze_jackup_mooring(vessel: JackUpData, wind_speed: float) -> Dict:
    """Analyze jack-up temporary mooring."""

    # Simplified environmental force
    wind_force = 0.5 * 1.225 * 800 * wind_speed**2 / 1000  # kN
    current_force = 300.0  # kN
    total_force = np.sqrt(wind_force**2 + current_force**2)

    # 8-point mooring (typical)
    num_lines = 8
    anchor_pattern_radius = 200.0  # m

    # Line tension (simplified)
    max_line_tension = total_force / (num_lines * 0.7)  # Not all lines equally loaded

    # Anchor capacity check
    anchor_capacity = 1500.0  # kN (typical drag embed anchor)
    anchor_sf = anchor_capacity / max_line_tension
    anchor_pass = anchor_sf >= 2.0

    return {
        'wind_speed': wind_speed,
        'total_force': total_force,
        'max_line_tension': max_line_tension,
        'anchor_sf': anchor_sf,
        'status': 'PASS' if anchor_pass else 'FAIL'
    }


def main():
    """Main execution."""
    logger.info("JACK-UP INSTALLATION MOORING ANALYSIS")

    vessel = JackUpData()

    # Analyze different wind conditions
    wind_speeds = np.linspace(10, 25, 8)
    results = [analyze_jackup_mooring(vessel, ws) for ws in wind_speeds]

    output_dir = Path(__file__).parent / "outputs" / "05_jackup"
    output_dir.mkdir(parents=True, exist_ok=True)

    # Save results
    df = pd.DataFrame(results)
    df.to_excel(output_dir / "jackup_analysis.xlsx", index=False)

    logger.info(f"Analysis complete. Outputs saved to: {output_dir}")


if __name__ == "__main__":
    main()
