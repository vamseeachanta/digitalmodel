#!/usr/bin/env python3
"""
Wave Energy Converter Mooring Analysis

Description: Point absorber WEC mooring design and fatigue analysis
Application: Ocean wave energy extraction device
Standards: DNV-RP-C205, EMEC guidelines, IEC TS 62600-10
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
class WECDevice:
    """Wave energy converter parameters."""
    device_type: str = "point_absorber"
    diameter: float = 10.0             # [m]
    draft: float = 5.0                 # [m]
    displacement: float = 500.0        # [tonnes]
    water_depth: float = 50.0          # [m]
    num_mooring_lines: int = 4


def analyze_wec_mooring(wec: WECDevice, wave_height: float) -> Dict:
    """Analyze WEC mooring loads."""

    # Wave force (simplified - Morrison equation)
    wave_force = 1025 * 9.81 * (np.pi * wec.diameter**2 / 4) * wave_height * 0.5 / 1000  # kN

    # Current force
    current_force = 100.0  # kN

    total_force = np.sqrt(wave_force**2 + current_force**2)

    # Mooring line tension
    max_line_tension = total_force / wec.num_mooring_lines * 1.3

    # Synthetic rope MBL
    rope_mbl = 2000.0  # kN (polyester rope)
    sf = rope_mbl / max_line_tension
    sf_pass = sf >= 2.5  # Higher SF for dynamic loading

    # Fatigue cycles (simplified)
    cycles_per_year = 3e7  # ~1 cycle per second
    fatigue_damage = max_line_tension / rope_mbl * cycles_per_year / 1e9

    return {
        'wave_height': wave_height,
        'wave_force': wave_force,
        'max_tension': max_line_tension,
        'safety_factor': sf,
        'fatigue_damage': fatigue_damage,
        'status': 'PASS' if sf_pass else 'FAIL'
    }


def main():
    """Main execution."""
    logger.info("WAVE ENERGY CONVERTER MOORING ANALYSIS")

    wec = WECDevice()

    # Analyze wave height range
    wave_heights = np.linspace(1, 10, 10)
    results = [analyze_wec_mooring(wec, hs) for hs in wave_heights]

    output_dir = Path(__file__).parent / "outputs" / "07_wec"
    output_dir.mkdir(parents=True, exist_ok=True)

    df = pd.DataFrame(results)
    df.to_excel(output_dir / "wec_analysis.xlsx", index=False)

    logger.info(f"Analysis complete. Outputs saved to: {output_dir}")


if __name__ == "__main__":
    main()
