#!/usr/bin/env python3
"""
CALM Buoy Design and Analysis

Description: Catenary Anchor Leg Mooring buoy for tanker offloading operations
Application: Offshore oil terminal with rotating buoy and swivel stack
Standards: OCIMF MEG4, API RP 2SK, ISO 19901-7
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

from digitalmodel.marine_ops.marine_engineering.mooring_analysis.catenary_solver import (
    CatenarySolver, CatenaryInput
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class CALMBuoyConfig:
    """CALM buoy configuration."""
    buoy_diameter: float = 15.0        # [m]
    buoy_draft: float = 8.0            # [m]
    num_anchor_legs: int = 6
    chain_diameter: float = 127.0      # [mm]
    chain_length: float = 1000.0       # [m]
    water_depth: float = 80.0          # [m]
    hawser_length: float = 150.0       # [m]
    hawser_mbl: float = 8000.0         # [kN]


@dataclass
class TankerData:
    """Tanker particulars."""
    vessel_type: str = "VLCC"
    loa: float = 330.0                 # [m]
    beam: float = 60.0                 # [m]
    draft: float = 22.0                # [m]
    displacement: float = 320000.0     # [tonnes]


def analyze_calm_buoy(config: CALMBuoyConfig, wind_speed: float, current_speed: float):
    """Analyze CALM buoy system."""

    logger.info(f"Analyzing CALM buoy - Wind: {wind_speed} m/s, Current: {current_speed} m/s")

    # Environmental forces (simplified)
    wind_force = 0.5 * 1.225 * 2000 * wind_speed**2 / 1000  # kN
    current_force = 0.5 * 1025 * 5000 * current_speed**2 / 1000  # kN
    total_force = np.sqrt(wind_force**2 + current_force**2)

    # Hawser tension (simplified equilibrium)
    hawser_tension = total_force * 1.2  # Include dynamic amplification

    # Buoy offset
    buoy_offset = 50.0  # m (simplified)

    # Anchor leg analysis
    leg_angles = np.linspace(0, 360, config.num_anchor_legs, endpoint=False)
    max_leg_tension = 0.0

    for angle in leg_angles:
        horizontal_span = np.sqrt((config.buoy_diameter/2)**2 + buoy_offset**2)
        vertical_span = config.water_depth - config.buoy_draft

        try:
            catenary_input = CatenaryInput(
                length=config.chain_length,
                horizontal_span=horizontal_span,
                vertical_span=vertical_span,
                weight_per_length=2000.0,  # N/m
                ea_stiffness=1.5e9,
                water_depth=config.water_depth
            )

            solver = CatenarySolver()
            result = solver.solve(catenary_input)
            tension = result.total_tension_fairlead / 1000.0  # kN

            max_leg_tension = max(max_leg_tension, tension)

        except Exception as e:
            logger.warning(f"Leg analysis failed: {e}")

    # Safety factors
    hawser_sf = config.hawser_mbl / hawser_tension
    hawser_pass = hawser_sf >= 2.0

    results = {
        'wind_speed': wind_speed,
        'current_speed': current_speed,
        'total_force': total_force,
        'hawser_tension': hawser_tension,
        'hawser_sf': hawser_sf,
        'hawser_pass': hawser_pass,
        'max_leg_tension': max_leg_tension,
        'buoy_offset': buoy_offset
    }

    logger.info(f"Hawser tension: {hawser_tension:.1f} kN (SF: {hawser_sf:.2f})")

    return results


def create_calm_plots(results_list: List[Dict], output_dir: Path):
    """Create CALM buoy analysis plots."""

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle('CALM Buoy Analysis', fontsize=16, fontweight='bold')

    df = pd.DataFrame(results_list)

    # Hawser tension vs wind speed
    ax = axes[0, 0]
    for current in df['current_speed'].unique():
        subset = df[df['current_speed'] == current]
        ax.plot(subset['wind_speed'], subset['hawser_tension'],
               'o-', label=f'Current {current} m/s', linewidth=2)
    ax.set_xlabel('Wind Speed [m/s]', fontsize=12)
    ax.set_ylabel('Hawser Tension [kN]', fontsize=12)
    ax.set_title('Hawser Tension vs Wind Speed', fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3)

    # Safety factor plot
    ax = axes[0, 1]
    for current in df['current_speed'].unique():
        subset = df[df['current_speed'] == current]
        ax.plot(subset['wind_speed'], subset['hawser_sf'],
               'o-', label=f'Current {current} m/s', linewidth=2)
    ax.axhline(y=2.0, color='r', linestyle='--', linewidth=2, label='Min SF = 2.0')
    ax.set_xlabel('Wind Speed [m/s]', fontsize=12)
    ax.set_ylabel('Safety Factor', fontsize=12)
    ax.set_title('Hawser Safety Factor', fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3)

    # Watch circle
    ax = axes[1, 0]
    circle = plt.Circle((0, 0), 150, fill=False, edgecolor='red',
                       linewidth=2, linestyle='--', label='Watch Circle')
    ax.add_patch(circle)
    ax.plot(0, 0, 'rs', markersize=15, label='Buoy')
    ax.arrow(0, 0, 100, 50, head_width=10, head_length=10, fc='blue', ec='blue')
    ax.set_xlabel('East [m]', fontsize=12)
    ax.set_ylabel('North [m]', fontsize=12)
    ax.set_title('Watch Circle Diagram', fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.axis('equal')
    ax.set_xlim(-200, 200)
    ax.set_ylim(-200, 200)

    # Summary table
    ax = axes[1, 1]
    ax.axis('off')
    summary = f"""
CALM BUOY SUMMARY

Buoy Diameter: 15.0 m
Water Depth: 80.0 m
Number of Legs: 6

Hawser MBL: 8000 kN
Min SF Required: 2.0

Operating Envelope:
- Max Wind: {df['wind_speed'].max():.0f} m/s
- Max Current: {df['current_speed'].max():.1f} m/s
- Max Tension: {df['hawser_tension'].max():.0f} kN

All Cases Pass: {'Yes' if df['hawser_pass'].all() else 'No'}
    """
    ax.text(0.1, 0.5, summary, fontsize=11, family='monospace',
           verticalalignment='center',
           bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

    plt.tight_layout()
    output_dir.mkdir(parents=True, exist_ok=True)
    output_file = output_dir / "calm_buoy_analysis.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    logger.info(f"Plot saved: {output_file}")
    plt.close()


def main():
    """Main execution."""
    logger.info("=" * 80)
    logger.info("CALM BUOY DESIGN ANALYSIS")
    logger.info("=" * 80)

    config = CALMBuoyConfig()
    tanker = TankerData()

    # Analyze different environmental conditions
    wind_speeds = [10, 15, 20, 25, 30]
    current_speeds = [0.5, 1.0, 1.5]

    results = []
    for wind in wind_speeds:
        for current in current_speeds:
            result = analyze_calm_buoy(config, wind, current)
            results.append(result)

    # Output
    output_dir = Path(__file__).parent / "outputs" / "03_calm_buoy"

    # Plots
    create_calm_plots(results, output_dir)

    # Excel
    df = pd.DataFrame(results)
    excel_file = output_dir / "calm_analysis.xlsx"
    df.to_excel(excel_file, index=False)
    logger.info(f"Excel saved: {excel_file}")

    logger.info("\n" + "=" * 80)
    logger.info("ANALYSIS COMPLETE")
    logger.info(f"Outputs saved to: {output_dir}")
    logger.info("=" * 80)


if __name__ == "__main__":
    main()
