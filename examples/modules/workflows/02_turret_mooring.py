#!/usr/bin/env python3
"""
Turret Mooring Analysis for Weathervaning FPSO

Description: Internal turret mooring system with 360° weathervaning capability
Application: Harsh environment FPSO with continuous heading optimization
Standards: API RP 2SK, OCIMF MEG4, DNV-OS-E301
Author: Marine Engineering Team
Date: 2025-10-03
"""

import sys
from pathlib import Path
from typing import Dict, List, Tuple
from dataclasses import dataclass
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import logging

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from marine_engineering.mooring_analysis.catenary_solver import (
    CatenarySolver,
    CatenaryInput
)

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class TurretConfiguration:
    """Turret mooring configuration."""
    num_lines: int = 9
    turret_radius: float = 5.0         # Turret radius [m]
    fairlead_depth: float = 25.0       # Below waterline [m]

    # Line segments (from seabed)
    ground_chain_length: float = 300.0  # [m]
    wire_rope_length: float = 1400.0    # [m]
    top_chain_length: float = 100.0     # [m]

    # Component properties
    chain_diameter: float = 152.0       # [mm] R4 studless
    wire_diameter: float = 152.0        # [mm] spiral strand
    chain_mbl: float = 33600.0         # [kN]
    wire_mbl: float = 26900.0          # [kN]


@dataclass
class EnvironmentalDirectional:
    """Directional environmental data."""
    water_depth: float = 1200.0

    # Environmental parameters by direction (N, NE, E, SE, S, SW, W, NW)
    wind_speeds: List[float] = None      # [m/s]
    current_speeds: List[float] = None   # [m/s]
    wave_heights: List[float] = None     # [m]
    wave_periods: List[float] = None     # [s]

    def __post_init__(self):
        if self.wind_speeds is None:
            # Example: prevailing westerly conditions
            self.wind_speeds = [35, 32, 28, 30, 33, 38, 42, 40]
        if self.current_speeds is None:
            self.current_speeds = [0.8, 0.9, 1.0, 1.1, 1.0, 0.9, 0.8, 0.7]
        if self.wave_heights is None:
            self.wave_heights = [6, 7, 8, 8.5, 8, 7.5, 9, 8.5]
        if self.wave_periods is None:
            self.wave_periods = [10, 10.5, 11, 11.5, 11, 10.5, 12, 11.5]


def analyze_turret_at_heading(
    heading: float,
    config: TurretConfiguration,
    env: EnvironmentalDirectional
) -> Dict:
    """
    Analyze turret mooring at specific heading.

    Parameters
    ----------
    heading : float
        Vessel heading [deg]
    config : TurretConfiguration
        Turret configuration
    env : EnvironmentalDirectional
        Environmental conditions

    Returns
    -------
    results : Dict
        Analysis results for this heading
    """
    # Line angles relative to turret (equally spaced)
    line_angles = np.linspace(0, 360, config.num_lines, endpoint=False)

    # Simplified environmental force
    env_direction_index = int((heading % 360) // 45)  # Nearest 45° sector
    env_force = 5000.0  # kN (simplified - should be from vessel analysis)

    # Estimate vessel offset (simplified)
    offset_x = 50.0 * np.cos(np.radians(heading))
    offset_y = 50.0 * np.sin(np.radians(heading))

    line_tensions = []

    for angle in line_angles:
        # Calculate line geometry
        fairlead_x = config.turret_radius * np.cos(np.radians(angle)) + offset_x
        fairlead_y = config.turret_radius * np.sin(np.radians(angle)) + offset_y

        horizontal_span = np.sqrt(fairlead_x**2 + fairlead_y**2)
        vertical_span = env.water_depth - config.fairlead_depth

        # Total line length
        total_length = (config.ground_chain_length +
                       config.wire_rope_length +
                       config.top_chain_length)

        # Average properties (simplified)
        chain_weight = 2500.0  # N/m for 152mm
        wire_weight = 1200.0   # N/m for 152mm
        chain_ea = 2.0e9       # N
        wire_ea = 1.8e9        # N

        L1 = config.ground_chain_length
        L2 = config.wire_rope_length
        L3 = config.top_chain_length

        avg_weight = ((L1 + L3) * chain_weight + L2 * wire_weight) / total_length
        avg_ea = ((L1 + L3) * chain_ea + L2 * wire_ea) / total_length

        try:
            catenary_input = CatenaryInput(
                length=total_length,
                horizontal_span=horizontal_span,
                vertical_span=vertical_span,
                weight_per_length=avg_weight,
                ea_stiffness=avg_ea,
                water_depth=env.water_depth
            )

            solver = CatenarySolver()
            result = solver.solve(catenary_input)

            line_tensions.append(result.total_tension_fairlead / 1000.0)  # kN

        except Exception as e:
            logger.warning(f"Failed at heading {heading}°, line {angle}°: {e}")
            line_tensions.append(np.nan)

    return {
        'heading': heading,
        'max_tension': np.nanmax(line_tensions) if line_tensions else np.nan,
        'mean_tension': np.nanmean(line_tensions) if line_tensions else np.nan,
        'line_tensions': line_tensions,
        'environmental_force': env_force
    }


def perform_360_analysis(
    config: TurretConfiguration,
    env: EnvironmentalDirectional,
    heading_step: float = 15.0
) -> pd.DataFrame:
    """
    Perform 360° heading analysis.

    Parameters
    ----------
    config : TurretConfiguration
        Turret configuration
    env : EnvironmentalDirectional
        Environmental data
    heading_step : float
        Heading increment [deg]

    Returns
    -------
    results_df : pd.DataFrame
        Results for all headings
    """
    logger.info("Performing 360° weathervaning analysis...")

    headings = np.arange(0, 360, heading_step)
    results = []

    for heading in headings:
        result = analyze_turret_at_heading(heading, config, env)
        results.append(result)
        logger.info(f"Heading {heading:5.0f}°: Max tension = {result['max_tension']:8.1f} kN")

    return pd.DataFrame(results)


def create_turret_plots(results_df: pd.DataFrame, config: TurretConfiguration, output_dir: Path):
    """Create comprehensive turret analysis plots."""

    fig = plt.figure(figsize=(16, 10))
    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

    # 1. Tension vs heading
    ax1 = fig.add_subplot(gs[0, :2])
    ax1.plot(results_df['heading'], results_df['max_tension'],
            'b-', linewidth=2, label='Maximum Tension')
    ax1.plot(results_df['heading'], results_df['mean_tension'],
            'g--', linewidth=2, label='Mean Tension')
    ax1.axhline(y=config.wire_mbl/1.67, color='r', linestyle='--',
               linewidth=2, label='Design Limit (SF=1.67)')
    ax1.set_xlabel('Vessel Heading [deg]', fontsize=12)
    ax1.set_ylabel('Tension [kN]', fontsize=12)
    ax1.set_title('Mooring Tension vs Vessel Heading', fontsize=14, fontweight='bold')
    ax1.legend(fontsize=10)
    ax1.grid(True, alpha=0.3)
    ax1.set_xlim(0, 360)

    # 2. Polar plot of max tension
    ax2 = fig.add_subplot(gs[0, 2], projection='polar')
    theta = np.radians(results_df['heading'].values)
    r = results_df['max_tension'].values

    ax2.plot(theta, r, 'b-', linewidth=2)
    ax2.fill(theta, r, alpha=0.3)
    ax2.set_title('Tension Polar Plot', fontsize=12, fontweight='bold', pad=20)
    ax2.set_theta_zero_location('N')
    ax2.set_theta_direction(-1)
    ax2.grid(True)

    # 3. Watch circle diagram
    ax3 = fig.add_subplot(gs[1, :2])

    # Simplified watch circle (actual would come from time-domain simulation)
    watch_circle_radius = 0.08 * env.water_depth  # 8% of water depth
    circle = plt.Circle((0, 0), watch_circle_radius, fill=False,
                       edgecolor='red', linewidth=2, linestyle='--',
                       label=f'Watch Circle ({watch_circle_radius:.0f}m)')

    ax3.add_patch(circle)

    # Plot vessel positions at different headings
    for idx, row in results_df.iterrows():
        if idx % 4 == 0:  # Plot every 4th position
            x = 30 * np.cos(np.radians(row['heading']))
            y = 30 * np.sin(np.radians(row['heading']))
            ax3.plot(x, y, 'bo', markersize=8)
            if idx % 8 == 0:
                ax3.text(x*1.2, y*1.2, f"{row['heading']:.0f}°",
                        ha='center', fontsize=8)

    ax3.plot(0, 0, 'r^', markersize=15, label='Turret Position')
    ax3.set_xlabel('East [m]', fontsize=12)
    ax3.set_ylabel('North [m]', fontsize=12)
    ax3.set_title('Watch Circle and Vessel Excursion', fontsize=14, fontweight='bold')
    ax3.legend(fontsize=10)
    ax3.grid(True, alpha=0.3)
    ax3.axis('equal')

    # 4. Statistics summary
    ax4 = fig.add_subplot(gs[1, 2])
    ax4.axis('off')

    max_overall = results_df['max_tension'].max()
    min_overall = results_df['max_tension'].min()
    mean_overall = results_df['max_tension'].mean()
    std_overall = results_df['max_tension'].std()

    optimal_heading = results_df.loc[results_df['max_tension'].idxmin(), 'heading']

    summary_text = f"""
TURRET MOORING STATISTICS

Maximum Tension: {max_overall:.1f} kN
Minimum Tension: {min_overall:.1f} kN
Mean Tension: {mean_overall:.1f} kN
Std Deviation: {std_overall:.1f} kN

Optimal Heading: {optimal_heading:.0f}°
(Minimum tension)

MBL (Wire): {config.wire_mbl:.0f} kN
Design Limit (SF=1.67): {config.wire_mbl/1.67:.0f} kN

Number of Lines: {config.num_lines}
Line Length: {config.ground_chain_length + config.wire_rope_length + config.top_chain_length:.0f}m
    """

    ax4.text(0.1, 0.5, summary_text, fontsize=10, family='monospace',
            verticalalignment='center',
            bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.5))

    fig.suptitle('Turret Mooring - 360° Weathervaning Analysis',
                fontsize=16, fontweight='bold', y=0.98)

    # Save
    output_dir.mkdir(parents=True, exist_ok=True)
    output_file = output_dir / "turret_mooring_analysis.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    logger.info(f"Plot saved: {output_file}")
    plt.close()


def main():
    """Main execution."""
    logger.info("=" * 80)
    logger.info("TURRET MOORING WEATHERVANING ANALYSIS")
    logger.info("=" * 80)

    # Configuration
    config = TurretConfiguration()
    env = EnvironmentalDirectional()

    # Perform 360° analysis
    results_df = perform_360_analysis(config, env, heading_step=15.0)

    # Output directory
    output_dir = Path(__file__).parent / "outputs" / "02_turret_mooring"

    # Generate plots
    create_turret_plots(results_df, config, output_dir)

    # Save Excel
    excel_file = output_dir / "turret_analysis_results.xlsx"
    results_df.to_excel(excel_file, index=False)
    logger.info(f"Excel report saved: {excel_file}")

    # Summary
    logger.info("\n" + "=" * 80)
    logger.info("ANALYSIS COMPLETE")
    logger.info("=" * 80)
    logger.info(f"Optimal heading: {results_df.loc[results_df['max_tension'].idxmin(), 'heading']:.0f}°")
    logger.info(f"Minimum tension: {results_df['max_tension'].min():.1f} kN")
    logger.info(f"Maximum tension: {results_df['max_tension'].max():.1f} kN")
    logger.info(f"\nOutputs saved to: {output_dir}")
    logger.info("=" * 80)


if __name__ == "__main__":
    main()
