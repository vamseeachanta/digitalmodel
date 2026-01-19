#!/usr/bin/env python3
"""
Subsea Pipeline Installation Analysis

Description: Pipeline catenary analysis during J-lay or S-lay installation
Application: Deepwater pipeline installation from lay vessel
Standards: DNV-ST-F101, API RP 1111, DNV-RP-F105
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

from marine_engineering.catenary.solver import CatenarySolver
from marine_engineering.catenary.utils import calculate_catenary_profile

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


@dataclass
class PipelineData:
    """Pipeline properties."""
    outer_diameter: float = 0.508          # [m] 20 inch
    wall_thickness: float = 0.0254         # [m] 1 inch
    steel_grade: str = "X65"
    coating_thickness: float = 0.003        # [m]
    weight_in_air: float = 1200.0          # [N/m]
    weight_in_water: float = 850.0         # [N/m]
    axial_stiffness: float = 1.5e10        # EA [N]


@dataclass
class InstallationVessel:
    """Installation vessel parameters."""
    vessel_type: str = "S-lay"             # "S-lay" or "J-lay"
    stinger_length: float = 140.0          # [m]
    stinger_angle: float = 6.0             # [deg]
    tensioner_capacity: float = 500.0      # [tonnes]
    water_depth: float = 2000.0            # [m]


def analyze_installation_catenary(
    pipe: PipelineData,
    vessel: InstallationVessel,
    top_tension: float,
    vessel_offset: float = 0.0
) -> Dict:
    """
    Analyze pipeline installation catenary.

    Parameters
    ----------
    pipe : PipelineData
        Pipeline properties
    vessel : InstallationVessel
        Vessel and stinger data
    top_tension : float
        Tension at top of catenary [kN]
    vessel_offset : float
        Horizontal vessel offset from vertical [m]

    Returns
    -------
    results : Dict
        Catenary analysis results
    """
    logger.info(f"Analyzing installation - Top tension: {top_tension} kN")

    # Simplified catenary calculation
    # Horizontal distance from stinger tip to touchdown
    horizontal_span = vessel.water_depth * 0.7 + vessel_offset

    # Vertical span (water depth minus stinger depth)
    vertical_span = vessel.water_depth - 30.0  # Stinger at ~30m depth

    # Catenary parameter
    H = top_tension * 1000  # Convert to N
    w = pipe.weight_in_water
    a = H / w  # Catenary parameter

    # Touchdown point
    s_total = horizontal_span + (w * horizontal_span**2) / (2 * H)
    touchdown_angle = np.arctan(w * horizontal_span / H)

    # Maximum curvature (at sagbend)
    max_curvature = w / H
    min_radius = 1.0 / max_curvature

    # Stress calculation (simplified)
    outer_radius = pipe.outer_diameter / 2.0
    bending_stress = (outer_radius / min_radius) * 210e9  # Young's modulus
    bending_stress_mpa = bending_stress / 1e6

    # Allowable stress (API 1111)
    allowable_stress = 0.90 * 448.0  # 90% SMYS for X65

    # Check
    stress_utilization = bending_stress_mpa / allowable_stress
    stress_pass = stress_utilization <= 1.0

    results = {
        'top_tension': top_tension,
        'horizontal_span': horizontal_span,
        'touchdown_angle_deg': np.degrees(touchdown_angle),
        'min_bend_radius': min_radius,
        'bending_stress_mpa': bending_stress_mpa,
        'allowable_stress_mpa': allowable_stress,
        'stress_utilization': stress_utilization,
        'status': 'PASS' if stress_pass else 'FAIL'
    }

    logger.info(f"Bending stress: {bending_stress_mpa:.1f} MPa (Util: {stress_utilization:.2%})")

    return results


def create_installation_plots(results_list: List[Dict], output_dir: Path):
    """Create pipeline installation analysis plots."""

    fig, axes = plt.subplots(2, 2, figsize=(14, 10))
    fig.suptitle('Pipeline Installation Analysis - S-Lay', fontsize=16, fontweight='bold')

    df = pd.DataFrame(results_list)

    # Tension vs stress
    ax = axes[0, 0]
    ax.plot(df['top_tension'], df['bending_stress_mpa'], 'bo-', linewidth=2, markersize=8)
    ax.axhline(y=df['allowable_stress_mpa'].iloc[0], color='r', linestyle='--',
              linewidth=2, label='Allowable Stress')
    ax.set_xlabel('Top Tension [kN]', fontsize=12)
    ax.set_ylabel('Bending Stress [MPa]', fontsize=12)
    ax.set_title('Bending Stress vs Top Tension', fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3)

    # Stress utilization
    ax = axes[0, 1]
    colors = ['green' if s == 'PASS' else 'red' for s in df['status']]
    ax.bar(df['top_tension'], df['stress_utilization'], color=colors, alpha=0.7)
    ax.axhline(y=1.0, color='r', linestyle='--', linewidth=2, label='Unity')
    ax.set_xlabel('Top Tension [kN]', fontsize=12)
    ax.set_ylabel('Stress Utilization', fontsize=12)
    ax.set_title('Stress Utilization Ratio', fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3, axis='y')

    # Catenary profile sketch
    ax = axes[1, 0]
    # Simplified profile
    x = np.linspace(0, 1500, 100)
    y = -2000 + 1500 * np.exp(-x/500)  # Simplified exponential
    ax.plot(x, y, 'b-', linewidth=2, label='Pipeline')
    ax.plot(0, -30, 'rs', markersize=15, label='Stinger Tip')
    ax.plot(x[-1], y[-1], 'go', markersize=10, label='Touchdown')
    ax.axhline(y=-2000, color='brown', linestyle='-', linewidth=3, label='Seabed')
    ax.set_xlabel('Horizontal Distance [m]', fontsize=12)
    ax.set_ylabel('Depth [m]', fontsize=12)
    ax.set_title('Installation Catenary Profile', fontweight='bold')
    ax.legend()
    ax.grid(True, alpha=0.3)
    ax.set_ylim(-2100, 0)

    # Summary table
    ax = axes[1, 1]
    ax.axis('off')
    summary = f"""
PIPELINE INSTALLATION SUMMARY

Pipeline OD: 508 mm (20")
Wall Thickness: 25.4 mm (1")
Steel Grade: X65
Water Depth: 2000 m

Installation Method: S-Lay
Stinger Length: 140 m
Tensioner Capacity: 500 tonnes

Operating Envelope:
- Min Tension: {df['top_tension'].min():.0f} kN
- Max Tension: {df['top_tension'].max():.0f} kN
- Max Stress: {df['bending_stress_mpa'].max():.0f} MPa

All Cases: {df['status'].value_counts().get('PASS', 0)}/{len(df)} PASS
    """
    ax.text(0.1, 0.5, summary, fontsize=10, family='monospace',
           verticalalignment='center',
           bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.5))

    plt.tight_layout()
    output_dir.mkdir(parents=True, exist_ok=True)
    output_file = output_dir / "pipeline_installation.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    logger.info(f"Plot saved: {output_file}")
    plt.close()


def main():
    """Main execution."""
    logger.info("=" * 80)
    logger.info("PIPELINE INSTALLATION ANALYSIS")
    logger.info("=" * 80)

    pipe = PipelineData()
    vessel = InstallationVessel()

    # Analyze different top tensions
    tensions = np.linspace(200, 450, 10)  # kN

    results = []
    for tension in tensions:
        result = analyze_installation_catenary(pipe, vessel, tension)
        results.append(result)

    # Output
    output_dir = Path(__file__).parent / "outputs" / "04_pipeline_installation"
    create_installation_plots(results, output_dir)

    # Excel
    df = pd.DataFrame(results)
    excel_file = output_dir / "pipeline_analysis.xlsx"
    df.to_excel(excel_file, index=False)
    logger.info(f"Excel saved: {excel_file}")

    logger.info("\n" + "=" * 80)
    logger.info("ANALYSIS COMPLETE")
    logger.info(f"Outputs saved to: {output_dir}")
    logger.info("=" * 80)


if __name__ == "__main__":
    main()
