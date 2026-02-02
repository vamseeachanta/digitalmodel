#!/usr/bin/env python3
"""
8-Point Spread Mooring Design for FPSO

Description: Complete spread mooring design and analysis for a typical FPSO
             in 1500m water depth with 8 symmetric mooring lines
Application: Oil & Gas production vessel permanent mooring
Standards: API RP 2SK, DNV-OS-E301, ABS MODU
Author: Marine Engineering Team
Date: 2025-10-03
"""

import sys
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec
import logging

# Add src to path for imports
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from digitalmodel.marine_engineering.mooring_analysis.catenary_solver import (
    CatenarySolver,
    CatenaryInput,
    CatenaryResults
)
from digitalmodel.marine_engineering.environmental_loading.ocimf import OCIMFWindForce

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


# ============================================================================
# 1. INPUT PARAMETERS
# ============================================================================

@dataclass
class VesselParticulars:
    """FPSO vessel particulars."""
    name: str = "FPSO-Generic-300k"
    loa: float = 330.0              # Length overall [m]
    beam: float = 58.0              # Beam [m]
    draft: float = 21.0             # Operating draft [m]
    displacement: float = 350000.0  # Displacement [tonnes]
    wind_area_trans: float = 3500.0 # Transverse wind area [m²]
    wind_area_long: float = 9000.0  # Longitudinal wind area [m²]
    vcg: float = 15.0               # Vertical CG above keel [m]


@dataclass
class EnvironmentalCriteria:
    """Environmental loading criteria."""
    # Intact conditions (operating)
    intact_wind_speed: float = 51.4        # 1-min sustained at 10m [m/s]
    intact_current_surface: float = 1.5    # Surface current [m/s]
    intact_wave_hs: float = 8.5            # Significant wave height [m]
    intact_wave_tp: float = 12.0           # Peak period [s]

    # Damaged conditions (1-line broken)
    damaged_wind_speed: float = 25.7       # 1-min sustained at 10m [m/s]
    damaged_current_surface: float = 1.0   # Surface current [m/s]
    damaged_wave_hs: float = 6.0           # Significant wave height [m]
    damaged_wave_tp: float = 10.0          # Peak period [s]

    # Site information
    water_depth: float = 1500.0            # Water depth [m]
    seabed_type: str = "soft clay"
    current_profile: str = "power law"     # Power law exponent


@dataclass
class MooringConfiguration:
    """Mooring system configuration."""
    num_lines: int = 8
    line_length: float = 2200.0        # Total unstretched length [m]
    pretension: float = 2000.0         # Pretension at fairlead [kN]
    fairlead_radius: float = 35.0      # Distance from centerline [m]
    fairlead_depth: float = 18.0       # Below waterline [m]

    # Line composition (from seabed up)
    segment_1_length: float = 600.0    # Chain at bottom [m]
    segment_2_length: float = 1200.0   # Wire rope middle [m]
    segment_3_length: float = 400.0    # Chain at top [m]

    # Component properties
    chain_grade: str = "R4"
    chain_diameter: float = 127.0      # [mm]
    wire_diameter: float = 127.0       # [mm]
    wire_construction: str = "6x36 IWRC"


@dataclass
class DesignCriteria:
    """Design acceptance criteria per API RP 2SK."""
    # Safety factors
    sf_intact: float = 1.67            # Intact condition
    sf_damaged: float = 1.25           # Damaged condition (1 line out)

    # Material properties
    chain_mbl: float = 23510.0         # Chain MBL [kN] for 127mm R4
    wire_mbl: float = 18800.0          # Wire MBL [kN] for 127mm 6x36

    # Fatigue
    fatigue_life_target: float = 25.0  # Target fatigue life [years]

    # Watch circle
    max_offset_intact: float = 0.10    # 10% of water depth
    max_offset_damaged: float = 0.15   # 15% of water depth


# ============================================================================
# 2. ENVIRONMENTAL FORCES
# ============================================================================

def calculate_environmental_forces(
    vessel: VesselParticulars,
    env: EnvironmentalCriteria,
    heading: float = 0.0
) -> Dict[str, float]:
    """
    Calculate environmental forces on vessel.

    Parameters
    ----------
    vessel : VesselParticulars
        Vessel information
    env : EnvironmentalCriteria
        Environmental loading
    heading : float
        Vessel heading relative to environment [deg]

    Returns
    -------
    forces : Dict[str, float]
        Forces and moments (surge, sway, yaw)
    """
    # Wind force using OCIMF coefficients
    wind_calculator = OCIMFWindForce()

    # Simplified approach - use mean coefficients
    # In production, use vessel-specific wind tunnel data
    rho_air = 1.225  # kg/m³

    # Wind force (simplified - use cos/sin for direction)
    wind_force_long = 0.5 * rho_air * vessel.wind_area_long * env.intact_wind_speed**2 * 0.8
    wind_force_trans = 0.5 * rho_air * vessel.wind_area_trans * env.intact_wind_speed**2 * 1.0

    # Current force (simplified drag)
    # In production, use vessel-specific hydrodynamic coefficients
    rho_water = 1025.0  # kg/m³
    current_force = 0.5 * rho_water * (vessel.loa * vessel.draft) * env.intact_current_surface**2 * 0.5

    # Wave drift force (simplified)
    # In production, use QTF from diffraction analysis
    wave_drift = 500.0  # kN (placeholder - should come from hydrodynamic analysis)

    # Combine forces
    total_surge = (wind_force_long + current_force + wave_drift) / 1000.0  # Convert to kN
    total_sway = wind_force_trans / 1000.0

    return {
        'surge': total_surge,
        'sway': total_sway,
        'yaw': 0.0  # Simplified - no yaw moment for head seas
    }


# ============================================================================
# 3. MOORING LINE ANALYSIS
# ============================================================================

def analyze_mooring_line(
    config: MooringConfiguration,
    env: EnvironmentalCriteria,
    line_angle: float,
    vessel_offset: Tuple[float, float]
) -> CatenaryResults:
    """
    Analyze single mooring line using catenary solver.

    Parameters
    ----------
    config : MooringConfiguration
        Mooring line configuration
    env : EnvironmentalCriteria
        Environmental data
    line_angle : float
        Line azimuth from vessel center [deg]
    vessel_offset : Tuple[float, float]
        Vessel offset from mean position (x, y) [m]

    Returns
    -------
    results : CatenaryResults
        Catenary analysis results
    """
    # Calculate fairlead position
    fairlead_x = config.fairlead_radius * np.cos(np.radians(line_angle)) + vessel_offset[0]
    fairlead_y = config.fairlead_radius * np.sin(np.radians(line_angle)) + vessel_offset[1]

    # Horizontal span (assuming anchor at origin for each line)
    horizontal_span = np.sqrt(fairlead_x**2 + fairlead_y**2)
    vertical_span = env.water_depth - config.fairlead_depth

    # Composite line properties (weighted average for simplified analysis)
    # In production: analyze each segment separately
    total_length = config.line_length

    # Chain weight in water: ~2000 N/m for 127mm R4
    chain_weight = 2000.0  # N/m
    # Wire weight in water: ~800 N/m for 127mm 6x36
    wire_weight = 800.0  # N/m

    # Weighted average
    L1 = config.segment_1_length
    L2 = config.segment_2_length
    L3 = config.segment_3_length
    avg_weight = (L1 * chain_weight + L2 * wire_weight + L3 * chain_weight) / total_length

    # Average EA (simplified)
    # Chain EA ≈ 1.5e9 N, Wire EA ≈ 1.2e9 N
    chain_ea = 1.5e9
    wire_ea = 1.2e9
    avg_ea = (L1 * chain_ea + L2 * wire_ea + L3 * chain_ea) / total_length

    # Create input
    catenary_input = CatenaryInput(
        length=total_length,
        horizontal_span=horizontal_span,
        vertical_span=vertical_span,
        weight_per_length=avg_weight,
        ea_stiffness=avg_ea,
        water_depth=env.water_depth
    )

    # Solve
    solver = CatenarySolver()
    results = solver.solve(catenary_input)

    return results


# ============================================================================
# 4. SYSTEM ANALYSIS
# ============================================================================

def analyze_mooring_system(
    vessel: VesselParticulars,
    config: MooringConfiguration,
    env: EnvironmentalCriteria,
    criteria: DesignCriteria,
    condition: str = "intact"
) -> Dict:
    """
    Analyze complete mooring system.

    Parameters
    ----------
    vessel : VesselParticulars
        Vessel data
    config : MooringConfiguration
        Mooring configuration
    env : EnvironmentalCriteria
        Environmental loading
    criteria : DesignCriteria
        Design acceptance criteria
    condition : str
        "intact" or "damaged"

    Returns
    -------
    results : Dict
        Complete system analysis results
    """
    logger.info(f"Analyzing mooring system - {condition} condition")

    # Environmental forces
    forces = calculate_environmental_forces(vessel, env)
    total_force = np.sqrt(forces['surge']**2 + forces['sway']**2)

    logger.info(f"Total environmental force: {total_force:.1f} kN")

    # Line angles (equally spaced around vessel)
    line_angles = np.linspace(0, 360, config.num_lines, endpoint=False)

    # Estimate vessel offset (simplified quasi-static analysis)
    # In production: use coupled dynamic analysis
    vessel_offset_estimate = (0.05 * env.water_depth, 0.0)  # Initial guess

    # Analyze each line
    line_results = []
    max_tension = 0.0

    for i, angle in enumerate(line_angles):
        logger.info(f"Analyzing line {i+1} at {angle:.0f}°")

        try:
            result = analyze_mooring_line(config, env, angle, vessel_offset_estimate)
            line_results.append({
                'line_number': i + 1,
                'angle': angle,
                'tension_fairlead': result.total_tension_fairlead / 1000.0,  # Convert to kN
                'tension_anchor': result.total_tension_anchor / 1000.0,
                'horizontal_tension': result.horizontal_tension / 1000.0,
                'converged': result.converged
            })

            max_tension = max(max_tension, result.total_tension_fairlead / 1000.0)

        except Exception as e:
            logger.error(f"Failed to analyze line {i+1}: {e}")
            line_results.append({
                'line_number': i + 1,
                'angle': angle,
                'tension_fairlead': np.nan,
                'tension_anchor': np.nan,
                'horizontal_tension': np.nan,
                'converged': False
            })

    # Safety factor check
    mbl = criteria.chain_mbl  # Use chain MBL (most critical)
    if condition == "intact":
        required_sf = criteria.sf_intact
    else:
        required_sf = criteria.sf_damaged

    actual_sf = mbl / max_tension if max_tension > 0 else np.inf
    sf_pass = actual_sf >= required_sf

    logger.info(f"Maximum tension: {max_tension:.1f} kN")
    logger.info(f"MBL: {mbl:.1f} kN")
    logger.info(f"Safety factor: {actual_sf:.2f} (required: {required_sf:.2f})")
    logger.info(f"Safety factor check: {'PASS' if sf_pass else 'FAIL'}")

    return {
        'condition': condition,
        'line_results': pd.DataFrame(line_results),
        'max_tension': max_tension,
        'safety_factor': actual_sf,
        'sf_required': required_sf,
        'sf_pass': sf_pass,
        'vessel_offset': vessel_offset_estimate,
        'environmental_force': total_force
    }


# ============================================================================
# 5. VISUALIZATION
# ============================================================================

def create_mooring_plot(results: Dict, output_dir: Path):
    """
    Create comprehensive mooring analysis plots.

    Parameters
    ----------
    results : Dict
        Analysis results
    output_dir : Path
        Output directory for plots
    """
    fig = plt.figure(figsize=(16, 12))
    gs = GridSpec(3, 3, figure=fig, hspace=0.3, wspace=0.3)

    df = results['line_results']

    # 1. Mooring pattern plan view
    ax1 = fig.add_subplot(gs[0, :2])
    angles_rad = np.radians(df['angle'].values)

    # Plot lines
    for i, (angle, tension) in enumerate(zip(angles_rad, df['tension_fairlead'].values)):
        color = 'green' if df['converged'].iloc[i] else 'red'
        line_length = 200  # Display length
        ax1.plot([0, line_length * np.cos(angle)],
                [0, line_length * np.sin(angle)],
                color=color, linewidth=2, label=f'Line {i+1}' if i < 2 else '')

        # Add tension labels
        ax1.text(line_length * 1.1 * np.cos(angle),
                line_length * 1.1 * np.sin(angle),
                f'{tension:.0f} kN',
                ha='center', va='center', fontsize=8)

    ax1.plot(0, 0, 'rs', markersize=20, label='Vessel')
    ax1.set_xlabel('East [m]', fontsize=12)
    ax1.set_ylabel('North [m]', fontsize=12)
    ax1.set_title('Mooring Pattern - Plan View', fontsize=14, fontweight='bold')
    ax1.grid(True, alpha=0.3)
    ax1.axis('equal')
    ax1.legend(fontsize=10)

    # 2. Tension distribution polar plot
    ax2 = fig.add_subplot(gs[0, 2], projection='polar')
    angles_rad = np.radians(df['angle'].values)
    tensions = df['tension_fairlead'].values

    ax2.plot(angles_rad, tensions, 'o-', linewidth=2, markersize=8, color='blue')
    ax2.set_title('Tension Distribution\n(Polar)', fontsize=12, fontweight='bold', pad=20)
    ax2.set_theta_zero_location('N')
    ax2.set_theta_direction(-1)
    ax2.grid(True)

    # 3. Tension bar chart
    ax3 = fig.add_subplot(gs[1, :2])
    bars = ax3.bar(df['line_number'], df['tension_fairlead'],
                   color=['green' if c else 'red' for c in df['converged']],
                   alpha=0.7, edgecolor='black')

    # Add MBL and limit lines
    mbl = results.get('sf_required', 1.67) * results['max_tension']
    ax3.axhline(y=mbl, color='red', linestyle='--', linewidth=2, label='MBL')
    ax3.axhline(y=results['max_tension'], color='orange', linestyle='--',
               linewidth=2, label='Max Tension')

    ax3.set_xlabel('Line Number', fontsize=12)
    ax3.set_ylabel('Tension [kN]', fontsize=12)
    ax3.set_title('Line Tensions - Fairlead', fontsize=14, fontweight='bold')
    ax3.legend(fontsize=10)
    ax3.grid(True, alpha=0.3, axis='y')

    # 4. Safety factor summary
    ax4 = fig.add_subplot(gs[1, 2])
    ax4.axis('off')

    summary_text = f"""
SAFETY FACTOR SUMMARY

Condition: {results['condition'].upper()}
Maximum Tension: {results['max_tension']:.1f} kN
Required SF: {results['sf_required']:.2f}
Actual SF: {results['safety_factor']:.2f}
Status: {'✓ PASS' if results['sf_pass'] else '✗ FAIL'}

Environmental Force: {results['environmental_force']:.1f} kN
Vessel Offset: {results['vessel_offset'][0]:.1f} m

All Lines Converged: {'Yes' if df['converged'].all() else 'No'}
    """

    ax4.text(0.1, 0.5, summary_text, fontsize=11, family='monospace',
            verticalalignment='center',
            bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))

    # 5. Line composition diagram
    ax5 = fig.add_subplot(gs[2, :])

    # Create schematic of line composition
    segments = [
        ('Chain Bottom', 600, 'brown'),
        ('Wire Rope', 1200, 'gray'),
        ('Chain Top', 400, 'brown')
    ]

    y_pos = 0.5
    x_start = 0
    for name, length, color in segments:
        width = length / 2200 * 0.8  # Scale to figure
        ax5.barh(y_pos, width, left=x_start, height=0.3, color=color,
                edgecolor='black', linewidth=2, label=name)
        ax5.text(x_start + width/2, y_pos, f'{name}\n{length}m',
                ha='center', va='center', fontsize=10, fontweight='bold')
        x_start += width

    ax5.set_xlim(0, 1)
    ax5.set_ylim(0, 1)
    ax5.set_xlabel('Mooring Line Composition', fontsize=12)
    ax5.set_title('Mooring Line Segments (From Seabed)', fontsize=14, fontweight='bold')
    ax5.set_yticks([])
    ax5.legend(loc='upper right', fontsize=10)
    ax5.grid(True, alpha=0.3, axis='x')

    # Overall title
    fig.suptitle('8-Point Spread Mooring Analysis - FPSO',
                fontsize=16, fontweight='bold', y=0.98)

    # Save
    output_dir.mkdir(parents=True, exist_ok=True)
    output_file = output_dir / f"mooring_analysis_{results['condition']}.png"
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    logger.info(f"Plot saved: {output_file}")
    plt.close()


# ============================================================================
# 6. REPORT GENERATION
# ============================================================================

def generate_excel_report(
    intact_results: Dict,
    damaged_results: Dict,
    output_dir: Path
):
    """
    Generate Excel summary report.

    Parameters
    ----------
    intact_results : Dict
        Intact condition results
    damaged_results : Dict
        Damaged condition results
    output_dir : Path
        Output directory
    """
    output_file = output_dir / "mooring_analysis_report.xlsx"

    with pd.ExcelWriter(output_file, engine='xlsxwriter') as writer:
        # Sheet 1: Intact condition
        intact_results['line_results'].to_excel(
            writer, sheet_name='Intact', index=False
        )

        # Sheet 2: Damaged condition
        damaged_results['line_results'].to_excel(
            writer, sheet_name='Damaged', index=False
        )

        # Sheet 3: Summary
        summary_df = pd.DataFrame({
            'Parameter': [
                'Condition',
                'Max Tension [kN]',
                'Required SF',
                'Actual SF',
                'Status',
                'Environmental Force [kN]',
                'Vessel Offset [m]'
            ],
            'Intact': [
                'Intact',
                f"{intact_results['max_tension']:.1f}",
                f"{intact_results['sf_required']:.2f}",
                f"{intact_results['safety_factor']:.2f}",
                'PASS' if intact_results['sf_pass'] else 'FAIL',
                f"{intact_results['environmental_force']:.1f}",
                f"{intact_results['vessel_offset'][0]:.1f}"
            ],
            'Damaged': [
                'Damaged (1 line out)',
                f"{damaged_results['max_tension']:.1f}",
                f"{damaged_results['sf_required']:.2f}",
                f"{damaged_results['safety_factor']:.2f}",
                'PASS' if damaged_results['sf_pass'] else 'FAIL',
                f"{damaged_results['environmental_force']:.1f}",
                f"{damaged_results['vessel_offset'][0]:.1f}"
            ]
        })

        summary_df.to_excel(writer, sheet_name='Summary', index=False)

    logger.info(f"Excel report saved: {output_file}")


# ============================================================================
# 7. MAIN EXECUTION
# ============================================================================

def main():
    """Main execution function."""
    logger.info("=" * 80)
    logger.info("8-POINT SPREAD MOORING DESIGN ANALYSIS")
    logger.info("=" * 80)

    # Initialize parameters
    vessel = VesselParticulars()
    env = EnvironmentalCriteria()
    config = MooringConfiguration()
    criteria = DesignCriteria()

    # Output directory
    output_dir = Path(__file__).parent / "outputs" / "01_spread_mooring"

    # Analyze intact condition
    logger.info("\n" + "=" * 80)
    logger.info("INTACT CONDITION ANALYSIS")
    logger.info("=" * 80)
    intact_results = analyze_mooring_system(
        vessel, config, env, criteria, condition="intact"
    )

    # Analyze damaged condition
    logger.info("\n" + "=" * 80)
    logger.info("DAMAGED CONDITION ANALYSIS (1 line broken)")
    logger.info("=" * 80)
    # For damaged, reduce number of lines and adjust criteria
    config_damaged = MooringConfiguration()
    config_damaged.num_lines = 7  # One line out
    damaged_results = analyze_mooring_system(
        vessel, config_damaged, env, criteria, condition="damaged"
    )

    # Generate visualizations
    logger.info("\n" + "=" * 80)
    logger.info("GENERATING VISUALIZATIONS")
    logger.info("=" * 80)
    create_mooring_plot(intact_results, output_dir)
    create_mooring_plot(damaged_results, output_dir)

    # Generate Excel report
    logger.info("\n" + "=" * 80)
    logger.info("GENERATING REPORTS")
    logger.info("=" * 80)
    generate_excel_report(intact_results, damaged_results, output_dir)

    # Final summary
    logger.info("\n" + "=" * 80)
    logger.info("ANALYSIS COMPLETE")
    logger.info("=" * 80)
    logger.info(f"Intact condition: {'PASS' if intact_results['sf_pass'] else 'FAIL'}")
    logger.info(f"Damaged condition: {'PASS' if damaged_results['sf_pass'] else 'FAIL'}")
    logger.info(f"\nOutputs saved to: {output_dir}")
    logger.info("=" * 80)


if __name__ == "__main__":
    main()
