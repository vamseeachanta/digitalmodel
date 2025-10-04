"""
Example script demonstrating complete integration workflow.

This script shows how all modules work together for a realistic
mooring analysis scenario.

Usage:
    python integration_example.py
"""

import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
import json

# Add src to path for imports
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent / "src"))

# Import all required modules
from digitalmodel.modules.marine_analysis.wave_spectra.spectra import (
    JONSWAPSpectrum,
    WaveSpectrumParameters
)
from digitalmodel.modules.marine_analysis.environmental_loading.ocimf import (
    OCIMFDatabase,
    EnvironmentalConditions,
    VesselGeometry,
    EnvironmentalForces,
    create_sample_database
)


def main():
    """Run complete integration example."""
    print("=" * 80)
    print("MARINE ENGINEERING INTEGRATION EXAMPLE")
    print("=" * 80)
    print()

    # Create output directory
    output_dir = Path(__file__).parent / "charts" / "example"
    output_dir.mkdir(parents=True, exist_ok=True)

    # =========================================================================
    # STEP 1: Define Environmental Conditions
    # =========================================================================
    print("STEP 1: Defining Environmental Conditions")
    print("-" * 80)

    # Wave conditions (severe North Sea storm)
    wave_params = WaveSpectrumParameters(
        Hs=8.0,      # 8m significant wave height
        Tp=12.0,     # 12s peak period
        gamma=3.3,   # Standard JONSWAP gamma
        freq_range=(0.01, 1.0),
        n_frequencies=200
    )
    wave_spectrum = JONSWAPSpectrum(wave_params)

    # Calculate wave statistics
    wave_stats = wave_spectrum.get_spectral_statistics()
    print(f"  Wave Conditions:")
    print(f"    Hs = {wave_stats['Hs']:.2f} m")
    print(f"    Tp = {wave_params.Tp:.2f} s")
    print(f"    Tz = {wave_stats['Tz']:.2f} s")
    print(f"    Tm = {wave_stats['Tm']:.2f} s")
    print(f"    Bandwidth = {wave_stats['bandwidth']:.3f}")
    print()

    # Wind and current conditions
    env_conditions = EnvironmentalConditions(
        wind_speed=25.0,         # 25 m/s (~50 knots)
        wind_direction=45.0,     # NE direction
        current_speed=2.0,       # 2 m/s (~4 knots)
        current_direction=30.0,  # Similar to wind
    )

    print(f"  Wind: {env_conditions.wind_speed:.1f} m/s at {env_conditions.wind_direction:.0f}°")
    print(f"  Current: {env_conditions.current_speed:.1f} m/s at {env_conditions.current_direction:.0f}°")
    print()

    # =========================================================================
    # STEP 2: Define Vessel
    # =========================================================================
    print("STEP 2: Defining Vessel")
    print("-" * 80)

    vessel_geometry = VesselGeometry(
        loa=330.0,   # VLCC length
        beam=60.0,   # VLCC beam
        draft=23.0,  # VLCC draft (loaded)
    )

    displacement = 320000  # tonnes (fully loaded VLCC)

    print(f"  Vessel Type: VLCC")
    print(f"  LOA = {vessel_geometry.loa:.1f} m")
    print(f"  Beam = {vessel_geometry.beam:.1f} m")
    print(f"  Draft = {vessel_geometry.draft:.1f} m")
    print(f"  Displacement = {displacement:,} tonnes")
    print()

    # =========================================================================
    # STEP 3: Calculate Environmental Forces
    # =========================================================================
    print("STEP 3: Calculating Environmental Forces")
    print("-" * 80)

    # Create OCIMF database
    import tempfile
    with tempfile.TemporaryDirectory() as tmpdir:
        db_path = Path(tmpdir) / "ocimf_example.csv"
        create_sample_database(str(db_path), num_vessels=5,
                              num_headings=37, num_displacements=5)
        ocimf_db = OCIMFDatabase(str(db_path))

        # Calculate forces
        env_forces = EnvironmentalForces(ocimf_db)
        force_results = env_forces.calculate_total_forces(
            env_conditions, vessel_geometry, displacement
        )

        print(f"  Wind Forces:")
        print(f"    Fx = {force_results.wind_fx/1e6:.2f} MN")
        print(f"    Fy = {force_results.wind_fy/1e6:.2f} MN")
        print(f"    Mz = {force_results.wind_mz/1e9:.2f} GN·m")
        print()
        print(f"  Current Forces:")
        print(f"    Fx = {force_results.current_fx/1e6:.2f} MN")
        print(f"    Fy = {force_results.current_fy/1e6:.2f} MN")
        print(f"    Mz = {force_results.current_mz/1e9:.2f} GN·m")
        print()
        print(f"  Total Forces:")
        print(f"    Fx = {force_results.total_fx/1e6:.2f} MN")
        print(f"    Fy = {force_results.total_fy/1e6:.2f} MN")
        print(f"    Mz = {force_results.total_mz/1e9:.2f} GN·m")
        print(f"    Magnitude = {np.sqrt(force_results.total_fx**2 + force_results.total_fy**2)/1e6:.2f} MN")
        print()

        # =====================================================================
        # STEP 4: Calculate Mooring System Response
        # =====================================================================
        print("STEP 4: Calculating Mooring System Response")
        print("-" * 80)

        # 8-point mooring system
        num_lines = 8
        line_angles = np.linspace(0, 360, num_lines, endpoint=False)

        print(f"  Mooring Configuration: {num_lines}-point spread")
        print(f"  Line Arrangement: {line_angles.tolist()}")
        print()

        # Calculate tension in each line
        line_tensions = []
        for i, angle in enumerate(line_angles):
            angle_rad = np.radians(angle)

            # Project environmental forces onto line direction
            force_component = (
                force_results.total_fx * np.cos(angle_rad) +
                force_results.total_fy * np.sin(angle_rad)
            )

            # Only tension (no compression)
            force_component = max(0, force_component / num_lines)

            # Add pretension (typical 500 kN)
            pretension = 500e3  # N
            total_tension = pretension + force_component

            line_tensions.append(total_tension)

            print(f"    Line {i+1} ({angle:6.1f}°): {total_tension/1e6:6.2f} MN")

        line_tensions = np.array(line_tensions)
        print()
        print(f"  Maximum Tension: {np.max(line_tensions)/1e6:.2f} MN")
        print(f"  Minimum Tension: {np.min(line_tensions)/1e6:.2f} MN")
        print(f"  Mean Tension: {np.mean(line_tensions)/1e6:.2f} MN")
        print()

        # =====================================================================
        # STEP 5: Safety Assessment
        # =====================================================================
        print("STEP 5: Safety Assessment")
        print("-" * 80)

        # Typical mooring line capacity (R4 studless chain, 120mm diameter)
        chain_diameter = 0.120  # m
        minimum_breaking_load = 15.0  # MN (approximate for R4 120mm)
        safety_factor_required = 2.0

        max_tension = np.max(line_tensions) / 1e6  # MN
        actual_safety_factor = minimum_breaking_load / max_tension

        print(f"  Chain Specification: R4 Studless, {chain_diameter*1000:.0f}mm diameter")
        print(f"  Minimum Breaking Load: {minimum_breaking_load:.1f} MN")
        print(f"  Maximum Line Tension: {max_tension:.2f} MN")
        print(f"  Safety Factor: {actual_safety_factor:.2f}")
        print(f"  Required Safety Factor: {safety_factor_required:.1f}")
        print()

        if actual_safety_factor >= safety_factor_required:
            print(f"  ✓ PASS: Safety factor adequate")
        else:
            print(f"  ✗ FAIL: Safety factor insufficient!")
        print()

        # =====================================================================
        # STEP 6: Generate Summary Visualizations
        # =====================================================================
        print("STEP 6: Generating Summary Visualizations")
        print("-" * 80)

        # Create comprehensive summary figure
        fig = plt.figure(figsize=(16, 12))
        gs = fig.add_gridspec(3, 3, hspace=0.3, wspace=0.3)

        # 1. Wave spectrum
        ax1 = fig.add_subplot(gs[0, :2])
        S = wave_spectrum.compute_spectrum()
        frequencies = wave_spectrum.frequencies
        ax1.plot(frequencies, S, linewidth=2, color='navy')
        ax1.fill_between(frequencies, 0, S, alpha=0.3)
        ax1.set_xlabel('Frequency [Hz]', fontsize=10)
        ax1.set_ylabel('S(f) [m²·s]', fontsize=10)
        ax1.set_title('Wave Spectrum (JONSWAP)', fontsize=12, fontweight='bold')
        ax1.grid(True, alpha=0.3)

        # 2. Wave statistics
        ax2 = fig.add_subplot(gs[0, 2])
        stats_text = f"Hs: {wave_stats['Hs']:.2f} m\n"
        stats_text += f"Tp: {wave_params.Tp:.2f} s\n"
        stats_text += f"Tz: {wave_stats['Tz']:.2f} s\n"
        stats_text += f"Tm: {wave_stats['Tm']:.2f} s\n"
        stats_text += f"BW: {wave_stats['bandwidth']:.3f}"
        ax2.text(0.5, 0.5, stats_text, ha='center', va='center',
                fontsize=11, family='monospace',
                bbox=dict(boxstyle='round', facecolor='lightblue', alpha=0.7))
        ax2.axis('off')
        ax2.set_title('Wave Statistics', fontsize=12, fontweight='bold')

        # 3. Environmental forces bar chart
        ax3 = fig.add_subplot(gs[1, 0])
        force_labels = ['Wind\nFx', 'Wind\nFy', 'Curr\nFx', 'Curr\nFy']
        force_values = [
            force_results.wind_fx / 1e6,
            force_results.wind_fy / 1e6,
            force_results.current_fx / 1e6,
            force_results.current_fy / 1e6
        ]
        colors = ['#1f77b4', '#aec7e8', '#2ca02c', '#98df8a']
        ax3.bar(range(len(force_labels)), force_values, color=colors, alpha=0.8)
        ax3.set_xticks(range(len(force_labels)))
        ax3.set_xticklabels(force_labels, fontsize=9)
        ax3.set_ylabel('Force (MN)', fontsize=10)
        ax3.set_title('Environmental Forces', fontsize=12, fontweight='bold')
        ax3.grid(True, alpha=0.3, axis='y')

        # 4. Force vectors
        ax4 = fig.add_subplot(gs[1, 1])
        scale = 1e-6
        ax4.arrow(0, 0, force_results.wind_fx*scale, force_results.wind_fy*scale,
                 head_width=0.5, head_length=0.6, fc='blue', ec='blue',
                 alpha=0.7, width=0.15, label='Wind')
        ax4.arrow(0, 0, force_results.current_fx*scale, force_results.current_fy*scale,
                 head_width=0.5, head_length=0.6, fc='green', ec='green',
                 alpha=0.7, width=0.15, label='Current')
        ax4.arrow(0, 0, force_results.total_fx*scale, force_results.total_fy*scale,
                 head_width=0.6, head_length=0.7, fc='red', ec='red',
                 alpha=0.9, width=0.2, label='Total', linewidth=2)
        ax4.set_xlabel('Fx (MN)', fontsize=10)
        ax4.set_ylabel('Fy (MN)', fontsize=10)
        ax4.set_title('Force Vectors', fontsize=12, fontweight='bold')
        ax4.legend(fontsize=9, loc='upper right')
        ax4.grid(True, alpha=0.3)
        ax4.axis('equal')

        # 5. Mooring tension bar chart
        ax5 = fig.add_subplot(gs[1, 2])
        line_numbers = [f'L{i+1}' for i in range(num_lines)]
        ax5.bar(range(num_lines), line_tensions / 1e6, color='darkred', alpha=0.7)
        ax5.axhline(minimum_breaking_load / safety_factor_required,
                   color='orange', linestyle='--', linewidth=2,
                   label=f'Design Limit ({minimum_breaking_load/safety_factor_required:.1f} MN)')
        ax5.set_xticks(range(num_lines))
        ax5.set_xticklabels(line_numbers, fontsize=9)
        ax5.set_ylabel('Tension (MN)', fontsize=10)
        ax5.set_title('Mooring Line Tensions', fontsize=12, fontweight='bold')
        ax5.legend(fontsize=8)
        ax5.grid(True, alpha=0.3, axis='y')

        # 6. Mooring tension polar
        ax6 = fig.add_subplot(gs[2, :2], projection='polar')
        theta = np.radians(line_angles)
        tensions_MN = line_tensions / 1e6
        ax6.plot(theta, tensions_MN, 'o-', linewidth=2, markersize=8, color='darkred')
        ax6.fill(theta, tensions_MN, alpha=0.25, color='red')
        ax6.set_theta_zero_location('N')
        ax6.set_theta_direction(-1)
        ax6.set_title('Mooring Tension Distribution (MN)', fontsize=12,
                     fontweight='bold', pad=20)
        ax6.grid(True)

        # 7. Safety assessment
        ax7 = fig.add_subplot(gs[2, 2])
        safety_text = f"Chain: R4 {chain_diameter*1000:.0f}mm\n"
        safety_text += f"MBL: {minimum_breaking_load:.1f} MN\n\n"
        safety_text += f"Max Tension:\n{max_tension:.2f} MN\n\n"
        safety_text += f"Safety Factor:\n{actual_safety_factor:.2f}\n\n"

        if actual_safety_factor >= safety_factor_required:
            status_text = "✓ PASS"
            status_color = 'lightgreen'
        else:
            status_text = "✗ FAIL"
            status_color = 'lightcoral'

        ax7.text(0.5, 0.6, safety_text, ha='center', va='center',
                fontsize=10, family='monospace',
                bbox=dict(boxstyle='round', facecolor='lightyellow', alpha=0.7))
        ax7.text(0.5, 0.2, status_text, ha='center', va='center',
                fontsize=16, fontweight='bold',
                bbox=dict(boxstyle='round', facecolor=status_color, alpha=0.7))
        ax7.axis('off')
        ax7.set_title('Safety Assessment', fontsize=12, fontweight='bold')

        plt.suptitle('Marine Engineering Integration - Complete Workflow Summary',
                    fontsize=14, fontweight='bold', y=0.995)

        output_file = output_dir / "integration_summary.png"
        plt.savefig(output_file, dpi=300, bbox_inches='tight')
        print(f"  Saved: {output_file}")
        plt.close()

        # =====================================================================
        # STEP 7: Export Results
        # =====================================================================
        print()
        print("STEP 7: Exporting Results")
        print("-" * 80)

        # Create comprehensive results dictionary
        results = {
            'environment': {
                'wave': {
                    'Hs_m': wave_stats['Hs'],
                    'Tp_s': wave_params.Tp,
                    'Tz_s': wave_stats['Tz'],
                    'Tm_s': wave_stats['Tm'],
                    'bandwidth': wave_stats['bandwidth']
                },
                'wind': {
                    'speed_ms': env_conditions.wind_speed,
                    'direction_deg': env_conditions.wind_direction
                },
                'current': {
                    'speed_ms': env_conditions.current_speed,
                    'direction_deg': env_conditions.current_direction
                }
            },
            'vessel': {
                'type': 'VLCC',
                'loa_m': vessel_geometry.loa,
                'beam_m': vessel_geometry.beam,
                'draft_m': vessel_geometry.draft,
                'displacement_tonnes': displacement
            },
            'forces': {
                'wind_fx_MN': force_results.wind_fx / 1e6,
                'wind_fy_MN': force_results.wind_fy / 1e6,
                'wind_mz_GNm': force_results.wind_mz / 1e9,
                'current_fx_MN': force_results.current_fx / 1e6,
                'current_fy_MN': force_results.current_fy / 1e6,
                'current_mz_GNm': force_results.current_mz / 1e9,
                'total_fx_MN': force_results.total_fx / 1e6,
                'total_fy_MN': force_results.total_fy / 1e6,
                'total_mz_GNm': force_results.total_mz / 1e9
            },
            'mooring': {
                'num_lines': num_lines,
                'line_angles_deg': line_angles.tolist(),
                'line_tensions_MN': (line_tensions / 1e6).tolist(),
                'max_tension_MN': float(max_tension),
                'min_tension_MN': float(np.min(line_tensions) / 1e6),
                'mean_tension_MN': float(np.mean(line_tensions) / 1e6)
            },
            'safety': {
                'chain_diameter_mm': chain_diameter * 1000,
                'minimum_breaking_load_MN': minimum_breaking_load,
                'required_safety_factor': safety_factor_required,
                'actual_safety_factor': float(actual_safety_factor),
                'status': 'PASS' if actual_safety_factor >= safety_factor_required else 'FAIL'
            }
        }

        # Save JSON results
        json_file = output_dir / "integration_results.json"
        with open(json_file, 'w') as f:
            json.dump(results, f, indent=2)
        print(f"  Saved: {json_file}")

        print()
        print("=" * 80)
        print("INTEGRATION EXAMPLE COMPLETE")
        print("=" * 80)


if __name__ == "__main__":
    main()
