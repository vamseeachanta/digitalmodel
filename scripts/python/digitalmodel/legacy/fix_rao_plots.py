"""Fix RAO Plots to Include All Periods

This script fixes the RAO plots to properly display all available periods
instead of only showing 3 data points.
"""

import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.gridspec import GridSpec

from digitalmodel.marine_analysis.unified_rao_reader import read_rao_file

def main():
    """Main function to generate fixed RAO plots."""

    # Input file
    lis_file = Path('specs/modules/aqwa/ship-analysis/go-by-ship-raos/001_SHIP_RAOS_REV3.LIS')

    # Output directory
    output_dir = Path('tests/marine_engineering/plots/rao_qa')
    output_dir.mkdir(parents=True, exist_ok=True)

    print('='*80)
    print('FIXING RAO PLOTS - DISPLAYING ALL PERIODS')
    print('='*80)
    print(f'\nInput file: {lis_file}')
    print(f'Output directory: {output_dir}\n')

    # Load RAO data
    print('Loading RAO data...')
    rao_data = read_rao_file(str(lis_file))
    disp = rao_data.displacement

    # Convert frequency to period
    periods = 2 * np.pi / disp.frequencies

    # Print data information
    print(f'\n{"="*80}')
    print('DATA INFORMATION')
    print(f'{"="*80}')
    print(f'Number of frequencies: {len(disp.frequencies)}')
    print(f'Frequencies (rad/s): {disp.frequencies}')
    print(f'Periods (s): {periods}')
    print(f'Period range: {periods.min():.2f}s to {periods.max():.2f}s')
    print(f'\nNumber of headings: {len(disp.headings)}')
    print(f'Unique headings: {sorted(list(set(disp.headings)))}')
    print(f'\nData array shape (periods x headings): {disp.surge.amplitude.shape}')
    print(f'{"="*80}\n')

    # DOF information
    dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
    dof_units = {
        'Surge': 'm/m', 'Sway': 'm/m', 'Heave': 'm/m',
        'Roll': 'deg/m', 'Pitch': 'deg/m', 'Yaw': 'deg/m'
    }

    # Get unique headings for plotting
    unique_headings = sorted(list(set(disp.headings)))

    # --- PLOT 1: Amplitude vs Period for all DOF ---
    print('Generating Plot 1: Amplitude vs Period (all DOF)...')

    fig = plt.figure(figsize=(18, 12))
    fig.suptitle(f'RAO Amplitude vs Wave Period - All DOF\n({len(periods)} periods from {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

    dof_data = [disp.surge, disp.sway, disp.heave, disp.roll, disp.pitch, disp.yaw]

    for idx, (dof, name) in enumerate(zip(dof_data, dof_names)):
        row = idx // 3
        col = idx % 3
        ax = fig.add_subplot(gs[row, col])

        # Plot each unique heading
        for heading in unique_headings:
            # Get all indices for this heading
            heading_indices = [i for i, h in enumerate(disp.headings) if h == heading]

            if len(heading_indices) > 0:
                # Extract data for this heading across all periods
                heading_amplitudes = dof.amplitude[:, heading_indices[0]]

                label = f'{heading:.0f}°'
                ax.plot(periods, heading_amplitudes, 'o-',
                       label=label, linewidth=1.5, markersize=4, alpha=0.8)

        ax.set_xlabel('Wave Period (s)', fontsize=11)
        ax.set_ylabel(f'Amplitude ({dof_units[name]})', fontsize=11)
        ax.set_title(f'{name} RAO', fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend(loc='best', fontsize=8, ncol=2)
        ax.invert_xaxis()

    plt.savefig(output_dir / 'rao_amplitude_all_dof.png', dpi=300, bbox_inches='tight')
    print(f'  Saved: {output_dir / "rao_amplitude_all_dof.png"}')
    plt.close()

    # --- PLOT 2: Phase vs Period for all DOF ---
    print('Generating Plot 2: Phase vs Period (all DOF)...')

    fig = plt.figure(figsize=(18, 12))
    fig.suptitle(f'RAO Phase vs Wave Period - All DOF\n({len(periods)} periods from {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

    for idx, (dof, name) in enumerate(zip(dof_data, dof_names)):
        row = idx // 3
        col = idx % 3
        ax = fig.add_subplot(gs[row, col])

        # Plot each unique heading
        for heading in unique_headings:
            # Get all indices for this heading
            heading_indices = [i for i, h in enumerate(disp.headings) if h == heading]

            if len(heading_indices) > 0:
                # Extract data for this heading across all periods
                heading_phases = dof.phase[:, heading_indices[0]]

                label = f'{heading:.0f}°'
                ax.plot(periods, heading_phases, 'o-',
                       label=label, linewidth=1.5, markersize=4, alpha=0.8)

        ax.set_xlabel('Wave Period (s)', fontsize=11)
        ax.set_ylabel('Phase (degrees)', fontsize=11)
        ax.set_title(f'{name} Phase', fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend(loc='best', fontsize=8, ncol=2)
        ax.axhline(y=0, color='k', linestyle='--', alpha=0.3)
        ax.axhline(y=180, color='k', linestyle='--', alpha=0.3)
        ax.axhline(y=-180, color='k', linestyle='--', alpha=0.3)
        ax.invert_xaxis()

    plt.savefig(output_dir / 'rao_phase_all_dof.png', dpi=300, bbox_inches='tight')
    print(f'  Saved: {output_dir / "rao_phase_all_dof.png"}')
    plt.close()

    # --- PLOT 3: Heading Comparison (Key Headings) ---
    print('Generating Plot 3: Heading Comparison...')

    # Select key headings that exist in the data
    key_headings = [0, 45, 90, 135, 180]
    available_key_headings = [h for h in key_headings if h in unique_headings]

    fig = plt.figure(figsize=(18, 12))
    fig.suptitle(f'RAO Amplitude Comparison - Key Headings\n({len(periods)} periods from {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

    for idx, (dof, name) in enumerate(zip(dof_data, dof_names)):
        row = idx // 3
        col = idx % 3
        ax = fig.add_subplot(gs[row, col])

        # Plot each key heading
        for heading in available_key_headings:
            # Get all indices for this heading
            heading_indices = [i for i, h in enumerate(disp.headings) if h == heading]

            if len(heading_indices) > 0:
                # Extract data for this heading across all periods
                heading_amplitudes = dof.amplitude[:, heading_indices[0]]

                label = f'{heading:.0f}°'
                ax.plot(periods, heading_amplitudes, 'o-',
                       label=label, linewidth=2, markersize=5)

        ax.set_xlabel('Wave Period (s)', fontsize=11)
        ax.set_ylabel(f'Amplitude ({dof_units[name]})', fontsize=11)
        ax.set_title(f'{name} RAO - Key Headings', fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend(loc='best', fontsize=10)
        ax.invert_xaxis()

    plt.savefig(output_dir / 'rao_heading_comparison.png', dpi=300, bbox_inches='tight')
    print(f'  Saved: {output_dir / "rao_heading_comparison.png"}')
    plt.close()

    # --- PLOT 4: Translation vs Rotation DOF ---
    print('Generating Plot 4: Translation vs Rotation DOF...')

    # Use head seas (180°) if available, otherwise use first available heading
    if 180 in unique_headings:
        reference_heading = 180
    else:
        reference_heading = unique_headings[-1]  # Use last heading

    heading_indices = [i for i, h in enumerate(disp.headings) if h == reference_heading]
    head_seas_idx = heading_indices[0] if len(heading_indices) > 0 else 0

    fig, axes = plt.subplots(2, 1, figsize=(16, 10))
    fig.suptitle(f'RAO Amplitude: Translation vs Rotation DOF - {reference_heading:.0f}° Heading\n({len(periods)} periods from {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    # Translation DOF
    ax = axes[0]
    ax.plot(periods, disp.surge.amplitude[:, head_seas_idx], 'o-',
           label='Surge', linewidth=2, markersize=5)
    ax.plot(periods, disp.sway.amplitude[:, head_seas_idx], 's-',
           label='Sway', linewidth=2, markersize=5)
    ax.plot(periods, disp.heave.amplitude[:, head_seas_idx], '^-',
           label='Heave', linewidth=2, markersize=5)
    ax.set_ylabel('Amplitude (m/m)', fontsize=12)
    ax.set_title(f'Translation DOF - {reference_heading:.0f}° Heading', fontsize=13, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(loc='best', fontsize=11)
    ax.invert_xaxis()

    # Rotation DOF
    ax = axes[1]
    ax.plot(periods, disp.roll.amplitude[:, head_seas_idx], 'o-',
           label='Roll', linewidth=2, markersize=5)
    ax.plot(periods, disp.pitch.amplitude[:, head_seas_idx], 's-',
           label='Pitch', linewidth=2, markersize=5)
    ax.plot(periods, disp.yaw.amplitude[:, head_seas_idx], '^-',
           label='Yaw', linewidth=2, markersize=5)
    ax.set_xlabel('Wave Period (s)', fontsize=12)
    ax.set_ylabel('Amplitude (deg/m)', fontsize=12)
    ax.set_title(f'Rotation DOF - {reference_heading:.0f}° Heading', fontsize=13, fontweight='bold')
    ax.grid(True, alpha=0.3)
    ax.legend(loc='best', fontsize=11)
    ax.invert_xaxis()

    plt.tight_layout()
    plt.savefig(output_dir / 'rao_translation_rotation.png', dpi=300, bbox_inches='tight')
    print(f'  Saved: {output_dir / "rao_translation_rotation.png"}')
    plt.close()

    print(f'\n{"="*80}')
    print('PLOT GENERATION COMPLETE')
    print(f'{"="*80}')
    print(f'\nGenerated 4 plots with ALL {len(periods)} periods:')
    print(f'  1. rao_amplitude_all_dof.png')
    print(f'  2. rao_phase_all_dof.png')
    print(f'  3. rao_heading_comparison.png')
    print(f'  4. rao_translation_rotation.png')
    print(f'\nAll plots saved to: {output_dir}')
    print(f'\n✅ Fixed: Now showing all {len(periods)} periods ({periods.min():.1f}s to {periods.max():.1f}s)')
    print(f'✅ Showing {len(unique_headings)} unique headings: {unique_headings}')

if __name__ == '__main__':
    main()
