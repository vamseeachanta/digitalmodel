"""Fix RAO Plots - Standalone version to avoid stdout issues"""

if __name__ == '__main__':
    import sys
    from pathlib import Path

    # Add src to path BEFORE any other imports
    sys.path.insert(0, str(Path(__file__).parent.parent / 'src'))

    # Now safe to import
    import numpy as np
    import matplotlib
    matplotlib.use('Agg')  # Non-interactive backend
    import matplotlib.pyplot as plt
    from matplotlib.gridspec import GridSpec

    # Import after path setup
    from digitalmodel.marine_analysis.unified_rao_reader import read_rao_file

    # Input file
    lis_file = Path(__file__).parent.parent / 'specs' / 'modules' / 'aqwa' / 'ship-analysis' / 'go-by-ship-raos' / '001_SHIP_RAOS_REV3.LIS'

    # Output directory
    output_dir = Path(__file__).parent.parent / 'tests' / 'marine_engineering' / 'plots' / 'rao_qa'
    output_dir.mkdir(parents=True, exist_ok=True)

    # Load RAO data
    rao_data = read_rao_file(str(lis_file))
    disp = rao_data.displacement

    # Convert frequency to period
    periods = 2 * np.pi / disp.frequencies

    # DOF information
    dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
    dof_units = {
        'Surge': 'm/m', 'Sway': 'm/m', 'Heave': 'm/m',
        'Roll': 'deg/m', 'Pitch': 'deg/m', 'Yaw': 'deg/m'
    }

    # Get unique headings
    unique_headings = sorted(list(set(disp.headings)))

    # PLOT 1: Amplitude vs Period
    fig = plt.figure(figsize=(18, 12))
    fig.suptitle(f'RAO Amplitude vs Wave Period - All DOF\n({len(periods)} periods: {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)
    dof_data = [disp.surge, disp.sway, disp.heave, disp.roll, disp.pitch, disp.yaw]

    for idx, (dof, name) in enumerate(zip(dof_data, dof_names)):
        row = idx // 3
        col = idx % 3
        ax = fig.add_subplot(gs[row, col])

        for heading in unique_headings:
            heading_indices = [i for i, h in enumerate(disp.headings) if h == heading]
            if len(heading_indices) > 0:
                heading_amplitudes = dof.amplitude[:, heading_indices[0]]
                ax.plot(periods, heading_amplitudes, 'o-',
                       label=f'{heading:.0f}°', linewidth=1.5, markersize=4, alpha=0.8)

        ax.set_xlabel('Wave Period (s)', fontsize=11)
        ax.set_ylabel(f'Amplitude ({dof_units[name]})', fontsize=11)
        ax.set_title(f'{name} RAO', fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
        ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
        ax.minorticks_on()
        ax.legend(loc='best', fontsize=8, ncol=2)
        # Wave period increasing left to right (no invert)

    plt.savefig(output_dir / 'rao_amplitude_all_dof.png', dpi=300, bbox_inches='tight')
    plt.close()

    # PLOT 2: Phase vs Period
    fig = plt.figure(figsize=(18, 12))
    fig.suptitle(f'RAO Phase vs Wave Period - All DOF\n({len(periods)} periods: {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

    for idx, (dof, name) in enumerate(zip(dof_data, dof_names)):
        row = idx // 3
        col = idx % 3
        ax = fig.add_subplot(gs[row, col])

        for heading in unique_headings:
            heading_indices = [i for i, h in enumerate(disp.headings) if h == heading]
            if len(heading_indices) > 0:
                heading_phases = dof.phase[:, heading_indices[0]]
                ax.plot(periods, heading_phases, 'o-',
                       label=f'{heading:.0f}°', linewidth=1.5, markersize=4, alpha=0.8)

        ax.set_xlabel('Wave Period (s)', fontsize=11)
        ax.set_ylabel('Phase (degrees)', fontsize=11)
        ax.set_title(f'{name} Phase', fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
        ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
        ax.minorticks_on()
        ax.legend(loc='best', fontsize=8, ncol=2)
        ax.axhline(y=0, color='k', linestyle='--', alpha=0.3)
        ax.axhline(y=180, color='k', linestyle='--', alpha=0.3)
        ax.axhline(y=-180, color='k', linestyle='--', alpha=0.3)
        # Wave period increasing left to right (no invert)

    plt.savefig(output_dir / 'rao_phase_all_dof.png', dpi=300, bbox_inches='tight')
    plt.close()

    # PLOT 3: Heading Comparison
    key_headings = [0, 45, 90, 135, 180]
    available_key_headings = [h for h in key_headings if h in unique_headings]

    fig = plt.figure(figsize=(18, 12))
    fig.suptitle(f'RAO Amplitude - Key Headings\n({len(periods)} periods: {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

    for idx, (dof, name) in enumerate(zip(dof_data, dof_names)):
        row = idx // 3
        col = idx % 3
        ax = fig.add_subplot(gs[row, col])

        for heading in available_key_headings:
            heading_indices = [i for i, h in enumerate(disp.headings) if h == heading]
            if len(heading_indices) > 0:
                heading_amplitudes = dof.amplitude[:, heading_indices[0]]
                ax.plot(periods, heading_amplitudes, 'o-',
                       label=f'{heading:.0f}°', linewidth=2, markersize=5)

        ax.set_xlabel('Wave Period (s)', fontsize=11)
        ax.set_ylabel(f'Amplitude ({dof_units[name]})', fontsize=11)
        ax.set_title(f'{name} RAO - Key Headings', fontsize=12, fontweight='bold')
        ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
        ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
        ax.minorticks_on()
        ax.legend(loc='best', fontsize=10)
        # Wave period increasing left to right (no invert)

    plt.savefig(output_dir / 'rao_heading_comparison.png', dpi=300, bbox_inches='tight')
    plt.close()

    # PLOT 4: Translation vs Rotation
    reference_heading = 180 if 180 in unique_headings else unique_headings[-1]
    heading_indices = [i for i, h in enumerate(disp.headings) if h == reference_heading]
    head_seas_idx = heading_indices[0] if len(heading_indices) > 0 else 0

    fig, axes = plt.subplots(2, 1, figsize=(16, 10))
    fig.suptitle(f'Translation vs Rotation DOF - {reference_heading:.0f}°\n({len(periods)} periods: {periods.min():.1f}s to {periods.max():.1f}s)',
                 fontsize=16, fontweight='bold')

    ax = axes[0]
    ax.plot(periods, disp.surge.amplitude[:, head_seas_idx], 'o-', label='Surge', linewidth=2, markersize=5)
    ax.plot(periods, disp.sway.amplitude[:, head_seas_idx], 's-', label='Sway', linewidth=2, markersize=5)
    ax.plot(periods, disp.heave.amplitude[:, head_seas_idx], '^-', label='Heave', linewidth=2, markersize=5)
    ax.set_ylabel('Amplitude (m/m)', fontsize=12)
    ax.set_title(f'Translation DOF', fontsize=13, fontweight='bold')
    ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
    ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
    ax.minorticks_on()
    ax.legend(loc='best', fontsize=11)
    # Wave period increasing left to right (no invert)

    ax = axes[1]
    ax.plot(periods, disp.roll.amplitude[:, head_seas_idx], 'o-', label='Roll', linewidth=2, markersize=5)
    ax.plot(periods, disp.pitch.amplitude[:, head_seas_idx], 's-', label='Pitch', linewidth=2, markersize=5)
    ax.plot(periods, disp.yaw.amplitude[:, head_seas_idx], '^-', label='Yaw', linewidth=2, markersize=5)
    ax.set_xlabel('Wave Period (s)', fontsize=12)
    ax.set_ylabel('Amplitude (deg/m)', fontsize=12)
    ax.set_title(f'Rotation DOF', fontsize=13, fontweight='bold')
    ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
    ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
    ax.minorticks_on()
    ax.legend(loc='best', fontsize=11)
    # Wave period increasing left to right (no invert)

    plt.tight_layout()
    plt.savefig(output_dir / 'rao_translation_rotation.png', dpi=300, bbox_inches='tight')
    plt.close()

    # Summary
    with open(output_dir / 'PLOT_FIX_SUMMARY.txt', 'w') as f:
        f.write('RAO PLOT FIX SUMMARY\n')
        f.write('='*80 + '\n\n')
        f.write(f'Generated 4 plots with ALL {len(periods)} periods\n')
        f.write(f'Period range: {periods.min():.1f}s to {periods.max():.1f}s\n')
        f.write(f'Periods: {list(periods)}\n\n')
        f.write(f'Number of headings: {len(unique_headings)}\n')
        f.write(f'Headings: {unique_headings}\n\n')
        f.write('Generated plots:\n')
        f.write('  1. rao_amplitude_all_dof.png\n')
        f.write('  2. rao_phase_all_dof.png\n')
        f.write('  3. rao_heading_comparison.png\n')
        f.write('  4. rao_translation_rotation.png\n')
