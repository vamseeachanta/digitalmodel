"""Comprehensive RAO Plotting Module for Quality Assurance.

This module provides production-ready RAO plotting functionality:
- Amplitude vs Frequency for all 6 DOF
- Phase vs Frequency for all 6 DOF
- Polar plots showing directional response
- Comparison plots across headings

Usage:
    from digitalmodel.modules.marine_analysis.visualization import RAOPlotter

    plotter = RAOPlotter('path/to/file.lis', output_dir='path/to/output')
    plotter.generate_all_plots()
"""

import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path
from matplotlib.gridspec import GridSpec
from typing import Optional

from ..unified_rao_reader import read_rao_file


class RAOPlotter:
    """Comprehensive RAO plotting for QA."""

    def __init__(self, rao_file_path: str, output_dir: Optional[str] = None):
        """Initialize plotter with RAO data.

        Args:
            rao_file_path: Path to .lis file
            output_dir: Directory to save plots (default: tests/marine_engineering/plots/rao_qa)
        """
        self.rao_file_path = rao_file_path

        if output_dir:
            self.output_dir = Path(output_dir)
        else:
            # Default to tests directory
            repo_root = Path(__file__).parent.parent.parent.parent.parent
            self.output_dir = repo_root / 'tests' / 'marine_engineering' / 'plots' / 'rao_qa'

        self.output_dir.mkdir(parents=True, exist_ok=True)

        # Load RAO data
        print(f'Loading RAO data from: {rao_file_path}')
        self.rao_data = read_rao_file(rao_file_path)
        self.disp = self.rao_data.displacement

        # Convert frequency to period for plotting
        self.periods = 2 * np.pi / self.disp.frequencies

        # DOF information
        self.dof_names = ['Surge', 'Sway', 'Heave', 'Roll', 'Pitch', 'Yaw']
        self.dof_units = {
            'Surge': 'm/m', 'Sway': 'm/m', 'Heave': 'm/m',
            'Roll': 'deg/m', 'Pitch': 'deg/m', 'Yaw': 'deg/m'
        }

        print(f'Loaded: {len(self.disp.frequencies)} frequencies, {len(self.disp.headings)} headings')

    def plot_amplitude_vs_frequency(self):
        """Plot amplitude vs frequency for all 6 DOF (2x3 subplots)."""
        fig = plt.figure(figsize=(18, 12))
        fig.suptitle('RAO Amplitude vs Wave Period - All DOF', fontsize=16, fontweight='bold')

        gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

        # Get DOF data
        dof_data = [
            self.disp.surge, self.disp.sway, self.disp.heave,
            self.disp.roll, self.disp.pitch, self.disp.yaw
        ]

        # Plot each DOF
        for idx, (dof, name) in enumerate(zip(dof_data, self.dof_names)):
            row = idx // 3
            col = idx % 3
            ax = fig.add_subplot(gs[row, col])

            # Plot each heading
            for j, heading in enumerate(self.disp.headings):
                label = f'{heading:.0f}°'
                ax.plot(self.periods, dof.amplitude[:, j], 'o-',
                       label=label, linewidth=1.5, markersize=4)

            ax.set_xlabel('Wave Period (s)', fontsize=11)
            ax.set_ylabel(f'Amplitude ({self.dof_units[name]})', fontsize=11)
            ax.set_title(f'{name} RAO', fontsize=12, fontweight='bold')
            ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
            ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
            ax.minorticks_on()
            ax.legend(loc='best', fontsize=8, ncol=2)

            # Wave period increasing left to right (no invert)

        plt.savefig(self.output_dir / 'rao_amplitude_all_dof.png', dpi=300, bbox_inches='tight')
        print(f'Saved: {self.output_dir / "rao_amplitude_all_dof.png"}')
        plt.close()

    def plot_phase_vs_frequency(self):
        """Plot phase vs frequency for all 6 DOF (2x3 subplots)."""
        fig = plt.figure(figsize=(18, 12))
        fig.suptitle('RAO Phase vs Wave Period - All DOF', fontsize=16, fontweight='bold')

        gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

        # Get DOF data
        dof_data = [
            self.disp.surge, self.disp.sway, self.disp.heave,
            self.disp.roll, self.disp.pitch, self.disp.yaw
        ]

        # Plot each DOF
        for idx, (dof, name) in enumerate(zip(dof_data, self.dof_names)):
            row = idx // 3
            col = idx % 3
            ax = fig.add_subplot(gs[row, col])

            # Plot each heading
            for j, heading in enumerate(self.disp.headings):
                label = f'{heading:.0f}°'
                ax.plot(self.periods, dof.phase[:, j], 'o-',
                       label=label, linewidth=1.5, markersize=4)

            ax.set_xlabel('Wave Period (s)', fontsize=11)
            ax.set_ylabel('Phase (deg)', fontsize=11)
            ax.set_title(f'{name} Phase', fontsize=12, fontweight='bold')
            ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
            ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
            ax.minorticks_on()
            ax.legend(loc='best', fontsize=8, ncol=2)
            ax.axhline(y=0, color='k', linestyle='--', alpha=0.3)
            ax.axhline(y=180, color='k', linestyle='--', alpha=0.3)
            ax.axhline(y=-180, color='k', linestyle='--', alpha=0.3)

            # Wave period increasing left to right (no invert)

        plt.savefig(self.output_dir / 'rao_phase_all_dof.png', dpi=300, bbox_inches='tight')
        print(f'Saved: {self.output_dir / "rao_phase_all_dof.png"}')
        plt.close()

    def plot_polar_response(self):
        """Create polar plots showing directional response for selected periods."""
        # Select 4 representative periods
        period_indices = [0, len(self.periods)//3, 2*len(self.periods)//3, -1]
        selected_periods = self.periods[period_indices]

        fig = plt.figure(figsize=(20, 10))
        fig.suptitle('RAO Directional Response - Polar Plots', fontsize=16, fontweight='bold')

        # Get DOF data
        dof_data = [
            self.disp.surge, self.disp.sway, self.disp.heave,
            self.disp.roll, self.disp.pitch, self.disp.yaw
        ]

        # Create subplots: 6 DOF x 4 periods = 24 subplots (4 rows x 6 cols)
        for period_idx, period in enumerate(selected_periods):
            freq_idx = period_indices[period_idx]

            for dof_idx, (dof, name) in enumerate(zip(dof_data, self.dof_names)):
                ax = plt.subplot(4, 6, period_idx * 6 + dof_idx + 1, projection='polar')

                # Convert headings to radians (AQWA convention: 0=head seas, 180=following)
                # Adjust to math convention: 0=North, counterclockwise
                theta = np.deg2rad(90 - self.disp.headings)  # Convert to math convention
                r = dof.amplitude[freq_idx, :]

                # Plot
                ax.plot(theta, r, 'o-', linewidth=2, markersize=6)
                ax.fill(theta, r, alpha=0.25)

                # Labels
                if dof_idx == 0:
                    ax.set_title(f'T={period:.1f}s\n{name}', fontsize=9, fontweight='bold')
                else:
                    ax.set_title(f'{name}', fontsize=9, fontweight='bold')

                # Set 0° at top (head seas)
                ax.set_theta_zero_location('N')
                ax.set_theta_direction(-1)  # Clockwise

                # Add heading labels
                ax.set_xticks(np.deg2rad([0, 45, 90, 135, 180, 225, 270, 315]))
                ax.set_xticklabels(['0°\n(Head)', '45°', '90°\n(Beam)', '135°',
                                   '180°\n(Follow)', '225°', '270°\n(Beam)', '315°'],
                                  fontsize=7)

        plt.tight_layout()
        plt.savefig(self.output_dir / 'rao_polar_directional.png', dpi=300, bbox_inches='tight')
        print(f'Saved: {self.output_dir / "rao_polar_directional.png"}')
        plt.close()

    def plot_heading_comparison(self):
        """Plot comparison of key headings (0°, 45°, 90°, 135°, 180°) for each DOF."""
        # Select key headings
        key_headings = [0, 45, 90, 135, 180]
        heading_indices = [np.where(self.disp.headings == h)[0][0] for h in key_headings]

        fig = plt.figure(figsize=(18, 12))
        fig.suptitle('RAO Amplitude Comparison - Key Headings', fontsize=16, fontweight='bold')

        gs = GridSpec(2, 3, figure=fig, hspace=0.3, wspace=0.3)

        # Get DOF data
        dof_data = [
            self.disp.surge, self.disp.sway, self.disp.heave,
            self.disp.roll, self.disp.pitch, self.disp.yaw
        ]

        # Plot each DOF
        for idx, (dof, name) in enumerate(zip(dof_data, self.dof_names)):
            row = idx // 3
            col = idx % 3
            ax = fig.add_subplot(gs[row, col])

            # Plot key headings only
            for j, h_idx in enumerate(heading_indices):
                heading = self.disp.headings[h_idx]
                label = f'{heading:.0f}°'
                ax.plot(self.periods, dof.amplitude[:, h_idx], 'o-',
                       label=label, linewidth=2, markersize=5)

            ax.set_xlabel('Wave Period (s)', fontsize=11)
            ax.set_ylabel(f'Amplitude ({self.dof_units[name]})', fontsize=11)
            ax.set_title(f'{name} RAO - Key Headings', fontsize=12, fontweight='bold')
            ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
            ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
            ax.minorticks_on()
            ax.legend(loc='best', fontsize=10)

            # Wave period increasing left to right (no invert)

        plt.savefig(self.output_dir / 'rao_heading_comparison.png', dpi=300, bbox_inches='tight')
        print(f'Saved: {self.output_dir / "rao_heading_comparison.png"}')
        plt.close()

    def plot_translation_vs_rotation(self):
        """Plot translation DOF (Surge/Sway/Heave) vs rotation DOF (Roll/Pitch/Yaw)."""
        fig, axes = plt.subplots(2, 1, figsize=(16, 10))
        fig.suptitle('RAO Amplitude: Translation vs Rotation DOF', fontsize=16, fontweight='bold')

        # Translation DOF (head seas - 180°)
        head_seas_idx = np.where(self.disp.headings == 180)[0][0]

        ax = axes[0]
        ax.plot(self.periods, self.disp.surge.amplitude[:, head_seas_idx], 'o-',
               label='Surge', linewidth=2, markersize=5)
        ax.plot(self.periods, self.disp.sway.amplitude[:, head_seas_idx], 's-',
               label='Sway', linewidth=2, markersize=5)
        ax.plot(self.periods, self.disp.heave.amplitude[:, head_seas_idx], '^-',
               label='Heave', linewidth=2, markersize=5)
        ax.set_ylabel('Amplitude (m/m)', fontsize=12)
        ax.set_title('Translation DOF - Head Seas (180°)', fontsize=13, fontweight='bold')
        ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
        ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
        ax.minorticks_on()
        ax.legend(loc='best', fontsize=11)
        # Wave period increasing left to right (no invert)

        # Rotation DOF (head seas - 180°)
        ax = axes[1]
        ax.plot(self.periods, self.disp.roll.amplitude[:, head_seas_idx], 'o-',
               label='Roll', linewidth=2, markersize=5)
        ax.plot(self.periods, self.disp.pitch.amplitude[:, head_seas_idx], 's-',
               label='Pitch', linewidth=2, markersize=5)
        ax.plot(self.periods, self.disp.yaw.amplitude[:, head_seas_idx], '^-',
               label='Yaw', linewidth=2, markersize=5)
        ax.set_xlabel('Wave Period (s)', fontsize=12)
        ax.set_ylabel('Amplitude (deg/m)', fontsize=12)
        ax.set_title('Rotation DOF - Head Seas (180°)', fontsize=13, fontweight='bold')
        ax.grid(True, alpha=0.3, which='major', linestyle='-', linewidth=0.8)
        ax.grid(True, alpha=0.15, which='minor', linestyle=':', linewidth=0.5)
        ax.minorticks_on()
        ax.legend(loc='best', fontsize=11)
        # Wave period increasing left to right (no invert)

        plt.tight_layout()
        plt.savefig(self.output_dir / 'rao_translation_rotation.png', dpi=300, bbox_inches='tight')
        print(f'Saved: {self.output_dir / "rao_translation_rotation.png"}')
        plt.close()

    def generate_all_plots(self):
        """Generate all QA plots."""
        print('\n' + '='*80)
        print('GENERATING COMPREHENSIVE RAO QA PLOTS')
        print('='*80 + '\n')

        print('1. Amplitude vs Frequency (all DOF)...')
        self.plot_amplitude_vs_frequency()

        print('2. Phase vs Frequency (all DOF)...')
        self.plot_phase_vs_frequency()

        print('3. Polar directional response...')
        self.plot_polar_response()

        print('4. Heading comparison (key headings)...')
        self.plot_heading_comparison()

        print('5. Translation vs Rotation DOF...')
        self.plot_translation_vs_rotation()

        print('\n' + '='*80)
        print(f'ALL PLOTS SAVED TO: {self.output_dir}')
        print('='*80 + '\n')


def main():
    """Main function to generate all RAO QA plots."""
    import sys
    from pathlib import Path

    # Get repo root
    repo_root = Path(__file__).parent.parent.parent.parent.parent

    # RAO file path
    rao_file = repo_root / 'specs' / 'modules' / 'aqwa' / 'ship-analysis' / 'go-by-ship-raos' / '001_SHIP_RAOS_REV3.LIS'

    if not rao_file.exists():
        print(f'Error: RAO file not found: {rao_file}')
        sys.exit(1)

    # Create plotter and generate all plots
    plotter = RAOPlotter(str(rao_file))
    plotter.generate_all_plots()

    print('QA plotting complete!')


if __name__ == '__main__':
    main()
