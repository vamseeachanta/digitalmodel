"""
Hydrodynamic Coefficients Visualization Module

Comprehensive plotting capabilities for hydrodynamic coefficient analysis including
frequency response plots, heatmaps, 3D surfaces, and animations.

Classes:
    HydrodynamicPlotter: Main plotting interface for coefficient visualization
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
from matplotlib.animation import FuncAnimation, PillowWriter
from mpl_toolkits.mplot3d import Axes3D
import seaborn as sns
from pathlib import Path
from typing import Union, Optional, List, Tuple, Dict
import warnings

from .coefficients import CoefficientDatabase, DOF_NAMES


class HydrodynamicPlotter:
    """Main plotting interface for hydrodynamic coefficient visualization.

    Provides 8+ comprehensive visualization methods for frequency-dependent
    hydrodynamic coefficients.
    """

    def __init__(self, database: CoefficientDatabase):
        """Initialize plotter with coefficient database.

        Args:
            database: CoefficientDatabase instance with loaded coefficients
        """
        self.db = database
        self.db._build_interpolators()

        # Set default plotting style
        plt.style.use('seaborn-v0_8-darkgrid')
        sns.set_palette("husl")

    def plot_frequency_response(self,
                                dof: Union[int, str],
                                coefficient_type: str = 'both',
                                coupled: bool = False,
                                save_path: Optional[Union[str, Path]] = None,
                                figsize: Tuple[int, int] = (12, 6)) -> plt.Figure:
        """Plot frequency response for a specific DOF.

        Args:
            dof: Degree of freedom (0-5 or 'Surge', 'Sway', etc.)
            coefficient_type: 'added_mass', 'damping', or 'both'
            coupled: If True, plot all coupling terms; if False, only diagonal
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        dof_idx = DOF_NAMES.index(dof) if isinstance(dof, str) else dof
        dof_name = DOF_NAMES[dof_idx]

        # Create figure
        if coefficient_type == 'both':
            fig, (ax1, ax2) = plt.subplots(1, 2, figsize=figsize)
            axes = [ax1, ax2]
        else:
            fig, ax = plt.subplots(figsize=figsize)
            axes = [ax]

        # Frequency array for plotting
        freq_plot = np.linspace(
            self.db.frequencies.min(),
            self.db.frequencies.max(),
            200
        )

        # Plot added mass
        if coefficient_type in ['added_mass', 'both']:
            ax_idx = 0 if coefficient_type == 'both' else 0
            ax = axes[ax_idx]

            if coupled:
                # Plot all coupling terms
                for j in range(6):
                    values = np.array([
                        self.db.get_added_mass(f, dof_idx, j) for f in freq_plot
                    ])
                    ax.plot(freq_plot, values, label=f'{dof_name}-{DOF_NAMES[j]}',
                           linewidth=2)
            else:
                # Plot only diagonal term
                values = np.array([
                    self.db.get_added_mass(f, dof_idx, dof_idx) for f in freq_plot
                ])
                ax.plot(freq_plot, values, linewidth=2, color='navy')

            ax.set_xlabel('Frequency (rad/s)', fontsize=12)
            ax.set_ylabel('Added Mass', fontsize=12)
            ax.set_title(f'Added Mass - {dof_name}', fontsize=14, fontweight='bold')
            ax.grid(True, alpha=0.3)
            if coupled:
                ax.legend(loc='best', fontsize=9)

        # Plot damping
        if coefficient_type in ['damping', 'both']:
            ax_idx = 1 if coefficient_type == 'both' else 0
            ax = axes[ax_idx]

            if coupled:
                # Plot all coupling terms
                for j in range(6):
                    values = np.array([
                        self.db.get_damping(f, dof_idx, j) for f in freq_plot
                    ])
                    ax.plot(freq_plot, values, label=f'{dof_name}-{DOF_NAMES[j]}',
                           linewidth=2)
            else:
                # Plot only diagonal term
                values = np.array([
                    self.db.get_damping(f, dof_idx, dof_idx) for f in freq_plot
                ])
                ax.plot(freq_plot, values, linewidth=2, color='darkred')

            ax.set_xlabel('Frequency (rad/s)', fontsize=12)
            ax.set_ylabel('Damping', fontsize=12)
            ax.set_title(f'Damping - {dof_name}', fontsize=14, fontweight='bold')
            ax.grid(True, alpha=0.3)
            if coupled:
                ax.legend(loc='best', fontsize=9)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    def plot_coefficient_matrix(self,
                               frequency: float,
                               coefficient_type: str = 'added_mass',
                               save_path: Optional[Union[str, Path]] = None,
                               figsize: Tuple[int, int] = (10, 8)) -> plt.Figure:
        """Plot 6×6 coefficient matrix heatmap at given frequency.

        Args:
            frequency: Wave frequency in rad/s
            coefficient_type: 'added_mass' or 'damping'
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        # Get matrix at frequency
        if coefficient_type == 'added_mass':
            matrix = self.db.get_added_mass_matrix(frequency)
            title = f'Added Mass Matrix at ω = {frequency:.3f} rad/s'
            cmap = 'RdYlBu_r'
        else:
            matrix = self.db.get_damping_matrix(frequency)
            title = f'Damping Matrix at ω = {frequency:.3f} rad/s'
            cmap = 'YlOrRd'

        # Create heatmap
        fig, ax = plt.subplots(figsize=figsize)

        im = ax.imshow(matrix, cmap=cmap, aspect='auto')

        # Set ticks and labels
        ax.set_xticks(np.arange(6))
        ax.set_yticks(np.arange(6))
        ax.set_xticklabels(DOF_NAMES, fontsize=11)
        ax.set_yticklabels(DOF_NAMES, fontsize=11)

        # Rotate x labels
        plt.setp(ax.get_xticklabels(), rotation=45, ha="right", rotation_mode="anchor")

        # Add colorbar
        cbar = plt.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
        cbar.set_label('Coefficient Value', fontsize=11)

        # Add value annotations
        for i in range(6):
            for j in range(6):
                text = ax.text(j, i, f'{matrix[i, j]:.1f}',
                             ha="center", va="center", color="black", fontsize=9)

        ax.set_title(title, fontsize=14, fontweight='bold', pad=15)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    def plot_added_mass_surface(self,
                               dof_i: Union[int, str],
                               dof_j: Union[int, str],
                               save_path: Optional[Union[str, Path]] = None,
                               figsize: Tuple[int, int] = (12, 8)) -> plt.Figure:
        """Plot 3D surface of added mass for DOF pair vs frequency.

        Args:
            dof_i: First DOF (0-5 or 'Surge', 'Sway', etc.)
            dof_j: Second DOF (0-5 or 'Surge', 'Sway', etc.)
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        i = DOF_NAMES.index(dof_i) if isinstance(dof_i, str) else dof_i
        j = DOF_NAMES.index(dof_j) if isinstance(dof_j, str) else dof_j

        # Create frequency grid
        freq_fine = np.linspace(
            self.db.frequencies.min(),
            self.db.frequencies.max(),
            100
        )

        # Get added mass values
        values = np.array([
            self.db.get_added_mass(f, i, j) for f in freq_fine
        ])

        # Create meshgrid for 3D plot
        X = freq_fine
        Y = np.zeros_like(freq_fine)
        Z = values

        # Create 3D surface
        fig = plt.figure(figsize=figsize)
        ax = fig.add_subplot(111, projection='3d')

        # Plot surface
        ax.plot(X, Y, Z, linewidth=3, color='navy')
        ax.scatter(self.db.frequencies,
                  np.zeros_like(self.db.frequencies),
                  [self.db.get_added_mass(f, i, j) for f in self.db.frequencies],
                  c='red', s=50, alpha=0.6, label='Data Points')

        ax.set_xlabel('Frequency (rad/s)', fontsize=11, labelpad=10)
        ax.set_ylabel('', fontsize=11)
        ax.set_zlabel('Added Mass', fontsize=11, labelpad=10)
        ax.set_title(f'Added Mass Surface: {DOF_NAMES[i]}-{DOF_NAMES[j]}',
                    fontsize=14, fontweight='bold', pad=15)
        ax.legend(loc='upper right')

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    def plot_damping_surface(self,
                            dof_i: Union[int, str],
                            dof_j: Union[int, str],
                            save_path: Optional[Union[str, Path]] = None,
                            figsize: Tuple[int, int] = (12, 8)) -> plt.Figure:
        """Plot 3D surface of damping for DOF pair vs frequency.

        Args:
            dof_i: First DOF (0-5 or 'Surge', 'Sway', etc.)
            dof_j: Second DOF (0-5 or 'Surge', 'Sway', etc.)
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        i = DOF_NAMES.index(dof_i) if isinstance(dof_i, str) else dof_i
        j = DOF_NAMES.index(dof_j) if isinstance(dof_j, str) else dof_j

        # Create frequency grid
        freq_fine = np.linspace(
            self.db.frequencies.min(),
            self.db.frequencies.max(),
            100
        )

        # Get damping values
        values = np.array([
            self.db.get_damping(f, i, j) for f in freq_fine
        ])

        # Create 3D surface
        fig = plt.figure(figsize=figsize)
        ax = fig.add_subplot(111, projection='3d')

        # Plot surface
        ax.plot(freq_fine, np.zeros_like(freq_fine), values,
               linewidth=3, color='darkred')
        ax.scatter(self.db.frequencies,
                  np.zeros_like(self.db.frequencies),
                  [self.db.get_damping(f, i, j) for f in self.db.frequencies],
                  c='orange', s=50, alpha=0.6, label='Data Points')

        ax.set_xlabel('Frequency (rad/s)', fontsize=11, labelpad=10)
        ax.set_ylabel('', fontsize=11)
        ax.set_zlabel('Damping', fontsize=11, labelpad=10)
        ax.set_title(f'Damping Surface: {DOF_NAMES[i]}-{DOF_NAMES[j]}',
                    fontsize=14, fontweight='bold', pad=15)
        ax.legend(loc='upper right')

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    def plot_cross_coupling(self,
                          coefficient_type: str = 'added_mass',
                          threshold: float = 0.1,
                          save_path: Optional[Union[str, Path]] = None,
                          figsize: Tuple[int, int] = (14, 8)) -> plt.Figure:
        """Plot off-diagonal coupling terms across frequencies.

        Args:
            coefficient_type: 'added_mass' or 'damping'
            threshold: Minimum coefficient magnitude to plot
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        fig, ax = plt.subplots(figsize=figsize)

        freq_plot = np.linspace(
            self.db.frequencies.min(),
            self.db.frequencies.max(),
            200
        )

        # Plot off-diagonal terms
        for i in range(6):
            for j in range(i + 1, 6):  # Upper triangular only
                if coefficient_type == 'added_mass':
                    values = np.array([
                        self.db.get_added_mass(f, i, j) for f in freq_plot
                    ])
                else:
                    values = np.array([
                        self.db.get_damping(f, i, j) for f in freq_plot
                    ])

                # Plot if exceeds threshold
                if np.max(np.abs(values)) > threshold:
                    ax.plot(freq_plot, values,
                           label=f'{DOF_NAMES[i]}-{DOF_NAMES[j]}',
                           linewidth=2, alpha=0.7)

        ax.set_xlabel('Frequency (rad/s)', fontsize=12)
        ax.set_ylabel(f'{"Added Mass" if coefficient_type == "added_mass" else "Damping"} Coefficient',
                     fontsize=12)
        ax.set_title(f'Cross-Coupling Terms - {coefficient_type.replace("_", " ").title()}',
                    fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend(loc='best', fontsize=9, ncol=2)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    def plot_critical_damping(self,
                            mass_matrix: np.ndarray,
                            stiffness_matrix: np.ndarray,
                            save_path: Optional[Union[str, Path]] = None,
                            figsize: Tuple[int, int] = (12, 6)) -> plt.Figure:
        """Plot critical damping ratios for all DOFs.

        Args:
            mass_matrix: 6×6 mass matrix (including added mass at reference frequency)
            stiffness_matrix: 6×6 stiffness matrix
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        fig, ax = plt.subplots(figsize=figsize)

        freq_plot = np.linspace(
            self.db.frequencies.min(),
            self.db.frequencies.max(),
            100
        )

        # Calculate critical damping ratios
        for dof_idx in range(6):
            mass = mass_matrix[dof_idx, dof_idx]
            stiffness = stiffness_matrix[dof_idx, dof_idx]

            if stiffness > 0 and mass > 0:
                ratios = np.array([
                    self.db.calculate_critical_damping_ratio(
                        mass, stiffness, f, dof_idx
                    ) for f in freq_plot
                ])

                ax.plot(freq_plot, ratios, label=DOF_NAMES[dof_idx],
                       linewidth=2)

        # Add reference lines
        ax.axhline(y=1.0, color='red', linestyle='--', linewidth=2,
                  label='Critical Damping', alpha=0.7)
        ax.axhline(y=0.7, color='orange', linestyle='--', linewidth=1,
                  label='Typical Design', alpha=0.5)

        ax.set_xlabel('Frequency (rad/s)', fontsize=12)
        ax.set_ylabel('Damping Ratio (ζ)', fontsize=12)
        ax.set_title('Critical Damping Ratios', fontsize=14, fontweight='bold')
        ax.set_ylim([0, 2])
        ax.grid(True, alpha=0.3)
        ax.legend(loc='best', fontsize=10, ncol=2)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    def animate_frequency_sweep(self,
                               coefficient_type: str = 'added_mass',
                               save_path: Optional[Union[str, Path]] = None,
                               fps: int = 5,
                               figsize: Tuple[int, int] = (10, 8)) -> None:
        """Create animated GIF of matrix evolution across frequencies.

        Args:
            coefficient_type: 'added_mass' or 'damping'
            save_path: Path to save GIF animation
            fps: Frames per second
            figsize: Figure size
        """
        if save_path is None:
            save_path = f'hydro_{coefficient_type}_animation.gif'

        # Create figure
        fig, ax = plt.subplots(figsize=figsize)

        # Determine global color scale
        all_values = []
        for freq in self.db.frequencies:
            if coefficient_type == 'added_mass':
                matrix = self.db.get_added_mass_matrix(freq)
            else:
                matrix = self.db.get_damping_matrix(freq)
            all_values.extend(matrix.flatten())

        vmin, vmax = np.min(all_values), np.max(all_values)

        def update(frame):
            ax.clear()
            freq = self.db.frequencies[frame]

            if coefficient_type == 'added_mass':
                matrix = self.db.get_added_mass_matrix(freq)
                title = f'Added Mass Matrix - ω = {freq:.3f} rad/s'
                cmap = 'RdYlBu_r'
            else:
                matrix = self.db.get_damping_matrix(freq)
                title = f'Damping Matrix - ω = {freq:.3f} rad/s'
                cmap = 'YlOrRd'

            im = ax.imshow(matrix, cmap=cmap, aspect='auto', vmin=vmin, vmax=vmax)

            ax.set_xticks(np.arange(6))
            ax.set_yticks(np.arange(6))
            ax.set_xticklabels(DOF_NAMES, fontsize=10)
            ax.set_yticklabels(DOF_NAMES, fontsize=10)
            plt.setp(ax.get_xticklabels(), rotation=45, ha="right", rotation_mode="anchor")

            ax.set_title(title, fontsize=13, fontweight='bold')

            return [im]

        anim = FuncAnimation(fig, update, frames=len(self.db.frequencies),
                           interval=1000/fps, blit=True, repeat=True)

        writer = PillowWriter(fps=fps)
        anim.save(save_path, writer=writer)
        plt.close(fig)

        print(f"Animation saved to: {save_path}")

    def plot_matrix_correlation(self,
                               coefficient_type: str = 'added_mass',
                               save_path: Optional[Union[str, Path]] = None,
                               figsize: Tuple[int, int] = (12, 10)) -> plt.Figure:
        """Plot correlation matrix of coefficients across frequencies.

        Args:
            coefficient_type: 'added_mass' or 'damping'
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        # Collect all coefficient time series
        data = {}
        for i in range(6):
            for j in range(6):
                key = f'{DOF_NAMES[i]}-{DOF_NAMES[j]}'
                if coefficient_type == 'added_mass':
                    values = np.array([
                        self.db.get_added_mass(f, i, j)
                        for f in self.db.frequencies
                    ])
                else:
                    values = np.array([
                        self.db.get_damping(f, i, j)
                        for f in self.db.frequencies
                    ])
                data[key] = values

        # Create DataFrame and correlation matrix
        df = pd.DataFrame(data)
        corr_matrix = df.corr()

        # Plot correlation heatmap
        fig, ax = plt.subplots(figsize=figsize)

        sns.heatmap(corr_matrix, annot=False, cmap='coolwarm', center=0,
                   square=True, linewidths=0.5, cbar_kws={"shrink": 0.8},
                   ax=ax, vmin=-1, vmax=1)

        title = f'Coefficient Correlation Matrix - {coefficient_type.replace("_", " ").title()}'
        ax.set_title(title, fontsize=14, fontweight='bold', pad=15)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    def plot_all_dofs_comparison(self,
                                coefficient_type: str = 'added_mass',
                                save_path: Optional[Union[str, Path]] = None,
                                figsize: Tuple[int, int] = (16, 10)) -> plt.Figure:
        """Plot comparison of all diagonal terms (uncoupled coefficients).

        Args:
            coefficient_type: 'added_mass' or 'damping'
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        fig, axes = plt.subplots(2, 3, figsize=figsize)
        axes = axes.flatten()

        freq_plot = np.linspace(
            self.db.frequencies.min(),
            self.db.frequencies.max(),
            200
        )

        for dof_idx in range(6):
            ax = axes[dof_idx]

            if coefficient_type == 'added_mass':
                values = np.array([
                    self.db.get_added_mass(f, dof_idx, dof_idx)
                    for f in freq_plot
                ])
                color = 'navy'
            else:
                values = np.array([
                    self.db.get_damping(f, dof_idx, dof_idx)
                    for f in freq_plot
                ])
                color = 'darkred'

            ax.plot(freq_plot, values, linewidth=2, color=color)
            ax.scatter(self.db.frequencies,
                      [self.db.get_added_mass(f, dof_idx, dof_idx) if coefficient_type == 'added_mass'
                       else self.db.get_damping(f, dof_idx, dof_idx)
                       for f in self.db.frequencies],
                      alpha=0.5, s=30, color='red')

            ax.set_xlabel('Frequency (rad/s)', fontsize=10)
            ax.set_ylabel(f'{"Added Mass" if coefficient_type == "added_mass" else "Damping"}',
                         fontsize=10)
            ax.set_title(f'{DOF_NAMES[dof_idx]}', fontsize=12, fontweight='bold')
            ax.grid(True, alpha=0.3)

        fig.suptitle(f'All DOFs - {coefficient_type.replace("_", " ").title()}',
                    fontsize=16, fontweight='bold', y=1.00)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig

    @staticmethod
    def create_comparison_plot(databases: Dict[str, CoefficientDatabase],
                              dof: Union[int, str],
                              coefficient_type: str = 'added_mass',
                              save_path: Optional[Union[str, Path]] = None,
                              figsize: Tuple[int, int] = (12, 6)) -> plt.Figure:
        """Create comparison plot of multiple databases.

        Args:
            databases: Dictionary mapping labels to CoefficientDatabase instances
            dof: Degree of freedom to compare
            coefficient_type: 'added_mass' or 'damping'
            save_path: Path to save figure
            figsize: Figure size

        Returns:
            Matplotlib figure object
        """
        fig, ax = plt.subplots(figsize=figsize)

        dof_idx = DOF_NAMES.index(dof) if isinstance(dof, str) else dof

        for label, db in databases.items():
            db._build_interpolators()
            freq_plot = np.linspace(
                db.frequencies.min(),
                db.frequencies.max(),
                200
            )

            if coefficient_type == 'added_mass':
                values = np.array([
                    db.get_added_mass(f, dof_idx, dof_idx) for f in freq_plot
                ])
            else:
                values = np.array([
                    db.get_damping(f, dof_idx, dof_idx) for f in freq_plot
                ])

            ax.plot(freq_plot, values, label=label, linewidth=2, alpha=0.8)

        ax.set_xlabel('Frequency (rad/s)', fontsize=12)
        ax.set_ylabel(f'{"Added Mass" if coefficient_type == "added_mass" else "Damping"}',
                     fontsize=12)
        ax.set_title(f'{DOF_NAMES[dof_idx]} - Database Comparison',
                    fontsize=14, fontweight='bold')
        ax.grid(True, alpha=0.3)
        ax.legend(loc='best', fontsize=11)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')

        return fig
