"""
S-N Curve Plotter Module

This module provides functionality to plot and compare fatigue S-N curves
from the database, similar to the reference Excel file capabilities.

Features:
- Log-log and linear-log plotting
- With/without stress concentration factors (SCF)
- With/without fatigue limits
- Multiple curve comparison
- Export to various formats
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from typing import List, Dict, Optional, Tuple, Union
from pathlib import Path


class SNCurvePlotter:
    """
    S-N Curve Plotter for fatigue analysis.

    Generates S-N curve plots with various options matching the reference
    Excel file functionality.
    """

    def __init__(self, data_path: Optional[str] = None):
        """
        Initialize the S-N curve plotter.

        Args:
            data_path: Path to fatigue_curves_structured.csv. If None, uses default path.
        """
        if data_path is None:
            # Default to repository location
            data_path = Path(__file__).parent.parent.parent.parent / "data" / "fatigue" / "fatigue_curves_structured.csv"

        self.df = pd.read_csv(data_path)
        self._validate_data()

    def _validate_data(self):
        """Validate that required columns exist in the dataset."""
        required_cols = ['Lookup Index', 'Curve Type', '# of Slopes']
        missing = [col for col in required_cols if col not in self.df.columns]
        if missing:
            raise ValueError(f"Missing required columns: {missing}")

    def get_curve_data(self, lookup_index: int) -> Dict:
        """
        Get S-N curve parameters for a specific lookup index.

        Args:
            lookup_index: The lookup index of the curve

        Returns:
            Dictionary containing curve parameters
        """
        curve = self.df[self.df['Lookup Index'] == lookup_index]
        if curve.empty:
            raise ValueError(f"Lookup index {lookup_index} not found")

        return curve.iloc[0].to_dict()

    def calculate_sn_points(
        self,
        lookup_index: int,
        n_min: float = 1e3,
        n_max: float = 1e10,
        num_points: int = 1000,
        scf: float = 1.0,
        include_fatigue_limit: bool = True
    ) -> Tuple[np.ndarray, np.ndarray]:
        """
        Calculate S-N curve data points.

        Args:
            lookup_index: The lookup index of the curve
            n_min: Minimum number of cycles
            n_max: Maximum number of cycles
            num_points: Number of points to generate
            scf: Stress concentration factor (default 1.0)
            include_fatigue_limit: Whether to include fatigue limit (cut-off)

        Returns:
            Tuple of (N_cycles, stress_range_MPa) arrays
        """
        curve = self.get_curve_data(lookup_index)
        num_slopes = int(curve['# of Slopes'])

        # Initialize arrays
        N_all = []
        S_all = []

        # Generate points for each slope
        for slope_idx in range(1, num_slopes + 1):
            log_a = curve.get(f'Log a{slope_idx}~', np.nan)
            m = curve.get(f'm{slope_idx}', np.nan)

            # Convert to float if string
            try:
                log_a = float(log_a) if not pd.isna(log_a) else np.nan
                m = float(m) if not pd.isna(m) else np.nan
            except (ValueError, TypeError):
                continue

            if pd.isna(log_a) or pd.isna(m):
                continue

            # Determine N range for this slope
            if slope_idx == 1:
                n_start = n_min
            else:
                n_start = curve.get(f'Transfer Cycles {slope_idx-1}', n_min)
                try:
                    n_start = float(n_start) if not pd.isna(n_start) else n_min
                except (ValueError, TypeError):
                    n_start = n_min

            if slope_idx < num_slopes:
                n_end = curve.get(f'Transfer Cycles {slope_idx}', n_max)
                try:
                    n_end = float(n_end) if not pd.isna(n_end) else n_max
                except (ValueError, TypeError):
                    n_end = n_max
            else:
                n_end = n_max

            # Generate points for this slope
            N_slope = np.logspace(np.log10(n_start), np.log10(n_end), num_points // num_slopes)

            # Calculate stress range: S = (10^log_a / N)^(1/m)
            a_value = 10 ** log_a
            S_slope = (a_value / N_slope) ** (1 / m)

            N_all.extend(N_slope)
            S_all.extend(S_slope)

        # Convert to arrays and sort
        N_array = np.array(N_all)
        S_array = np.array(S_all)
        sort_idx = np.argsort(N_array)
        N_array = N_array[sort_idx]
        S_array = S_array[sort_idx]

        # Apply SCF
        S_array = S_array / scf

        # Apply fatigue limit if requested
        if include_fatigue_limit and num_slopes > 1:
            # Get the last transfer stress as fatigue limit
            fatigue_limit = curve.get(f'Transfer Stress {num_slopes-1} (Mpa)', None)
            if fatigue_limit is not None and not pd.isna(fatigue_limit):
                try:
                    fatigue_limit = float(fatigue_limit)
                    # Set stress below fatigue limit to the limit value
                    S_array = np.maximum(S_array, fatigue_limit / scf)
                except (ValueError, TypeError):
                    pass  # Skip fatigue limit if cannot convert

        return N_array, S_array

    def plot_curves(
        self,
        lookup_indices: Union[int, List[int]],
        scf: float = 1.0,
        include_fatigue_limit: bool = True,
        plot_type: str = 'log-log',
        figsize: Tuple[int, int] = (12, 8),
        title: Optional[str] = None,
        save_path: Optional[str] = None,
        show_plot: bool = True
    ) -> plt.Figure:
        """
        Plot S-N curves for comparison.

        Args:
            lookup_indices: Single index or list of indices to plot
            scf: Stress concentration factor
            include_fatigue_limit: Whether to include fatigue limit
            plot_type: 'log-log' or 'linear-log'
            figsize: Figure size tuple
            title: Plot title (auto-generated if None)
            save_path: Path to save figure (None = don't save)
            show_plot: Whether to display the plot

        Returns:
            Matplotlib figure object
        """
        if isinstance(lookup_indices, int):
            lookup_indices = [lookup_indices]

        fig, ax = plt.subplots(figsize=figsize)

        # Plot each curve
        for idx in lookup_indices:
            try:
                N, S = self.calculate_sn_points(
                    idx,
                    scf=scf,
                    include_fatigue_limit=include_fatigue_limit
                )

                curve_info = self.get_curve_data(idx)
                label = f"{curve_info['Curve Type']} - {curve_info['In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater']}"

                ax.plot(N, S, linewidth=2, label=label)

            except Exception as e:
                print(f"Warning: Could not plot curve {idx}: {e}")
                continue

        # Set scales
        ax.set_xscale('log')
        if plot_type == 'log-log':
            ax.set_yscale('log')

        # Labels and formatting
        ax.set_xlabel('Number of Cycles (N)', fontsize=12, fontweight='bold')
        ax.set_ylabel('Stress Range (MPa)', fontsize=12, fontweight='bold')

        if title is None:
            scf_text = f"with SCF={scf}" if scf != 1.0 else "without SCF"
            limit_text = "with" if include_fatigue_limit else "without"
            title = f'S-N Curve Comparison ({plot_type}) - {scf_text}, {limit_text} fatigue limit'

        ax.set_title(title, fontsize=14, fontweight='bold', pad=20)

        # Grid
        ax.grid(True, which='both', linestyle='--', linewidth=0.5, alpha=0.7)

        # Legend
        ax.legend(loc='best', fontsize=10, framealpha=0.9)

        # Tight layout
        plt.tight_layout()

        # Save if requested
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            print(f"Figure saved to: {save_path}")

        # Show if requested
        if show_plot:
            plt.show()

        return fig

    def list_curves(
        self,
        curve_type_filter: Optional[str] = None,
        environment_filter: Optional[str] = None,
        joint_type_filter: Optional[str] = None
    ) -> pd.DataFrame:
        """
        List available curves with optional filtering.

        Args:
            curve_type_filter: Filter by curve type (e.g., 'DNV', 'API')
            environment_filter: Filter by environment
            joint_type_filter: Filter by joint type

        Returns:
            Filtered dataframe of curves
        """
        df_filtered = self.df.copy()

        if curve_type_filter:
            df_filtered = df_filtered[
                df_filtered['Curve Type'].str.contains(curve_type_filter, case=False, na=False)
            ]

        if environment_filter:
            df_filtered = df_filtered[
                df_filtered['In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater'].str.contains(
                    environment_filter, case=False, na=False
                )
            ]

        if joint_type_filter:
            df_filtered = df_filtered[
                df_filtered['Joint Type'].str.contains(joint_type_filter, case=False, na=False)
            ]

        columns_to_show = [
            'Lookup Index',
            'Curve Type',
            'Joint Type',
            'In air (or) Corrosion Protected in seawater (or) Free Corrosion in seawater',
            '# of Slopes'
        ]

        return df_filtered[columns_to_show]

    def create_comparison_plot(
        self,
        reference_index: int,
        comparison_indices: List[int],
        scf: float = 1.0,
        include_fatigue_limit: bool = True,
        plot_type: str = 'log-log',
        save_path: Optional[str] = None
    ) -> plt.Figure:
        """
        Create a comparison plot highlighting a reference curve.

        Args:
            reference_index: The reference curve to highlight
            comparison_indices: List of curves to compare against
            scf: Stress concentration factor
            include_fatigue_limit: Whether to include fatigue limit
            plot_type: 'log-log' or 'linear-log'
            save_path: Path to save figure

        Returns:
            Matplotlib figure object
        """
        fig, ax = plt.subplots(figsize=(14, 9))

        # Plot comparison curves first (lighter colors)
        for idx in comparison_indices:
            try:
                N, S = self.calculate_sn_points(
                    idx,
                    scf=scf,
                    include_fatigue_limit=include_fatigue_limit
                )

                curve_info = self.get_curve_data(idx)
                label = f"{curve_info['Curve Type']}"

                ax.plot(N, S, linewidth=1.5, alpha=0.6, label=label)

            except Exception as e:
                print(f"Warning: Could not plot curve {idx}: {e}")
                continue

        # Plot reference curve last (highlighted)
        try:
            N_ref, S_ref = self.calculate_sn_points(
                reference_index,
                scf=scf,
                include_fatigue_limit=include_fatigue_limit
            )

            ref_info = self.get_curve_data(reference_index)
            ref_label = f"{ref_info['Curve Type']} (REFERENCE)"

            ax.plot(N_ref, S_ref, linewidth=3, color='red', label=ref_label, zorder=10)

        except Exception as e:
            print(f"Error: Could not plot reference curve {reference_index}: {e}")

        # Set scales
        ax.set_xscale('log')
        if plot_type == 'log-log':
            ax.set_yscale('log')

        # Labels and formatting
        ax.set_xlabel('Number of Cycles (N)', fontsize=12, fontweight='bold')
        ax.set_ylabel('Stress Range (MPa)', fontsize=12, fontweight='bold')

        scf_text = f"with SCF={scf}" if scf != 1.0 else "without SCF"
        limit_text = "with" if include_fatigue_limit else "without"
        title = f'S-N Curve Comparison ({plot_type}) - {scf_text}, {limit_text} fatigue limit'

        ax.set_title(title, fontsize=14, fontweight='bold', pad=20)

        # Grid
        ax.grid(True, which='both', linestyle='--', linewidth=0.5, alpha=0.7)

        # Legend
        ax.legend(loc='best', fontsize=9, framealpha=0.9, ncol=2)

        # Tight layout
        plt.tight_layout()

        # Save if requested
        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            print(f"Figure saved to: {save_path}")

        return fig


def main():
    """Example usage of SNCurvePlotter."""
    # Initialize plotter
    plotter = SNCurvePlotter()

    # List available DNV curves in air
    print("Available DNV curves in air:")
    print(plotter.list_curves(curve_type_filter='DNV', environment_filter='Air'))

    # Plot some example curves
    dnv_indices = [1, 5, 10, 15]  # Example DNV curve indices
    plotter.plot_curves(
        lookup_indices=dnv_indices,
        scf=1.0,
        include_fatigue_limit=True,
        plot_type='log-log',
        title='DNV S-N Curves Comparison',
        save_path='dnv_curves_comparison.png'
    )


if __name__ == '__main__':
    main()
