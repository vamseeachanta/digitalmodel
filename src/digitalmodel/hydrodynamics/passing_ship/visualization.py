"""
Visualization module for passing ship forces.

Provides plotting capabilities for force distributions, parametric studies,
and comparison visualizations with interactive features and export options.
"""

import numpy as np
import matplotlib.pyplot as plt
from matplotlib.figure import Figure
from matplotlib.axes import Axes
from matplotlib.gridspec import GridSpec
from matplotlib.widgets import Slider, Button, CheckButtons
from mpl_toolkits.mplot3d import Axes3D
from mpl_toolkits.axes_grid1 import make_axes_locatable
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Union, Any, Callable
import logging

# Try to import mplcursors, fall back if not available
try:
    import mplcursors
    HAS_MPLCURSORS = True
except ImportError:
    HAS_MPLCURSORS = False

from .calculator import ForceResults

logger = logging.getLogger(__name__)

# Configure matplotlib for better appearance
plt.style.use('seaborn-v0_8-darkgrid')
plt.rcParams['figure.dpi'] = 100
plt.rcParams['savefig.dpi'] = 150
plt.rcParams['font.size'] = 10
plt.rcParams['axes.labelsize'] = 12
plt.rcParams['axes.titlesize'] = 14
plt.rcParams['legend.fontsize'] = 10


class ForceDistributionPlotter:
    """Handles plotting of force distributions vs stagger distance."""
    
    def __init__(self, figsize: Tuple[float, float] = (12, 10)):
        """
        Initialize force distribution plotter.
        
        Args:
            figsize: Default figure size for plots
        """
        self.figsize = figsize
        self.colors = plt.cm.tab10(np.linspace(0, 1, 10))
    
    def plot_force_distribution(
        self,
        staggers: np.ndarray,
        forces: List[ForceResults],
        title: str = "Passing Ship Force Distribution",
        label: Optional[str] = None,
        fig: Optional[Figure] = None
    ) -> Figure:
        """
        Plot force distributions for surge, sway, and yaw.
        
        Args:
            staggers: Array of stagger distances (m)
            forces: List of ForceResults for each stagger
            title: Overall plot title
            label: Label for the data series
            fig: Existing figure to plot on (creates new if None)
        
        Returns:
            Matplotlib figure with force distribution plots
        """
        if fig is None:
            fig = plt.figure(figsize=self.figsize)
        
        # Extract force components
        surge_forces = np.array([f.surge for f in forces])
        sway_forces = np.array([f.sway for f in forces])
        yaw_moments = np.array([f.yaw for f in forces])
        
        # Create subplots
        gs = GridSpec(3, 1, figure=fig, hspace=0.3)
        
        # Surge force plot
        ax1 = fig.add_subplot(gs[0, 0])
        ax1.plot(staggers, surge_forces/1e6, 'b-', linewidth=2, label=label)
        ax1.set_xlabel('Stagger Distance (m)')
        ax1.set_ylabel('Surge Force (MN)')
        ax1.set_title('Surge Force Distribution')
        ax1.grid(True, alpha=0.3)
        ax1.axhline(y=0, color='k', linestyle='-', alpha=0.3)
        if label:
            ax1.legend(loc='best')
        
        # Sway force plot  
        ax2 = fig.add_subplot(gs[1, 0])
        ax2.plot(staggers, sway_forces/1e6, 'r-', linewidth=2, label=label)
        ax2.set_xlabel('Stagger Distance (m)')
        ax2.set_ylabel('Sway Force (MN)')
        ax2.set_title('Sway Force Distribution')
        ax2.grid(True, alpha=0.3)
        ax2.axhline(y=0, color='k', linestyle='-', alpha=0.3)
        if label:
            ax2.legend(loc='best')
        
        # Yaw moment plot
        ax3 = fig.add_subplot(gs[2, 0])
        ax3.plot(staggers, yaw_moments/1e6, 'g-', linewidth=2, label=label)
        ax3.set_xlabel('Stagger Distance (m)')
        ax3.set_ylabel('Yaw Moment (MN·m)')
        ax3.set_title('Yaw Moment Distribution')
        ax3.grid(True, alpha=0.3)
        ax3.axhline(y=0, color='k', linestyle='-', alpha=0.3)
        if label:
            ax3.legend(loc='best')
        
        # Add main title
        fig.suptitle(title, fontsize=16, fontweight='bold')
        
        # Add vertical line at zero stagger
        for ax in [ax1, ax2, ax3]:
            ax.axvline(x=0, color='gray', linestyle='--', alpha=0.5)
        
        return fig
    
    def plot_multiple_distributions(
        self,
        staggers: np.ndarray,
        datasets: List[Tuple[List[ForceResults], str]],
        title: str = "Force Distribution Comparison",
        figsize: Optional[Tuple[float, float]] = None
    ) -> Figure:
        """
        Plot multiple force distributions for comparison.
        
        Args:
            staggers: Array of stagger distances
            datasets: List of (forces, label) tuples
            title: Overall plot title
            figsize: Figure size (uses default if None)
        
        Returns:
            Figure with comparison plots
        """
        fig = plt.figure(figsize=figsize or self.figsize)
        gs = GridSpec(3, 1, figure=fig, hspace=0.3)
        
        axes = [
            fig.add_subplot(gs[0, 0]),
            fig.add_subplot(gs[1, 0]),
            fig.add_subplot(gs[2, 0])
        ]
        
        for idx, (forces, label) in enumerate(datasets):
            color = self.colors[idx % len(self.colors)]
            
            surge = np.array([f.surge for f in forces]) / 1e6
            sway = np.array([f.sway for f in forces]) / 1e6
            yaw = np.array([f.yaw for f in forces]) / 1e6
            
            axes[0].plot(staggers, surge, linewidth=2, label=label, color=color)
            axes[1].plot(staggers, sway, linewidth=2, label=label, color=color)
            axes[2].plot(staggers, yaw, linewidth=2, label=label, color=color)
        
        # Configure axes
        axes[0].set_ylabel('Surge Force (MN)')
        axes[0].set_title('Surge Force Comparison')
        
        axes[1].set_ylabel('Sway Force (MN)')
        axes[1].set_title('Sway Force Comparison')
        
        axes[2].set_ylabel('Yaw Moment (MN·m)')
        axes[2].set_xlabel('Stagger Distance (m)')
        axes[2].set_title('Yaw Moment Comparison')
        
        # Add common elements
        for ax in axes:
            ax.grid(True, alpha=0.3)
            ax.axhline(y=0, color='k', linestyle='-', alpha=0.3)
            ax.axvline(x=0, color='gray', linestyle='--', alpha=0.5)
            ax.legend(loc='best')
        
        fig.suptitle(title, fontsize=16, fontweight='bold')
        
        return fig


class ParametricStudyVisualizer:
    """Visualizes parametric study results."""
    
    def __init__(self):
        """Initialize parametric study visualizer."""
        self.cmap = 'viridis'
    
    def plot_2d_parametric(
        self,
        param1: np.ndarray,
        param2: np.ndarray,
        results: np.ndarray,
        xlabel: str = "Parameter 1",
        ylabel: str = "Parameter 2",
        title: str = "Parametric Study",
        cbar_label: str = "Force (N)",
        figsize: Tuple[float, float] = (10, 8)
    ) -> Figure:
        """
        Create 2D contour plot for parametric study.
        
        Args:
            param1: First parameter values
            param2: Second parameter values
            results: 2D array of results
            xlabel: Label for x-axis
            ylabel: Label for y-axis
            title: Plot title
            cbar_label: Colorbar label
            figsize: Figure size
        
        Returns:
            Figure with 2D parametric study plot
        """
        fig, ax = plt.subplots(figsize=figsize)
        
        # Create meshgrid if needed
        if results.shape == (len(param1), len(param2)):
            X, Y = np.meshgrid(param2, param1)
        else:
            X, Y = param1, param2
        
        # Create contour plot
        contour = ax.contourf(X, Y, results, levels=20, cmap=self.cmap)
        contour_lines = ax.contour(X, Y, results, levels=10, colors='black', 
                                   alpha=0.3, linewidths=0.5)
        
        # Add contour labels
        ax.clabel(contour_lines, inline=True, fontsize=8, fmt='%.0f')
        
        # Add colorbar
        divider = make_axes_locatable(ax)
        cax = divider.append_axes("right", size="5%", pad=0.1)
        cbar = fig.colorbar(contour, cax=cax)
        cbar.set_label(cbar_label)
        
        # Labels and title
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_title(title)
        ax.grid(True, alpha=0.3)
        
        return fig
    
    def plot_3d_surface(
        self,
        X: np.ndarray,
        Y: np.ndarray,
        Z: np.ndarray,
        xlabel: str = "X",
        ylabel: str = "Y",
        zlabel: str = "Z",
        title: str = "3D Surface Plot",
        figsize: Tuple[float, float] = (12, 9)
    ) -> Figure:
        """
        Create 3D surface plot for parametric study.
        
        Args:
            X, Y, Z: Meshgrid data for surface
            xlabel, ylabel, zlabel: Axis labels
            title: Plot title
            figsize: Figure size
        
        Returns:
            Figure with 3D surface plot
        """
        fig = plt.figure(figsize=figsize)
        ax = fig.add_subplot(111, projection='3d')
        
        # Create surface plot
        surf = ax.plot_surface(X, Y, Z, cmap=self.cmap, 
                              edgecolor='none', alpha=0.8)
        
        # Add contour projections
        ax.contour(X, Y, Z, zdir='z', offset=Z.min(), 
                  cmap=self.cmap, alpha=0.5)
        
        # Add colorbar
        fig.colorbar(surf, ax=ax, shrink=0.5, aspect=5)
        
        # Labels
        ax.set_xlabel(xlabel)
        ax.set_ylabel(ylabel)
        ax.set_zlabel(zlabel)
        ax.set_title(title)
        
        # Improve viewing angle
        ax.view_init(elev=30, azim=45)
        
        return fig
    
    def plot_sensitivity_analysis(
        self,
        parameters: List[str],
        sensitivities: List[float],
        title: str = "Parameter Sensitivity Analysis",
        figsize: Tuple[float, float] = (10, 6)
    ) -> Figure:
        """
        Create bar chart for sensitivity analysis.
        
        Args:
            parameters: List of parameter names
            sensitivities: List of sensitivity values
            title: Plot title
            figsize: Figure size
        
        Returns:
            Figure with sensitivity analysis
        """
        fig, ax = plt.subplots(figsize=figsize)
        
        # Sort by sensitivity
        sorted_idx = np.argsort(sensitivities)[::-1]
        params_sorted = [parameters[i] for i in sorted_idx]
        sens_sorted = [sensitivities[i] for i in sorted_idx]
        
        # Create bar chart
        bars = ax.bar(params_sorted, sens_sorted, 
                      color=plt.cm.RdYlGn_r(np.linspace(0.2, 0.8, len(parameters))))
        
        # Add value labels on bars
        for bar, val in zip(bars, sens_sorted):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{val:.2f}', ha='center', va='bottom')
        
        # Formatting
        ax.set_xlabel('Parameters')
        ax.set_ylabel('Sensitivity Index')
        ax.set_title(title)
        ax.grid(True, alpha=0.3, axis='y')
        
        # Rotate x labels if many parameters
        if len(parameters) > 5:
            plt.xticks(rotation=45, ha='right')
        
        plt.tight_layout()
        return fig


class InteractivePlotManager:
    """Manages interactive features for plots."""
    
    def enable_interactivity(self, fig: Figure) -> None:
        """
        Enable zoom, pan, and basic interactivity.

        Args:
            fig: Figure to make interactive
        """
        # This is automatically enabled with matplotlib
        # Additional configuration can be added here
        # Note: In non-interactive backends (e.g., Agg), toolbar exists
        # as an attribute but is None. Guard against that.
        toolbar = getattr(fig.canvas, 'toolbar', None)
        if toolbar is not None:
            toolbar.update()
    
    def add_data_cursor(self, fig: Figure) -> Any:
        """
        Add data cursor for viewing exact values.
        
        Args:
            fig: Figure to add cursor to
        
        Returns:
            Cursor object or None if mplcursors not available
        """
        if not HAS_MPLCURSORS:
            logger.warning("mplcursors not installed. Data cursor not available.")
            return None
            
        cursor = mplcursors.cursor(fig.axes, hover=True)
        
        @cursor.connect("add")
        def on_add(sel):
            x, y = sel.target
            sel.annotation.set_text(f'x={x:.2f}\ny={y:.2f}')
            sel.annotation.get_bbox_patch().set(alpha=0.9)
        
        return cursor
    
    def add_parameter_sliders(
        self,
        fig: Figure,
        parameters: Dict[str, Tuple[float, float, float]],
        update_function: Callable
    ) -> Dict[str, Slider]:
        """
        Add parameter adjustment sliders to figure.
        
        Args:
            fig: Figure to add sliders to
            parameters: Dict of param_name: (min, max, initial)
            update_function: Function to call on slider change
        
        Returns:
            Dictionary of slider objects
        """
        sliders = {}
        
        # Adjust plot to make room for sliders
        plt.subplots_adjust(bottom=0.15 + 0.05 * len(parameters))
        
        # Create sliders
        for idx, (name, (vmin, vmax, vinit)) in enumerate(parameters.items()):
            ax_slider = plt.axes([0.2, 0.05 + idx * 0.05, 0.6, 0.03])
            slider = Slider(ax_slider, name, vmin, vmax, valinit=vinit)
            
            # Connect update function
            slider.on_changed(lambda val: update_function(**{
                k: s.val for k, s in sliders.items()
            }))
            
            sliders[name] = slider
        
        return sliders


class ComparisonLayoutManager:
    """Manages multi-plot comparison layouts."""
    
    def create_grid_layout(
        self,
        rows: int,
        cols: int,
        figsize: Tuple[float, float] = (15, 12),
        **subplot_kw
    ) -> Figure:
        """
        Create grid layout for multiple plots.
        
        Args:
            rows: Number of rows
            cols: Number of columns
            figsize: Figure size
            **subplot_kw: Additional subplot keywords
        
        Returns:
            Figure with grid layout
        """
        fig, axes = plt.subplots(rows, cols, figsize=figsize, **subplot_kw)
        return fig
    
    def create_comparison_matrix(
        self,
        datasets: Dict[str, Tuple[np.ndarray, np.ndarray]],
        plot_types: List[str] = ['line'],
        figsize: Tuple[float, float] = (15, 10)
    ) -> Figure:
        """
        Create matrix of comparison plots.
        
        Args:
            datasets: Dictionary of dataset_name: (x, y) tuples
            plot_types: Types of plots to create
            figsize: Figure size
        
        Returns:
            Figure with comparison matrix
        """
        n_datasets = len(datasets)
        n_types = len(plot_types)
        
        fig, axes = plt.subplots(n_datasets, n_types, figsize=figsize,
                                 squeeze=False)
        
        for i, (name, (x, y)) in enumerate(datasets.items()):
            for j, plot_type in enumerate(plot_types):
                ax = axes[i, j]
                
                if plot_type == 'line':
                    ax.plot(x, y, 'b-', linewidth=2)
                elif plot_type == 'scatter':
                    ax.scatter(x, y, alpha=0.6)
                
                if i == 0:
                    ax.set_title(f'{plot_type.capitalize()} Plot')
                if j == 0:
                    ax.set_ylabel(name)
                
                ax.grid(True, alpha=0.3)
        
        plt.tight_layout()
        return fig
    
    def create_side_by_side(
        self,
        data1: Tuple[np.ndarray, np.ndarray],
        data2: Tuple[np.ndarray, np.ndarray],
        titles: List[str] = ['Dataset 1', 'Dataset 2'],
        shared_y: bool = True,
        figsize: Tuple[float, float] = (12, 5)
    ) -> Figure:
        """
        Create side-by-side comparison.
        
        Args:
            data1, data2: (x, y) tuples for each dataset
            titles: Titles for each plot
            shared_y: Whether to share y-axis
            figsize: Figure size
        
        Returns:
            Figure with side-by-side plots
        """
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=figsize,
                                       sharey=shared_y)
        
        ax1.plot(data1[0], data1[1], 'b-', linewidth=2)
        ax1.set_title(titles[0])
        ax1.grid(True, alpha=0.3)
        
        ax2.plot(data2[0], data2[1], 'r-', linewidth=2)
        ax2.set_title(titles[1])
        ax2.grid(True, alpha=0.3)
        
        plt.tight_layout()
        return fig


class PlotExporter:
    """Handles exporting plots to various formats."""
    
    def export(
        self,
        fig: Figure,
        filepath: Union[str, Path],
        format: str = 'png',
        dpi: int = 150,
        **kwargs
    ) -> Path:
        """
        Export plot to file.
        
        Args:
            fig: Figure to export
            filepath: Output file path
            format: Export format (png, pdf, svg)
            dpi: Resolution for raster formats
            **kwargs: Additional savefig arguments
        
        Returns:
            Path to exported file
        """
        filepath = Path(filepath)
        
        # Ensure extension matches format
        if not filepath.suffix:
            filepath = filepath.with_suffix(f'.{format}')
        
        # Create directory if needed
        filepath.parent.mkdir(parents=True, exist_ok=True)
        
        # Export figure
        fig.savefig(filepath, format=format, dpi=dpi, 
                   bbox_inches='tight', **kwargs)
        
        logger.info(f"Exported plot to {filepath}")
        return filepath
    
    def export_batch(
        self,
        fig: Figure,
        base_path: Union[str, Path],
        formats: List[str] = ['png', 'pdf', 'svg'],
        **kwargs
    ) -> List[Path]:
        """
        Export plot to multiple formats.
        
        Args:
            fig: Figure to export
            base_path: Base path without extension
            formats: List of formats to export
            **kwargs: Additional savefig arguments
        
        Returns:
            List of exported file paths
        """
        base_path = Path(base_path)
        exported_paths = []
        
        for fmt in formats:
            filepath = base_path.with_suffix(f'.{fmt}')
            exported_path = self.export(fig, filepath, format=fmt, **kwargs)
            exported_paths.append(exported_path)
        
        return exported_paths


# High-level convenience functions

def plot_forces(
    staggers: np.ndarray,
    forces: List[ForceResults],
    title: str = "Passing Ship Forces",
    export_path: Optional[Path] = None,
    interactive: bool = False
) -> Figure:
    """
    High-level function to plot force distributions.
    
    Args:
        staggers: Stagger distances
        forces: Force results
        title: Plot title
        export_path: Optional path to export plot
        interactive: Enable interactive features
    
    Returns:
        Figure with force plots
    """
    plotter = ForceDistributionPlotter()
    fig = plotter.plot_force_distribution(staggers, forces, title=title)
    
    if interactive:
        manager = InteractivePlotManager()
        manager.enable_interactivity(fig)
        manager.add_data_cursor(fig)
    
    if export_path:
        exporter = PlotExporter()
        exporter.export(fig, export_path)
    
    return fig


def create_parametric_study(
    param1: np.ndarray,
    param2: np.ndarray,
    results: np.ndarray,
    param1_name: str = "Parameter 1",
    param2_name: str = "Parameter 2",
    result_name: str = "Result",
    plot_3d: bool = False
) -> Figure:
    """
    Create parametric study visualization.
    
    Args:
        param1, param2: Parameter arrays
        results: 2D results array
        param1_name, param2_name: Parameter names
        result_name: Result quantity name
        plot_3d: Whether to create 3D plot
    
    Returns:
        Figure with parametric study
    """
    visualizer = ParametricStudyVisualizer()
    
    if plot_3d:
        X, Y = np.meshgrid(param2, param1)
        fig = visualizer.plot_3d_surface(
            X, Y, results,
            xlabel=param2_name,
            ylabel=param1_name,
            zlabel=result_name,
            title=f"{result_name} Parametric Study"
        )
    else:
        fig = visualizer.plot_2d_parametric(
            param1, param2, results,
            xlabel=param2_name,
            ylabel=param1_name,
            title=f"{result_name} Parametric Study",
            cbar_label=result_name
        )
    
    return fig


def create_comparison_plots(
    datasets: Dict[str, Dict[str, Any]],
    layout: str = 'grid',
    **kwargs
) -> Figure:
    """
    Create comparison plots for multiple datasets.
    
    Args:
        datasets: Dictionary of dataset_name: data_dict
        layout: Layout type ('grid', 'side_by_side', 'matrix')
        **kwargs: Additional layout arguments
    
    Returns:
        Figure with comparison plots
    """
    manager = ComparisonLayoutManager()
    
    if layout == 'side_by_side' and len(datasets) == 2:
        keys = list(datasets.keys())
        data1 = (datasets[keys[0]]['staggers'], 
                [f.surge for f in datasets[keys[0]]['forces']])
        data2 = (datasets[keys[1]]['staggers'],
                [f.surge for f in datasets[keys[1]]['forces']])
        
        fig = manager.create_side_by_side(
            data1, data2,
            titles=keys,
            **kwargs
        )
    else:
        # Default to grid layout
        n_datasets = len(datasets)
        cols = min(3, n_datasets)
        rows = (n_datasets + cols - 1) // cols
        
        fig = manager.create_grid_layout(rows, cols, **kwargs)
        
        for idx, (name, data) in enumerate(datasets.items()):
            ax = fig.axes[idx]
            staggers = data['staggers']
            forces = [f.surge for f in data['forces']]
            ax.plot(staggers, forces)
            ax.set_title(name)
            ax.grid(True, alpha=0.3)
    
    return fig