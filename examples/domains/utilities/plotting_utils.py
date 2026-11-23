"""
Professional plotting utilities for marine engineering tutorials.

Provides standardized plot templates and styling for consistent
publication-quality visualizations across all tutorials.
"""

import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import numpy as np
import seaborn as sns
from typing import Optional, Tuple, List
import plotly.graph_objects as go
import plotly.express as px
from pathlib import Path


# Publication-quality plot settings
PLOT_STYLE = {
    'figure.figsize': (12, 6),
    'figure.dpi': 150,
    'font.size': 10,
    'axes.labelsize': 11,
    'axes.titlesize': 12,
    'xtick.labelsize': 10,
    'ytick.labelsize': 10,
    'legend.fontsize': 10,
    'font.family': 'sans-serif',
    'font.sans-serif': ['Arial', 'Helvetica'],
    'axes.grid': True,
    'grid.alpha': 0.3,
    'axes.axisbelow': True
}


def apply_plot_style():
    """Apply professional plot styling."""
    plt.rcParams.update(PLOT_STYLE)
    sns.set_palette("husl")


def plot_catenary_shape(
    x: np.ndarray,
    y: np.ndarray,
    tensions: np.ndarray,
    anchor_pos: Tuple[float, float] = (0, 0),
    fairlead_pos: Optional[Tuple[float, float]] = None,
    water_depth: Optional[float] = None,
    title: str = "Mooring Line Catenary Shape",
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Create professional catenary shape plot.

    Parameters
    ----------
    x : array
        Horizontal positions [m]
    y : array
        Vertical positions [m]
    tensions : array
        Tension distribution [N]
    anchor_pos : tuple
        Anchor position (x, y)
    fairlead_pos : tuple, optional
        Fairlead position (x, y)
    water_depth : float, optional
        Water depth for seabed line [m]
    title : str
        Plot title
    save_path : str, optional
        Path to save figure

    Returns
    -------
    fig : matplotlib.Figure
    """
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

    # Plot 1: Catenary shape
    ax1.plot(x, y, 'b-', linewidth=2.5, label='Mooring Line', zorder=3)
    ax1.plot(anchor_pos[0], anchor_pos[1], 'go', markersize=14,
             label='Anchor', zorder=5, markeredgecolor='darkgreen', markeredgewidth=2)

    if fairlead_pos:
        ax1.plot(fairlead_pos[0], fairlead_pos[1], 'ro', markersize=14,
                label='Fairlead', zorder=5, markeredgecolor='darkred', markeredgewidth=2)

    if water_depth is not None:
        ax1.axhline(y=-water_depth, color='brown', linestyle='--',
                   linewidth=2, label='Seabed', alpha=0.7, zorder=1)
        ax1.fill_between([x.min(), x.max()], -water_depth, y.min()-50,
                        color='brown', alpha=0.15, zorder=0)

    ax1.set_xlabel('Horizontal Distance [m]', fontweight='bold')
    ax1.set_ylabel('Vertical Distance [m]', fontweight='bold')
    ax1.set_title(title, fontweight='bold', fontsize=13)
    ax1.legend(loc='best', framealpha=0.9)
    ax1.set_aspect('equal', adjustable='datalim')
    ax1.grid(True, alpha=0.3, linestyle='-', linewidth=0.5)

    # Plot 2: Tension distribution
    ax2.plot(x, tensions/1000, 'r-', linewidth=2.5, label='Tension')
    ax2.fill_between(x, tensions/1000, alpha=0.3, color='red')
    ax2.axhline(y=tensions[0]/1000, color='blue', linestyle='--',
               linewidth=1.5, label=f'H = {tensions[0]/1000:.1f} kN', alpha=0.7)
    ax2.set_xlabel('Horizontal Distance [m]', fontweight='bold')
    ax2.set_ylabel('Tension [kN]', fontweight='bold')
    ax2.set_title('Tension Distribution', fontweight='bold', fontsize=13)
    ax2.legend(loc='best', framealpha=0.9)
    ax2.grid(True, alpha=0.3)

    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')

    return fig


def plot_wave_spectrum(
    frequencies: np.ndarray,
    spectrum: np.ndarray,
    Tp: float,
    Hs: float,
    spectrum_type: str = "JONSWAP",
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Create professional wave spectrum plot.

    Parameters
    ----------
    frequencies : array
        Frequency array [Hz]
    spectrum : array
        Spectral density [m²s]
    Tp : float
        Peak period [s]
    Hs : float
        Significant wave height [m]
    spectrum_type : str
        Type of spectrum (JONSWAP, P-M, etc.)
    save_path : str, optional
        Path to save figure

    Returns
    -------
    fig : matplotlib.Figure
    """
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

    # Frequency domain
    ax1.plot(frequencies, spectrum, 'b-', linewidth=2.5)
    ax1.fill_between(frequencies, spectrum, alpha=0.3)
    ax1.axvline(x=1/Tp, color='r', linestyle='--', linewidth=2,
               label=f'Peak: {1/Tp:.3f} Hz (Tp={Tp:.1f}s)', alpha=0.8)
    ax1.set_xlabel('Frequency [Hz]', fontweight='bold')
    ax1.set_ylabel('Spectral Density S(f) [m²s]', fontweight='bold')
    ax1.set_title(f'{spectrum_type} Wave Spectrum (Hs={Hs:.1f}m, Tp={Tp:.1f}s)',
                 fontweight='bold', fontsize=13)
    ax1.legend(loc='best', framealpha=0.9)
    ax1.grid(True, alpha=0.3)

    # Period domain
    periods = 1 / frequencies[1:]
    spectrum_T = spectrum[1:] / (frequencies[1:]**2)
    ax2.plot(periods, spectrum_T, 'g-', linewidth=2.5)
    ax2.fill_between(periods, spectrum_T, alpha=0.3, color='green')
    ax2.axvline(x=Tp, color='r', linestyle='--', linewidth=2,
               label=f'Tp = {Tp:.1f}s', alpha=0.8)
    ax2.set_xlabel('Period [s]', fontweight='bold')
    ax2.set_ylabel('Spectral Density S(T) [m²/s]', fontweight='bold')
    ax2.set_title('Period Domain Spectrum', fontweight='bold', fontsize=13)
    ax2.set_xlim(0, min(25, periods.max()))
    ax2.legend(loc='best', framealpha=0.9)
    ax2.grid(True, alpha=0.3)

    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')

    return fig


def plot_mooring_layout(
    line_positions: List[Tuple[float, float]],
    vessel_position: Tuple[float, float] = (0, 0),
    vessel_heading: float = 0,
    vessel_length: float = 300,
    vessel_beam: float = 60,
    title: str = "Mooring System Layout",
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Create top-view mooring system layout.

    Parameters
    ----------
    line_positions : list of tuples
        Anchor positions [(x1, y1), (x2, y2), ...]
    vessel_position : tuple
        Vessel center position (x, y)
    vessel_heading : float
        Vessel heading [degrees]
    vessel_length : float
        Vessel length [m]
    vessel_beam : float
        Vessel beam [m]
    title : str
        Plot title
    save_path : str, optional
        Path to save figure

    Returns
    -------
    fig : matplotlib.Figure
    """
    fig, ax = plt.subplots(figsize=(12, 10))

    # Draw vessel
    heading_rad = np.radians(vessel_heading)
    vessel_rect = mpatches.Rectangle(
        (vessel_position[0] - vessel_beam/2, vessel_position[1] - vessel_length/2),
        vessel_beam, vessel_length,
        angle=vessel_heading,
        facecolor='lightblue',
        edgecolor='navy',
        linewidth=3,
        label='Vessel',
        zorder=3
    )
    ax.add_patch(vessel_rect)

    # Draw mooring lines
    for idx, anchor_pos in enumerate(line_positions, 1):
        ax.plot([vessel_position[0], anchor_pos[0]],
               [vessel_position[1], anchor_pos[1]],
               'b-', linewidth=2, alpha=0.6, zorder=1)
        ax.plot(anchor_pos[0], anchor_pos[1], 'ko', markersize=12,
               markeredgecolor='white', markeredgewidth=2, zorder=2)
        ax.text(anchor_pos[0], anchor_pos[1]-50, f'L{idx}',
               ha='center', fontsize=9, fontweight='bold')

    # Compass rose
    compass_x = ax.get_xlim()[1] * 0.85
    compass_y = ax.get_ylim()[1] * 0.85
    ax.arrow(compass_x, compass_y, 0, 100, head_width=30, head_length=20,
            fc='red', ec='red', linewidth=2)
    ax.text(compass_x, compass_y + 130, 'N', ha='center', fontsize=12,
           fontweight='bold', color='red')

    ax.set_xlabel('East [m]', fontweight='bold', fontsize=12)
    ax.set_ylabel('North [m]', fontweight='bold', fontsize=12)
    ax.set_title(title, fontweight='bold', fontsize=14)
    ax.set_aspect('equal')
    ax.grid(True, alpha=0.3, linestyle='-')
    ax.legend(loc='upper left', framealpha=0.9, fontsize=11)

    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')

    return fig


def plot_environmental_forces(
    wind_force: Tuple[float, float, float],
    current_force: Tuple[float, float, float],
    conditions: dict,
    save_path: Optional[str] = None
) -> plt.Figure:
    """
    Visualize environmental forces.

    Parameters
    ----------
    wind_force : tuple
        Wind (Fx, Fy, Mz) in N and N·m
    current_force : tuple
        Current (Fx, Fy, Mz) in N and N·m
    conditions : dict
        Environmental conditions
    save_path : str, optional
        Path to save figure

    Returns
    -------
    fig : matplotlib.Figure
    """
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

    # Vector diagram
    ax1.set_aspect('equal')
    scale = 1e-3  # Convert to kN

    # Wind
    ax1.arrow(0, 0, wind_force[0]*scale, wind_force[1]*scale,
             head_width=20, head_length=30, fc='blue', ec='blue',
             alpha=0.7, width=5, label='Wind')

    # Current
    ax1.arrow(0, 0, current_force[0]*scale, current_force[1]*scale,
             head_width=20, head_length=30, fc='green', ec='green',
             alpha=0.7, width=5, label='Current')

    # Total
    total_fx = wind_force[0] + current_force[0]
    total_fy = wind_force[1] + current_force[1]
    ax1.arrow(0, 0, total_fx*scale, total_fy*scale,
             head_width=25, head_length=40, fc='red', ec='red',
             alpha=0.9, width=7, label='Total', linewidth=2)

    ax1.grid(True, alpha=0.3)
    ax1.axhline(y=0, color='k', linewidth=0.5)
    ax1.axvline(x=0, color='k', linewidth=0.5)
    ax1.set_xlabel('Fx [kN]', fontweight='bold')
    ax1.set_ylabel('Fy [kN]', fontweight='bold')
    ax1.set_title('Force Vectors', fontweight='bold', fontsize=13)
    ax1.legend(fontsize=10, framealpha=0.9)

    # Bar chart
    components = ['Fx\\n(Long.)', 'Fy\\n(Lat.)', 'Mz\\n(Yaw)']
    wind_vals = [wind_force[0]*1e-3, wind_force[1]*1e-3, wind_force[2]*1e-6]
    current_vals = [current_force[0]*1e-3, current_force[1]*1e-3, current_force[2]*1e-6]

    x = np.arange(len(components))
    width = 0.35

    ax2.bar(x - width/2, wind_vals, width, label='Wind', color='blue', alpha=0.7)
    ax2.bar(x + width/2, current_vals, width, label='Current', color='green', alpha=0.7)

    ax2.set_ylabel('Force [kN] / Moment [MN·m]', fontweight='bold')
    ax2.set_title('Force Components', fontweight='bold', fontsize=13)
    ax2.set_xticks(x)
    ax2.set_xticklabels(components)
    ax2.legend(framealpha=0.9)
    ax2.grid(True, alpha=0.3, axis='y')
    ax2.axhline(y=0, color='k', linewidth=0.5)

    plt.tight_layout()

    if save_path:
        plt.savefig(save_path, dpi=300, bbox_inches='tight')

    return fig


def create_interactive_3d_surface(
    X: np.ndarray,
    Y: np.ndarray,
    Z: np.ndarray,
    title: str = "3D Surface",
    xlabel: str = "X",
    ylabel: str = "Y",
    zlabel: str = "Z",
    save_path: Optional[str] = None
) -> go.Figure:
    """
    Create interactive 3D surface plot using Plotly.

    Parameters
    ----------
    X, Y, Z : array
        Grid data
    title : str
        Plot title
    xlabel, ylabel, zlabel : str
        Axis labels
    save_path : str, optional
        Path to save HTML

    Returns
    -------
    fig : plotly.graph_objects.Figure
    """
    fig = go.Figure(data=[
        go.Surface(
            x=X,
            y=Y,
            z=Z,
            colorscale='Viridis',
            name=title
        )
    ])

    fig.update_layout(
        title=title,
        scene=dict(
            xaxis_title=xlabel,
            yaxis_title=ylabel,
            zaxis_title=zlabel,
            camera=dict(eye=dict(x=1.5, y=1.5, z=1.3))
        ),
        width=900,
        height=700,
        font=dict(family="Arial", size=12)
    )

    if save_path:
        fig.write_html(save_path)

    return fig


def save_figure_multiple_formats(
    fig: plt.Figure,
    base_path: str,
    formats: List[str] = ['png', 'pdf', 'svg']
):
    """
    Save figure in multiple formats.

    Parameters
    ----------
    fig : matplotlib.Figure
        Figure to save
    base_path : str
        Base path without extension
    formats : list
        List of formats to save
    """
    base = Path(base_path).with_suffix('')

    for fmt in formats:
        save_path = f"{base}.{fmt}"
        if fmt == 'pdf':
            fig.savefig(save_path, format='pdf', bbox_inches='tight', dpi=300)
        elif fmt == 'svg':
            fig.savefig(save_path, format='svg', bbox_inches='tight')
        else:
            fig.savefig(save_path, format=fmt, bbox_inches='tight', dpi=300)

        print(f"✓ Saved: {save_path}")


# Apply styling when module is imported
apply_plot_style()
