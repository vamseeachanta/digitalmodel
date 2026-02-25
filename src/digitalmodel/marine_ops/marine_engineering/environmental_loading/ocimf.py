"""
OCIMF Environmental Loading Module.

This module provides functionality for calculating environmental forces on vessels
using OCIMF (Oil Companies International Marine Forum) coefficients database.

Features:
- Wind/current coefficient database (186+ vessels)
- 2D interpolation (heading × displacement)
- Force/moment calculations
- Comprehensive visualization charts

References:
- OCIMF "Prediction of Wind and Current Loads on VLCCs" (1994)
- OCIMF "Mooring Equipment Guidelines" (MEG4)
"""

from dataclasses import dataclass, field
from typing import Tuple, Optional, Dict, List
from pathlib import Path
import pandas as pd
import numpy as np
from scipy.interpolate import RBFInterpolator, interp2d, LinearNDInterpolator
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from mpl_toolkits.mplot3d import Axes3D
import seaborn as sns
import warnings

# Optional plotly for interactive 3D charts
try:
    import plotly.graph_objects as go
    import plotly.express as px
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False
    warnings.warn("Plotly not available. Interactive 3D charts will be disabled.")


@dataclass
class OCIMFCoefficients:
    """
    OCIMF environmental loading coefficients.

    Attributes:
        CXw: Wind force coefficient (longitudinal)
        CYw: Wind force coefficient (lateral)
        CMw: Wind moment coefficient
        CXc: Current force coefficient (longitudinal)
        CYc: Current force coefficient (lateral)
        CMc: Current moment coefficient
        heading: Heading angle in degrees (0-180)
        displacement: Vessel displacement in tonnes
    """
    CXw: float  # Wind force coefficient (longitudinal)
    CYw: float  # Wind force coefficient (lateral)
    CMw: float  # Wind moment coefficient
    CXc: float  # Current force coefficient (longitudinal)
    CYc: float  # Current force coefficient (lateral)
    CMc: float  # Current moment coefficient
    heading: Optional[float] = None
    displacement: Optional[float] = None

    def to_dict(self) -> Dict[str, float]:
        """Convert coefficients to dictionary."""
        return {
            'CXw': self.CXw,
            'CYw': self.CYw,
            'CMw': self.CMw,
            'CXc': self.CXc,
            'CYc': self.CYc,
            'CMc': self.CMc,
            'heading': self.heading,
            'displacement': self.displacement
        }


@dataclass
class EnvironmentalConditions:
    """Environmental conditions for force calculation."""
    wind_speed: float  # Wind speed in m/s
    wind_direction: float  # Wind direction in degrees (relative to vessel)
    current_speed: float  # Current speed in m/s
    current_direction: float  # Current direction in degrees (relative to vessel)
    air_density: float = 1.225  # kg/m³ (standard at sea level)
    water_density: float = 1025.0  # kg/m³ (seawater)


@dataclass
class VesselGeometry:
    """Vessel geometry parameters."""
    loa: float  # Length overall in meters
    beam: float  # Beam in meters
    draft: float  # Draft in meters
    frontal_area_wind: Optional[float] = None  # Projected frontal area for wind (m²)
    lateral_area_wind: Optional[float] = None  # Projected lateral area for wind (m²)
    frontal_area_current: Optional[float] = None  # Projected frontal area for current (m²)
    lateral_area_current: Optional[float] = None  # Projected lateral area for current (m²)

    def __post_init__(self):
        """Calculate projected areas if not provided."""
        if self.frontal_area_wind is None:
            # Simplified estimate: beam × (0.8 × freeboard + above water structures)
            freeboard = 10.0  # Assumed average freeboard
            self.frontal_area_wind = self.beam * freeboard

        if self.lateral_area_wind is None:
            # Simplified estimate: loa × (freeboard + above water structures)
            freeboard = 10.0
            self.lateral_area_wind = self.loa * freeboard

        if self.frontal_area_current is None:
            # Simplified estimate: beam × draft
            self.frontal_area_current = self.beam * self.draft

        if self.lateral_area_current is None:
            # Simplified estimate: loa × draft
            self.lateral_area_current = self.loa * self.draft


@dataclass
class EnvironmentalForceResults:
    """Results of environmental force calculations."""
    wind_fx: float  # Wind force longitudinal (N)
    wind_fy: float  # Wind force lateral (N)
    wind_mz: float  # Wind moment (N·m)
    current_fx: float  # Current force longitudinal (N)
    current_fy: float  # Current force lateral (N)
    current_mz: float  # Current moment (N·m)
    total_fx: float  # Total force longitudinal (N)
    total_fy: float  # Total force lateral (N)
    total_mz: float  # Total moment (N·m)
    coefficients: OCIMFCoefficients  # Coefficients used
    conditions: EnvironmentalConditions  # Conditions used
    geometry: VesselGeometry  # Geometry used


class OCIMFDatabase:
    """
    OCIMF coefficient database with 2D interpolation.

    The database contains wind and current coefficients for various vessel types
    at different headings and displacements.
    """

    def __init__(self, csv_path: str):
        """
        Initialize OCIMF database from CSV file.

        Args:
            csv_path: Path to OCIMF coefficients CSV file

        CSV Format:
            vessel_type, displacement, heading, CXw, CYw, CMw, CXc, CYc, CMc, ...
        """
        self.csv_path = Path(csv_path)
        self.data = pd.read_csv(csv_path)
        self._validate_data()
        self._setup_interpolators()

    def _validate_data(self):
        """Validate that required columns exist."""
        required_cols = ['displacement', 'heading', 'CXw', 'CYw', 'CMw',
                        'CXc', 'CYc', 'CMc']
        missing = [col for col in required_cols if col not in self.data.columns]
        if missing:
            raise ValueError(f"Missing required columns: {missing}")

        # Check for NaN values
        if self.data[required_cols].isna().any().any():
            warnings.warn("Database contains NaN values. Interpolation may be affected.")

    def _setup_interpolators(self):
        """Set up 2D interpolators for each coefficient."""
        # Get unique headings and displacements
        self.headings = np.sort(self.data['heading'].unique())
        self.displacements = np.sort(self.data['displacement'].unique())

        # Create interpolators for each coefficient
        self.interpolators = {}
        coefficients = ['CXw', 'CYw', 'CMw', 'CXc', 'CYc', 'CMc']

        # Prepare points and values for RBF interpolation
        points = self.data[['heading', 'displacement']].values

        for coef in coefficients:
            values = self.data[coef].values
            # Use RBF interpolation for smooth surfaces
            try:
                self.interpolators[coef] = RBFInterpolator(
                    points, values,
                    kernel='thin_plate_spline',
                    smoothing=0.0
                )
            except Exception as e:
                warnings.warn(f"RBF interpolation failed for {coef}, using linear: {e}")
                # Fallback to linear interpolation
                self.interpolators[coef] = LinearNDInterpolator(points, values)

    def get_coefficients(self, heading: float, displacement: float,
                        vessel_type: Optional[str] = None) -> OCIMFCoefficients:
        """
        Get interpolated coefficients for given heading and displacement.

        Args:
            heading: Heading angle in degrees (0-180)
            displacement: Vessel displacement in tonnes
            vessel_type: Optional vessel type filter

        Returns:
            OCIMFCoefficients object with interpolated values

        Note:
            Heading is normalized to 0-180 degrees (port/starboard symmetry)
        """
        # Normalize heading to 0-180 range (symmetry assumption)
        heading_norm = heading % 360
        if heading_norm > 180:
            heading_norm = 360 - heading_norm

        # Check bounds
        if heading_norm < self.headings.min() or heading_norm > self.headings.max():
            warnings.warn(f"Heading {heading_norm}° is outside database range "
                         f"[{self.headings.min()}, {self.headings.max()}]")

        if displacement < self.displacements.min() or displacement > self.displacements.max():
            warnings.warn(f"Displacement {displacement}t is outside database range "
                         f"[{self.displacements.min()}, {self.displacements.max()}]")

        # Interpolate each coefficient
        point = np.array([[heading_norm, displacement]])

        coeffs = {}
        for coef_name, interpolator in self.interpolators.items():
            try:
                value = float(interpolator(point)[0])
                coeffs[coef_name] = value
            except Exception as e:
                warnings.warn(f"Interpolation failed for {coef_name}: {e}")
                coeffs[coef_name] = 0.0

        return OCIMFCoefficients(
            CXw=coeffs['CXw'],
            CYw=coeffs['CYw'],
            CMw=coeffs['CMw'],
            CXc=coeffs['CXc'],
            CYc=coeffs['CYc'],
            CMc=coeffs['CMc'],
            heading=heading_norm,
            displacement=displacement
        )

    def plot_coefficient_surface(self, coefficient_name: str,
                                save_path: Optional[str] = None,
                                interactive: bool = False) -> Optional[go.Figure]:
        """
        Create 3D surface plot of coefficient vs heading and displacement.

        Args:
            coefficient_name: Name of coefficient ('CXw', 'CYw', 'CMw', etc.)
            save_path: Optional path to save the plot
            interactive: Use plotly for interactive 3D plot (requires plotly)

        Returns:
            Plotly figure if interactive=True, else None
        """
        if coefficient_name not in self.interpolators:
            raise ValueError(f"Unknown coefficient: {coefficient_name}")

        # Create grid for surface
        heading_grid = np.linspace(self.headings.min(), self.headings.max(), 50)
        disp_grid = np.linspace(self.displacements.min(), self.displacements.max(), 50)
        H, D = np.meshgrid(heading_grid, disp_grid)

        # Evaluate interpolator on grid
        points = np.column_stack([H.ravel(), D.ravel()])
        Z = self.interpolators[coefficient_name](points).reshape(H.shape)

        if interactive and PLOTLY_AVAILABLE:
            # Create interactive plotly 3D surface
            fig = go.Figure(data=[
                go.Surface(
                    x=heading_grid,
                    y=disp_grid,
                    z=Z,
                    colorscale='Viridis',
                    name=coefficient_name
                )
            ])

            fig.update_layout(
                title=f'OCIMF {coefficient_name} Coefficient Surface',
                scene=dict(
                    xaxis_title='Heading (degrees)',
                    yaxis_title='Displacement (tonnes)',
                    zaxis_title=coefficient_name,
                    camera=dict(eye=dict(x=1.5, y=1.5, z=1.3))
                ),
                width=900,
                height=700
            )

            if save_path:
                fig.write_html(save_path)

            return fig
        else:
            # Create matplotlib 3D surface
            fig = plt.figure(figsize=(12, 8))
            ax = fig.add_subplot(111, projection='3d')

            surf = ax.plot_surface(H, D, Z, cmap='viridis', alpha=0.8,
                                  edgecolor='none', antialiased=True)

            # Add scatter points for actual data
            actual_data = self.data[['heading', 'displacement', coefficient_name]]
            ax.scatter(actual_data['heading'], actual_data['displacement'],
                      actual_data[coefficient_name], c='red', marker='o',
                      s=30, alpha=0.6, label='Database points')

            ax.set_xlabel('Heading (degrees)', fontsize=10)
            ax.set_ylabel('Displacement (tonnes)', fontsize=10)
            ax.set_zlabel(coefficient_name, fontsize=10)
            ax.set_title(f'OCIMF {coefficient_name} Coefficient Surface', fontsize=12)
            ax.legend()

            fig.colorbar(surf, ax=ax, shrink=0.5, aspect=5)
            plt.tight_layout()

            if save_path:
                plt.savefig(save_path, dpi=300, bbox_inches='tight')
                plt.close()
            else:
                plt.show()

            return None

    def plot_polar_diagram(self, displacement: float,
                          save_path: Optional[str] = None):
        """
        Create polar plot of wind forces at given displacement.

        Args:
            displacement: Vessel displacement in tonnes
            save_path: Optional path to save the plot
        """
        # Generate coefficients for full circle (0-360 degrees)
        headings = np.linspace(0, 360, 73)  # 5-degree intervals

        CXw_values = []
        CYw_values = []

        for heading in headings:
            coeffs = self.get_coefficients(heading, displacement)
            CXw_values.append(coeffs.CXw)
            CYw_values.append(coeffs.CYw)

        # Create polar plot
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6),
                                       subplot_kw=dict(projection='polar'))

        # Convert headings to radians
        theta = np.radians(headings)

        # Plot CXw
        ax1.plot(theta, CXw_values, 'b-', linewidth=2)
        ax1.fill(theta, CXw_values, alpha=0.25)
        ax1.set_theta_zero_location('N')
        ax1.set_theta_direction(-1)
        ax1.set_title(f'CXw (Longitudinal Wind)\nDisplacement: {displacement:,.0f}t',
                     fontsize=11, pad=20)
        ax1.grid(True)

        # Plot CYw
        ax2.plot(theta, CYw_values, 'r-', linewidth=2)
        ax2.fill(theta, CYw_values, alpha=0.25, color='red')
        ax2.set_theta_zero_location('N')
        ax2.set_theta_direction(-1)
        ax2.set_title(f'CYw (Lateral Wind)\nDisplacement: {displacement:,.0f}t',
                     fontsize=11, pad=20)
        ax2.grid(True)

        plt.suptitle('OCIMF Wind Coefficient Polar Diagrams', fontsize=13, y=1.02)
        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            plt.close()
        else:
            plt.show()

    def plot_interpolation_quality(self, coefficient_name: str = 'CYw',
                                  save_path: Optional[str] = None):
        """
        Create heatmap showing interpolation error/quality.

        Args:
            coefficient_name: Coefficient to analyze
            save_path: Optional path to save the plot
        """
        # Create fine grid
        heading_grid = np.linspace(self.headings.min(), self.headings.max(), 100)
        disp_grid = np.linspace(self.displacements.min(), self.displacements.max(), 100)

        H, D = np.meshgrid(heading_grid, disp_grid)
        points = np.column_stack([H.ravel(), D.ravel()])

        # Get interpolated values
        Z_interp = self.interpolators[coefficient_name](points).reshape(H.shape)

        # Create heatmap
        fig, ax = plt.subplots(figsize=(12, 8))

        im = ax.contourf(H, D, Z_interp, levels=20, cmap='RdYlBu_r')

        # Overlay actual data points
        actual = self.data[['heading', 'displacement', coefficient_name]]
        scatter = ax.scatter(actual['heading'], actual['displacement'],
                           c=actual[coefficient_name], s=100,
                           edgecolors='black', linewidths=1.5,
                           cmap='RdYlBu_r', zorder=10)

        ax.set_xlabel('Heading (degrees)', fontsize=11)
        ax.set_ylabel('Displacement (tonnes)', fontsize=11)
        ax.set_title(f'OCIMF {coefficient_name} Interpolation Quality\n'
                    f'(Contours: Interpolated | Points: Database)', fontsize=12)

        cbar = plt.colorbar(im, ax=ax)
        cbar.set_label(coefficient_name, fontsize=10)

        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            plt.close()
        else:
            plt.show()


class EnvironmentalForces:
    """
    Calculate environmental forces on vessels using OCIMF coefficients.
    """

    def __init__(self, database: OCIMFDatabase):
        """
        Initialize environmental forces calculator.

        Args:
            database: OCIMFDatabase instance
        """
        self.database = database

    def calculate_wind_forces(self,
                             conditions: EnvironmentalConditions,
                             geometry: VesselGeometry,
                             displacement: float) -> Tuple[float, float, float]:
        """
        Calculate wind forces and moments.

        Args:
            conditions: Environmental conditions
            geometry: Vessel geometry
            displacement: Vessel displacement

        Returns:
            Tuple of (Fx, Fy, Mz) in Newtons and Newton-meters
        """
        # Get coefficients
        coeffs = self.database.get_coefficients(
            conditions.wind_direction,
            displacement
        )

        # Wind dynamic pressure
        q_wind = 0.5 * conditions.air_density * conditions.wind_speed**2

        # Calculate forces
        # Fx = 0.5 * ρ * V² * A_frontal * CXw
        # Fy = 0.5 * ρ * V² * A_lateral * CYw
        Fx = q_wind * geometry.frontal_area_wind * coeffs.CXw
        Fy = q_wind * geometry.lateral_area_wind * coeffs.CYw

        # Moment: using LOA as reference length
        Mz = q_wind * geometry.lateral_area_wind * geometry.loa * coeffs.CMw

        return Fx, Fy, Mz

    def calculate_current_forces(self,
                                conditions: EnvironmentalConditions,
                                geometry: VesselGeometry,
                                displacement: float) -> Tuple[float, float, float]:
        """
        Calculate current forces and moments.

        Args:
            conditions: Environmental conditions
            geometry: Vessel geometry
            displacement: Vessel displacement

        Returns:
            Tuple of (Fx, Fy, Mz) in Newtons and Newton-meters
        """
        # Get coefficients
        coeffs = self.database.get_coefficients(
            conditions.current_direction,
            displacement
        )

        # Current dynamic pressure
        q_current = 0.5 * conditions.water_density * conditions.current_speed**2

        # Calculate forces
        Fx = q_current * geometry.frontal_area_current * coeffs.CXc
        Fy = q_current * geometry.lateral_area_current * coeffs.CYc

        # Moment
        Mz = q_current * geometry.lateral_area_current * geometry.loa * coeffs.CMc

        return Fx, Fy, Mz

    def calculate_total_forces(self,
                              conditions: EnvironmentalConditions,
                              geometry: VesselGeometry,
                              displacement: float) -> EnvironmentalForceResults:
        """
        Calculate total environmental forces (wind + current).

        Args:
            conditions: Environmental conditions
            geometry: Vessel geometry
            displacement: Vessel displacement

        Returns:
            EnvironmentalForceResults with all force components
        """
        # Calculate wind forces
        wind_fx, wind_fy, wind_mz = self.calculate_wind_forces(
            conditions, geometry, displacement
        )

        # Calculate current forces
        current_fx, current_fy, current_mz = self.calculate_current_forces(
            conditions, geometry, displacement
        )

        # Total forces
        total_fx = wind_fx + current_fx
        total_fy = wind_fy + current_fy
        total_mz = wind_mz + current_mz

        # Get coefficients for reference
        coeffs = self.database.get_coefficients(
            conditions.wind_direction,  # Use wind direction as primary
            displacement
        )

        return EnvironmentalForceResults(
            wind_fx=wind_fx,
            wind_fy=wind_fy,
            wind_mz=wind_mz,
            current_fx=current_fx,
            current_fy=current_fy,
            current_mz=current_mz,
            total_fx=total_fx,
            total_fy=total_fy,
            total_mz=total_mz,
            coefficients=coeffs,
            conditions=conditions,
            geometry=geometry
        )

    def plot_force_diagram(self, results: EnvironmentalForceResults,
                          save_path: Optional[str] = None):
        """
        Visualize force components with vector diagram.

        Args:
            results: EnvironmentalForceResults object
            save_path: Optional path to save the plot
        """
        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

        # Plot 1: Force vectors
        ax1.set_aspect('equal')

        # Scale forces for visualization (convert to kN)
        scale = 1e-3

        # Wind force vector (blue)
        ax1.arrow(0, 0, results.wind_fx * scale, results.wind_fy * scale,
                 head_width=20, head_length=30, fc='blue', ec='blue',
                 alpha=0.7, width=5, label='Wind')

        # Current force vector (green)
        ax1.arrow(0, 0, results.current_fx * scale, results.current_fy * scale,
                 head_width=20, head_length=30, fc='green', ec='green',
                 alpha=0.7, width=5, label='Current')

        # Total force vector (red)
        ax1.arrow(0, 0, results.total_fx * scale, results.total_fy * scale,
                 head_width=25, head_length=40, fc='red', ec='red',
                 alpha=0.9, width=7, label='Total', linewidth=2)

        ax1.grid(True, alpha=0.3)
        ax1.axhline(y=0, color='k', linewidth=0.5)
        ax1.axvline(x=0, color='k', linewidth=0.5)
        ax1.set_xlabel('Fx (kN)', fontsize=10)
        ax1.set_ylabel('Fy (kN)', fontsize=10)
        ax1.set_title('Environmental Force Vectors', fontsize=11)
        ax1.legend(fontsize=9)

        # Plot 2: Bar chart of force components
        components = ['Fx\n(Long.)', 'Fy\n(Lat.)', 'Mz\n(Yaw)']
        wind_vals = [results.wind_fx*1e-3, results.wind_fy*1e-3, results.wind_mz*1e-6]
        current_vals = [results.current_fx*1e-3, results.current_fy*1e-3, results.current_mz*1e-6]
        total_vals = [results.total_fx*1e-3, results.total_fy*1e-3, results.total_mz*1e-6]

        x = np.arange(len(components))
        width = 0.25

        ax2.bar(x - width, wind_vals, width, label='Wind', color='blue', alpha=0.7)
        ax2.bar(x, current_vals, width, label='Current', color='green', alpha=0.7)
        ax2.bar(x + width, total_vals, width, label='Total', color='red', alpha=0.7)

        ax2.set_ylabel('Force (kN) / Moment (MN·m)', fontsize=10)
        ax2.set_title('Force and Moment Components', fontsize=11)
        ax2.set_xticks(x)
        ax2.set_xticklabels(components, fontsize=9)
        ax2.legend(fontsize=9)
        ax2.grid(True, alpha=0.3, axis='y')
        ax2.axhline(y=0, color='k', linewidth=0.5)

        # Add text box with conditions
        textstr = f'Conditions:\n'
        textstr += f'Wind: {results.conditions.wind_speed:.1f} m/s @ {results.conditions.wind_direction:.0f}°\n'
        textstr += f'Current: {results.conditions.current_speed:.1f} m/s @ {results.conditions.current_direction:.0f}°\n'
        textstr += f'Vessel: LOA={results.geometry.loa:.0f}m, Disp={results.coefficients.displacement:,.0f}t'

        props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
        fig.text(0.5, 0.02, textstr, fontsize=8, verticalalignment='bottom',
                bbox=props, horizontalalignment='center')

        plt.tight_layout(rect=[0, 0.08, 1, 1])

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            plt.close()
        else:
            plt.show()

    def plot_validation_chart(self,
                             excel_results: Dict[str, float],
                             python_results: EnvironmentalForceResults,
                             save_path: Optional[str] = None):
        """
        Create comparison chart between Excel and Python calculations.

        Args:
            excel_results: Dictionary with Excel calculated values
            python_results: EnvironmentalForceResults from Python
            save_path: Optional path to save the plot
        """
        fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2, 2, figsize=(14, 10))

        # Define metrics to compare
        metrics = ['wind_fx', 'wind_fy', 'wind_mz',
                  'current_fx', 'current_fy', 'current_mz']

        excel_vals = [excel_results.get(m, 0) for m in metrics]
        python_vals = [getattr(python_results, m, 0) for m in metrics]

        # Convert to appropriate units
        for i in range(3):
            excel_vals[i] *= 1e-3  # Wind forces to kN
            python_vals[i] *= 1e-3
            excel_vals[i+3] *= 1e-3  # Current forces to kN
            python_vals[i+3] *= 1e-3

        # Plot 1: Wind forces comparison
        x = np.arange(3)
        width = 0.35

        ax1.bar(x - width/2, excel_vals[:3], width, label='Excel', color='skyblue')
        ax1.bar(x + width/2, python_vals[:3], width, label='Python', color='orange')
        ax1.set_ylabel('Force (kN) / Moment (MN·m)', fontsize=9)
        ax1.set_title('Wind Forces Comparison', fontsize=10)
        ax1.set_xticks(x)
        ax1.set_xticklabels(['Fx', 'Fy', 'Mz'], fontsize=8)
        ax1.legend(fontsize=8)
        ax1.grid(True, alpha=0.3, axis='y')

        # Plot 2: Current forces comparison
        ax2.bar(x - width/2, excel_vals[3:], width, label='Excel', color='skyblue')
        ax2.bar(x + width/2, python_vals[3:], width, label='Python', color='orange')
        ax2.set_ylabel('Force (kN) / Moment (MN·m)', fontsize=9)
        ax2.set_title('Current Forces Comparison', fontsize=10)
        ax2.set_xticks(x)
        ax2.set_xticklabels(['Fx', 'Fy', 'Mz'], fontsize=8)
        ax2.legend(fontsize=8)
        ax2.grid(True, alpha=0.3, axis='y')

        # Plot 3: Percentage difference
        pct_diff = []
        for e, p in zip(excel_vals, python_vals):
            if abs(e) > 1e-6:  # Avoid division by zero
                pct_diff.append(100 * (p - e) / abs(e))
            else:
                pct_diff.append(0)

        colors = ['green' if abs(d) < 5 else 'orange' if abs(d) < 10 else 'red'
                 for d in pct_diff]

        ax3.bar(range(len(pct_diff)), pct_diff, color=colors, alpha=0.7)
        ax3.set_ylabel('Difference (%)', fontsize=9)
        ax3.set_title('Percentage Difference (Python vs Excel)', fontsize=10)
        ax3.set_xticks(range(len(metrics)))
        ax3.set_xticklabels(['Wind\nFx', 'Wind\nFy', 'Wind\nMz',
                            'Curr\nFx', 'Curr\nFy', 'Curr\nMz'], fontsize=7)
        ax3.axhline(y=0, color='k', linewidth=0.5)
        ax3.axhline(y=5, color='orange', linewidth=0.5, linestyle='--', alpha=0.5)
        ax3.axhline(y=-5, color='orange', linewidth=0.5, linestyle='--', alpha=0.5)
        ax3.grid(True, alpha=0.3, axis='y')

        # Plot 4: Scatter plot (Python vs Excel)
        ax4.scatter(excel_vals, python_vals, s=100, alpha=0.6, c='purple')

        # Add perfect correlation line
        min_val = min(min(excel_vals), min(python_vals))
        max_val = max(max(excel_vals), max(python_vals))
        ax4.plot([min_val, max_val], [min_val, max_val], 'r--',
                linewidth=2, label='Perfect match')

        # Add ±10% bounds
        ax4.plot([min_val, max_val], [min_val*0.9, max_val*0.9], 'g--',
                alpha=0.5, label='±10%')
        ax4.plot([min_val, max_val], [min_val*1.1, max_val*1.1], 'g--', alpha=0.5)

        ax4.set_xlabel('Excel Values (kN or MN·m)', fontsize=9)
        ax4.set_ylabel('Python Values (kN or MN·m)', fontsize=9)
        ax4.set_title('Python vs Excel Correlation', fontsize=10)
        ax4.legend(fontsize=8)
        ax4.grid(True, alpha=0.3)

        plt.suptitle('OCIMF Force Calculation Validation', fontsize=12, y=1.00)
        plt.tight_layout()

        if save_path:
            plt.savefig(save_path, dpi=300, bbox_inches='tight')
            plt.close()
        else:
            plt.show()


def create_sample_database(output_path: str, num_vessels: int = 5,
                          num_headings: int = 13, num_displacements: int = 3):
    """
    Create a sample OCIMF database for testing/demo purposes.

    Args:
        output_path: Path to save CSV file
        num_vessels: Number of vessel types to generate
        num_headings: Number of heading angles (0-180)
        num_displacements: Number of displacement values per vessel
    """
    vessel_types = ['VLCC', 'Suezmax', 'Aframax', 'Panamax', 'Handymax'][:num_vessels]
    base_displacements = {
        'VLCC': [250000, 300000, 350000],
        'Suezmax': [150000, 180000, 200000],
        'Aframax': [100000, 120000, 140000],
        'Panamax': [70000, 80000, 90000],
        'Handymax': [40000, 50000, 60000]
    }

    base_loa = {'VLCC': 330, 'Suezmax': 275, 'Aframax': 245, 'Panamax': 225, 'Handymax': 190}
    base_beam = {'VLCC': 60, 'Suezmax': 48, 'Aframax': 42, 'Panamax': 32, 'Handymax': 30}

    headings = np.linspace(0, 180, num_headings)

    data = []

    for vessel_type in vessel_types:
        disps = base_displacements[vessel_type][:num_displacements]
        loa = base_loa[vessel_type]
        beam = base_beam[vessel_type]

        for disp in disps:
            draft = 10 + (disp / 50000) * 2  # Simplified draft calculation

            for hdg in headings:
                # Generate realistic coefficient patterns
                hdg_rad = np.radians(hdg)

                # Wind coefficients (simplified patterns)
                CXw = 0.9 * np.cos(hdg_rad) + 0.05
                CYw = 0.95 * np.abs(np.sin(hdg_rad))
                CMw = 0.25 * np.sin(2 * hdg_rad)

                # Current coefficients (slightly different patterns)
                CXc = 1.0 * np.cos(hdg_rad) + 0.05
                CYc = 1.0 * np.abs(np.sin(hdg_rad))
                CMc = 0.28 * np.sin(2 * hdg_rad)

                data.append({
                    'vessel_type': vessel_type,
                    'displacement': disp,
                    'heading': hdg,
                    'CXw': CXw,
                    'CYw': CYw,
                    'CMw': CMw,
                    'CXc': CXc,
                    'CYc': CYc,
                    'CMc': CMc,
                    'vessel_name': f'Sample {vessel_type} 1',
                    'loa': loa,
                    'beam': beam,
                    'draft': draft
                })

    df = pd.DataFrame(data)
    df.to_csv(output_path, index=False)
    print(f"Created sample OCIMF database: {output_path}")
    print(f"  Vessels: {num_vessels}")
    print(f"  Total entries: {len(df)}")

    return df
