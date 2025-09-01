"""
Visualization module for OrcaFlex mooring analysis results.

This module provides visualization capabilities for:
- Mooring line forces (X, Y, Z components)
- Mooring system stiffness characteristics
- Force distribution and directional analysis
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.patches import Circle, FancyArrowPatch
from matplotlib.patches import Rectangle
import matplotlib.patches as mpatches
from mpl_toolkits.mplot3d import Axes3D
import os
from pathlib import Path


class MooringVisualization:
    """Visualization class for mooring analysis results."""
    
    def __init__(self, output_directory=None):
        """
        Initialize visualization module.
        
        Args:
            output_directory (str): Directory to save plots
        """
        self.output_directory = output_directory or os.getcwd()
        Path(self.output_directory).mkdir(parents=True, exist_ok=True)
        
    def plot_mooring_forces(self, pretension_df, stiffness_df=None, save_name="mooring_forces"):
        """
        Create comprehensive mooring force visualization.
        
        Args:
            pretension_df (pd.DataFrame): Pretension analysis data with force components
            stiffness_df (pd.DataFrame): Optional stiffness analysis data
            save_name (str): Base name for saved plots
        """
        # Create figure with subplots (3x4 grid for better organization)
        fig = plt.figure(figsize=(28, 21))
        
        # Row 1: Vector Components (X, Y, Z) - Individual line contributions
        # 1. X Force Components (symmetric +/- range)
        ax1 = plt.subplot(3, 4, 1)
        self._plot_x_force_components(ax1, pretension_df)
        
        # 2. Y Force Components (symmetric +/- range)
        ax2 = plt.subplot(3, 4, 2)
        self._plot_y_force_components(ax2, pretension_df)
        
        # 3. Z Force Components
        ax3 = plt.subplot(3, 4, 3)
        self._plot_z_force_components(ax3, pretension_df)
        
        # 4. Force Magnitude (3D)
        ax4 = plt.subplot(3, 4, 4)
        self._plot_force_magnitudes(ax4, pretension_df)
        
        # Row 2: Spatial Visualizations and System Summary
        # 5. Top view - XY force vectors
        ax5 = plt.subplot(3, 4, 5)
        self._plot_xy_forces(ax5, pretension_df)
        
        # 6. Side view - XZ force vectors
        ax6 = plt.subplot(3, 4, 6)
        self._plot_xz_forces(ax6, pretension_df)
        
        # 7. Force component stacked distribution
        ax7 = plt.subplot(3, 4, 7)
        self._plot_force_components(ax7, pretension_df)
        
        # 8. Directional force summary
        ax8 = plt.subplot(3, 4, 8)
        self._plot_directional_forces(ax8, pretension_df, stiffness_df)
        
        # Row 3: Analysis and Convergence
        # 9. Tension comparison (target vs actual)
        ax9 = plt.subplot(3, 4, 9)
        self._plot_tension_comparison(ax9, pretension_df)
        
        # 10. Force polar plot
        ax10 = plt.subplot(3, 4, 10, projection='polar')
        self._plot_force_polar(ax10, pretension_df)
        
        # 11. Force imbalance analysis
        ax11 = plt.subplot(3, 4, 11)
        self._plot_force_imbalance(ax11, pretension_df)
        
        # 12. System summary text
        ax12 = plt.subplot(3, 4, 12)
        self._plot_system_summary(ax12, pretension_df, stiffness_df)
        
        plt.suptitle('Mooring System Force Analysis', fontsize=16, fontweight='bold')
        plt.tight_layout()
        
        # Save figure
        save_path = os.path.join(self.output_directory, f"{save_name}.png")
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Force visualization saved to: {save_path}")
        
        return fig
    
    def _plot_xy_forces(self, ax, df):
        """Plot XY plane force vectors."""
        # Vessel representation
        vessel_rect = Rectangle((-50, -10), 100, 20, 
                               fill=False, edgecolor='black', linewidth=2)
        ax.add_patch(vessel_rect)
        ax.text(0, 0, 'LNGC', ha='center', va='center', fontsize=10, fontweight='bold')
        
        # Plot force vectors for each line
        for idx, row in df.iterrows():
            line_name = row['ObjectName']
            fx = row.get('end_Gx_force', 0)
            fy = row.get('end_Gy_force', 0)
            
            # Determine line attachment point (simplified)
            line_num = int(line_name.replace('Line', ''))
            if line_num <= 4:  # Bow lines
                x_start, y_start = -40, 8
            elif line_num <= 8:  # Bow-mid lines
                x_start, y_start = -20, 10
            elif line_num <= 12:  # Stern-mid lines
                x_start, y_start = 20, 10
            else:  # Stern lines
                x_start, y_start = 40, 8
                
            # Scale forces for visualization
            scale = 0.3
            dx = fx * scale
            dy = fy * scale
            
            # Color based on force direction
            color = 'red' if fx > 0 else 'blue'
            
            # Draw force vector
            arrow = FancyArrowPatch((x_start, y_start), 
                                  (x_start + dx, y_start + dy),
                                  arrowstyle='->', 
                                  color=color, 
                                  linewidth=1.5,
                                  mutation_scale=10,
                                  alpha=0.7)
            ax.add_patch(arrow)
            
            # Add line label
            ax.text(x_start + dx/2, y_start + dy/2, 
                   f'L{line_num}', fontsize=6, alpha=0.7)
        
        ax.set_xlim(-150, 150)
        ax.set_ylim(-150, 150)
        ax.set_aspect('equal')
        ax.grid(True, alpha=0.3)
        ax.set_xlabel('X [m]')
        ax.set_ylabel('Y [m]')
        ax.set_title('Top View - XY Force Vectors')
        
        # Add legend
        red_patch = mpatches.Patch(color='red', label='+X Forces')
        blue_patch = mpatches.Patch(color='blue', label='-X Forces')
        ax.legend(handles=[red_patch, blue_patch], loc='upper right')
    
    def _plot_xz_forces(self, ax, df):
        """Plot XZ plane force vectors."""
        # Vessel representation (side view)
        vessel_rect = Rectangle((-50, -5), 100, 10, 
                               fill=False, edgecolor='black', linewidth=2)
        ax.add_patch(vessel_rect)
        ax.text(0, 0, 'LNGC', ha='center', va='center', fontsize=10, fontweight='bold')
        
        # Water line
        ax.axhline(y=0, color='cyan', linestyle='--', alpha=0.5, label='Water Line')
        
        # Plot force vectors
        for idx, row in df.iterrows():
            line_name = row['ObjectName']
            fx = row.get('end_Gx_force', 0)
            fz = row.get('end_Gz_force', 0)
            
            # Determine attachment point
            line_num = int(line_name.replace('Line', ''))
            if line_num <= 8:  # Forward lines
                x_start = -30 + (line_num-1) * 5
            else:  # Aft lines
                x_start = 10 + (line_num-9) * 5
            z_start = 2  # Deck level
            
            # Scale forces
            scale = 0.3
            dx = fx * scale
            dz = -fz * scale  # Negative because Z is downward
            
            # Draw vector
            color = 'red' if fx > 0 else 'blue'
            arrow = FancyArrowPatch((x_start, z_start), 
                                  (x_start + dx, z_start + dz),
                                  arrowstyle='->', 
                                  color=color, 
                                  linewidth=1.5,
                                  mutation_scale=10,
                                  alpha=0.7)
            ax.add_patch(arrow)
        
        ax.set_xlim(-150, 150)
        ax.set_ylim(-50, 20)
        ax.grid(True, alpha=0.3)
        ax.set_xlabel('X [m]')
        ax.set_ylabel('Z [m]')
        ax.set_title('Side View - XZ Force Vectors')
        ax.legend(loc='upper right')
    
    def _plot_force_magnitudes(self, ax, df):
        """Plot force magnitude bar chart."""
        line_names = df['ObjectName'].values
        line_numbers = [int(name.replace('Line', '')) for name in line_names]
        
        # Calculate force magnitudes
        fx = df['end_Gx_force'].values
        fy = df['end_Gy_force'].values
        fz = df['end_Gz_force'].values
        magnitudes = np.sqrt(fx**2 + fy**2 + fz**2)
        
        # Color based on X-direction
        colors = ['red' if f > 0 else 'blue' for f in fx]
        
        bars = ax.bar(line_numbers, magnitudes, color=colors, alpha=0.7, edgecolor='black')
        
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Force Magnitude [kN]')
        ax.set_title('Mooring Line Force Magnitudes')
        ax.grid(True, alpha=0.3, axis='y')
        ax.set_xticks(line_numbers)
        
        # Add value labels on bars
        for bar, mag in zip(bars, magnitudes):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{mag:.1f}', ha='center', va='bottom', fontsize=8)
    
    def _plot_tension_comparison(self, ax, df):
        """Plot target vs actual tension comparison."""
        if 'target_tension' not in df.columns or 'current_tension' not in df.columns:
            ax.text(0.5, 0.5, 'Tension comparison data not available', 
                   ha='center', va='center', transform=ax.transAxes)
            ax.set_title('Target vs Actual Tension')
            return
        
        line_numbers = [int(name.replace('Line', '')) for name in df['ObjectName']]
        x = np.arange(len(line_numbers))
        width = 0.35
        
        target = df['target_tension'].values
        actual = df['current_tension'].values
        
        bars1 = ax.bar(x - width/2, target, width, label='Target', alpha=0.7, color='green')
        bars2 = ax.bar(x + width/2, actual, width, label='Actual', alpha=0.7, color='orange')
        
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Tension [kN]')
        ax.set_title('Target vs Actual Mooring Tension')
        ax.set_xticks(x)
        ax.set_xticklabels(line_numbers)
        ax.legend()
        ax.grid(True, alpha=0.3, axis='y')
        
        # Add percentage difference
        for i, (t, a) in enumerate(zip(target, actual)):
            if t > 0:
                diff_pct = ((a - t) / t) * 100
                color = 'red' if abs(diff_pct) > 50 else 'black'
                ax.text(i, max(t, a) + 10, f'{diff_pct:+.0f}%', 
                       ha='center', fontsize=7, color=color)
    
    def _plot_force_components(self, ax, df):
        """Plot force component distribution."""
        line_numbers = [int(name.replace('Line', '')) for name in df['ObjectName']]
        
        fx = df['end_Gx_force'].values
        fy = df['end_Gy_force'].values
        fz = df['end_Gz_force'].values
        
        # Stacked bar chart
        width = 0.8
        x = np.arange(len(line_numbers))
        
        p1 = ax.bar(x, np.abs(fx), width, label='|Fx|', alpha=0.7, color='red')
        p2 = ax.bar(x, np.abs(fy), width, bottom=np.abs(fx), label='|Fy|', alpha=0.7, color='green')
        p3 = ax.bar(x, np.abs(fz), width, bottom=np.abs(fx)+np.abs(fy), label='|Fz|', alpha=0.7, color='blue')
        
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Force Components [kN]')
        ax.set_title('Force Component Distribution (Stacked)')
        ax.set_xticks(x)
        ax.set_xticklabels(line_numbers)
        ax.legend()
        ax.grid(True, alpha=0.3, axis='y')
    
    def _plot_directional_forces(self, ax, df, stiffness_df=None):
        """Plot directional force summary."""
        # Calculate directional sums
        fx_positive = df[df['end_Gx_force'] > 0]['end_Gx_force'].sum()
        fx_negative = df[df['end_Gx_force'] < 0]['end_Gx_force'].sum()
        fy_total = df['end_Gy_force'].sum()
        fz_total = df['end_Gz_force'].sum()
        
        # Net forces
        fx_net = fx_positive + fx_negative
        
        # Create summary bars
        categories = ['Fx(+)', 'Fx(-)', 'Fx(net)', 'Fy(total)', 'Fz(total)']
        values = [fx_positive, abs(fx_negative), fx_net, fy_total, fz_total]
        colors = ['red', 'blue', 'purple', 'green', 'orange']
        
        bars = ax.bar(categories, values, color=colors, alpha=0.7, edgecolor='black')
        
        # Add value labels
        for bar, val in zip(bars, values):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{val:.1f} kN', ha='center', va='bottom' if val > 0 else 'top', 
                   fontsize=9, fontweight='bold')
        
        ax.set_ylabel('Force [kN]')
        ax.set_title('Directional Force Summary')
        ax.grid(True, alpha=0.3, axis='y')
        ax.axhline(y=0, color='black', linewidth=0.5)
        
        # Add stiffness info if available
        if stiffness_df is not None and not stiffness_df.empty:
            summary = stiffness_df.iloc[0]
            text = f"Stiffness (kN/m):\n"
            text += f"Kxx(+): {summary.get('K_xx_positive', 0):.1f}\n"
            text += f"Kxx(-): {summary.get('K_xx_negative', 0):.1f}\n"
            text += f"Kxx(net): {summary.get('K_xx_net', 0):.1f}\n"
            text += f"Kyy: {summary.get('K_yy_total', 0):.1f}"
            ax.text(0.95, 0.95, text, transform=ax.transAxes,
                   fontsize=8, verticalalignment='top', horizontalalignment='right',
                   bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    
    def _plot_x_force_components(self, ax, df):
        """Plot X force components with symmetric +/- range."""
        line_names = df['ObjectName'].values
        line_numbers = [int(name.replace('Line', '')) for name in line_names]
        
        fx = df['end_Gx_force'].values
        
        # Create bar chart with positive and negative forces
        colors = ['red' if f > 0 else 'blue' for f in fx]
        bars = ax.bar(line_numbers, fx, color=colors, alpha=0.7, edgecolor='black', linewidth=1)
        
        # Set symmetric y-axis limits
        max_abs_force = np.max(np.abs(fx)) * 1.1  # Add 10% padding
        ax.set_ylim(-max_abs_force, max_abs_force)
        
        # Add horizontal line at zero
        ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
        
        # Add grid
        ax.grid(True, alpha=0.3, axis='y')
        
        # Labels and title
        ax.set_xlabel('Line Number')
        ax.set_ylabel('X Force Component [kN]')
        ax.set_title('X Force Components by Line\n(Positive: +X direction, Negative: -X direction)')
        ax.set_xticks(line_numbers)
        
        # Add value labels on bars
        for bar, force in zip(bars, fx):
            height = bar.get_height()
            if abs(height) > max_abs_force * 0.02:  # Only label if bar is visible
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{force:.1f}', ha='center', 
                       va='bottom' if force > 0 else 'top', 
                       fontsize=7, rotation=90 if abs(force) > 100 else 0)
        
        # Add summary text
        fx_positive_sum = np.sum(fx[fx > 0])
        fx_negative_sum = np.sum(fx[fx < 0])
        fx_net = fx_positive_sum + fx_negative_sum
        
        summary_text = f"Sum(+X): {fx_positive_sum:.1f} kN\n"
        summary_text += f"Sum(-X): {fx_negative_sum:.1f} kN\n"
        summary_text += f"Net: {fx_net:.1f} kN"
        
        ax.text(0.02, 0.98, summary_text, transform=ax.transAxes,
               fontsize=8, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Add legend
        red_patch = mpatches.Patch(color='red', alpha=0.7, label='+X Forces')
        blue_patch = mpatches.Patch(color='blue', alpha=0.7, label='-X Forces')
        ax.legend(handles=[red_patch, blue_patch], loc='upper right', fontsize=8)
    
    def _plot_y_force_components(self, ax, df):
        """Plot Y force components with symmetric +/- range."""
        line_names = df['ObjectName'].values
        line_numbers = [int(name.replace('Line', '')) for name in line_names]
        
        fy = df['end_Gy_force'].values
        
        # Create bar chart with positive and negative forces
        colors = ['green' if f > 0 else 'purple' for f in fy]
        bars = ax.bar(line_numbers, fy, color=colors, alpha=0.7, edgecolor='black', linewidth=1)
        
        # Set symmetric y-axis limits
        max_abs_force = np.max(np.abs(fy)) * 1.1  # Add 10% padding
        ax.set_ylim(-max_abs_force, max_abs_force)
        
        # Add horizontal line at zero
        ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
        
        # Add grid
        ax.grid(True, alpha=0.3, axis='y')
        
        # Labels and title
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Y Force Component [kN]')
        ax.set_title('Y Force Components by Line\n(Positive: +Y direction, Negative: -Y direction)')
        ax.set_xticks(line_numbers)
        
        # Add value labels on bars
        for bar, force in zip(bars, fy):
            height = bar.get_height()
            if abs(height) > max_abs_force * 0.02:  # Only label if bar is visible
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{force:.1f}', ha='center', 
                       va='bottom' if force > 0 else 'top', 
                       fontsize=7, rotation=90 if abs(force) > 100 else 0)
        
        # Add summary text
        fy_positive_sum = np.sum(fy[fy > 0])
        fy_negative_sum = np.sum(fy[fy < 0])
        fy_net = fy_positive_sum + fy_negative_sum
        
        summary_text = f"Sum(+Y): {fy_positive_sum:.1f} kN\n"
        summary_text += f"Sum(-Y): {fy_negative_sum:.1f} kN\n"
        summary_text += f"Net: {fy_net:.1f} kN"
        
        ax.text(0.02, 0.98, summary_text, transform=ax.transAxes,
               fontsize=8, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Add legend
        green_patch = mpatches.Patch(color='green', alpha=0.7, label='+Y Forces')
        purple_patch = mpatches.Patch(color='purple', alpha=0.7, label='-Y Forces')
        ax.legend(handles=[green_patch, purple_patch], loc='upper right', fontsize=8)
    
    def _plot_z_force_components(self, ax, df):
        """Plot Z force components."""
        line_names = df['ObjectName'].values
        line_numbers = [int(name.replace('Line', '')) for name in line_names]
        
        fz = df['end_Gz_force'].values
        
        # Create bar chart (Z forces are typically all negative for mooring)
        colors = ['orange' if f > 0 else 'brown' for f in fz]
        bars = ax.bar(line_numbers, fz, color=colors, alpha=0.7, edgecolor='black', linewidth=1)
        
        # Set y-axis limits with some padding
        min_force = np.min(fz) * 1.1 if np.min(fz) < 0 else np.min(fz) * 0.9
        max_force = np.max(fz) * 1.1 if np.max(fz) > 0 else np.max(fz) * 0.9
        ax.set_ylim(min_force, max_force)
        
        # Add horizontal line at zero
        ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
        
        # Add grid
        ax.grid(True, alpha=0.3, axis='y')
        
        # Labels and title
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Z Force Component [kN]')
        ax.set_title('Z Force Components by Line\n(Positive: upward, Negative: downward)')
        ax.set_xticks(line_numbers)
        
        # Add value labels on bars
        for bar, force in zip(bars, fz):
            height = bar.get_height()
            if abs(height) > abs(min_force - max_force) * 0.02:  # Only label if bar is visible
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{force:.1f}', ha='center', 
                       va='bottom' if force > 0 else 'top', 
                       fontsize=7, rotation=90 if abs(force) > 50 else 0)
        
        # Add summary text
        fz_positive_sum = np.sum(fz[fz > 0])
        fz_negative_sum = np.sum(fz[fz < 0])
        fz_net = fz_positive_sum + fz_negative_sum
        
        summary_text = f"Sum(+Z): {fz_positive_sum:.1f} kN\n"
        summary_text += f"Sum(-Z): {fz_negative_sum:.1f} kN\n"
        summary_text += f"Net: {fz_net:.1f} kN"
        
        ax.text(0.02, 0.98, summary_text, transform=ax.transAxes,
               fontsize=8, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Add legend if both positive and negative forces exist
        if fz_positive_sum > 0 and fz_negative_sum < 0:
            orange_patch = mpatches.Patch(color='orange', alpha=0.7, label='+Z (Upward)')
            brown_patch = mpatches.Patch(color='brown', alpha=0.7, label='-Z (Downward)')
            ax.legend(handles=[orange_patch, brown_patch], loc='upper right', fontsize=8)
        elif fz_negative_sum < 0:
            brown_patch = mpatches.Patch(color='brown', alpha=0.7, label='-Z (Downward)')
            ax.legend(handles=[brown_patch], loc='upper right', fontsize=8)
    
    def _plot_force_polar(self, ax, df):
        """Plot force orientations in polar coordinates."""
        # Calculate angles and magnitudes from force components
        fx = df['end_Gx_force'].values
        fy = df['end_Gy_force'].values
        angles = np.arctan2(fy, fx)
        magnitudes = np.sqrt(fx**2 + fy**2)
        
        # Color by quadrant
        colors = []
        for angle in angles:
            if -np.pi/4 <= angle < np.pi/4:
                colors.append('red')  # +X dominant
            elif np.pi/4 <= angle < 3*np.pi/4:
                colors.append('green')  # +Y dominant
            elif -3*np.pi/4 <= angle < -np.pi/4:
                colors.append('purple')  # -Y dominant
            else:
                colors.append('blue')  # -X dominant
        
        # Plot lines
        for i, (angle, mag, color) in enumerate(zip(angles, magnitudes, colors)):
            ax.plot([angle, angle], [0, mag], color=color, linewidth=2, alpha=0.7)
            ax.scatter(angle, mag, color=color, s=30, edgecolors='black', linewidth=0.5)
            if mag > np.max(magnitudes) * 0.1:  # Label significant forces
                ax.text(angle, mag, f'L{i+1}', fontsize=6, ha='center')
        
        ax.set_title('Force Orientations (XY Plane)', pad=20)
        ax.set_theta_zero_location('E')
        ax.set_theta_direction(-1)
        ax.grid(True, alpha=0.3)
    
    def _plot_force_imbalance(self, ax, df):
        """Plot force imbalance analysis."""
        # Calculate imbalances
        fx = df['end_Gx_force'].values
        fy = df['end_Gy_force'].values
        fz = df['end_Gz_force'].values
        
        fx_pos = fx[fx > 0].sum()
        fx_neg = fx[fx < 0].sum()
        fy_pos = fy[fy > 0].sum()
        fy_neg = fy[fy < 0].sum()
        fz_pos = fz[fz > 0].sum()
        fz_neg = fz[fz < 0].sum()
        
        # Create imbalance metrics
        categories = ['X-Balance', 'Y-Balance', 'Z-Balance']
        positive = [fx_pos, fy_pos, fz_pos]
        negative = [abs(fx_neg), abs(fy_neg), abs(fz_neg)]
        net = [fx_pos + fx_neg, fy_pos + fy_neg, fz_pos + fz_neg]
        
        x = np.arange(len(categories))
        width = 0.25
        
        # Plot grouped bars
        bars1 = ax.bar(x - width, positive, width, label='Positive', color='lightcoral', alpha=0.7)
        bars2 = ax.bar(x, negative, width, label='Negative', color='lightblue', alpha=0.7)
        bars3 = ax.bar(x + width, np.abs(net), width, label='|Net|', color='lightgreen', alpha=0.7)
        
        # Add value labels
        for bars in [bars1, bars2, bars3]:
            for bar in bars:
                height = bar.get_height()
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{height:.0f}', ha='center', va='bottom', fontsize=8)
        
        ax.set_xlabel('Direction')
        ax.set_ylabel('Force [kN]')
        ax.set_title('Force Balance Analysis')
        ax.set_xticks(x)
        ax.set_xticklabels(categories)
        ax.legend()
        ax.grid(True, alpha=0.3, axis='y')
        
        # Add balance indicator
        for i, (cat, n) in enumerate(zip(categories, net)):
            balance_pct = abs(n) / max(positive[i], negative[i]) * 100 if max(positive[i], negative[i]) > 0 else 0
            color = 'green' if balance_pct < 10 else 'orange' if balance_pct < 25 else 'red'
            ax.text(i, -max(positive + negative) * 0.1, f'{balance_pct:.1f}%',
                   ha='center', color=color, fontweight='bold', fontsize=9)
    
    def _plot_system_summary(self, ax, pretension_df, stiffness_df):
        """Plot system summary text panel."""
        ax.axis('off')
        
        # Calculate summary statistics
        fx = pretension_df['end_Gx_force'].sum()
        fy = pretension_df['end_Gy_force'].sum()
        fz = pretension_df['end_Gz_force'].sum()
        f_total = np.sqrt(fx**2 + fy**2 + fz**2)
        
        # Build summary text
        summary_text = "SYSTEM FORCE SUMMARY\n"
        summary_text += "=" * 30 + "\n\n"
        summary_text += f"Net Forces:\n"
        summary_text += f"  Fx: {fx:8.1f} kN\n"
        summary_text += f"  Fy: {fy:8.1f} kN\n"
        summary_text += f"  Fz: {fz:8.1f} kN\n"
        summary_text += f"  |F|: {f_total:8.1f} kN\n\n"
        
        if 'tension_diff_percent' in pretension_df.columns:
            max_diff = pretension_df['tension_diff_percent'].abs().max()
            converged = len(pretension_df[pretension_df['tension_diff_percent'].abs() <= 10])
            total = len(pretension_df)
            summary_text += f"Convergence Status:\n"
            summary_text += f"  Max Error: {max_diff:.1f}%\n"
            summary_text += f"  Converged: {converged}/{total} lines\n"
            summary_text += f"  Status: {'✓ OK' if converged == total else '✗ ITERATE'}\n\n"
        
        if stiffness_df is not None and not stiffness_df.empty:
            s = stiffness_df.iloc[0]
            summary_text += f"Stiffness Preview:\n"
            summary_text += f"  Kxx(net): {s.get('K_xx_net', 0):7.1f} kN/m\n"
            summary_text += f"  Kyy:      {s.get('K_yy_total', 0):7.1f} kN/m\n"
            summary_text += f"  T_surge:  {s.get('T_surge', 0):7.1f} s\n"
        
        ax.text(0.1, 0.9, summary_text, transform=ax.transAxes,
               fontsize=10, verticalalignment='top', family='monospace',
               bbox=dict(boxstyle='round', facecolor='lightgray', alpha=0.3))
    
    def plot_stiffness_characteristics(self, stiffness_analysis_df, stiffness_summary_df, 
                                      save_name="mooring_stiffness"):
        """
        Create comprehensive stiffness visualization matching force plot layout.
        
        Args:
            stiffness_analysis_df (pd.DataFrame): Line-by-line stiffness data
            stiffness_summary_df (pd.DataFrame): System stiffness summary
            save_name (str): Base name for saved plots
        """
        # Create figure with subplots (3x4 grid matching force layout)
        fig = plt.figure(figsize=(28, 21))
        
        # Row 1: Vector Components (X, Y, Z) - Individual line stiffness
        # 1. X Stiffness Components (symmetric +/- range)
        ax1 = plt.subplot(3, 4, 1)
        self._plot_x_stiffness_components(ax1, stiffness_analysis_df)
        
        # 2. Y Stiffness Components
        ax2 = plt.subplot(3, 4, 2)
        self._plot_y_stiffness_components(ax2, stiffness_analysis_df)
        
        # 3. Z Stiffness Components
        ax3 = plt.subplot(3, 4, 3)
        self._plot_z_stiffness_components(ax3, stiffness_analysis_df)
        
        # 4. Total axial stiffness
        ax4 = plt.subplot(3, 4, 4)
        self._plot_axial_stiffness(ax4, stiffness_analysis_df)
        
        # Row 2: System Stiffness Characteristics
        # 5. Individual line stiffness contributions (X and Y)
        ax5 = plt.subplot(3, 4, 5)
        self._plot_line_stiffness(ax5, stiffness_analysis_df)
        
        # 6. Stiffness matrix visualization
        ax6 = plt.subplot(3, 4, 6)
        self._plot_stiffness_matrix(ax6, stiffness_summary_df)
        
        # 7. Directional stiffness breakdown
        ax7 = plt.subplot(3, 4, 7)
        self._plot_directional_stiffness(ax7, stiffness_summary_df)
        
        # 8. Cross-coupling terms
        ax8 = plt.subplot(3, 4, 8)
        self._plot_cross_coupling(ax8, stiffness_summary_df)
        
        # Row 3: Analysis and Natural Response
        # 9. Natural periods
        ax9 = plt.subplot(3, 4, 9)
        self._plot_natural_periods(ax9, stiffness_summary_df)
        
        # 10. Line orientation polar plot
        ax10 = plt.subplot(3, 4, 10, projection='polar')
        self._plot_line_orientations(ax10, stiffness_analysis_df)
        
        # 11. Stiffness vs Force correlation
        ax11 = plt.subplot(3, 4, 11)
        self._plot_stiffness_force_correlation(ax11, stiffness_analysis_df)
        
        # 12. System stiffness summary text
        ax12 = plt.subplot(3, 4, 12)
        self._plot_stiffness_summary(ax12, stiffness_summary_df)
        
        plt.suptitle('Mooring System Stiffness Analysis', fontsize=16, fontweight='bold')
        plt.tight_layout()
        
        # Save figure
        save_path = os.path.join(self.output_directory, f"{save_name}.png")
        plt.savefig(save_path, dpi=150, bbox_inches='tight')
        print(f"Stiffness visualization saved to: {save_path}")
        
        return fig
    
    def _plot_line_stiffness(self, ax, df):
        """Plot individual line stiffness contributions with symmetric +/- range."""
        line_numbers = [int(name.replace('Line', '')) for name in df['ObjectName']]
        
        # Get stiffness components with sign
        k_x_signed = df['k_x_signed'].values if 'k_x_signed' in df.columns else df['k_x'].values
        k_y = df['k_y'].values
        
        # Prepare data for plotting
        x = np.arange(len(line_numbers))
        width = 0.35
        
        # Create bar chart for X stiffness (with +/- values)
        colors_x = ['red' if k > 0 else 'blue' for k in k_x_signed]
        bars_x = ax.bar(x - width/2, k_x_signed, width, 
                        color=colors_x, alpha=0.7, edgecolor='black', 
                        label='Kx (+/- based on Fx direction)')
        
        # Create bar chart for Y stiffness (always same sign based on mooring geometry)
        # Y stiffness sign based on dominant force direction
        colors_y = ['green' if k > 0 else 'purple' for k in k_y]
        bars_y = ax.bar(x + width/2, k_y, width, 
                       color=colors_y, alpha=0.7, edgecolor='black',
                       label='Ky')
        
        # Set symmetric y-axis limits
        all_values = np.concatenate([k_x_signed, k_y])
        max_abs_stiffness = np.max(np.abs(all_values)) * 1.1  # Add 10% padding
        ax.set_ylim(-max_abs_stiffness, max_abs_stiffness)
        
        # Add horizontal line at zero
        ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
        
        # Add grid
        ax.grid(True, alpha=0.3, axis='y')
        
        # Labels and title
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Stiffness [kN/m]')
        ax.set_title('Individual Line Stiffness Contributions\n(Sign indicates force direction contribution)')
        ax.set_xticks(x)
        ax.set_xticklabels(line_numbers)
        
        # Add value labels on significant bars
        for bars in [bars_x, bars_y]:
            for bar in bars:
                height = bar.get_height()
                if abs(height) > max_abs_stiffness * 0.02:  # Only label if bar is visible
                    ax.text(bar.get_x() + bar.get_width()/2., height,
                           f'{height:.0f}', ha='center', 
                           va='bottom' if height > 0 else 'top', 
                           fontsize=6, rotation=90 if abs(height) > 50 else 0)
        
        # Calculate and display summary statistics
        kx_positive = np.sum(k_x_signed[k_x_signed > 0])
        kx_negative = np.sum(k_x_signed[k_x_signed < 0])
        ky_total = np.sum(k_y)
        
        summary_text = f"Kx(+): {kx_positive:.1f} kN/m\n"
        summary_text += f"Kx(-): {abs(kx_negative):.1f} kN/m\n"
        summary_text += f"Kx(net): {kx_positive + kx_negative:.1f} kN/m\n"
        summary_text += f"Ky(total): {ky_total:.1f} kN/m"
        
        ax.text(0.02, 0.98, summary_text, transform=ax.transAxes,
               fontsize=8, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Custom legend
        red_patch = mpatches.Patch(color='red', alpha=0.7, label='Kx (+X direction)')
        blue_patch = mpatches.Patch(color='blue', alpha=0.7, label='Kx (-X direction)')
        green_patch = mpatches.Patch(color='green', alpha=0.7, label='Ky')
        ax.legend(handles=[red_patch, blue_patch, green_patch], 
                 loc='upper right', fontsize=8)
    
    def _plot_directional_stiffness(self, ax, summary_df):
        """Plot directional stiffness breakdown."""
        if summary_df.empty:
            ax.text(0.5, 0.5, 'Summary data not available', 
                   ha='center', va='center', transform=ax.transAxes)
            return
        
        summary = summary_df.iloc[0]
        
        categories = ['Kxx(+)', 'Kxx(-)', 'Kxx(net)', 'Kyy', 'Kzz']
        values = [
            summary.get('K_xx_positive', 0),
            -summary.get('K_xx_negative', 0),  # Show as negative
            summary.get('K_xx_net', 0),
            summary.get('K_yy_total', 0),
            summary.get('K_zz_total', 0)
        ]
        colors = ['red', 'blue', 'purple', 'green', 'orange']
        
        bars = ax.bar(categories, values, color=colors, alpha=0.7, edgecolor='black')
        
        # Add value labels
        for bar, val in zip(bars, values):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{val:.1f}', ha='center', 
                   va='bottom' if val > 0 else 'top', 
                   fontsize=9, fontweight='bold')
        
        ax.set_ylabel('Stiffness [kN/m]')
        ax.set_title('System Directional Stiffness')
        ax.grid(True, alpha=0.3, axis='y')
        ax.axhline(y=0, color='black', linewidth=0.5)
    
    def _plot_stiffness_matrix(self, ax, summary_df):
        """Visualize stiffness matrix."""
        if summary_df.empty:
            ax.text(0.5, 0.5, 'Summary data not available', 
                   ha='center', va='center', transform=ax.transAxes)
            return
        
        summary = summary_df.iloc[0]
        
        # Create stiffness matrix
        K = np.array([
            [summary.get('K_xx_total', 0), summary.get('K_xy_total', 0), summary.get('K_xz_total', 0)],
            [summary.get('K_xy_total', 0), summary.get('K_yy_total', 0), summary.get('K_yz_total', 0)],
            [summary.get('K_xz_total', 0), summary.get('K_yz_total', 0), summary.get('K_zz_total', 0)]
        ])
        
        # Plot matrix as heatmap
        im = ax.imshow(K, cmap='RdBu_r', aspect='auto')
        
        # Add colorbar
        plt.colorbar(im, ax=ax, label='Stiffness [kN/m]')
        
        # Add labels
        labels = ['X', 'Y', 'Z']
        ax.set_xticks(np.arange(3))
        ax.set_yticks(np.arange(3))
        ax.set_xticklabels(labels)
        ax.set_yticklabels(labels)
        
        # Add values
        for i in range(3):
            for j in range(3):
                text = ax.text(j, i, f'{K[i, j]:.1f}',
                             ha="center", va="center", color="black", fontsize=10)
        
        ax.set_title('Stiffness Matrix [kN/m]')
    
    def _plot_natural_periods(self, ax, summary_df):
        """Plot natural periods."""
        if summary_df.empty or 'T_surge' not in summary_df.columns:
            ax.text(0.5, 0.5, 'Natural period data not available', 
                   ha='center', va='center', transform=ax.transAxes)
            return
        
        summary = summary_df.iloc[0]
        
        periods = [
            summary.get('T_surge', 0),
            summary.get('T_sway', 0),
            summary.get('T_heave', 0)
        ]
        labels = ['Surge', 'Sway', 'Heave']
        colors = ['red', 'green', 'blue']
        
        bars = ax.bar(labels, periods, color=colors, alpha=0.7, edgecolor='black')
        
        # Add value labels
        for bar, period in zip(bars, periods):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{period:.1f} s', ha='center', va='bottom', 
                   fontsize=10, fontweight='bold')
        
        ax.set_ylabel('Natural Period [s]')
        ax.set_title('System Natural Periods')
        ax.grid(True, alpha=0.3, axis='y')
        
        # Add typical ranges
        ax.axhspan(15, 25, alpha=0.2, color='red', label='Typical Surge')
        ax.axhspan(25, 40, alpha=0.2, color='green', label='Typical Sway')
        ax.axhspan(30, 45, alpha=0.2, color='blue', label='Typical Heave')
        ax.legend(loc='upper right', fontsize=8)
    
    def _plot_line_orientations(self, ax, df):
        """Plot line orientations in polar coordinates."""
        # Calculate angles from force components
        angles = np.arctan2(df['Fy'].values, df['Fx'].values)
        magnitudes = np.sqrt(df['Fx'].values**2 + df['Fy'].values**2)
        
        # Normalize magnitudes for visualization
        max_mag = magnitudes.max()
        if max_mag > 0:
            magnitudes = magnitudes / max_mag
        
        # Plot lines
        for i, (angle, mag) in enumerate(zip(angles, magnitudes)):
            line_num = i + 1
            color = 'red' if df.iloc[i]['Fx'] > 0 else 'blue'
            ax.plot([angle, angle], [0, mag], color=color, linewidth=2, alpha=0.7)
            ax.text(angle, mag, f'L{line_num}', fontsize=6)
        
        ax.set_title('Line Force Orientations (XY Plane)', pad=20)
        ax.set_theta_zero_location('E')
        ax.set_theta_direction(-1)
    
    def _plot_stiffness_force_correlation(self, ax, df):
        """Plot correlation between stiffness and force."""
        # Get force magnitudes and stiffness
        force_mag = np.sqrt(df['Fx'].values**2 + df['Fy'].values**2 + df['Fz'].values**2)
        k_axial = df['k_axial'].values
        
        # Color by X-direction
        colors = ['red' if f > 0 else 'blue' for f in df['Fx'].values]
        
        scatter = ax.scatter(force_mag, k_axial, c=colors, alpha=0.6, s=50, edgecolors='black')
        
        # Add trend line
        z = np.polyfit(force_mag, k_axial, 1)
        p = np.poly1d(z)
        x_trend = np.linspace(force_mag.min(), force_mag.max(), 100)
        ax.plot(x_trend, p(x_trend), "k--", alpha=0.5, label=f'Trend: k = {z[0]:.2f}F + {z[1]:.1f}')
        
        ax.set_xlabel('Force Magnitude [kN]')
        ax.set_ylabel('Axial Stiffness [kN/m]')
        ax.set_title('Stiffness vs Force Correlation')
        ax.grid(True, alpha=0.3)
        ax.legend()
        
        # Add R-squared
        residuals = k_axial - p(force_mag)
        ss_res = np.sum(residuals**2)
        ss_tot = np.sum((k_axial - np.mean(k_axial))**2)
        r_squared = 1 - (ss_res / ss_tot) if ss_tot > 0 else 0
        ax.text(0.05, 0.95, f'R² = {r_squared:.3f}', transform=ax.transAxes,
               fontsize=10, verticalalignment='top')
    
    def _plot_x_stiffness_components(self, ax, df):
        """Plot X stiffness components with symmetric +/- range matching force plot style."""
        line_numbers = [int(name.replace('Line', '')) for name in df['ObjectName']]
        
        # Get signed X stiffness
        k_x_signed = df['k_x_signed'].values if 'k_x_signed' in df.columns else df['k_x'].values
        
        # Create bar chart with consistent colors (red for +X, blue for -X)
        colors = ['red' if k > 0 else 'blue' for k in k_x_signed]
        bars = ax.bar(line_numbers, k_x_signed, color=colors, alpha=0.7, edgecolor='black', linewidth=1)
        
        # Set symmetric y-axis limits
        max_abs_stiff = np.max(np.abs(k_x_signed)) * 1.1
        ax.set_ylim(-max_abs_stiff, max_abs_stiff)
        
        # Add horizontal line at zero
        ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
        
        # Add grid
        ax.grid(True, alpha=0.3, axis='y')
        
        # Labels and title
        ax.set_xlabel('Line Number')
        ax.set_ylabel('X Stiffness Component [kN/m]')
        ax.set_title('X Stiffness Components by Line\n(Sign from force direction)')
        ax.set_xticks(line_numbers)
        
        # Add value labels on bars
        for bar, stiff in zip(bars, k_x_signed):
            height = bar.get_height()
            if abs(height) > max_abs_stiff * 0.02:
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{stiff:.0f}', ha='center',
                       va='bottom' if stiff > 0 else 'top',
                       fontsize=7, rotation=90 if abs(stiff) > 50 else 0)
        
        # Add summary text
        kx_positive = np.sum(k_x_signed[k_x_signed > 0])
        kx_negative = np.sum(k_x_signed[k_x_signed < 0])
        kx_net = kx_positive + kx_negative
        
        summary_text = f"Sum(+X): {kx_positive:.1f} kN/m\n"
        summary_text += f"Sum(-X): {kx_negative:.1f} kN/m\n"
        summary_text += f"Net: {kx_net:.1f} kN/m"
        
        ax.text(0.02, 0.98, summary_text, transform=ax.transAxes,
               fontsize=8, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Add legend
        red_patch = mpatches.Patch(color='red', alpha=0.7, label='+X Stiffness')
        blue_patch = mpatches.Patch(color='blue', alpha=0.7, label='-X Stiffness')
        ax.legend(handles=[red_patch, blue_patch], loc='upper right', fontsize=8)
    
    def _plot_y_stiffness_components(self, ax, df):
        """Plot Y stiffness components matching force plot style."""
        line_numbers = [int(name.replace('Line', '')) for name in df['ObjectName']]
        
        k_y = df['k_y'].values
        
        # Create bar chart with consistent colors (green for +Y, purple for -Y)
        colors = ['green' if k > 0 else 'purple' for k in k_y]
        bars = ax.bar(line_numbers, k_y, color=colors, alpha=0.7, edgecolor='black', linewidth=1)
        
        # Set symmetric y-axis limits
        max_abs_stiff = np.max(np.abs(k_y)) * 1.1
        ax.set_ylim(-max_abs_stiff, max_abs_stiff)
        
        # Add horizontal line at zero
        ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
        
        # Add grid
        ax.grid(True, alpha=0.3, axis='y')
        
        # Labels and title
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Y Stiffness Component [kN/m]')
        ax.set_title('Y Stiffness Components by Line')
        ax.set_xticks(line_numbers)
        
        # Add value labels on bars
        for bar, stiff in zip(bars, k_y):
            height = bar.get_height()
            if abs(height) > max_abs_stiff * 0.02:
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{stiff:.0f}', ha='center',
                       va='bottom' if stiff > 0 else 'top',
                       fontsize=7, rotation=90 if abs(stiff) > 50 else 0)
        
        # Add summary text
        ky_positive = np.sum(k_y[k_y > 0])
        ky_negative = np.sum(k_y[k_y < 0])
        ky_total = np.sum(k_y)
        
        summary_text = f"Sum(+Y): {ky_positive:.1f} kN/m\n"
        summary_text += f"Sum(-Y): {abs(ky_negative):.1f} kN/m\n"
        summary_text += f"Total: {ky_total:.1f} kN/m"
        
        ax.text(0.02, 0.98, summary_text, transform=ax.transAxes,
               fontsize=8, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        
        # Add legend
        green_patch = mpatches.Patch(color='green', alpha=0.7, label='+Y Stiffness')
        purple_patch = mpatches.Patch(color='purple', alpha=0.7, label='-Y Stiffness')
        ax.legend(handles=[green_patch, purple_patch], loc='upper right', fontsize=8)
    
    def _plot_z_stiffness_components(self, ax, df):
        """Plot Z stiffness components."""
        line_numbers = [int(name.replace('Line', '')) for name in df['ObjectName']]
        
        k_z = df['k_z'].values
        
        # Create bar chart (orange for +Z, brown for -Z matching force colors)
        colors = ['orange' if k > 0 else 'brown' for k in k_z]
        bars = ax.bar(line_numbers, k_z, color=colors, alpha=0.7, edgecolor='black', linewidth=1)
        
        # Set y-axis limits with padding
        min_stiff = np.min(k_z) * 1.1 if np.min(k_z) < 0 else np.min(k_z) * 0.9
        max_stiff = np.max(k_z) * 1.1 if np.max(k_z) > 0 else 0
        ax.set_ylim(min_stiff, max_stiff * 1.2)
        
        # Add horizontal line at zero
        ax.axhline(y=0, color='black', linewidth=1, linestyle='-')
        
        # Add grid
        ax.grid(True, alpha=0.3, axis='y')
        
        # Labels and title
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Z Stiffness Component [kN/m]')
        ax.set_title('Z Stiffness Components by Line')
        ax.set_xticks(line_numbers)
        
        # Add value labels on bars
        for bar, stiff in zip(bars, k_z):
            height = bar.get_height()
            if abs(height) > abs(max_stiff - min_stiff) * 0.02:
                ax.text(bar.get_x() + bar.get_width()/2., height,
                       f'{stiff:.1f}', ha='center',
                       va='bottom' if stiff > 0 else 'top',
                       fontsize=7, rotation=0)
        
        # Add summary text
        kz_total = np.sum(k_z)
        summary_text = f"Total Kz: {kz_total:.1f} kN/m"
        
        ax.text(0.02, 0.98, summary_text, transform=ax.transAxes,
               fontsize=8, verticalalignment='top',
               bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
    
    def _plot_axial_stiffness(self, ax, df):
        """Plot axial stiffness of each line."""
        line_numbers = [int(name.replace('Line', '')) for name in df['ObjectName']]
        k_axial = df['k_axial'].values
        
        # Color based on magnitude
        colors = plt.cm.viridis(k_axial / k_axial.max())
        bars = ax.bar(line_numbers, k_axial, color=colors, alpha=0.7, edgecolor='black')
        
        ax.set_xlabel('Line Number')
        ax.set_ylabel('Axial Stiffness [kN/m]')
        ax.set_title('Line Axial Stiffness (EA/L)')
        ax.set_xticks(line_numbers)
        ax.grid(True, alpha=0.3, axis='y')
        
        # Add value labels on bars
        for bar, stiff in zip(bars, k_axial):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{stiff:.0f}', ha='center', va='bottom', fontsize=7)
        
        # Add statistics
        mean_k = np.mean(k_axial)
        std_k = np.std(k_axial)
        ax.axhline(y=mean_k, color='red', linestyle='--', alpha=0.5, label=f'Mean: {mean_k:.1f}')
        ax.fill_between(range(-1, len(line_numbers)+1), mean_k-std_k, mean_k+std_k,
                        color='red', alpha=0.1, label=f'±σ: {std_k:.1f}')
        ax.legend(loc='upper right', fontsize=8)
    
    def _plot_cross_coupling(self, ax, summary_df):
        """Plot cross-coupling stiffness terms."""
        if summary_df.empty:
            ax.text(0.5, 0.5, 'Summary data not available',
                   ha='center', va='center', transform=ax.transAxes)
            return
        
        summary = summary_df.iloc[0]
        
        # Get cross-coupling terms
        k_xy = summary.get('K_xy_total', 0)
        k_xz = summary.get('K_xz_total', 0)
        k_yz = summary.get('K_yz_total', 0)
        
        categories = ['Kxy', 'Kxz', 'Kyz']
        values = [k_xy, k_xz, k_yz]
        colors = ['purple', 'orange', 'brown']
        
        bars = ax.bar(categories, values, color=colors, alpha=0.7, edgecolor='black')
        
        # Set symmetric y-axis
        max_abs = np.max(np.abs(values)) * 1.2
        ax.set_ylim(-max_abs, max_abs)
        ax.axhline(y=0, color='black', linewidth=1)
        
        # Add value labels
        for bar, val in zip(bars, values):
            height = bar.get_height()
            ax.text(bar.get_x() + bar.get_width()/2., height,
                   f'{val:.1f}', ha='center',
                   va='bottom' if val > 0 else 'top',
                   fontsize=9, fontweight='bold')
        
        ax.set_ylabel('Cross-Coupling Stiffness [kN/m]')
        ax.set_title('Stiffness Cross-Coupling Terms')
        ax.grid(True, alpha=0.3, axis='y')
        
        # Add interpretation text
        coupling_strength = np.mean(np.abs(values)) / np.mean([summary.get('K_xx_total', 1), 
                                                               summary.get('K_yy_total', 1)])
        interpretation = "Weak" if coupling_strength < 0.1 else "Moderate" if coupling_strength < 0.3 else "Strong"
        ax.text(0.5, 0.95, f'Coupling: {interpretation} ({coupling_strength*100:.1f}%)',
               transform=ax.transAxes, ha='center', fontsize=9,
               bbox=dict(boxstyle='round', facecolor='yellow', alpha=0.3))
    
    def _plot_stiffness_summary(self, ax, summary_df):
        """Plot stiffness summary text panel."""
        ax.axis('off')
        
        if summary_df.empty:
            ax.text(0.5, 0.5, 'Summary data not available',
                   ha='center', va='center', transform=ax.transAxes)
            return
        
        s = summary_df.iloc[0]
        
        # Build summary text
        summary_text = "SYSTEM STIFFNESS SUMMARY\n"
        summary_text += "=" * 30 + "\n\n"
        summary_text += f"Diagonal Terms:\n"
        summary_text += f"  Kxx(total): {s.get('K_xx_total', 0):8.1f} kN/m\n"
        summary_text += f"  Kxx(+):     {s.get('K_xx_positive', 0):8.1f} kN/m\n"
        summary_text += f"  Kxx(-):     {s.get('K_xx_negative', 0):8.1f} kN/m\n"
        summary_text += f"  Kxx(net):   {s.get('K_xx_net', 0):8.1f} kN/m\n"
        summary_text += f"  Kyy:        {s.get('K_yy_total', 0):8.1f} kN/m\n"
        summary_text += f"  Kzz:        {s.get('K_zz_total', 0):8.1f} kN/m\n\n"
        
        summary_text += f"Cross-Coupling:\n"
        summary_text += f"  Kxy:        {s.get('K_xy_total', 0):8.1f} kN/m\n"
        summary_text += f"  Kxz:        {s.get('K_xz_total', 0):8.1f} kN/m\n"
        summary_text += f"  Kyz:        {s.get('K_yz_total', 0):8.1f} kN/m\n\n"
        
        summary_text += f"Natural Periods:\n"
        summary_text += f"  T_surge:    {s.get('T_surge', 0):8.1f} s\n"
        summary_text += f"  T_sway:     {s.get('T_sway', 0):8.1f} s\n"
        summary_text += f"  T_heave:    {s.get('T_heave', 0):8.1f} s\n\n"
        
        # Add directional balance indicator
        if s.get('K_xx_net', 0) != 0:
            balance = abs(s.get('K_xx_net', 0)) / s.get('K_xx_total', 1) * 100
            status = '✓ Balanced' if balance < 10 else '⚠ Imbalanced'
            summary_text += f"X-Direction Balance:\n"
            summary_text += f"  Status: {status}\n"
            summary_text += f"  Imbalance: {balance:.1f}%\n"
        
        ax.text(0.1, 0.9, summary_text, transform=ax.transAxes,
               fontsize=10, verticalalignment='top', family='monospace',
               bbox=dict(boxstyle='round', facecolor='lightgray', alpha=0.3))