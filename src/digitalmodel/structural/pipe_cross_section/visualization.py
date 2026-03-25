# ABOUTME: Visualization module for pipe cross-section diagrams.
# ABOUTME: Creates interactive HTML reports and static PNG exports using Plotly and matplotlib.

"""
Pipe Cross-Section Visualization
================================

Provides visualization tools for pipe cross-section analysis:
- Interactive HTML reports with Plotly
- Static PNG/SVG exports for engineering reports
- CSV data export

Compatible with digitalmodel reporting standards.
"""

import math
from pathlib import Path
from typing import Optional, List, Tuple, Union
import csv

from .calculator import PipeCrossSection
from .models import PipeLayer


def create_circle_points(
    radius: float, center: Tuple[float, float] = (0, 0), num_points: int = 100
) -> Tuple[List[float], List[float]]:
    """Generate x, y coordinates for a circle."""
    angles = [2 * math.pi * i / num_points for i in range(num_points + 1)]
    x = [center[0] + radius * math.cos(a) for a in angles]
    y = [center[1] + radius * math.sin(a) for a in angles]
    return x, y


class PipeCrossSectionVisualizer:
    """
    Visualization generator for pipe cross-sections.

    Provides methods for generating:
    - Interactive Plotly figures
    - HTML reports with summary cards
    - Static matplotlib exports
    - CSV data exports
    """

    def __init__(self, pipe: PipeCrossSection):
        """
        Initialize visualizer with a pipe cross-section.

        Args:
            pipe: PipeCrossSection instance to visualize
        """
        self.pipe = pipe

    def create_cross_section_figure(self):
        """Create interactive cross-section diagram with Plotly."""
        try:
            import plotly.graph_objects as go
        except ImportError:
            raise ImportError("plotly is required for interactive visualization. Install with: pip install plotly")

        fig = go.Figure()

        # Draw layers from outside to inside
        for i, layer in enumerate(reversed(self.pipe.layers)):
            x_outer, y_outer = create_circle_points(layer.outer_diameter_mm / 2)

            hover_text = (
                f"<b>{layer.name}</b><br>"
                f"ID: {layer.inner_diameter_mm:.1f} mm<br>"
                f"OD: {layer.outer_diameter_mm:.1f} mm<br>"
                f"Thickness: {layer.thickness_mm:.2f} mm<br>"
                f"Density: {layer.density_kg_m3} kg/m³<br>"
                f"Weight: {layer.weight_per_meter_kg:.1f} kg/m"
            )

            fig.add_trace(
                go.Scatter(
                    x=x_outer,
                    y=y_outer,
                    fill="toself",
                    fillcolor=layer.color,
                    line=dict(color="black", width=1),
                    name=layer.name,
                    hoverinfo="text",
                    hovertext=hover_text,
                )
            )

        # Draw inner bore
        bore_radius = self.pipe.inner_diameter_mm / 2
        x_bore, y_bore = create_circle_points(bore_radius)
        fig.add_trace(
            go.Scatter(
                x=x_bore,
                y=y_bore,
                fill="toself",
                fillcolor="white",
                line=dict(color="black", width=1),
                name="Internal Bore",
                hoverinfo="text",
                hovertext=f"<b>Internal Bore</b><br>Diameter: {self.pipe.inner_diameter_mm:.1f} mm<br>Contents: {self.pipe.internal_contents}",
            )
        )

        # Dimension annotation
        outer_radius = self.pipe.outer_diameter_mm / 2
        fig.add_annotation(
            x=outer_radius + 20,
            y=0,
            text=f"OD: {self.pipe.outer_diameter_mm:.1f} mm<br>({self.pipe.outer_diameter_inch:.2f} in)",
            showarrow=True,
            arrowhead=2,
            ax=60,
            ay=0,
        )

        # Update layout
        fig.update_layout(
            title=dict(
                text="<b>Cross-Section View</b>",
                x=0.5,
                xanchor="center",
            ),
            showlegend=True,
            legend=dict(
                orientation="h",
                yanchor="top",
                y=-0.05,
                xanchor="center",
                x=0.5,
                bgcolor="rgba(255,255,255,0.8)",
                bordercolor="lightgray",
                borderwidth=1,
            ),
            height=550,
            margin=dict(l=40, r=40, t=60, b=100),
        )

        fig.update_xaxes(
            scaleanchor="y",
            scaleratio=1,
            showgrid=True,
            gridcolor="lightgray",
            title="mm",
        )
        fig.update_yaxes(showgrid=True, gridcolor="lightgray", title="mm")

        return fig

    def create_weight_distribution_figure(self):
        """Create weight distribution pie chart with Plotly."""
        try:
            import plotly.graph_objects as go
        except ImportError:
            raise ImportError("plotly is required for interactive visualization. Install with: pip install plotly")

        weights = [layer.weight_per_meter_kg for layer in self.pipe.layers]
        labels = [layer.name for layer in self.pipe.layers]
        colors = [layer.color for layer in self.pipe.layers]

        fig = go.Figure()

        fig.add_trace(
            go.Pie(
                values=weights,
                labels=labels,
                marker=dict(colors=colors, line=dict(color="black", width=1)),
                textinfo="label+percent",
                textposition="inside",
                hovertemplate="<b>%{label}</b><br>Weight: %{value:.1f} kg/m<br>Percentage: %{percent}<extra></extra>",
            )
        )

        # Update layout
        fig.update_layout(
            title=dict(
                text="<b>Weight Distribution</b>",
                x=0.5,
                xanchor="center",
            ),
            showlegend=True,
            legend=dict(
                orientation="h",
                yanchor="top",
                y=-0.05,
                xanchor="center",
                x=0.5,
                bgcolor="rgba(255,255,255,0.8)",
                bordercolor="lightgray",
                borderwidth=1,
            ),
            height=550,
            margin=dict(l=40, r=40, t=60, b=100),
        )

        return fig

    def create_plotly_figure(self):
        """Create combined cross-section and weight distribution figures.

        Returns a tuple of (cross_section_fig, weight_distribution_fig).
        For backward compatibility, can also be used as a single figure context.
        """
        return self.create_cross_section_figure(), self.create_weight_distribution_figure()

    def get_plots_html(self) -> str:
        """Generate HTML for both charts in a responsive grid layout."""
        cross_section_fig = self.create_cross_section_figure()
        weight_fig = self.create_weight_distribution_figure()

        # Convert figures to HTML divs (without full HTML wrapper)
        cross_section_html = cross_section_fig.to_html(
            full_html=False,
            include_plotlyjs=False,
            div_id="cross-section-chart"
        )
        weight_html = weight_fig.to_html(
            full_html=False,
            include_plotlyjs=False,
            div_id="weight-chart"
        )

        return f"""
        <div style="display: grid; grid-template-columns: 1.2fr 1fr; gap: 20px; margin: 20px 0;">
            <div style="background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); padding: 10px;">
                {cross_section_html}
            </div>
            <div style="background: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); padding: 10px;">
                {weight_html}
            </div>
        </div>
        """

    def _create_summary_cards_html(self) -> str:
        """Generate HTML for summary statistics cards."""
        summary = self.pipe.get_summary()
        sink_float = "SINKS" if self.pipe.submerged_weight_kg_m > 0 else "FLOATS"
        status_color = "#dc3545" if self.pipe.submerged_weight_kg_m > 0 else "#28a745"

        return f"""
        <div style="display: flex; flex-wrap: wrap; gap: 15px; margin: 20px 0;">
            <div style="flex: 1; min-width: 200px; padding: 15px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); border-radius: 10px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);">
                <h4 style="margin: 0 0 10px 0; font-size: 14px; opacity: 0.9;">OUTER DIAMETER</h4>
                <p style="margin: 0; font-size: 24px; font-weight: bold;">{summary['Outer_Diameter_mm']:.1f} mm</p>
                <p style="margin: 5px 0 0 0; font-size: 14px; opacity: 0.8;">({summary['Outer_Diameter_inch']:.2f} in)</p>
            </div>
            <div style="flex: 1; min-width: 200px; padding: 15px; background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); border-radius: 10px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);">
                <h4 style="margin: 0 0 10px 0; font-size: 14px; opacity: 0.9;">WEIGHT IN AIR</h4>
                <p style="margin: 0; font-size: 24px; font-weight: bold;">{summary['Total_Weight_Air_kg_m']:.1f} kg/m</p>
                <p style="margin: 5px 0 0 0; font-size: 14px; opacity: 0.8;">({summary['Total_Weight_Air_kN_m']:.3f} kN/m)</p>
            </div>
            <div style="flex: 1; min-width: 200px; padding: 15px; background: linear-gradient(135deg, #4facfe 0%, #00f2fe 100%); border-radius: 10px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);">
                <h4 style="margin: 0 0 10px 0; font-size: 14px; opacity: 0.9;">BUOYANCY FORCE</h4>
                <p style="margin: 0; font-size: 24px; font-weight: bold;">{summary['Buoyancy_Force_kg_m']:.1f} kg/m</p>
                <p style="margin: 5px 0 0 0; font-size: 14px; opacity: 0.8;">Displaced: {summary['Displaced_Volume_m3_m']:.4f} m³/m</p>
            </div>
            <div style="flex: 1; min-width: 200px; padding: 15px; background: linear-gradient(135deg, {status_color} 0%, {status_color}99 100%); border-radius: 10px; color: white; box-shadow: 0 4px 6px rgba(0,0,0,0.1);">
                <h4 style="margin: 0 0 10px 0; font-size: 14px; opacity: 0.9;">SUBMERGED WEIGHT</h4>
                <p style="margin: 0; font-size: 24px; font-weight: bold;">{summary['Submerged_Weight_kg_m']:.1f} kg/m</p>
                <p style="margin: 5px 0 0 0; font-size: 14px; opacity: 0.8;">Pipe {sink_float}</p>
            </div>
        </div>
        """

    def _create_layer_table_html(self) -> str:
        """Generate HTML table for layer details."""
        rows = ""
        for layer in self.pipe.layers:
            rows += f"""
            <tr>
                <td style="border: 1px solid #ddd; padding: 10px;">
                    <span style="display: inline-block; width: 15px; height: 15px; background: {layer.color}; border-radius: 3px; margin-right: 8px; vertical-align: middle;"></span>
                    {layer.name}
                </td>
                <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{layer.inner_diameter_mm:.1f}</td>
                <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{layer.outer_diameter_mm:.1f}</td>
                <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{layer.thickness_mm:.2f}</td>
                <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{layer.density_kg_m3}</td>
                <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{layer.cross_sectional_area_m2:.5f}</td>
                <td style="border: 1px solid #ddd; padding: 10px; text-align: right; font-weight: bold;">{layer.weight_per_meter_kg:.1f}</td>
            </tr>
            """

        return f"""
        <h3 style="margin-top: 30px;">Layer Properties</h3>
        <table style="width: 100%; border-collapse: collapse; margin: 15px 0; font-size: 14px;">
            <thead>
                <tr style="background: #f8f9fa;">
                    <th style="border: 1px solid #ddd; padding: 12px; text-align: left;">Layer</th>
                    <th style="border: 1px solid #ddd; padding: 12px; text-align: right;">ID (mm)</th>
                    <th style="border: 1px solid #ddd; padding: 12px; text-align: right;">OD (mm)</th>
                    <th style="border: 1px solid #ddd; padding: 12px; text-align: right;">Thickness (mm)</th>
                    <th style="border: 1px solid #ddd; padding: 12px; text-align: right;">Density (kg/m³)</th>
                    <th style="border: 1px solid #ddd; padding: 12px; text-align: right;">Area (m²/m)</th>
                    <th style="border: 1px solid #ddd; padding: 12px; text-align: right;">Weight (kg/m)</th>
                </tr>
            </thead>
            <tbody>
                {rows}
                <tr style="background: #e9ecef; font-weight: bold;">
                    <td style="border: 1px solid #ddd; padding: 10px;">TOTAL</td>
                    <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{self.pipe.inner_diameter_mm:.1f}</td>
                    <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{self.pipe.outer_diameter_mm:.1f}</td>
                    <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">-</td>
                    <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">-</td>
                    <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{sum(l.cross_sectional_area_m2 for l in self.pipe.layers):.5f}</td>
                    <td style="border: 1px solid #ddd; padding: 10px; text-align: right;">{self.pipe.total_weight_in_air_kg_m:.1f}</td>
                </tr>
            </tbody>
        </table>
        """

    def generate_html_report(self, output_path: Union[str, Path]) -> Path:
        """Generate complete interactive HTML report."""
        output_path = Path(output_path)

        # Get the grid layout HTML for both charts
        plots_html = self.get_plots_html()

        summary_cards = self._create_summary_cards_html()
        layer_table = self._create_layer_table_html()

        html_content = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Pipe Cross-Section Analysis Report</title>
    <script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
            color: #333;
        }}
        .container {{
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 10px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }}
        h1 {{
            color: #2c3e50;
            border-bottom: 3px solid #3498db;
            padding-bottom: 15px;
        }}
        h2 {{
            color: #34495e;
            margin-top: 30px;
        }}
        .input-params {{
            background: #f8f9fa;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
        }}
        .input-params table {{
            width: 100%;
        }}
        .input-params td {{
            padding: 8px;
        }}
        .footer {{
            margin-top: 40px;
            padding-top: 20px;
            border-top: 1px solid #ddd;
            text-align: center;
            color: #666;
            font-size: 12px;
        }}
        @media (max-width: 900px) {{
            .container > div > div {{
                grid-template-columns: 1fr !important;
            }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <h1>Pipe Cross-Section Analysis Report</h1>

        <h2>Summary</h2>
        {summary_cards}

        <h2>Cross-Section Visualization</h2>
        {plots_html}

        {layer_table}

        <h2>Input Parameters</h2>
        <div class="input-params">
            <table>
                <tr>
                    <td><strong>Steel Pipe OD:</strong></td>
                    <td>{self.pipe.steel_od_mm:.1f} mm ({self.pipe.steel_od_mm/25.4:.2f} in)</td>
                    <td><strong>Steel Density:</strong></td>
                    <td>{self.pipe.steel_density} kg/m³</td>
                </tr>
                <tr>
                    <td><strong>Steel Wall Thickness:</strong></td>
                    <td>{self.pipe.steel_wt_mm:.2f} mm ({self.pipe.steel_wt_mm/25.4:.4f} in)</td>
                    <td><strong>3LPP Density:</strong></td>
                    <td>{self.pipe.lpp_density} kg/m³</td>
                </tr>
                <tr>
                    <td><strong>3LPP Thickness:</strong></td>
                    <td>{self.pipe.lpp_thickness_mm:.1f} mm</td>
                    <td><strong>Concrete Density:</strong></td>
                    <td>{self.pipe.concrete_density} kg/m³</td>
                </tr>
                <tr>
                    <td><strong>Concrete Thickness:</strong></td>
                    <td>{self.pipe.concrete_thickness_mm:.1f} mm ({self.pipe.concrete_thickness_mm/25.4:.2f} in)</td>
                    <td><strong>Seawater Density:</strong></td>
                    <td>{self.pipe.seawater_density} kg/m³</td>
                </tr>
                <tr>
                    <td><strong>Internal Contents:</strong></td>
                    <td>{self.pipe.internal_contents}</td>
                    <td></td>
                    <td></td>
                </tr>
            </table>
        </div>

        <div class="footer">
            <p>Generated by DigitalModel Pipe Cross-Section Module</p>
            <p>Calculations per DNV-ST-F101, API 5L, ISO 21809 standards</p>
        </div>
    </div>
</body>
</html>
        """

        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(html_content)
        return output_path

    def export_static_image(self, output_path: Union[str, Path]) -> Optional[Path]:
        """Export static PNG image of cross-section using matplotlib."""
        output_path = Path(output_path)

        try:
            import matplotlib.pyplot as plt
            import matplotlib.patches as patches
        except ImportError:
            print("matplotlib not available for static export")
            return None

        fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 7))

        # Cross-section view
        ax1.set_aspect("equal")
        ax1.set_title("Pipe Cross-Section", fontsize=14, fontweight="bold")

        # Draw layers from outside to inside
        for layer in reversed(self.pipe.layers):
            circle = patches.Circle(
                (0, 0),
                layer.outer_diameter_mm / 2,
                facecolor=layer.color,
                edgecolor="black",
                linewidth=1,
                label=layer.name,
            )
            ax1.add_patch(circle)

        # Inner bore
        bore = patches.Circle(
            (0, 0),
            self.pipe.inner_diameter_mm / 2,
            facecolor="white",
            edgecolor="black",
            linewidth=1,
            label="Internal Bore",
        )
        ax1.add_patch(bore)

        # Set axis limits
        limit = self.pipe.outer_diameter_mm / 2 * 1.2
        ax1.set_xlim(-limit, limit)
        ax1.set_ylim(-limit, limit)
        ax1.set_xlabel("mm")
        ax1.set_ylabel("mm")
        ax1.grid(True, alpha=0.3)
        ax1.legend(loc="upper right", fontsize=9)

        # Dimension annotation
        ax1.annotate(
            f"OD: {self.pipe.outer_diameter_mm:.1f} mm\n({self.pipe.outer_diameter_inch:.2f} in)",
            xy=(self.pipe.outer_diameter_mm / 2, 0),
            xytext=(self.pipe.outer_diameter_mm / 2 + 50, 50),
            fontsize=9,
            arrowprops=dict(arrowstyle="->", color="gray"),
        )

        # Pie chart
        weights = [layer.weight_per_meter_kg for layer in self.pipe.layers]
        labels = [f"{layer.name}\n{layer.weight_per_meter_kg:.1f} kg/m" for layer in self.pipe.layers]
        colors = [layer.color for layer in self.pipe.layers]

        ax2.pie(weights, labels=labels, colors=colors, autopct="%1.1f%%", startangle=90)
        ax2.set_title("Weight Distribution", fontsize=14, fontweight="bold")

        # Summary text
        summary_text = (
            f"Total Weight: {self.pipe.total_weight_in_air_kg_m:.1f} kg/m | "
            f"Buoyancy: {self.pipe.buoyancy_force_kg_m:.1f} kg/m | "
            f"Submerged: {self.pipe.submerged_weight_kg_m:.1f} kg/m"
        )
        fig.text(0.5, 0.02, summary_text, ha="center", fontsize=10, style="italic")

        plt.suptitle(
            f"{self.pipe.steel_od_mm:.0f}mm Coated Subsea Pipeline Cross-Section",
            fontsize=16,
            fontweight="bold",
            y=0.98,
        )

        plt.tight_layout(rect=[0, 0.05, 1, 0.95])

        output_path.parent.mkdir(parents=True, exist_ok=True)
        plt.savefig(output_path, dpi=150, bbox_inches="tight", facecolor="white")
        plt.close()

        return output_path

    def export_csv(self, output_path: Union[str, Path]) -> Path:
        """Export pipe properties to CSV file."""
        output_path = Path(output_path)
        data = self.pipe.to_csv_data()
        output_path.parent.mkdir(parents=True, exist_ok=True)

        with open(output_path, "w", newline="") as f:
            writer = csv.DictWriter(f, fieldnames=data[0].keys())
            writer.writeheader()
            writer.writerows(data)

        return output_path


# Convenience functions for backward compatibility
def generate_html_report(pipe: PipeCrossSection, output_path: Union[str, Path]) -> Path:
    """Generate HTML report for a pipe cross-section."""
    viz = PipeCrossSectionVisualizer(pipe)
    return viz.generate_html_report(output_path)


def export_static_image(pipe: PipeCrossSection, output_path: Union[str, Path]) -> Optional[Path]:
    """Export static PNG image of pipe cross-section."""
    viz = PipeCrossSectionVisualizer(pipe)
    return viz.export_static_image(output_path)


def export_csv(pipe: PipeCrossSection, output_path: Union[str, Path]) -> Path:
    """Export pipe properties to CSV."""
    viz = PipeCrossSectionVisualizer(pipe)
    return viz.export_csv(output_path)
