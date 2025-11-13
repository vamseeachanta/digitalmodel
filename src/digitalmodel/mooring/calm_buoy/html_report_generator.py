"""
ABOUTME: Generate interactive HTML reports for CALM buoy operational analysis using Plotly.
ABOUTME: Creates comprehensive dashboards with environmental conditions, mooring analysis, and performance metrics.
"""

import pandas as pd
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.express as px
from pathlib import Path
from typing import Dict, List, Optional
from datetime import datetime


class CALMBuoyReportGenerator:
    """Generate interactive HTML reports for CALM buoy operational analysis."""

    def __init__(self, project_path: Path):
        """
        Initialize report generator for a CALM buoy project.

        Args:
            project_path: Path to project-specific data directory
        """
        self.project_path = Path(project_path)
        self.project_name = self.project_path.name
        self.data = {}
        self.metadata = {}

    def load_data(self) -> None:
        """Load all CSV files from project directory."""
        csv_files = list(self.project_path.glob("*.csv"))

        for csv_file in csv_files:
            file_key = csv_file.stem
            try:
                self.data[file_key] = pd.read_csv(csv_file)
                print(f"✓ Loaded {file_key}: {len(self.data[file_key])} rows")
            except Exception as e:
                print(f"✗ Failed to load {file_key}: {e}")

        # Extract metadata if available
        if 'metadata' in self.data:
            meta_df = self.data['metadata']
            self.metadata = dict(zip(meta_df['attribute'], meta_df['value']))

    def create_environmental_plot(self) -> go.Figure:
        """Create interactive plot for environmental conditions."""
        if 'environmental_conditions' not in self.data:
            return None

        df = self.data['environmental_conditions']

        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=(
                'Wave Conditions', 'Wind Speed',
                'Current Profile', 'Water Depth Range'
            ),
            specs=[
                [{'type': 'bar'}, {'type': 'bar'}],
                [{'type': 'bar'}, {'type': 'bar'}]
            ]
        )

        # Wave conditions
        fig.add_trace(
            go.Bar(
                x=df['scenario'],
                y=df['wave_hs_m'],
                name='Hs (m)',
                marker_color='steelblue',
                hovertemplate='<b>%{x}</b><br>Hs: %{y:.2f} m<br>Tp: %{text:.1f} s',
                text=df['wave_tp_s']
            ),
            row=1, col=1
        )

        # Wind speed
        fig.add_trace(
            go.Bar(
                x=df['scenario'],
                y=df['wind_speed_m_s'],
                name='Wind Speed (m/s)',
                marker_color='lightcoral',
                hovertemplate='<b>%{x}</b><br>Wind: %{y:.1f} m/s'
            ),
            row=1, col=2
        )

        # Current profile
        fig.add_trace(
            go.Bar(
                x=df['scenario'],
                y=df['surface_current_m_s'],
                name='Surface',
                marker_color='lightgreen',
                hovertemplate='Surface: %{y:.2f} m/s'
            ),
            row=2, col=1
        )
        fig.add_trace(
            go.Bar(
                x=df['scenario'],
                y=df['mid_depth_current_m_s'],
                name='Mid-depth',
                marker_color='mediumseagreen',
                hovertemplate='Mid-depth: %{y:.2f} m/s'
            ),
            row=2, col=1
        )
        fig.add_trace(
            go.Bar(
                x=df['scenario'],
                y=df['seabed_current_m_s'],
                name='Seabed',
                marker_color='darkgreen',
                hovertemplate='Seabed: %{y:.2f} m/s'
            ),
            row=2, col=1
        )

        # Water depth (assuming format "min-max")
        if 'water_depth_range_m' in df.columns:
            depth_min = []
            depth_max = []
            for depth_str in df['water_depth_range_m']:
                parts = str(depth_str).split('-')
                if len(parts) == 2:
                    depth_min.append(float(parts[0]))
                    depth_max.append(float(parts[1]))
                else:
                    depth_min.append(0)
                    depth_max.append(0)

            fig.add_trace(
                go.Bar(
                    x=df['scenario'],
                    y=depth_max,
                    name='Max Depth',
                    marker_color='navy',
                    hovertemplate='Max: %{y:.2f} m'
                ),
                row=2, col=2
            )

        fig.update_xaxes(title_text="Scenario", row=1, col=1)
        fig.update_xaxes(title_text="Scenario", row=1, col=2)
        fig.update_xaxes(title_text="Scenario", row=2, col=1)
        fig.update_xaxes(title_text="Scenario", row=2, col=2)

        fig.update_yaxes(title_text="Hs (m)", row=1, col=1)
        fig.update_yaxes(title_text="Wind Speed (m/s)", row=1, col=2)
        fig.update_yaxes(title_text="Current (m/s)", row=2, col=1)
        fig.update_yaxes(title_text="Depth (m)", row=2, col=2)

        fig.update_layout(
            title_text="Environmental Conditions",
            showlegend=True,
            height=800,
            hovermode='x unified'
        )

        return fig

    def create_mooring_plot(self) -> go.Figure:
        """Create interactive plot for mooring line analysis."""
        if 'mooring_lines' not in self.data:
            return None

        df = self.data['mooring_lines']

        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=(
                'Chain Properties', 'Pretension & MBL',
                'Safety Factors', 'Line Configuration'
            ),
            specs=[
                [{'type': 'bar'}, {'type': 'bar'}],
                [{'type': 'bar'}, {'type': 'scatter'}]
            ]
        )

        # Chain properties
        fig.add_trace(
            go.Bar(
                x=['Diameter'],
                y=[df['diameter_mm'].iloc[0]],
                name='Diameter (mm)',
                marker_color='orange',
                hovertemplate='Diameter: %{y:.0f} mm'
            ),
            row=1, col=1
        )

        # Pretension and MBL
        fig.add_trace(
            go.Bar(
                x=['Pretension', 'MBL', 'MBL (corroded)'],
                y=[df['pretension_kN'].iloc[0], df['mbl_kN'].iloc[0], df['mbl_with_corrosion_kN'].iloc[0]],
                name='Load (kN)',
                marker_color=['steelblue', 'darkgreen', 'coral'],
                hovertemplate='%{x}: %{y:.0f} kN'
            ),
            row=1, col=2
        )

        # Safety factors
        safety_factors = [
            df['operational_safety_factor_intact'].iloc[0],
            df['operational_safety_factor_one_line_damaged'].iloc[0]
        ]
        fig.add_trace(
            go.Bar(
                x=['Intact', 'One Line Damaged'],
                y=safety_factors,
                name='Safety Factor',
                marker_color=['green', 'orange'],
                hovertemplate='%{x}: %{y:.1f}'
            ),
            row=2, col=1
        )

        # Add safety factor reference line
        fig.add_hline(y=2.0, line_dash="dash", line_color="red",
                     annotation_text="Min SF", row=2, col=1)

        # Mooring configuration (polar-like representation)
        angles = [i * df['spacing_deg'].iloc[0] for i in range(6)]  # 6 legs at 60° spacing
        radius = df['anchor_radius_m'].iloc[0]

        # Create circular mooring pattern
        x_coords = [radius * np.cos(np.radians(angle)) for angle in angles]
        y_coords = [radius * np.sin(np.radians(angle)) for angle in angles]
        x_coords.append(x_coords[0])  # Close the circle
        y_coords.append(y_coords[0])

        fig.add_trace(
            go.Scatter(
                x=x_coords,
                y=y_coords,
                mode='lines+markers',
                name='Mooring Pattern',
                marker=dict(size=12, color='red'),
                line=dict(color='blue', width=2),
                hovertemplate='Anchor %{pointIndex}<br>Radius: ' + str(radius) + ' m'
            ),
            row=2, col=2
        )

        # Add buoy at center
        fig.add_trace(
            go.Scatter(
                x=[0],
                y=[0],
                mode='markers',
                name='Buoy',
                marker=dict(size=20, color='yellow', line=dict(width=2, color='black')),
                hovertext='CALM Buoy'
            ),
            row=2, col=2
        )

        fig.update_xaxes(title_text="Property", row=1, col=1)
        fig.update_xaxes(title_text="Load Type", row=1, col=2)
        fig.update_xaxes(title_text="Condition", row=2, col=1)
        fig.update_xaxes(title_text="X Position (m)", row=2, col=2)

        fig.update_yaxes(title_text="Value (mm)", row=1, col=1)
        fig.update_yaxes(title_text="Force (kN)", row=1, col=2)
        fig.update_yaxes(title_text="Safety Factor", row=2, col=1)
        fig.update_yaxes(title_text="Y Position (m)", row=2, col=2)

        fig.update_layout(
            title_text=f"Mooring Line Analysis - {df['chain_grade'].iloc[0]} {df['diameter_mm'].iloc[0]}mm",
            showlegend=True,
            height=800
        )

        return fig

    def create_performance_plot(self) -> go.Figure:
        """Create interactive plot for performance summary."""
        if 'performance_summary' not in self.data:
            return None

        df = self.data['performance_summary']

        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=(
                'Maximum Offsets', 'Motion Response',
                'Mooring & Hawser Tensions', 'Safety Factors'
            )
        )

        # Group data by condition
        conditions = df['condition'].unique()
        colors = px.colors.qualitative.Set1

        # Maximum offsets
        for i, condition in enumerate(conditions):
            cond_df = df[df['condition'] == condition]
            fig.add_trace(
                go.Bar(
                    x=cond_df['mooring_state'],
                    y=cond_df['max_offset_m'],
                    name=f'{condition} - Offset',
                    marker_color=colors[i],
                    legendgroup=condition,
                    hovertemplate=f'{condition}<br>State: %{{x}}<br>Offset: %{{y:.1f}} m'
                ),
                row=1, col=1
            )

        # Motion response
        for i, condition in enumerate(conditions):
            cond_df = df[df['condition'] == condition]
            fig.add_trace(
                go.Scatter(
                    x=['Heave', 'Pitch'],
                    y=[cond_df['max_heave_m'].iloc[0], cond_df['max_pitch_deg'].iloc[0]],
                    mode='lines+markers',
                    name=f'{condition} - Motion',
                    marker_color=colors[i],
                    legendgroup=condition,
                    showlegend=False,
                    hovertemplate=f'{condition}<br>%{{x}}: %{{y:.1f}}'
                ),
                row=1, col=2
            )

        # Tensions
        for i, condition in enumerate(conditions):
            cond_df = df[df['condition'] == condition]
            fig.add_trace(
                go.Bar(
                    x=cond_df['mooring_state'],
                    y=cond_df['max_mooring_tension_kN'],
                    name=f'{condition} - Mooring',
                    marker_color=colors[i],
                    legendgroup=condition,
                    showlegend=False,
                    hovertemplate=f'{condition}<br>Mooring: %{{y:.0f}} kN'
                ),
                row=2, col=1
            )

        # Safety factors
        for i, condition in enumerate(conditions):
            cond_df = df[df['condition'] == condition]
            fig.add_trace(
                go.Scatter(
                    x=cond_df['mooring_state'],
                    y=cond_df['mooring_safety_factor'],
                    mode='lines+markers',
                    name=f'{condition} - Safety',
                    marker_color=colors[i],
                    legendgroup=condition,
                    showlegend=False,
                    hovertemplate=f'{condition}<br>SF: %{{y:.1f}}'
                ),
                row=2, col=2
            )

        # Add minimum safety factor reference
        fig.add_hline(y=2.0, line_dash="dash", line_color="red",
                     annotation_text="Min SF=2.0", row=2, col=2)

        fig.update_xaxes(title_text="Mooring State", row=1, col=1)
        fig.update_xaxes(title_text="Motion Type", row=1, col=2)
        fig.update_xaxes(title_text="Mooring State", row=2, col=1)
        fig.update_xaxes(title_text="Mooring State", row=2, col=2)

        fig.update_yaxes(title_text="Offset (m)", row=1, col=1)
        fig.update_yaxes(title_text="Response", row=1, col=2)
        fig.update_yaxes(title_text="Tension (kN)", row=2, col=1)
        fig.update_yaxes(title_text="Safety Factor", row=2, col=2)

        fig.update_layout(
            title_text="Performance Summary",
            height=800,
            hovermode='x unified'
        )

        return fig

    def create_vessel_plot(self) -> go.Figure:
        """Create interactive plot for vessel design parameters."""
        if 'vessel_design' not in self.data:
            return None

        df = self.data['vessel_design']

        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=(
                'Principal Dimensions', 'Displacement & Draft',
                'Center of Gravity', 'Wind Area'
            )
        )

        conditions = df['condition'].tolist()
        colors = ['steelblue', 'lightcoral']

        # Principal dimensions
        dimensions = ['LOA', 'LPP', 'Breadth', 'Depth']
        for i, condition in enumerate(conditions):
            values = [
                df.loc[i, 'loa_m'],
                df.loc[i, 'lpp_m'],
                df.loc[i, 'breadth_m'],
                df.loc[i, 'depth_m']
            ]
            fig.add_trace(
                go.Bar(
                    x=dimensions,
                    y=values,
                    name=condition,
                    marker_color=colors[i],
                    hovertemplate=f'{condition}<br>%{{x}}: %{{y:.1f}} m'
                ),
                row=1, col=1
            )

        # Displacement & Draft
        for i, condition in enumerate(conditions):
            fig.add_trace(
                go.Bar(
                    x=['Displacement', 'Draft'],
                    y=[df.loc[i, 'displacement_t'], df.loc[i, 'draft_m']],
                    name=f'{condition} - Load',
                    marker_color=colors[i],
                    showlegend=False,
                    hovertemplate=f'{condition}<br>%{{x}}: %{{y:.0f}}'
                ),
                row=1, col=2
            )

        # Center of gravity
        for i, condition in enumerate(conditions):
            fig.add_trace(
                go.Bar(
                    x=['LCG', 'VCG'],
                    y=[df.loc[i, 'lcg_fwd_midship_m'], df.loc[i, 'vcg_m']],
                    name=f'{condition} - CG',
                    marker_color=colors[i],
                    showlegend=False,
                    hovertemplate=f'{condition}<br>%{{x}}: %{{y:.1f}} m'
                ),
                row=2, col=1
            )

        # Wind area
        for i, condition in enumerate(conditions):
            fig.add_trace(
                go.Bar(
                    x=['Longitudinal', 'Transverse'],
                    y=[df.loc[i, 'longitudinal_wind_area_m2'], df.loc[i, 'transverse_wind_area_m2']],
                    name=f'{condition} - Wind Area',
                    marker_color=colors[i],
                    showlegend=False,
                    hovertemplate=f'{condition}<br>%{{x}}: %{{y:.0f}} m²'
                ),
                row=2, col=2
            )

        fig.update_xaxes(title_text="Dimension", row=1, col=1)
        fig.update_xaxes(title_text="Parameter", row=1, col=2)
        fig.update_xaxes(title_text="CG Position", row=2, col=1)
        fig.update_xaxes(title_text="Direction", row=2, col=2)

        fig.update_yaxes(title_text="Length (m)", row=1, col=1)
        fig.update_yaxes(title_text="Value", row=1, col=2)
        fig.update_yaxes(title_text="Distance (m)", row=2, col=1)
        fig.update_yaxes(title_text="Area (m²)", row=2, col=2)

        fig.update_layout(
            title_text="Vessel Design Parameters",
            height=800,
            barmode='group'
        )

        return fig

    def generate_report(self, output_path: Optional[Path] = None) -> str:
        """
        Generate complete HTML report with all visualizations.

        Args:
            output_path: Path where HTML report will be saved

        Returns:
            Path to generated HTML report
        """
        if output_path is None:
            output_path = Path(f"reports/mooring/calm_buoy/{self.project_name}_report.html")

        output_path.parent.mkdir(parents=True, exist_ok=True)

        # Load all data
        self.load_data()

        # Generate plots
        env_fig = self.create_environmental_plot()
        mooring_fig = self.create_mooring_plot()
        performance_fig = self.create_performance_plot()
        vessel_fig = self.create_vessel_plot()

        # Create HTML report
        html_content = f"""
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>CALM Buoy Operational Analysis - {self.project_name}</title>
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            margin: 0;
            padding: 0;
            background-color: #f5f5f5;
        }}
        .header {{
            background: linear-gradient(135deg, #1e3c72 0%, #2a5298 100%);
            color: white;
            padding: 30px;
            text-align: center;
            box-shadow: 0 2px 10px rgba(0,0,0,0.2);
        }}
        .header h1 {{
            margin: 0;
            font-size: 2.5em;
            font-weight: 300;
        }}
        .header p {{
            margin: 10px 0 0 0;
            font-size: 1.1em;
            opacity: 0.9;
        }}
        .metadata {{
            background: white;
            padding: 20px;
            margin: 20px auto;
            max-width: 1400px;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }}
        .metadata h2 {{
            color: #1e3c72;
            margin-top: 0;
            border-bottom: 2px solid #2a5298;
            padding-bottom: 10px;
        }}
        .metadata-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 15px;
            margin-top: 20px;
        }}
        .metadata-item {{
            padding: 10px;
            background: #f8f9fa;
            border-left: 4px solid #2a5298;
            border-radius: 4px;
        }}
        .metadata-item strong {{
            color: #1e3c72;
            display: block;
            margin-bottom: 5px;
        }}
        .section {{
            background: white;
            padding: 30px;
            margin: 20px auto;
            max-width: 1400px;
            border-radius: 8px;
            box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }}
        .section h2 {{
            color: #1e3c72;
            margin-top: 0;
            border-bottom: 2px solid #2a5298;
            padding-bottom: 10px;
        }}
        .plot-container {{
            margin: 20px 0;
        }}
        .footer {{
            text-align: center;
            padding: 20px;
            color: #666;
            font-size: 0.9em;
        }}
        .status-badge {{
            display: inline-block;
            padding: 5px 15px;
            border-radius: 20px;
            font-size: 0.9em;
            font-weight: bold;
            margin-left: 10px;
        }}
        .status-complete {{
            background-color: #28a745;
            color: white;
        }}
        .status-partial {{
            background-color: #ffc107;
            color: #333;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>CALM Buoy Operational Analysis</h1>
        <p>{self.project_name.replace('_', ' ').title()}</p>
        <p>Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
    </div>

    <div class="metadata">
        <h2>Project Metadata</h2>
        <div class="metadata-grid">
"""

        # Add metadata items
        key_metadata = [
            'project_name', 'operator', 'design_location',
            'design_life', 'spm_configuration', 'chart_datum_water_depth'
        ]
        for key in key_metadata:
            if key in self.metadata:
                label = key.replace('_', ' ').title()
                value = self.metadata[key]
                html_content += f"""
            <div class="metadata-item">
                <strong>{label}:</strong>
                {value}
            </div>
"""

        html_content += """
        </div>
    </div>
"""

        # Add environmental conditions section
        if env_fig:
            html_content += """
    <div class="section">
        <h2>1. Environmental Conditions</h2>
        <p>Operational (1-year) and survival (100-year) environmental design parameters including wave, wind, and current conditions.</p>
        <div class="plot-container" id="env-plot"></div>
    </div>
"""

        # Add mooring analysis section
        if mooring_fig:
            html_content += """
    <div class="section">
        <h2>2. Mooring Line Analysis</h2>
        <p>Mooring system configuration, chain properties, pretensions, and safety factors for intact and damaged conditions.</p>
        <div class="plot-container" id="mooring-plot"></div>
    </div>
"""

        # Add performance section
        if performance_fig:
            html_content += """
    <div class="section">
        <h2>3. Performance Summary</h2>
        <p>Coupled analysis results showing maximum offsets, motion responses, tensions, and safety factors.</p>
        <div class="plot-container" id="performance-plot"></div>
    </div>
"""

        # Add vessel design section
        if vessel_fig:
            html_content += """
    <div class="section">
        <h2>4. Vessel Design Parameters</h2>
        <p>300k DWT tanker design particulars for full load and ballast conditions.</p>
        <div class="plot-container" id="vessel-plot"></div>
    </div>
"""

        html_content += """
    <div class="footer">
        <p>Digital Model - Engineering Asset Lifecycle Management</p>
        <p>Generated from CSV data using Plotly interactive visualizations</p>
    </div>

    <script>
"""

        # Add plot JavaScript
        if env_fig:
            html_content += f"        Plotly.newPlot('env-plot', {env_fig.to_json()});\n"
        if mooring_fig:
            html_content += f"        Plotly.newPlot('mooring-plot', {mooring_fig.to_json()});\n"
        if performance_fig:
            html_content += f"        Plotly.newPlot('performance-plot', {performance_fig.to_json()});\n"
        if vessel_fig:
            html_content += f"        Plotly.newPlot('vessel-plot', {vessel_fig.to_json()});\n"

        html_content += """
    </script>
</body>
</html>
"""

        # Write HTML file
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(html_content)

        print(f"\n✓ HTML report generated: {output_path}")
        return str(output_path)


def main():
    """Generate reports for all CALM buoy projects."""
    base_path = Path("data/mooring/results/calm_buoy/project_specific")

    projects = [p for p in base_path.iterdir() if p.is_dir()]

    for project in projects:
        print(f"\n{'='*60}")
        print(f"Generating report for: {project.name}")
        print(f"{'='*60}")

        try:
            generator = CALMBuoyReportGenerator(project)
            report_path = generator.generate_report()
            print(f"✓ Report saved: {report_path}")
        except Exception as e:
            print(f"✗ Failed to generate report: {e}")
            import traceback
            traceback.print_exc()


if __name__ == "__main__":
    main()
