#!/usr/bin/env python3
"""
GoM SCR Design Study - Interactive HTML Report Generator

Generates interactive HTML dashboard from design study results using Plotly.
Follows HTML_REPORTING_STANDARDS.md requirements:
- Interactive plots only (Plotly)
- Data imported from CSV with relative paths
- Interactive features: hover, zoom, pan, export
"""

import pandas as pd
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
from pathlib import Path
from datetime import datetime


class DesignStudyReportGenerator:
    """Generate interactive HTML reports for GoM SCR design study"""

    def __init__(self, study_dir: Path = None):
        """Initialize report generator

        Args:
            study_dir: Path to design study directory (defaults to script location)
        """
        if study_dir is None:
            study_dir = Path(__file__).parent

        self.study_dir = Path(study_dir)
        self.results_dir = self.study_dir / "results"
        self.data_file = self.results_dir / "comparison_matrix.csv"

        # Load data
        if not self.data_file.exists():
            raise FileNotFoundError(
                f"Data file not found: {self.data_file}\n"
                "Please run design_study.py and analyze_results.py first."
            )

        self.df = pd.read_csv(self.data_file)
        print(f"Loaded {len(self.df)} model configurations")

    def create_parameter_matrix_heatmap(self) -> go.Figure:
        """Create interactive heatmap showing model distribution across parameters"""

        # Create pivot table for heatmap
        pivot = self.df.pivot_table(
            values='riser_od',
            index='water_depth',
            columns='environment',
            aggfunc='count',
            fill_value=0
        )

        fig = go.Figure(data=go.Heatmap(
            z=pivot.values,
            x=pivot.columns,
            y=pivot.index,
            colorscale='Blues',
            text=pivot.values,
            texttemplate='%{text}',
            textfont={"size": 12},
            hovertemplate='Water Depth: %{y}m<br>Environment: %{x}<br>Models: %{z}<extra></extra>'
        ))

        fig.update_layout(
            title='Model Distribution: Water Depth vs Environment',
            xaxis_title='Environmental Condition',
            yaxis_title='Water Depth (m)',
            height=400,
            font=dict(size=12)
        )

        return fig

    def create_riser_type_distribution(self) -> go.Figure:
        """Create bar chart showing riser type distribution"""

        riser_counts = self.df['riser_type'].value_counts()

        fig = go.Figure(data=[
            go.Bar(
                x=riser_counts.index,
                y=riser_counts.values,
                text=riser_counts.values,
                textposition='auto',
                marker_color=['#1f77b4', '#ff7f0e', '#2ca02c'],
                hovertemplate='Riser Type: %{x}<br>Models: %{y}<extra></extra>'
            )
        ])

        fig.update_layout(
            title='Model Distribution by Riser Type',
            xaxis_title='Riser Configuration',
            yaxis_title='Number of Models',
            height=400,
            showlegend=False
        )

        return fig

    def create_depth_environment_scatter(self) -> go.Figure:
        """Create scatter plot showing configurations across depth and environment"""

        # Map environment to numeric values for plotting
        env_map = {'1yr': 1, '10yr': 10, '100yr': 100}
        self.df['env_numeric'] = self.df['environment'].map(env_map)

        fig = px.scatter(
            self.df,
            x='water_depth',
            y='env_numeric',
            color='riser_type',
            size='riser_od',
            hover_data=['model_name', 'riser_od'],
            title='Design Space Coverage',
            labels={
                'water_depth': 'Water Depth (m)',
                'env_numeric': 'Return Period (years)',
                'riser_type': 'Riser Type',
                'riser_od': 'OD (m)'
            }
        )

        fig.update_layout(
            yaxis_type='log',
            height=500,
            yaxis=dict(tickvals=[1, 10, 100], ticktext=['1-year', '10-year', '100-year'])
        )

        return fig

    def create_parameter_comparison_table(self) -> go.Figure:
        """Create interactive table showing all model parameters"""

        # Select key columns for table
        table_df = self.df[[
            'model_name', 'riser_type', 'water_depth',
            'environment', 'riser_od', 'riser_length', 'env_hs', 'env_tp'
        ]].copy()

        fig = go.Figure(data=[go.Table(
            header=dict(
                values=['Model', 'Riser Type', 'Depth (m)', 'Environment',
                       'OD (m)', 'Length (m)', 'Hs (m)', 'Tp (s)'],
                fill_color='paleturquoise',
                align='left',
                font=dict(size=11)
            ),
            cells=dict(
                values=[table_df[col] for col in table_df.columns],
                fill_color='lavender',
                align='left',
                font=dict(size=10)
            )
        )])

        fig.update_layout(
            title='Complete Model Comparison Matrix',
            height=600
        )

        return fig

    def create_depth_analysis(self) -> go.Figure:
        """Create grouped analysis by water depth"""

        depth_summary = self.df.groupby(['water_depth', 'riser_type']).size().reset_index(name='count')

        fig = px.bar(
            depth_summary,
            x='water_depth',
            y='count',
            color='riser_type',
            barmode='group',
            title='Configuration Distribution by Water Depth',
            labels={
                'water_depth': 'Water Depth (m)',
                'count': 'Number of Models',
                'riser_type': 'Riser Type'
            }
        )

        fig.update_layout(height=400)

        return fig

    def generate_report(self, output_file: str = None) -> str:
        """Generate complete interactive HTML report

        Args:
            output_file: Output HTML file path (defaults to results/design_study_dashboard.html)

        Returns:
            Path to generated HTML file
        """
        if output_file is None:
            output_file = self.results_dir / "design_study_dashboard.html"
        else:
            output_file = Path(output_file)

        print("Generating interactive visualizations...")

        # Create all figures
        fig_matrix = self.create_parameter_matrix_heatmap()
        fig_riser = self.create_riser_type_distribution()
        fig_scatter = self.create_depth_environment_scatter()
        fig_depth = self.create_depth_analysis()
        fig_table = self.create_parameter_comparison_table()

        # Create HTML content
        html_content = f"""
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>GoM SCR Design Study - Interactive Dashboard</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            margin: 0;
            padding: 20px;
            background-color: #f5f5f5;
        }}
        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }}
        .header h1 {{
            margin: 0 0 10px 0;
            font-size: 2.5em;
        }}
        .header p {{
            margin: 5px 0;
            font-size: 1.1em;
            opacity: 0.9;
        }}
        .section {{
            background: white;
            padding: 20px;
            margin-bottom: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        .section h2 {{
            color: #333;
            border-bottom: 2px solid #667eea;
            padding-bottom: 10px;
            margin-top: 0;
        }}
        .plot-container {{
            margin: 20px 0;
        }}
        .summary-stats {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            margin: 20px 0;
        }}
        .stat-card {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 8px;
            text-align: center;
        }}
        .stat-value {{
            font-size: 2.5em;
            font-weight: bold;
            margin: 10px 0;
        }}
        .stat-label {{
            font-size: 1em;
            opacity: 0.9;
        }}
        .footer {{
            text-align: center;
            padding: 20px;
            color: #666;
            font-size: 0.9em;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>Gulf of Mexico SCR Design Study</h1>
        <p>Interactive Analysis Dashboard</p>
        <p>Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
    </div>

    <div class="section">
        <h2>Study Summary</h2>
        <div class="summary-stats">
            <div class="stat-card">
                <div class="stat-label">Total Models</div>
                <div class="stat-value">{len(self.df)}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Riser Configurations</div>
                <div class="stat-value">{self.df['riser_type'].nunique()}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Water Depths</div>
                <div class="stat-value">{self.df['water_depth'].nunique()}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Environments</div>
                <div class="stat-value">{self.df['environment'].nunique()}</div>
            </div>
        </div>
    </div>

    <div class="section">
        <h2>Design Space Overview</h2>
        <div class="plot-container" id="scatter-plot"></div>
    </div>

    <div class="section">
        <h2>Model Distribution Analysis</h2>
        <div class="plot-container" id="matrix-plot"></div>
        <div class="plot-container" id="riser-plot"></div>
        <div class="plot-container" id="depth-plot"></div>
    </div>

    <div class="section">
        <h2>Complete Comparison Matrix</h2>
        <div class="plot-container" id="table-plot"></div>
    </div>

    <div class="footer">
        <p>Data Source: {self.data_file.relative_to(self.study_dir)}</p>
        <p>Generated with Plotly | Interactive Dashboard | Hover for details</p>
    </div>

    <script>
        // Render all plots
        Plotly.newPlot('scatter-plot', {fig_scatter.to_json()});
        Plotly.newPlot('matrix-plot', {fig_matrix.to_json()});
        Plotly.newPlot('riser-plot', {fig_riser.to_json()});
        Plotly.newPlot('depth-plot', {fig_depth.to_json()});
        Plotly.newPlot('table-plot', {fig_table.to_json()});
    </script>
</body>
</html>
"""

        # Write HTML file with proper Plotly JSON embedding
        from plotly.io import to_html

        html_parts = []
        html_parts.append(f"""
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <title>GoM SCR Design Study - Interactive Dashboard</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            margin: 0;
            padding: 20px;
            background-color: #f5f5f5;
        }}
        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 20px;
            box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }}
        .header h1 {{
            margin: 0 0 10px 0;
            font-size: 2.5em;
        }}
        .header p {{
            margin: 5px 0;
            font-size: 1.1em;
            opacity: 0.9;
        }}
        .section {{
            background: white;
            padding: 20px;
            margin-bottom: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }}
        .section h2 {{
            color: #333;
            border-bottom: 2px solid #667eea;
            padding-bottom: 10px;
            margin-top: 0;
        }}
        .plot-container {{
            margin: 20px 0;
        }}
        .summary-stats {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            margin: 20px 0;
        }}
        .stat-card {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 8px;
            text-align: center;
        }}
        .stat-value {{
            font-size: 2.5em;
            font-weight: bold;
            margin: 10px 0;
        }}
        .stat-label {{
            font-size: 1em;
            opacity: 0.9;
        }}
        .footer {{
            text-align: center;
            padding: 20px;
            color: #666;
            font-size: 0.9em;
        }}
    </style>
</head>
<body>
    <div class="header">
        <h1>Gulf of Mexico SCR Design Study</h1>
        <p>Interactive Analysis Dashboard</p>
        <p>Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
    </div>

    <div class="section">
        <h2>Study Summary</h2>
        <div class="summary-stats">
            <div class="stat-card">
                <div class="stat-label">Total Models</div>
                <div class="stat-value">{len(self.df)}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Riser Configurations</div>
                <div class="stat-value">{self.df['riser_type'].nunique()}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Water Depths</div>
                <div class="stat-value">{self.df['water_depth'].nunique()}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Environments</div>
                <div class="stat-value">{self.df['environment'].nunique()}</div>
            </div>
        </div>
    </div>

    <div class="section">
        <h2>Design Space Overview</h2>
        <div class="plot-container">
""")

        html_parts.append(to_html(fig_scatter, include_plotlyjs=False, full_html=False))
        html_parts.append("""
        </div>
    </div>

    <div class="section">
        <h2>Model Distribution Analysis</h2>
        <div class="plot-container">
""")
        html_parts.append(to_html(fig_matrix, include_plotlyjs=False, full_html=False))
        html_parts.append("</div><div class='plot-container'>")
        html_parts.append(to_html(fig_riser, include_plotlyjs=False, full_html=False))
        html_parts.append("</div><div class='plot-container'>")
        html_parts.append(to_html(fig_depth, include_plotlyjs=False, full_html=False))
        html_parts.append("""
        </div>
    </div>

    <div class="section">
        <h2>Complete Comparison Matrix</h2>
        <div class="plot-container">
""")
        html_parts.append(to_html(fig_table, include_plotlyjs=False, full_html=False))
        html_parts.append(f"""
        </div>
    </div>

    <div class="footer">
        <p>Data Source: {self.data_file.relative_to(self.study_dir)}</p>
        <p>Generated with Plotly | Interactive Dashboard | Hover for details</p>
    </div>
</body>
</html>
""")

        # Write complete HTML
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(''.join(html_parts))

        print(f"\nInteractive HTML report generated: {output_file}")
        print(f"Open in browser to view interactive dashboard")

        return str(output_file)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Generate interactive HTML report for GoM SCR design study')
    parser.add_argument('--output', '-o', help='Output HTML file path',
                       default='results/design_study_dashboard.html')

    args = parser.parse_args()

    # Generate report
    generator = DesignStudyReportGenerator()
    output_path = generator.generate_report(args.output)

    print(f"\nSuccess! Report available at: {output_path}")
