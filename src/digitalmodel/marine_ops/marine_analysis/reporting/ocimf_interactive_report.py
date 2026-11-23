"""
OCIMF Interactive Data Analysis Report Generator.

Generates comprehensive HTML report with interactive Plotly visualizations:
- Data availability analysis
- Interactive 3D surface plots for all 6 coefficients
- Interactive polar diagrams
- Interactive heatmaps
- Heading sensitivity analysis with interactive charts
- Force comparison charts

Compliance: HTML_REPORTING_STANDARDS.md ✅
- Uses Plotly for all visualizations (interactive)
- Loads data from CSV with relative paths
- Generates responsive HTML reports
- Includes hover tooltips, zoom, pan functionality
"""

import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.express as px
import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime


class OCIMFInteractiveReport:
    """Generate interactive OCIMF analysis report."""

    def __init__(self, csv_file: str, output_dir: str):
        """
        Initialize report generator.

        Args:
            csv_file: Path to OCIMF CSV data file (relative or absolute)
            output_dir: Directory to save HTML report
        """
        self.csv_file = csv_file
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)
        self.df = None
        self.figures = []

    def load_data(self):
        """Load OCIMF data from CSV."""
        self.df = pd.read_csv(self.csv_file)
        return self.df

    def analyze_data_availability(self):
        """Analyze data availability and completeness."""
        analysis = {
            'total_records': len(self.df),
            'vessel_types': self.df['vessel_type'].nunique(),
            'vessel_type_list': self.df['vessel_type'].unique().tolist(),
            'displacements': sorted(self.df['displacement'].unique()),
            'heading_range': (self.df['heading'].min(), self.df['heading'].max()),
            'coefficients': {}
        }

        # Analyze each coefficient
        coefficients = ['CXw', 'CYw', 'CMw', 'CXc', 'CYc', 'CMc']
        for coef in coefficients:
            if coef in self.df.columns:
                missing = self.df[coef].isna().sum()
                complete = 100 * (len(self.df) - missing) / len(self.df)
                values = self.df[coef].dropna()

                analysis['coefficients'][coef] = {
                    'complete_pct': complete,
                    'missing': missing,
                    'min': values.min() if len(values) > 0 else None,
                    'max': values.max() if len(values) > 0 else None,
                    'mean': values.mean() if len(values) > 0 else None,
                    'std': values.std() if len(values) > 0 else None
                }

        return analysis

    def create_3d_surface_plot(self, coefficient: str, title: str = None):
        """
        Create interactive 3D surface plot using Plotly.

        Args:
            coefficient: Coefficient to plot (e.g., 'CXw')
            title: Plot title (optional)
        """
        if title is None:
            title = f'{coefficient} Coefficient - 3D Surface'

        # Filter valid data
        data = self.df[['heading', 'displacement', coefficient]].dropna()

        # Create meshgrid
        heading_unique = np.sort(data['heading'].unique())
        disp_unique = np.sort(data['displacement'].unique())
        H, D = np.meshgrid(heading_unique, disp_unique)

        # Pivot data for surface
        pivot = data.pivot_table(
            values=coefficient,
            index='displacement',
            columns='heading',
            aggfunc='mean'
        )

        # Create 3D surface plot
        fig = go.Figure(data=[
            go.Surface(
                z=pivot.values,
                x=pivot.columns,
                y=pivot.index,
                colorscale='Viridis',
                hovertemplate='Heading: %{x}°<br>Displacement: %{y} tonnes<br>' +
                              f'{coefficient}: %{{z:.3f}}<extra></extra>'
            )
        ])

        # Add scatter points for actual data
        fig.add_trace(go.Scatter3d(
            x=data['heading'],
            y=data['displacement'],
            z=data[coefficient],
            mode='markers',
            marker=dict(size=3, color='red', opacity=0.6),
            name='Database Points',
            hovertemplate='Heading: %{x}°<br>Displacement: %{y} tonnes<br>' +
                          f'{coefficient}: %{{z:.3f}}<extra></extra>'
        ))

        fig.update_layout(
            title=title,
            scene=dict(
                xaxis_title='Heading (degrees)',
                yaxis_title='Displacement (tonnes)',
                zaxis_title=coefficient,
                camera=dict(eye=dict(x=1.5, y=1.5, z=1.3))
            ),
            height=600,
            template='plotly_white',
            hovermode='closest'
        )

        return fig

    def create_polar_diagrams(self, displacements=None):
        """
        Create interactive polar diagrams for wind coefficients.

        Args:
            displacements: List of displacements to plot (defaults to first 3)
        """
        if displacements is None:
            displacements = sorted(self.df['displacement'].unique())[:3]

        # Create subplots for CXw and CYw
        fig = make_subplots(
            rows=2, cols=len(displacements),
            specs=[[{'type': 'polar'}] * len(displacements),
                   [{'type': 'polar'}] * len(displacements)],
            subplot_titles=[f'CXw - {d/1000:.0f}k tonnes' for d in displacements] +
                          [f'CYw - {d/1000:.0f}k tonnes' for d in displacements]
        )

        for idx, disp in enumerate(displacements):
            data = self.df[self.df['displacement'] == disp].sort_values('heading')

            # Extend 0-180° to 0-360° using symmetry
            if data['heading'].max() <= 180:
                mirror_data = data[data['heading'] > 0].copy()
                mirror_data['heading'] = 360 - mirror_data['heading']
                mirror_data['CXw'] = data[data['heading'] > 0]['CXw'].values[::-1]
                mirror_data['CYw'] = -data[data['heading'] > 0]['CYw'].values[::-1]
                data = pd.concat([data, mirror_data]).sort_values('heading')

            # CXw polar plot (row 1)
            fig.add_trace(
                go.Scatterpolar(
                    r=data['CXw'].values,
                    theta=data['heading'].values,
                    fill='toself',
                    name=f'CXw {disp/1000:.0f}k',
                    line=dict(color='blue'),
                    fillcolor='rgba(0, 0, 255, 0.2)',
                    hovertemplate='Heading: %{theta}°<br>CXw: %{r:.3f}<extra></extra>'
                ),
                row=1, col=idx+1
            )

            # CYw polar plot (row 2)
            fig.add_trace(
                go.Scatterpolar(
                    r=data['CYw'].values,
                    theta=data['heading'].values,
                    fill='toself',
                    name=f'CYw {disp/1000:.0f}k',
                    line=dict(color='red'),
                    fillcolor='rgba(255, 0, 0, 0.2)',
                    hovertemplate='Heading: %{theta}°<br>CYw: %{r:.3f}<extra></extra>'
                ),
                row=2, col=idx+1
            )

        fig.update_layout(
            title_text='OCIMF Wind Coefficient Polar Diagrams (0-360°)',
            showlegend=False,
            height=800,
            template='plotly_white'
        )

        # Update polar axes
        for i in range(1, len(displacements) + 1):
            fig.update_polars(
                radialaxis=dict(showticklabels=True, gridcolor='lightgray'),
                angularaxis=dict(direction='clockwise', rotation=90),
                row=1, col=i
            )
            fig.update_polars(
                radialaxis=dict(showticklabels=True, gridcolor='lightgray'),
                angularaxis=dict(direction='clockwise', rotation=90),
                row=2, col=i
            )

        return fig

    def create_coefficient_heatmap(self, coefficient: str):
        """Create interactive heatmap for coefficient."""
        # Pivot data
        pivot = self.df.pivot_table(
            values=coefficient,
            index='displacement',
            columns='heading',
            aggfunc='mean'
        )

        fig = go.Figure(data=go.Heatmap(
            z=pivot.values,
            x=pivot.columns,
            y=pivot.index,
            colorscale='RdYlBu_r',
            zmid=0,
            hovertemplate='Heading: %{x}°<br>Displacement: %{y} tonnes<br>' +
                          f'{coefficient}: %{{z:.3f}}<extra></extra>',
            colorbar=dict(title=coefficient)
        ))

        fig.update_layout(
            title=f'{coefficient} Heatmap',
            xaxis_title='Heading (degrees)',
            yaxis_title='Displacement (tonnes)',
            height=500,
            template='plotly_white'
        )

        return fig

    def create_heading_sensitivity_chart(self, coefficient: str, num_displacements: int = 4):
        """Create interactive heading sensitivity chart."""
        displacements = sorted(self.df['displacement'].unique())[:num_displacements]

        fig = go.Figure()

        for disp in displacements:
            data = self.df[self.df['displacement'] == disp].sort_values('heading')
            fig.add_trace(go.Scatter(
                x=data['heading'],
                y=data[coefficient],
                mode='lines+markers',
                name=f'{disp/1000:.0f}k tonnes',
                hovertemplate='Heading: %{x}°<br>' +
                              f'{coefficient}: %{{y:.3f}}<extra></extra>'
            ))

        fig.update_layout(
            title=f'{coefficient} vs Heading - Sensitivity Analysis',
            xaxis_title='Heading (degrees)',
            yaxis_title=coefficient,
            height=500,
            template='plotly_white',
            hovermode='x unified',
            legend=dict(title='Displacement')
        )

        fig.add_hline(y=0, line_dash='dash', line_color='black', opacity=0.3)

        return fig

    def create_force_comparison_chart(self):
        """Create interactive force comparison chart."""
        # Select mid displacement
        mid_disp = sorted(self.df['displacement'].unique())[
            len(self.df['displacement'].unique()) // 2
        ]
        data = self.df[self.df['displacement'] == mid_disp].sort_values('heading')

        fig = make_subplots(
            rows=2, cols=2,
            subplot_titles=('Wind Forces (CXw, CYw)', 'Current Forces (CXc, CYc)',
                          'Moments (CMw)', 'Moments (CMc)')
        )

        # Wind forces
        fig.add_trace(
            go.Scatter(x=data['heading'], y=data['CXw'], name='CXw',
                      mode='lines+markers', line=dict(color='blue'),
                      hovertemplate='Heading: %{x}°<br>CXw: %{y:.3f}<extra></extra>'),
            row=1, col=1
        )
        fig.add_trace(
            go.Scatter(x=data['heading'], y=data['CYw'], name='CYw',
                      mode='lines+markers', line=dict(color='lightblue'),
                      hovertemplate='Heading: %{x}°<br>CYw: %{y:.3f}<extra></extra>'),
            row=1, col=1
        )

        # Current forces
        fig.add_trace(
            go.Scatter(x=data['heading'], y=data['CXc'], name='CXc',
                      mode='lines+markers', line=dict(color='green'),
                      hovertemplate='Heading: %{x}°<br>CXc: %{y:.3f}<extra></extra>'),
            row=1, col=2
        )
        fig.add_trace(
            go.Scatter(x=data['heading'], y=data['CYc'], name='CYc',
                      mode='lines+markers', line=dict(color='lightgreen'),
                      hovertemplate='Heading: %{x}°<br>CYc: %{y:.3f}<extra></extra>'),
            row=1, col=2
        )

        # Wind moment
        fig.add_trace(
            go.Scatter(x=data['heading'], y=data['CMw'], name='CMw',
                      mode='lines+markers', line=dict(color='purple'),
                      hovertemplate='Heading: %{x}°<br>CMw: %{y:.3f}<extra></extra>'),
            row=2, col=1
        )

        # Current moment
        fig.add_trace(
            go.Scatter(x=data['heading'], y=data['CMc'], name='CMc',
                      mode='lines+markers', line=dict(color='orange'),
                      hovertemplate='Heading: %{x}°<br>CMc: %{y:.3f}<extra></extra>'),
            row=2, col=2
        )

        fig.update_xaxes(title_text='Heading (degrees)')
        fig.update_yaxes(title_text='Coefficient Value')

        fig.update_layout(
            title_text=f'Force & Moment Coefficients Comparison (Displacement: {mid_disp/1000:.0f}k tonnes)',
            height=700,
            template='plotly_white',
            showlegend=True
        )

        return fig

    def generate_html_report(self, filename: str = 'ocimf_interactive_report.html'):
        """Generate comprehensive HTML report."""

        # Load and analyze data
        self.load_data()
        analysis = self.analyze_data_availability()

        # Generate header HTML
        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>OCIMF Interactive Data Analysis Report</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        * {{ margin: 0; padding: 0; box-sizing: border-box; }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            padding: 20px;
            color: #2d3748;
        }}
        .container {{ max-width: 1600px; margin: 0 auto; }}
        .report-header {{
            background: white;
            padding: 40px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 10px 30px rgba(0, 0, 0, 0.2);
        }}
        .report-header h1 {{
            font-size: 2.5em;
            margin-bottom: 10px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
        }}
        .metadata {{ color: #718096; font-size: 0.95em; margin: 10px 0; }}
        .section {{
            background: white;
            padding: 30px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }}
        .section h2 {{
            color: #2d3748;
            margin-bottom: 20px;
            border-bottom: 3px solid #667eea;
            padding-bottom: 10px;
        }}
        .stats-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin: 20px 0;
        }}
        .stat-card {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 20px;
            border-radius: 10px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }}
        .stat-label {{ font-size: 0.9em; opacity: 0.9; margin-bottom: 8px; }}
        .stat-value {{ font-size: 2em; font-weight: bold; }}
        .table-container {{ overflow-x: auto; margin: 20px 0; }}
        table {{
            width: 100%;
            border-collapse: collapse;
            background: white;
        }}
        th, td {{ padding: 12px; text-align: left; border-bottom: 1px solid #e2e8f0; }}
        th {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            font-weight: 600;
        }}
        tr:hover {{ background: #f7fafc; }}
        .success {{ color: #38a169; font-weight: bold; }}
        .warning {{ color: #dd6b20; font-weight: bold; }}
        .plot-container {{ margin: 30px 0; }}
        .footer {{
            background: white;
            padding: 20px;
            border-radius: 10px;
            text-align: center;
            color: #718096;
            margin-top: 30px;
        }}
        @media (max-width: 768px) {{
            .report-header {{ padding: 25px; }}
            .report-header h1 {{ font-size: 1.8em; }}
            .stats-grid {{ grid-template-columns: 1fr; }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="report-header">
            <h1>📊 OCIMF Interactive Data Analysis Report</h1>
            <p class="metadata"><strong>Generated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
            <p class="metadata"><strong>Data Source:</strong> {Path(self.csv_file).name}</p>
            <p class="metadata"><strong>Module:</strong> Marine Analysis | <strong>Repository:</strong> digitalmodel</p>
            <p class="metadata"><strong>Standards Compliance:</strong> ✅ HTML_REPORTING_STANDARDS.md</p>
        </div>

        <div class="section">
            <h2>📋 Data Availability Summary</h2>
            <div class="stats-grid">
                <div class="stat-card">
                    <div class="stat-label">Total Records</div>
                    <div class="stat-value">{analysis['total_records']:,}</div>
                </div>
                <div class="stat-card">
                    <div class="stat-label">Vessel Types</div>
                    <div class="stat-value">{analysis['vessel_types']}</div>
                </div>
                <div class="stat-card">
                    <div class="stat-label">Displacements</div>
                    <div class="stat-value">{len(analysis['displacements'])}</div>
                </div>
                <div class="stat-card">
                    <div class="stat-label">Heading Range</div>
                    <div class="stat-value">{analysis['heading_range'][0]}° - {analysis['heading_range'][1]}°</div>
                </div>
            </div>

            <h3>Vessel Types in Database</h3>
            <p>{', '.join(analysis['vessel_type_list'])}</p>

            <h3>Displacement Range</h3>
            <p>{analysis['displacements'][0]/1000:.0f}k - {analysis['displacements'][-1]/1000:.0f}k tonnes</p>
            <p><em>Specific displacements: {', '.join([f'{d/1000:.0f}k' for d in analysis['displacements']])}</em></p>
        </div>

        <div class="section">
            <h2>📊 Coefficient Data Quality</h2>
            <div class="table-container">
                <table>
                    <thead>
                        <tr>
                            <th>Coefficient</th>
                            <th>Completeness</th>
                            <th>Missing Records</th>
                            <th>Min Value</th>
                            <th>Max Value</th>
                            <th>Mean</th>
                            <th>Std Dev</th>
                        </tr>
                    </thead>
                    <tbody>
"""

        # Add coefficient statistics
        for coef, stats in analysis['coefficients'].items():
            complete_class = 'success' if stats['complete_pct'] >= 90 else 'warning'
            min_val = f"{stats['min']:.3f}" if stats['min'] is not None else 'N/A'
            max_val = f"{stats['max']:.3f}" if stats['max'] is not None else 'N/A'
            mean_val = f"{stats['mean']:.3f}" if stats['mean'] is not None else 'N/A'
            std_val = f"{stats['std']:.3f}" if stats['std'] is not None else 'N/A'
            html += f"""                        <tr>
                            <td><strong>{coef}</strong></td>
                            <td class="{complete_class}">{stats['complete_pct']:.1f}%</td>
                            <td>{stats['missing']}</td>
                            <td>{min_val}</td>
                            <td>{max_val}</td>
                            <td>{mean_val}</td>
                            <td>{std_val}</td>
                        </tr>
"""

        html += """                    </tbody>
                </table>
            </div>
        </div>

        <div class="section">
            <h2>📈 Interactive Visualizations</h2>
            <p><em>All charts are fully interactive: hover for details, click and drag to zoom, double-click to reset</em></p>
        </div>
"""

        # Generate plots
        coefficients = ['CXw', 'CYw', 'CMw', 'CXc', 'CYc', 'CMc']
        plot_id = 0

        for coef in coefficients:
            plot_id += 1
            fig = self.create_3d_surface_plot(coef)
            html += f"""        <div class="section">
            <h2>3D Surface: {coef}</h2>
            <div id="plot{plot_id}" class="plot-container"></div>
        </div>
"""
            html += f"""
    <script>
        var plot{plot_id}Data = {fig.to_json()};
        Plotly.newPlot('plot{plot_id}', plot{plot_id}Data.data, plot{plot_id}Data.layout, {{responsive: true}});
    </script>
"""

        # Polar diagrams
        plot_id += 1
        fig = self.create_polar_diagrams()
        html += f"""        <div class="section">
            <h2>Polar Diagrams - Wind Coefficients</h2>
            <div id="plot{plot_id}" class="plot-container"></div>
        </div>
"""
        html += f"""
    <script>
        var plot{plot_id}Data = {fig.to_json()};
        Plotly.newPlot('plot{plot_id}', plot{plot_id}Data.data, plot{plot_id}Data.layout, {{responsive: true}});
    </script>
"""

        # Heatmaps
        for coef in coefficients:
            plot_id += 1
            fig = self.create_coefficient_heatmap(coef)
            html += f"""        <div class="section">
            <h2>Heatmap: {coef}</h2>
            <div id="plot{plot_id}" class="plot-container"></div>
        </div>
"""
            html += f"""
    <script>
        var plot{plot_id}Data = {fig.to_json()};
        Plotly.newPlot('plot{plot_id}', plot{plot_id}Data.data, plot{plot_id}Data.layout, {{responsive: true}});
    </script>
"""

        # Heading sensitivity
        for coef in coefficients:
            plot_id += 1
            fig = self.create_heading_sensitivity_chart(coef)
            html += f"""        <div class="section">
            <h2>Heading Sensitivity: {coef}</h2>
            <div id="plot{plot_id}" class="plot-container"></div>
        </div>
"""
            html += f"""
    <script>
        var plot{plot_id}Data = {fig.to_json()};
        Plotly.newPlot('plot{plot_id}', plot{plot_id}Data.data, plot{plot_id}Data.layout, {{responsive: true}});
    </script>
"""

        # Force comparison
        plot_id += 1
        fig = self.create_force_comparison_chart()
        html += f"""        <div class="section">
            <h2>Force & Moment Comparison</h2>
            <div id="plot{plot_id}" class="plot-container"></div>
        </div>
"""
        html += f"""
    <script>
        var plot{plot_id}Data = {fig.to_json()};
        Plotly.newPlot('plot{plot_id}', plot{plot_id}Data.data, plot{plot_id}Data.layout, {{responsive: true}});
    </script>
"""

        # Footer
        html += """
        <div class="footer">
            <p><strong>🎯 Interactive HTML Report | Marine Analysis Module | Generated with Plotly</strong></p>
            <p>✅ Compliant with HTML_REPORTING_STANDARDS.md</p>
            <p>All plots are fully interactive with hover tooltips, zoom, pan, and export capabilities</p>
        </div>
    </div>
</body>
</html>
"""

        # Save report
        output_file = self.output_dir / filename
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(html)

        return output_file


def main():
    """Generate OCIMF interactive report."""
    import warnings
    warnings.filterwarnings('ignore')

    # Paths (using relative paths from script location)
    csv_file = '../../../data/ocimf/ocimf_coefficients_sample.csv'
    output_dir = '../../../docs/reports/ocimf'

    # Generate report
    reporter = OCIMFInteractiveReport(csv_file, output_dir)
    output_file = reporter.generate_html_report()
    return output_file


if __name__ == '__main__':
    main()
