"""
Plotly HTML Report Template Generator.

Creates professional, interactive HTML reports with Plotly visualizations
and CSV data import using relative paths.
"""

import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.express as px
import pandas as pd
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Optional


class PlotlyReportGenerator:
    """Generate interactive HTML reports with Plotly."""

    def __init__(self, title: str, module_name: str, repository_name: str):
        """
        Initialize report generator.

        Args:
            title: Report title
            module_name: Name of the module generating the report
            repository_name: Name of the repository
        """
        self.title = title
        self.module_name = module_name
        self.repository_name = repository_name
        self.figures = []
        self.summary_stats = {}

    def add_summary_stat(self, label: str, value: str, unit: str = ''):
        """
        Add a summary statistic to display.

        Args:
            label: Statistic label
            value: Statistic value
            unit: Optional unit of measurement
        """
        self.summary_stats[label] = {
            'value': value,
            'unit': unit
        }

    def add_line_plot(self, df: pd.DataFrame, x: str, y: str,
                      title: str, color: Optional[str] = None):
        """
        Add an interactive line plot.

        Args:
            df: DataFrame with data
            x: Column name for x-axis
            y: Column name for y-axis
            title: Plot title
            color: Optional column for color grouping
        """
        fig = px.line(df, x=x, y=y, color=color,
                      title=title,
                      template='plotly_white',
                      hover_data=df.columns.tolist())

        fig.update_layout(
            hovermode='x unified',
            height=500,
            font=dict(size=12),
        )

        self.figures.append(fig)

    def add_scatter_plot(self, df: pd.DataFrame, x: str, y: str,
                         title: str, color: Optional[str] = None,
                         size: Optional[str] = None):
        """
        Add an interactive scatter plot.

        Args:
            df: DataFrame with data
            x: Column name for x-axis
            y: Column name for y-axis
            title: Plot title
            color: Optional column for color
            size: Optional column for size
        """
        fig = px.scatter(df, x=x, y=y, color=color, size=size,
                         title=title,
                         template='plotly_white',
                         hover_data=df.columns.tolist())

        fig.update_layout(
            height=500,
            font=dict(size=12),
        )

        self.figures.append(fig)

    def add_bar_chart(self, df: pd.DataFrame, x: str, y: str,
                      title: str, color: Optional[str] = None):
        """
        Add an interactive bar chart.

        Args:
            df: DataFrame with data
            x: Column name for x-axis (categories)
            y: Column name for y-axis (values)
            title: Plot title
            color: Optional column for color grouping
        """
        fig = px.bar(df, x=x, y=y, color=color,
                     title=title,
                     template='plotly_white',
                     hover_data=df.columns.tolist())

        fig.update_layout(
            height=500,
            font=dict(size=12),
        )

        self.figures.append(fig)

    def add_histogram(self, df: pd.DataFrame, column: str, title: str,
                      nbins: int = 30):
        """
        Add an interactive histogram.

        Args:
            df: DataFrame with data
            column: Column name to plot
            title: Plot title
            nbins: Number of bins
        """
        fig = px.histogram(df, x=column, nbins=nbins,
                           title=title,
                           template='plotly_white')

        fig.update_layout(
            height=400,
            font=dict(size=12),
        )

        self.figures.append(fig)

    def add_heatmap(self, df: pd.DataFrame, title: str):
        """
        Add a correlation heatmap.

        Args:
            df: DataFrame with numeric columns
            title: Plot title
        """
        # Calculate correlation matrix
        corr = df.select_dtypes(include='number').corr()

        fig = go.Figure(data=go.Heatmap(
            z=corr.values,
            x=corr.columns,
            y=corr.columns,
            colorscale='RdBu',
            zmid=0,
            text=corr.values.round(2),
            texttemplate='%{text}',
            textfont={"size": 10},
            hovertemplate='%{x} vs %{y}: %{z:.2f}<extra></extra>'
        ))

        fig.update_layout(
            title=title,
            template='plotly_white',
            height=500,
            font=dict(size=12),
        )

        self.figures.append(fig)

    def add_custom_figure(self, fig: go.Figure):
        """
        Add a custom Plotly figure.

        Args:
            fig: Plotly Figure object
        """
        self.figures.append(fig)

    def generate_html(self, output_file: str):
        """
        Generate the complete HTML report.

        Args:
            output_file: Path to save HTML file
        """
        # Generate HTML header
        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>{self.title}</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background-color: #f5f7fa;
            padding: 20px;
            color: #2d3748;
        }}
        .container {{
            max-width: 1400px;
            margin: 0 auto;
        }}
        .report-header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 40px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }}
        .report-header h1 {{
            font-size: 2.5em;
            margin-bottom: 10px;
            font-weight: 700;
        }}
        .report-header p {{
            font-size: 1.1em;
            opacity: 0.95;
            margin: 5px 0;
        }}
        .summary-stats {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }}
        .stat-card {{
            background: white;
            padding: 25px;
            border-radius: 10px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
            transition: transform 0.2s, box-shadow 0.2s;
        }}
        .stat-card:hover {{
            transform: translateY(-4px);
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.12);
        }}
        .stat-label {{
            font-size: 0.9em;
            color: #718096;
            margin-bottom: 8px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }}
        .stat-value {{
            font-size: 2.5em;
            font-weight: 700;
            color: #667eea;
            line-height: 1;
        }}
        .stat-unit {{
            font-size: 0.5em;
            color: #a0aec0;
            margin-left: 5px;
        }}
        .plot-container {{
            background: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 25px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
        }}
        .plot-container h2 {{
            font-size: 1.5em;
            margin-bottom: 20px;
            color: #2d3748;
            font-weight: 600;
        }}
        .plot {{
            width: 100%;
        }}
        .footer {{
            background: white;
            padding: 20px;
            border-radius: 10px;
            margin-top: 30px;
            text-align: center;
            color: #718096;
            font-size: 0.9em;
        }}
        @media (max-width: 768px) {{
            .report-header {{
                padding: 25px;
            }}
            .report-header h1 {{
                font-size: 1.8em;
            }}
            .summary-stats {{
                grid-template-columns: 1fr;
            }}
            .stat-value {{
                font-size: 2em;
            }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="report-header">
            <h1>{self.title}</h1>
            <p><strong>Generated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
            <p><strong>Module:</strong> {self.module_name}</p>
            <p><strong>Repository:</strong> {self.repository_name}</p>
        </div>
"""

        # Add summary statistics
        if self.summary_stats:
            html += '        <div class="summary-stats">\n'
            for label, stat in self.summary_stats.items():
                html += f"""            <div class="stat-card">
                <div class="stat-label">{label}</div>
                <div class="stat-value">{stat['value']}<span class="stat-unit">{stat['unit']}</span></div>
            </div>
"""
            html += '        </div>\n'

        # Add plots
        for i, fig in enumerate(self.figures):
            plot_id = f'plot{i+1}'
            html += f"""        <div class="plot-container">
            <div id="{plot_id}" class="plot"></div>
        </div>
"""

        # Add footer
        html += f"""        <div class="footer">
            <p>Interactive HTML Report | {self.module_name} | Generated with Plotly</p>
            <p>All plots are interactive: hover for details, click and drag to zoom, double-click to reset</p>
        </div>
    </div>

    <script>
"""

        # Add plot configurations
        for i, fig in enumerate(self.figures):
            plot_id = f'plot{i+1}'
            plot_json = fig.to_json()
            html += f"""
        // Plot {i+1}
        var plot{i+1}Data = {plot_json};
        Plotly.newPlot('{plot_id}', plot{i+1}Data.data, plot{i+1}Data.layout, {{responsive: true}});
"""

        html += """
    </script>
</body>
</html>
"""

        # Write to file
        Path(output_file).parent.mkdir(parents=True, exist_ok=True)
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(html)

        print(f"✓ HTML report generated: {output_file}")


if __name__ == '__main__':
    # Example usage
    import numpy as np

    # Create sample data
    dates = pd.date_range('2025-01-01', periods=100, freq='D')
    df = pd.DataFrame({
        'date': dates,
        'value': np.cumsum(np.random.randn(100)) + 100,
        'category': np.random.choice(['A', 'B', 'C'], 100)
    })

    # Generate report
    report = PlotlyReportGenerator(
        title='Sample Analysis Report',
        module_name='Example Module',
        repository_name='workspace-hub'
    )

    # Add summary stats
    report.add_summary_stat('Total Records', '100')
    report.add_summary_stat('Average Value', '99.8', 'units')
    report.add_summary_stat('Categories', '3')

    # Add plots
    report.add_line_plot(df, x='date', y='value',
                         title='Value Over Time', color='category')
    report.add_histogram(df, 'value', 'Value Distribution')

    # Generate HTML
    report.generate_html('example_report.html')
