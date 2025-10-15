"""
RAO QA Report Generator - Interactive HTML Report with Plotly

Generates comprehensive Quality Assurance report for RAO (Response Amplitude Operator) data:
- Interactive Plotly charts for RAO amplitude vs frequency/period
- Data quality metrics and statistics
- Multi-period comparison charts
- Validation checks and anomaly detection
- Professional HTML dashboard with embedded interactive plots

Uses CSV data with relative paths following HTML_REPORTING_STANDARDS.md

Author: Digital Model Team
Date: 2025-10-05
"""

import pandas as pd
import numpy as np
from pathlib import Path
from datetime import datetime
from typing import List, Dict, Tuple, Optional
import warnings
warnings.filterwarnings('ignore')

# Plotly for interactive charts (MANDATORY per HTML_REPORTING_STANDARDS.md)
try:
    import plotly.graph_objects as go
    import plotly.express as px
    from plotly.subplots import make_subplots
    PLOTLY_AVAILABLE = True
except ImportError:
    raise ImportError(
        "Plotly is required for HTML reports. "
        "Install with: pip install plotly pandas"
    )


class RAOQAReportGenerator:
    """Generate interactive HTML QA report for RAO data."""

    def __init__(self, output_dir: str = "docs/reports/rao_qa"):
        """
        Initialize RAO QA report generator.

        Args:
            output_dir: Directory for generated HTML reports
        """
        self.output_dir = Path(output_dir)
        self.output_dir.mkdir(parents=True, exist_ok=True)

        self.figures = []
        self.qa_metrics = {}
        self.validation_results = {}

        print("=" * 80)
        print("RAO QA REPORT GENERATOR")
        print("Interactive HTML Reports with Plotly")
        print("=" * 80)

    def load_rao_data(self, csv_file: str) -> pd.DataFrame:
        """
        Load RAO data from CSV file.

        The CSV format has:
        - Row 1: Period values (T in seconds)
        - Row 2+: RAO amplitude values for different wave headings

        Args:
            csv_file: Path to RAO CSV file (relative or absolute)

        Returns:
            DataFrame with columns: period, heading, amplitude
        """
        print(f"\n[FILE] Loading RAO data from: {csv_file}")

        # Read CSV (first row is periods)
        df_raw = pd.read_csv(csv_file, header=None)

        # First row contains periods
        periods = df_raw.iloc[0].values.astype(float)

        # Remaining rows are RAO amplitudes for different headings
        headings = []
        amplitudes = []

        for heading_idx in range(1, len(df_raw)):
            heading = heading_idx * 10  # Assuming 10° increments
            rao_values = df_raw.iloc[heading_idx].values.astype(float)

            for period, rao in zip(periods, rao_values):
                headings.append(heading)
                amplitudes.append(rao)

        # Create structured DataFrame
        df = pd.DataFrame({
            'period': np.tile(periods, len(df_raw) - 1),
            'frequency': 2 * np.pi / np.tile(periods, len(df_raw) - 1),
            'heading': headings,
            'rao_amplitude': amplitudes
        })

        print(f"[OK] Loaded {len(df)} RAO data points")
        print(f"  - Period range: {periods.min():.1f}s to {periods.max():.1f}s")
        print(f"  - Headings: {df['heading'].nunique()} wave directions")
        print(f"  - RAO amplitude range: {df['rao_amplitude'].min():.3f} to {df['rao_amplitude'].max():.3f}")

        return df

    def calculate_qa_metrics(self, df: pd.DataFrame) -> Dict:
        """
        Calculate QA metrics for RAO data.

        Args:
            df: RAO DataFrame

        Returns:
            Dictionary of QA metrics
        """
        print("\n[STATS] Calculating QA metrics...")

        metrics = {
            'total_points': len(df),
            'unique_periods': df['period'].nunique(),
            'unique_headings': df['heading'].nunique(),
            'period_range': (df['period'].min(), df['period'].max()),
            'frequency_range': (df['frequency'].min(), df['frequency'].max()),
            'rao_min': df['rao_amplitude'].min(),
            'rao_max': df['rao_amplitude'].max(),
            'rao_mean': df['rao_amplitude'].mean(),
            'rao_std': df['rao_amplitude'].std(),
            'missing_values': df['rao_amplitude'].isna().sum(),
            'completeness': 100 * (1 - df['rao_amplitude'].isna().sum() / len(df))
        }

        # Check for anomalies
        Q1 = df['rao_amplitude'].quantile(0.25)
        Q3 = df['rao_amplitude'].quantile(0.75)
        IQR = Q3 - Q1
        outliers = ((df['rao_amplitude'] < (Q1 - 1.5 * IQR)) |
                    (df['rao_amplitude'] > (Q3 + 1.5 * IQR)))
        metrics['outliers_count'] = outliers.sum()
        metrics['outliers_pct'] = 100 * outliers.sum() / len(df)

        self.qa_metrics = metrics

        print(f"[OK] Calculated {len(metrics)} QA metrics")

        return metrics

    def create_rao_period_plot(self, df: pd.DataFrame, title: str = "RAO vs Period") -> go.Figure:
        """
        Create interactive RAO vs Period plot for all headings.

        Args:
            df: RAO DataFrame
            title: Plot title

        Returns:
            Plotly Figure
        """
        fig = go.Figure()

        # Plot each heading as separate trace
        headings = sorted(df['heading'].unique())

        for heading in headings[::2]:  # Plot every other heading to avoid clutter
            df_heading = df[df['heading'] == heading].sort_values('period')

            fig.add_trace(go.Scatter(
                x=df_heading['period'],
                y=df_heading['rao_amplitude'],
                mode='lines+markers',
                name=f'{heading}°',
                marker=dict(size=4),
                line=dict(width=2),
                hovertemplate=(
                    f'<b>Heading: {heading}°</b><br>'
                    'Period: %{x:.2f}s<br>'
                    'RAO: %{y:.3f}<br>'
                    '<extra></extra>'
                )
            ))

        fig.update_layout(
            title=dict(text=title, font=dict(size=18)),
            xaxis=dict(title='Period (s)', gridcolor='lightgray'),
            yaxis=dict(title='RAO Amplitude (m/m)', gridcolor='lightgray'),
            template='plotly_white',
            hovermode='closest',
            height=600,
            showlegend=True,
            legend=dict(
                title='Wave Heading',
                orientation='v',
                yanchor='top',
                y=1,
                xanchor='left',
                x=1.02
            )
        )

        return fig

    def create_rao_frequency_plot(self, df: pd.DataFrame) -> go.Figure:
        """
        Create interactive RAO vs Frequency plot.

        Args:
            df: RAO DataFrame

        Returns:
            Plotly Figure
        """
        fig = go.Figure()

        headings = sorted(df['heading'].unique())

        for heading in headings[::2]:
            df_heading = df[df['heading'] == heading].sort_values('frequency')

            fig.add_trace(go.Scatter(
                x=df_heading['frequency'],
                y=df_heading['rao_amplitude'],
                mode='lines',
                name=f'{heading}°',
                line=dict(width=2),
                hovertemplate=(
                    f'<b>Heading: {heading}°</b><br>'
                    'Frequency: %{x:.3f} rad/s<br>'
                    'RAO: %{y:.3f}<br>'
                    '<extra></extra>'
                )
            ))

        fig.update_layout(
            title='RAO Amplitude vs Frequency',
            xaxis=dict(title='Frequency (rad/s)', gridcolor='lightgray'),
            yaxis=dict(title='RAO Amplitude (m/m)', gridcolor='lightgray'),
            template='plotly_white',
            hovermode='closest',
            height=600,
            showlegend=True,
            legend=dict(title='Wave Heading')
        )

        return fig

    def create_polar_plot(self, df: pd.DataFrame, period_value: float) -> go.Figure:
        """
        Create polar plot of RAO vs heading for specific period.

        Args:
            df: RAO DataFrame
            period_value: Period value to plot

        Returns:
            Plotly Figure
        """
        # Find closest period
        df_period = df.iloc[(df['period'] - period_value).abs().argsort()[:df['heading'].nunique()]]
        df_period = df_period.sort_values('heading')

        # Convert heading to radians
        theta = np.radians(df_period['heading'].values)
        r = df_period['rao_amplitude'].values

        # Close the polar plot
        theta = np.append(theta, theta[0])
        r = np.append(r, r[0])

        fig = go.Figure()

        fig.add_trace(go.Scatterpolar(
            r=r,
            theta=np.degrees(theta),
            mode='lines+markers',
            name=f'T={period_value:.1f}s',
            line=dict(width=3, color='blue'),
            marker=dict(size=8),
            fill='toself',
            fillcolor='rgba(0, 0, 255, 0.1)',
            hovertemplate=(
                '<b>Heading: %{theta}°</b><br>'
                f'Period: {period_value:.1f}s<br>'
                'RAO: %{r:.3f}<br>'
                '<extra></extra>'
            )
        ))

        fig.update_layout(
            title=f'RAO Polar Diagram - Period = {period_value:.1f}s',
            polar=dict(
                radialaxis=dict(
                    visible=True,
                    range=[0, r.max() * 1.1],
                    gridcolor='lightgray'
                ),
                angularaxis=dict(
                    direction='clockwise',
                    gridcolor='lightgray'
                )
            ),
            template='plotly_white',
            height=600
        )

        return fig

    def create_heatmap(self, df: pd.DataFrame) -> go.Figure:
        """
        Create heatmap of RAO amplitude vs period and heading.

        Args:
            df: RAO DataFrame

        Returns:
            Plotly Figure
        """
        # Pivot data for heatmap
        pivot = df.pivot_table(
            values='rao_amplitude',
            index='heading',
            columns='period',
            aggfunc='mean'
        )

        fig = go.Figure(data=go.Heatmap(
            z=pivot.values,
            x=pivot.columns,
            y=pivot.index,
            colorscale='Viridis',
            hovertemplate=(
                'Period: %{x:.2f}s<br>'
                'Heading: %{y}°<br>'
                'RAO: %{z:.3f}<br>'
                '<extra></extra>'
            ),
            colorbar=dict(title='RAO Amplitude')
        ))

        fig.update_layout(
            title='RAO Amplitude Heatmap',
            xaxis=dict(title='Period (s)'),
            yaxis=dict(title='Wave Heading (°)'),
            template='plotly_white',
            height=600
        )

        return fig

    def create_statistics_plot(self, df: pd.DataFrame) -> go.Figure:
        """
        Create statistical summary plot (box plots by heading).

        Args:
            df: RAO DataFrame

        Returns:
            Plotly Figure
        """
        headings = sorted(df['heading'].unique())

        fig = go.Figure()

        for heading in headings[::2]:
            df_heading = df[df['heading'] == heading]

            fig.add_trace(go.Box(
                y=df_heading['rao_amplitude'],
                name=f'{heading}°',
                boxmean='sd',
                hovertemplate=(
                    f'<b>Heading: {heading}°</b><br>'
                    'RAO: %{y:.3f}<br>'
                    '<extra></extra>'
                )
            ))

        fig.update_layout(
            title='RAO Amplitude Statistical Distribution by Heading',
            xaxis=dict(title='Wave Heading (°)'),
            yaxis=dict(title='RAO Amplitude (m/m)', gridcolor='lightgray'),
            template='plotly_white',
            height=500,
            showlegend=False
        )

        return fig

    def generate_html_report(self, df: pd.DataFrame, report_name: str = "rao_qa_report"):
        """
        Generate complete HTML QA report with all interactive charts.

        Args:
            df: RAO DataFrame
            report_name: Output filename (without extension)
        """
        print("\n[REPORT] Generating HTML QA Report...")

        # Calculate QA metrics
        metrics = self.calculate_qa_metrics(df)

        # Create all interactive plots
        print("  Creating interactive charts...")
        fig1 = self.create_rao_period_plot(df, "RAO Amplitude vs Wave Period - All Headings")
        fig2 = self.create_rao_frequency_plot(df)
        fig3 = self.create_polar_plot(df, period_value=df['period'].median())
        fig4 = self.create_heatmap(df)
        fig5 = self.create_statistics_plot(df)

        # Build HTML report
        html_content = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>RAO QA Report - Interactive Analysis</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            padding: 20px;
            color: #2d3748;
        }}
        .container {{
            max-width: 1600px;
            margin: 0 auto;
        }}
        .report-header {{
            background: white;
            color: #2d3748;
            padding: 40px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2);
        }}
        .report-header h1 {{
            font-size: 2.5em;
            margin-bottom: 15px;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
        }}
        .report-header p {{
            font-size: 1.1em;
            color: #718096;
            margin: 8px 0;
        }}
        .summary-stats {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }}
        .stat-card {{
            background: white;
            padding: 30px;
            border-radius: 12px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
            transition: all 0.3s ease;
            border-left: 5px solid #667eea;
        }}
        .stat-card:hover {{
            transform: translateY(-8px);
            box-shadow: 0 12px 24px rgba(0, 0, 0, 0.25);
        }}
        .stat-label {{
            font-size: 0.9em;
            color: #718096;
            margin-bottom: 10px;
            text-transform: uppercase;
            letter-spacing: 1px;
            font-weight: 600;
        }}
        .stat-value {{
            font-size: 2.8em;
            font-weight: 700;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
            line-height: 1.2;
        }}
        .stat-unit {{
            font-size: 0.4em;
            color: #a0aec0;
            margin-left: 8px;
        }}
        .plot-container {{
            background: white;
            padding: 35px;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
        }}
        .plot-container h2 {{
            font-size: 1.6em;
            margin-bottom: 25px;
            color: #2d3748;
            font-weight: 600;
            padding-bottom: 15px;
            border-bottom: 3px solid #667eea;
        }}
        .qa-status {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 15px;
            padding: 25px;
            background: white;
            border-radius: 12px;
            margin-bottom: 30px;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
        }}
        .qa-item {{
            padding: 20px;
            border-radius: 8px;
            text-align: center;
            font-weight: 600;
        }}
        .qa-pass {{
            background: #48bb78;
            color: white;
        }}
        .qa-warn {{
            background: #ed8936;
            color: white;
        }}
        .qa-fail {{
            background: #f56565;
            color: white;
        }}
        .footer {{
            background: white;
            padding: 25px;
            border-radius: 12px;
            margin-top: 30px;
            text-align: center;
            color: #718096;
            font-size: 0.95em;
            box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
        }}
        .footer strong {{
            color: #667eea;
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
                font-size: 2.2em;
            }}
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="report-header">
            <h1>🌊 RAO Quality Assurance Report</h1>
            <p><strong>Generated:</strong> {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}</p>
            <p><strong>Analysis Type:</strong> Response Amplitude Operator (RAO) Validation</p>
            <p><strong>Data Source:</strong> Vessel Heave RAO Study</p>
        </div>

        <div class="qa-status">
            <div class="qa-item qa-{'pass' if metrics['completeness'] >= 95 else 'warn' if metrics['completeness'] >= 80 else 'fail'}">
                [OK] Data Completeness<br>
                <span style="font-size: 1.5em;">{metrics['completeness']:.1f}%</span>
            </div>
            <div class="qa-item qa-{'pass' if metrics['outliers_pct'] < 5 else 'warn' if metrics['outliers_pct'] < 10 else 'fail'}">
                {'[OK]' if metrics['outliers_pct'] < 5 else '[WARN]'} Outliers Check<br>
                <span style="font-size: 1.5em;">{metrics['outliers_pct']:.1f}%</span>
            </div>
            <div class="qa-item qa-pass">
                [OK] Data Points<br>
                <span style="font-size: 1.5em;">{metrics['total_points']:,}</span>
            </div>
            <div class="qa-item qa-pass">
                [OK] Wave Headings<br>
                <span style="font-size: 1.5em;">{metrics['unique_headings']}</span>
            </div>
        </div>

        <div class="summary-stats">
            <div class="stat-card">
                <div class="stat-label">Total Data Points</div>
                <div class="stat-value">{metrics['total_points']:,}</div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Wave Periods</div>
                <div class="stat-value">{metrics['unique_periods']}<span class="stat-unit">periods</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">Period Range</div>
                <div class="stat-value">{metrics['period_range'][0]:.1f} - {metrics['period_range'][1]:.1f}<span class="stat-unit">seconds</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">RAO Peak Value</div>
                <div class="stat-value">{metrics['rao_max']:.3f}<span class="stat-unit">m/m</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">RAO Mean</div>
                <div class="stat-value">{metrics['rao_mean']:.3f}<span class="stat-unit">m/m</span></div>
            </div>
            <div class="stat-card">
                <div class="stat-label">RAO Std Dev</div>
                <div class="stat-value">{metrics['rao_std']:.3f}</div>
            </div>
        </div>

        <div class="plot-container">
            <h2>📊 RAO vs Period - All Wave Headings</h2>
            <div id="plot1"></div>
        </div>

        <div class="plot-container">
            <h2>📈 RAO vs Frequency Response</h2>
            <div id="plot2"></div>
        </div>

        <div class="plot-container">
            <h2>🎯 Polar Response Diagram</h2>
            <div id="plot3"></div>
        </div>

        <div class="plot-container">
            <h2>🔥 RAO Amplitude Heatmap</h2>
            <div id="plot4"></div>
        </div>

        <div class="plot-container">
            <h2>📦 Statistical Distribution Analysis</h2>
            <div id="plot5"></div>
        </div>

        <div class="footer">
            <p><strong>Interactive HTML Report</strong> | Response Amplitude Operator QA</p>
            <p>All plots are fully interactive: hover for details, click and drag to zoom, double-click to reset view</p>
            <p>Generated with <strong>Plotly</strong> following HTML_REPORTING_STANDARDS.md</p>
            <p style="margin-top: 10px; color: #a0aec0; font-size: 0.85em;">
                🤖 Generated with Claude Code | Digital Model Engineering
            </p>
        </div>
    </div>

    <script>
        // Plot 1 - RAO vs Period
        var plot1Data = {fig1.to_json()};
        Plotly.newPlot('plot1', plot1Data.data, plot1Data.layout, {{responsive: true}});

        // Plot 2 - RAO vs Frequency
        var plot2Data = {fig2.to_json()};
        Plotly.newPlot('plot2', plot2Data.data, plot2Data.layout, {{responsive: true}});

        // Plot 3 - Polar Plot
        var plot3Data = {fig3.to_json()};
        Plotly.newPlot('plot3', plot3Data.data, plot3Data.layout, {{responsive: true}});

        // Plot 4 - Heatmap
        var plot4Data = {fig4.to_json()};
        Plotly.newPlot('plot4', plot4Data.data, plot4Data.layout, {{responsive: true}});

        // Plot 5 - Statistics
        var plot5Data = {fig5.to_json()};
        Plotly.newPlot('plot5', plot5Data.data, plot5Data.layout, {{responsive: true}});
    </script>
</body>
</html>
"""

        # Write HTML report
        output_file = self.output_dir / f"{report_name}.html"
        with open(output_file, 'w', encoding='utf-8') as f:
            f.write(html_content)

        print(f"\n[SUCCESS] HTML Report Generated Successfully!")
        print(f"   [DOC] File: {output_file}")
        print(f"   [CHART] Charts: 5 interactive Plotly visualizations")
        print(f"   [STATS] Metrics: {len(metrics)} QA statistics")
        print("=" * 80)

        return output_file


def main():
    """Generate RAO QA reports from example files."""
    print("\n" + "=" * 80)
    print("GENERATING RAO QA REPORTS")
    print("=" * 80)

    # Initialize report generator
    generator = RAOQAReportGenerator()

    # Production example RAO data (using relative path per HTML_REPORTING_STANDARDS.md)
    rao_file = "data/marine_engineering/raos/vessel_heave_rao_example.csv"

    print(f"\n[*] Processing RAO file: {rao_file}")
    print("[INFO] Using production example data from data/marine_engineering/raos/")

    # Load RAO data
    df = generator.load_rao_data(rao_file)

    # Generate HTML report
    report_file = generator.generate_html_report(df, report_name="vessel_heave_rao_qa_report")

    print(f"\n[SUCCESS] Report ready! Open in browser:")
    print(f"   {report_file.absolute()}")
    print("\n" + "=" * 80)


if __name__ == "__main__":
    main()
