#!/usr/bin/env python3
"""
ABOUTME: Operability analysis module for CALM Buoy systems
ABOUTME: Generates operability envelopes, weather downtime statistics, and interactive reports
"""

import sys
from pathlib import Path
from typing import Dict, List, Any, Optional, Tuple
import pandas as pd
import numpy as np
from datetime import datetime

# Import visualization libraries
try:
    import plotly.graph_objects as go
    from plotly.subplots import make_subplots
    import plotly.express as px
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False
    print("Warning: Plotly not available. Install with: pip install plotly")


class OperabilityAnalyzer:
    """
    Analyze operability results from OrcaFlex simulations.

    PHASE 4: Operability Results Post-Processing

    This class provides comprehensive operability analysis including:
    - Operability envelope generation (360° tension plots)
    - Weather downtime calculations based on wave scatter diagrams
    - Critical heading identification
    - Interactive HTML reports with Plotly visualizations
    """

    def __init__(self, project_root: Path):
        """
        Initialize operability analyzer.

        Args:
            project_root: Path to project root directory
        """
        self.project_root = Path(project_root)
        self.results_dir = self.project_root / "results"
        self.reports_dir = self.project_root / "reports"

        # Create directories if they don't exist
        self.results_dir.mkdir(parents=True, exist_ok=True)
        self.reports_dir.mkdir(parents=True, exist_ok=True)

    def load_simulation_results(self, results_file: Optional[Path] = None) -> pd.DataFrame:
        """
        Load OrcaFlex simulation results from CSV file.

        Args:
            results_file: Path to results CSV file (optional)

        Returns:
            DataFrame with columns: heading, max_tension, mean_tension, std_tension, etc.
        """
        if results_file is None:
            # Look for results file in standard location
            results_file = self.results_dir / "operability_results.csv"

        if not results_file.exists():
            raise FileNotFoundError(
                f"Results file not found: {results_file}. "
                "Run OrcaFlex simulations first or specify results file path."
            )

        results = pd.read_csv(results_file)

        # Validate required columns
        required_columns = ['heading', 'max_tension']
        missing = [col for col in required_columns if col not in results.columns]
        if missing:
            raise ValueError(f"Results file missing required columns: {missing}")

        return results

    def generate_operability_envelope(
        self,
        results: pd.DataFrame,
        mbl: float,
        sf_intact: float,
        sf_damaged: float = None,
        output_path: Optional[Path] = None
    ) -> Path:
        """
        Generate interactive Plotly operability envelope plot.

        Args:
            results: DataFrame with heading and tension data
            mbl: Minimum Breaking Load (kN)
            sf_intact: Safety factor for intact condition
            sf_damaged: Safety factor for damaged condition (optional)
            output_path: Path to save HTML file (optional)

        Returns:
            Path to generated HTML file
        """
        if not PLOTLY_AVAILABLE:
            raise ImportError("Plotly is required for operability envelope generation")

        if output_path is None:
            timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
            output_path = self.reports_dir / f"operability_envelope_{timestamp}.html"

        # Create polar plot
        fig = go.Figure()

        # Add maximum tension line
        fig.add_trace(go.Scatterpolar(
            r=results['max_tension'],
            theta=results['heading'],
            mode='lines+markers',
            name='Max Tension',
            line=dict(color='rgb(31, 119, 180)', width=2),
            marker=dict(size=6),
            hovertemplate='Heading: %{theta}°<br>Max Tension: %{r:.1f} kN<extra></extra>'
        ))

        # Add mean tension line if available
        if 'mean_tension' in results.columns:
            fig.add_trace(go.Scatterpolar(
                r=results['mean_tension'],
                theta=results['heading'],
                mode='lines',
                name='Mean Tension',
                line=dict(color='rgb(44, 160, 44)', width=1, dash='dash'),
                hovertemplate='Heading: %{theta}°<br>Mean Tension: %{r:.1f} kN<extra></extra>'
            ))

        # Add intact limit line
        intact_limit = mbl / sf_intact
        fig.add_trace(go.Scatterpolar(
            r=[intact_limit] * 360,
            theta=list(range(360)),
            mode='lines',
            name=f'Intact Limit (MBL/{sf_intact:.1f})',
            line=dict(color='rgb(255, 127, 14)', width=2, dash='dash'),
            hovertemplate=f'Intact Limit: {intact_limit:.1f} kN<extra></extra>'
        ))

        # Add damaged limit line if specified
        if sf_damaged:
            damaged_limit = mbl / sf_damaged
            fig.add_trace(go.Scatterpolar(
                r=[damaged_limit] * 360,
                theta=list(range(360)),
                mode='lines',
                name=f'Damaged Limit (MBL/{sf_damaged:.1f})',
                line=dict(color='rgb(214, 39, 40)', width=2, dash='dot'),
                hovertemplate=f'Damaged Limit: {damaged_limit:.1f} kN<extra></extra>'
            ))

        # Update layout
        fig.update_layout(
            title=dict(
                text='CALM Buoy Operability Envelope<br><sub>Maximum Mooring Line Tension vs. Heading</sub>',
                font=dict(size=20)
            ),
            polar=dict(
                radialaxis=dict(
                    title='Tension (kN)',
                    visible=True,
                    range=[0, max(results['max_tension'].max() * 1.1, intact_limit * 1.1)]
                ),
                angularaxis=dict(
                    direction='clockwise',
                    rotation=90,
                    tickmode='linear',
                    tick0=0,
                    dtick=30
                )
            ),
            showlegend=True,
            legend=dict(
                x=1.05,
                y=0.5,
                orientation='v'
            ),
            width=900,
            height=700,
            template='plotly_white'
        )

        # Save to HTML
        fig.write_html(
            str(output_path),
            include_plotlyjs='cdn',
            config={'displayModeBar': True, 'displaylogo': False}
        )

        print(f"✅ Operability envelope saved: {output_path}")
        return output_path

    def calculate_weather_downtime(
        self,
        wave_scatter: pd.DataFrame,
        operability_limit_hs: float,
        location_name: str = "Site"
    ) -> Dict[str, Any]:
        """
        Calculate weather downtime statistics based on wave scatter diagram.

        Args:
            wave_scatter: DataFrame with columns [Hs, Tp, occurrence_pct]
            operability_limit_hs: Operability limit Hs (m)
            location_name: Site location name

        Returns:
            Dictionary with downtime statistics
        """
        # Calculate annual downtime
        downtime_pct = wave_scatter[
            wave_scatter['Hs'] > operability_limit_hs
        ]['occurrence_pct'].sum()

        operational_pct = 100 - downtime_pct

        # Convert to hours per year
        hours_per_year = 8760
        downtime_hours = downtime_pct / 100 * hours_per_year
        operational_hours = operational_pct / 100 * hours_per_year

        statistics = {
            'location': location_name,
            'operability_limit_hs': operability_limit_hs,
            'downtime_percentage': downtime_pct,
            'operational_percentage': operational_pct,
            'downtime_hours_per_year': downtime_hours,
            'operational_hours_per_year': operational_hours,
            'downtime_days_per_year': downtime_hours / 24,
        }

        return statistics

    def generate_critical_headings_report(
        self,
        results: pd.DataFrame,
        mbl: float,
        sf_intact: float,
        top_n: int = 10
    ) -> pd.DataFrame:
        """
        Identify critical headings with highest mooring tensions.

        Args:
            results: DataFrame with heading and tension data
            mbl: Minimum Breaking Load (kN)
            sf_intact: Safety factor for intact condition
            top_n: Number of critical headings to return

        Returns:
            DataFrame with critical heading analysis
        """
        # Calculate utilization ratio
        results['utilization_ratio'] = results['max_tension'] / (mbl / sf_intact)

        # Sort by maximum tension
        critical = results.nlargest(top_n, 'max_tension').copy()

        # Add status column
        critical['status'] = critical['utilization_ratio'].apply(
            lambda x: '❌ FAIL' if x > 1.0 else '⚠️ WARNING' if x > 0.9 else '✅ PASS'
        )

        # Select relevant columns
        report_columns = ['heading', 'max_tension', 'utilization_ratio', 'status']
        if 'mean_tension' in results.columns:
            report_columns.insert(2, 'mean_tension')
        if 'std_tension' in results.columns:
            report_columns.insert(3, 'std_tension')

        critical_report = critical[report_columns].reset_index(drop=True)

        return critical_report

    def generate_comprehensive_report(
        self,
        results: pd.DataFrame,
        mbl: float,
        sf_intact: float,
        sf_damaged: float,
        wave_scatter: Optional[pd.DataFrame] = None,
        operability_limit_hs: Optional[float] = None,
        project_info: Optional[Dict[str, Any]] = None
    ) -> Path:
        """
        Generate comprehensive operability analysis HTML report.

        Args:
            results: DataFrame with simulation results
            mbl: Minimum Breaking Load (kN)
            sf_intact: Safety factor for intact condition
            sf_damaged: Safety factor for damaged condition
            wave_scatter: Optional wave scatter diagram
            operability_limit_hs: Optional operability limit Hs (m)
            project_info: Optional project metadata

        Returns:
            Path to generated HTML report
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        output_path = self.reports_dir / f"operability_report_{timestamp}.html"

        # Generate envelope plot
        envelope_path = self.generate_operability_envelope(
            results, mbl, sf_intact, sf_damaged
        )

        # Generate critical headings report
        critical_headings = self.generate_critical_headings_report(
            results, mbl, sf_intact, top_n=10
        )

        # Calculate weather downtime if wave scatter provided
        downtime_stats = None
        if wave_scatter is not None and operability_limit_hs is not None:
            downtime_stats = self.calculate_weather_downtime(
                wave_scatter, operability_limit_hs
            )

        # Build HTML report
        html_content = self._build_html_report(
            envelope_path=envelope_path,
            critical_headings=critical_headings,
            downtime_stats=downtime_stats,
            mbl=mbl,
            sf_intact=sf_intact,
            sf_damaged=sf_damaged,
            project_info=project_info
        )

        # Write HTML file
        with open(output_path, 'w', encoding='utf-8') as f:
            f.write(html_content)

        print(f"✅ Comprehensive report saved: {output_path}")
        return output_path

    def _build_html_report(
        self,
        envelope_path: Path,
        critical_headings: pd.DataFrame,
        downtime_stats: Optional[Dict],
        mbl: float,
        sf_intact: float,
        sf_damaged: float,
        project_info: Optional[Dict]
    ) -> str:
        """Build HTML report content."""
        # Read envelope plot HTML
        with open(envelope_path, 'r', encoding='utf-8') as f:
            envelope_html = f.read()

        # Extract just the plot div from envelope HTML
        start_marker = '<body>'
        end_marker = '</body>'
        start_idx = envelope_html.find(start_marker) + len(start_marker)
        end_idx = envelope_html.find(end_marker)
        envelope_plot = envelope_html[start_idx:end_idx]

        # Build project info section
        project_section = ""
        if project_info:
            project_section = f"""
        <div class="card">
            <h2>📋 Project Information</h2>
            <table class="info-table">
                <tr><th>Project Name</th><td>{project_info.get('name', 'N/A')}</td></tr>
                <tr><th>Project Code</th><td>{project_info.get('code', 'N/A')}</td></tr>
                <tr><th>Location</th><td>{project_info.get('location', 'N/A')}</td></tr>
                <tr><th>Water Depth</th><td>{project_info.get('water_depth', 'N/A')} m</td></tr>
            </table>
        </div>
        """

        # Build design criteria section
        criteria_section = f"""
        <div class="card">
            <h2>📐 Design Criteria</h2>
            <table class="info-table">
                <tr><th>Minimum Breaking Load (MBL)</th><td>{mbl:.0f} kN</td></tr>
                <tr><th>Safety Factor (Intact)</th><td>{sf_intact:.2f}</td></tr>
                <tr><th>Safety Factor (Damaged)</th><td>{sf_damaged:.2f}</td></tr>
                <tr><th>Intact Tension Limit</th><td>{mbl/sf_intact:.0f} kN</td></tr>
                <tr><th>Damaged Tension Limit</th><td>{mbl/sf_damaged:.0f} kN</td></tr>
            </table>
        </div>
        """

        # Build critical headings table
        critical_table_rows = ""
        for _, row in critical_headings.iterrows():
            mean_tension_str = f"{row.get('mean_tension', 0):.1f}" if pd.notna(row.get('mean_tension')) else 'N/A'
            critical_table_rows += f"""
                <tr>
                    <td>{row['heading']:.0f}°</td>
                    <td>{row['max_tension']:.1f}</td>
                    <td>{mean_tension_str}</td>
                    <td>{row['utilization_ratio']:.3f}</td>
                    <td>{row['status']}</td>
                </tr>
            """

        critical_section = f"""
        <div class="card">
            <h2>⚠️ Critical Headings</h2>
            <p>Top 10 headings with highest mooring line tensions:</p>
            <table class="data-table">
                <thead>
                    <tr>
                        <th>Heading (°)</th>
                        <th>Max Tension (kN)</th>
                        <th>Mean Tension (kN)</th>
                        <th>Utilization Ratio</th>
                        <th>Status</th>
                    </tr>
                </thead>
                <tbody>
                    {critical_table_rows}
                </tbody>
            </table>
        </div>
        """

        # Build weather downtime section if available
        downtime_section = ""
        if downtime_stats:
            downtime_section = f"""
        <div class="card">
            <h2>⏱️ Weather Downtime Analysis</h2>
            <table class="info-table">
                <tr><th>Location</th><td>{downtime_stats['location']}</td></tr>
                <tr><th>Operability Limit (Hs)</th><td>{downtime_stats['operability_limit_hs']:.1f} m</td></tr>
                <tr><th>Operational Time</th><td>{downtime_stats['operational_percentage']:.1f}%</td></tr>
                <tr><th>Weather Downtime</th><td>{downtime_stats['downtime_percentage']:.1f}%</td></tr>
                <tr><th>Operational Hours/Year</th><td>{downtime_stats['operational_hours_per_year']:.0f} hrs</td></tr>
                <tr><th>Downtime Hours/Year</th><td>{downtime_stats['downtime_hours_per_year']:.0f} hrs ({downtime_stats['downtime_days_per_year']:.1f} days)</td></tr>
            </table>
        </div>
        """

        # Complete HTML document
        html = f"""
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>CALM Buoy Operability Analysis Report</title>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}

        body {{
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            padding: 20px;
            color: #333;
        }}

        .container {{
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            border-radius: 10px;
            box-shadow: 0 10px 40px rgba(0, 0, 0, 0.2);
            overflow: hidden;
        }}

        header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            text-align: center;
        }}

        header h1 {{
            font-size: 2.5em;
            margin-bottom: 10px;
        }}

        header p {{
            font-size: 1.1em;
            opacity: 0.9;
        }}

        .content {{
            padding: 30px;
        }}

        .card {{
            background: #f8f9fa;
            border-radius: 8px;
            padding: 25px;
            margin-bottom: 25px;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        }}

        .card h2 {{
            color: #667eea;
            margin-bottom: 15px;
            font-size: 1.5em;
        }}

        .info-table {{
            width: 100%;
            border-collapse: collapse;
        }}

        .info-table th {{
            text-align: left;
            padding: 10px;
            background: #667eea;
            color: white;
            width: 40%;
        }}

        .info-table td {{
            padding: 10px;
            border-bottom: 1px solid #dee2e6;
        }}

        .data-table {{
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
        }}

        .data-table th {{
            background: #667eea;
            color: white;
            padding: 12px;
            text-align: left;
        }}

        .data-table td {{
            padding: 10px;
            border-bottom: 1px solid #dee2e6;
        }}

        .data-table tbody tr:hover {{
            background: #f1f3f5;
        }}

        .plot-container {{
            background: white;
            border-radius: 8px;
            padding: 20px;
            margin-bottom: 25px;
            box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
        }}

        footer {{
            background: #f8f9fa;
            padding: 20px;
            text-align: center;
            color: #6c757d;
            font-size: 0.9em;
        }}

        .timestamp {{
            margin-top: 10px;
            font-style: italic;
        }}
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>🌊 CALM Buoy Operability Analysis Report</h1>
            <p>Comprehensive 360° operability envelope and weather downtime analysis</p>
            <p class="timestamp">Generated: {datetime.now().strftime("%Y-%m-%d %H:%M:%S")}</p>
        </header>

        <div class="content">
            {project_section}

            {criteria_section}

            <div class="plot-container">
                <h2 style="color: #667eea; margin-bottom: 20px;">📊 Operability Envelope</h2>
                {envelope_plot}
            </div>

            {critical_section}

            {downtime_section}
        </div>

        <footer>
            <p>🤖 Generated by CALM Buoy Operability Analyzer</p>
            <p>Digital Model Automation Framework • OrcaFlex Analysis Tool</p>
        </footer>
    </div>
</body>
</html>
        """

        return html


# Example usage
if __name__ == "__main__":
    # Example: Create analyzer and generate sample report
    analyzer = OperabilityAnalyzer(Path("./projects/example_project"))

    # Example results data (would come from OrcaFlex simulations)
    sample_results = pd.DataFrame({
        'heading': list(range(0, 360, 30)),
        'max_tension': [3500, 3800, 4200, 4500, 4100, 3900, 3600, 3400, 3300, 3500, 3700, 4000],
        'mean_tension': [2800, 3000, 3300, 3600, 3200, 3100, 2900, 2700, 2600, 2800, 3000, 3200],
    })

    # Example wave scatter (simplified)
    sample_scatter = pd.DataFrame({
        'Hs': [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0],
        'Tp': [5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0],
        'occurrence_pct': [15.0, 25.0, 20.0, 15.0, 10.0, 8.0, 5.0, 2.0]
    })

    # Generate comprehensive report
    report_path = analyzer.generate_comprehensive_report(
        results=sample_results,
        mbl=7300,  # kN
        sf_intact=1.8,
        sf_damaged=1.3,
        wave_scatter=sample_scatter,
        operability_limit_hs=2.5,
        project_info={
            'name': 'North Sea CALM Buoy',
            'code': 'NSE_CALM_001',
            'location': 'North Sea',
            'water_depth': 120
        }
    )

    print(f"✅ Example report generated: {report_path}")
