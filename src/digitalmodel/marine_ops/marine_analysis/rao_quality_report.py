"""RAO Quality Report Generator.

ABOUTME: Generates interactive HTML reports for displacement RAO quality checks.
ABOUTME: Uses Plotly for visualizations following HTML_REPORTING_STANDARDS.md.

This module provides HTML report generation functionality for RAO quality checks,
including phase angle validation results, peak period detection, and vessel type
classification summaries.
"""

from typing import Dict, Any, List, Optional
from pathlib import Path
from datetime import datetime
import json
import csv

from .rao_validators import (
    DisplacementRAOQualityReport,
    PhaseCheckResult,
    PeakDetectionResult,
    VesselType,
    VESSEL_CHARACTERISTICS,
    LONG_PERIOD_EXPECTATIONS,
)


class RAOQualityReportGenerator:
    """Generator for interactive HTML reports of RAO quality checks."""

    def __init__(self, output_dir: Optional[str] = None):
        """Initialize report generator.

        Args:
            output_dir: Directory for output files. Defaults to docs/reports/rao_qa/
        """
        if output_dir:
            self.output_dir = Path(output_dir)
        else:
            self.output_dir = Path("docs/reports/rao_qa")

        self.output_dir.mkdir(parents=True, exist_ok=True)

    def generate_html_report(
        self,
        quality_report: DisplacementRAOQualityReport,
        rao_data: Any = None,
        report_name: Optional[str] = None
    ) -> Path:
        """Generate interactive HTML report for RAO quality check results.

        Args:
            quality_report: DisplacementRAOQualityReport from validation
            rao_data: Optional RAOData object for additional visualizations
            report_name: Optional custom report filename

        Returns:
            Path to generated HTML report
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        if report_name:
            filename = f"{report_name}_{timestamp}.html"
        else:
            filename = f"rao_quality_report_{timestamp}.html"

        report_path = self.output_dir / filename

        # Generate HTML content
        html_content = self._generate_html_content(quality_report, rao_data)

        # Write report
        with open(report_path, 'w', encoding='utf-8') as f:
            f.write(html_content)

        return report_path

    def export_csv_summary(
        self,
        quality_report: DisplacementRAOQualityReport,
        report_name: Optional[str] = None
    ) -> Path:
        """Export quality check results to CSV format.

        Args:
            quality_report: DisplacementRAOQualityReport from validation
            report_name: Optional custom filename

        Returns:
            Path to generated CSV file
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        if report_name:
            filename = f"{report_name}_{timestamp}.csv"
        else:
            filename = f"rao_quality_summary_{timestamp}.csv"

        csv_path = self.output_dir / filename

        with open(csv_path, 'w', newline='', encoding='utf-8') as f:
            writer = csv.writer(f)

            # Header
            writer.writerow(['RAO Quality Check Summary'])
            writer.writerow(['Generated', quality_report.timestamp.isoformat()])
            writer.writerow(['Source File', quality_report.source_file])
            writer.writerow(['Vessel Type', quality_report.vessel_type.value])
            writer.writerow(['Confidence', f"{quality_report.vessel_type_confidence:.1%}"])
            writer.writerow(['Overall Status', quality_report.overall_status])
            writer.writerow(['Pass Rate', f"{quality_report.pass_rate:.1f}%"])
            writer.writerow([])

            # Phase Check Results
            writer.writerow(['PHASE ANGLE CHECKS'])
            writer.writerow([
                'DOF', 'Heading (deg)', 'Period (s)',
                'Actual Amplitude', 'Expected Amplitude', 'Amplitude Error (%)',
                'Actual Phase (deg)', 'Expected Phase (deg)', 'Phase Error (deg)',
                'Status', 'Message'
            ])

            for check in quality_report.phase_checks:
                writer.writerow([
                    check.dof.upper(),
                    f"{check.heading:.1f}",
                    f"{check.period:.1f}",
                    f"{check.actual_amplitude:.4f}",
                    f"{check.expected_amplitude:.4f}",
                    f"{check.amplitude_error:.2f}",
                    f"{check.actual_phase:.1f}",
                    f"{check.expected_phase:.1f}",
                    f"{check.phase_error:.1f}",
                    check.status,
                    check.message
                ])

            writer.writerow([])

            # Peak Detection Results
            writer.writerow(['PEAK PERIOD CHECKS'])
            writer.writerow([
                'DOF', 'Heading (deg)', 'Peak Period (s)', 'Peak Amplitude',
                'Expected Range', 'In Range', 'Amplification Factor',
                'Status', 'Message'
            ])

            for check in quality_report.peak_checks:
                writer.writerow([
                    check.dof.upper(),
                    f"{check.heading:.1f}",
                    f"{check.peak_period:.2f}",
                    f"{check.peak_amplitude:.4f}",
                    f"({check.expected_range[0]:.1f}, {check.expected_range[1]:.1f})",
                    'Yes' if check.is_within_expected_range else 'No',
                    f"{check.amplification_factor:.2f}",
                    check.status,
                    check.message
                ])

        return csv_path

    def _generate_html_content(
        self,
        report: DisplacementRAOQualityReport,
        rao_data: Any = None
    ) -> str:
        """Generate complete HTML content for the report."""

        # Status colors
        status_colors = {
            'PASS': '#28a745',
            'WARNING': '#ffc107',
            'FAIL': '#dc3545',
            'UNKNOWN': '#6c757d'
        }

        overall_color = status_colors.get(report.overall_status, '#6c757d')

        # Generate Plotly chart data
        phase_chart_data = self._generate_phase_chart_data(report.phase_checks)
        peak_chart_data = self._generate_peak_chart_data(report.peak_checks)
        summary_chart_data = self._generate_summary_chart_data(report)

        html = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>RAO Quality Report - {report.timestamp.strftime('%Y-%m-%d %H:%M')}</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        * {{
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }}
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
            background-color: #f5f5f5;
            color: #333;
            line-height: 1.6;
        }}
        .container {{
            max-width: 1400px;
            margin: 0 auto;
            padding: 20px;
        }}
        header {{
            background: linear-gradient(135deg, #1a237e 0%, #0d47a1 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
        }}
        h1 {{
            font-size: 2.5rem;
            margin-bottom: 10px;
        }}
        .subtitle {{
            opacity: 0.9;
            font-size: 1.1rem;
        }}
        .summary-cards {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }}
        .card {{
            background: white;
            border-radius: 10px;
            padding: 20px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }}
        .card-title {{
            font-size: 0.9rem;
            color: #666;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 10px;
        }}
        .card-value {{
            font-size: 2rem;
            font-weight: bold;
        }}
        .status-pass {{ color: #28a745; }}
        .status-warning {{ color: #ffc107; }}
        .status-fail {{ color: #dc3545; }}
        .status-unknown {{ color: #6c757d; }}
        .section {{
            background: white;
            border-radius: 10px;
            padding: 25px;
            margin-bottom: 30px;
            box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }}
        .section-title {{
            font-size: 1.5rem;
            margin-bottom: 20px;
            padding-bottom: 10px;
            border-bottom: 2px solid #e0e0e0;
        }}
        .chart-container {{
            width: 100%;
            min-height: 400px;
            margin: 20px 0;
        }}
        table {{
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
        }}
        th, td {{
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #e0e0e0;
        }}
        th {{
            background: #f8f9fa;
            font-weight: 600;
            color: #333;
        }}
        tr:hover {{
            background: #f8f9fa;
        }}
        .badge {{
            display: inline-block;
            padding: 4px 12px;
            border-radius: 20px;
            font-size: 0.85rem;
            font-weight: 600;
        }}
        .badge-pass {{ background: #d4edda; color: #155724; }}
        .badge-warning {{ background: #fff3cd; color: #856404; }}
        .badge-fail {{ background: #f8d7da; color: #721c24; }}
        .info-grid {{
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
            gap: 20px;
        }}
        .info-item {{
            display: flex;
            justify-content: space-between;
            padding: 10px 0;
            border-bottom: 1px solid #e0e0e0;
        }}
        .info-label {{
            color: #666;
        }}
        .info-value {{
            font-weight: 600;
        }}
        footer {{
            text-align: center;
            padding: 20px;
            color: #666;
            font-size: 0.9rem;
        }}
        .vessel-type-badge {{
            display: inline-block;
            padding: 8px 16px;
            background: #e3f2fd;
            color: #1565c0;
            border-radius: 25px;
            font-weight: 600;
            margin-top: 10px;
        }}
        .expectations-table {{
            font-size: 0.9rem;
        }}
        .expectations-table td {{
            padding: 8px;
        }}
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>Displacement RAO Quality Report</h1>
            <p class="subtitle">Generated: {report.timestamp.strftime('%Y-%m-%d %H:%M:%S')}</p>
            {f'<p class="subtitle">Source: {report.source_file}</p>' if report.source_file else ''}
        </header>

        <!-- Summary Cards -->
        <div class="summary-cards">
            <div class="card">
                <div class="card-title">Overall Status</div>
                <div class="card-value" style="color: {overall_color};">{report.overall_status}</div>
            </div>
            <div class="card">
                <div class="card-title">Pass Rate</div>
                <div class="card-value">{report.pass_rate:.1f}%</div>
            </div>
            <div class="card">
                <div class="card-title">Total Checks</div>
                <div class="card-value">{report.total_checks}</div>
            </div>
            <div class="card">
                <div class="card-title">Passed</div>
                <div class="card-value status-pass">{report.passed_checks}</div>
            </div>
            <div class="card">
                <div class="card-title">Warnings</div>
                <div class="card-value status-warning">{report.warning_checks}</div>
            </div>
            <div class="card">
                <div class="card-title">Failed</div>
                <div class="card-value status-fail">{report.failed_checks}</div>
            </div>
        </div>

        <!-- Vessel Type Section -->
        <div class="section">
            <h2 class="section-title">Vessel Type Classification</h2>
            <div class="info-grid">
                <div>
                    <div class="info-item">
                        <span class="info-label">Detected Type:</span>
                        <span class="info-value">{report.vessel_type.value.replace('_', ' ').title()}</span>
                    </div>
                    <div class="info-item">
                        <span class="info-label">Confidence:</span>
                        <span class="info-value">{report.vessel_type_confidence:.1%}</span>
                    </div>
                    <div class="info-item">
                        <span class="info-label">Long Period Threshold:</span>
                        <span class="info-value">{report.long_period_threshold:.1f} seconds</span>
                    </div>
                </div>
                <div>
                    {self._generate_vessel_characteristics_html(report.vessel_type)}
                </div>
            </div>
        </div>

        <!-- Summary Chart -->
        <div class="section">
            <h2 class="section-title">Results Summary</h2>
            <div id="summaryChart" class="chart-container"></div>
        </div>

        <!-- Phase Check Results -->
        <div class="section">
            <h2 class="section-title">Long Period Phase Angle Checks</h2>
            <p style="margin-bottom: 20px; color: #666;">
                Phase angles checked at longest available period (T &gt; {report.long_period_threshold:.0f}s).
                Expected values based on Orcina convention (phase lag from wave crest).
            </p>
            <div id="phaseChart" class="chart-container"></div>
            {self._generate_phase_table_html(report.phase_checks)}
        </div>

        <!-- Peak Detection Results -->
        <div class="section">
            <h2 class="section-title">Peak Period Detection</h2>
            <p style="margin-bottom: 20px; color: #666;">
                RAO peaks detected and compared against expected natural period ranges for {report.vessel_type.value.replace('_', ' ')}.
            </p>
            <div id="peakChart" class="chart-container"></div>
            {self._generate_peak_table_html(report.peak_checks)}
        </div>

        <!-- Expected Values Reference -->
        <div class="section">
            <h2 class="section-title">Reference: Expected Long Period Values</h2>
            <p style="margin-bottom: 20px; color: #666;">
                Expected RAO amplitudes and phases at long periods (quasi-static regime).
            </p>
            {self._generate_expectations_table_html()}
        </div>

        <footer>
            <p>Generated by Digital Model RAO Quality Checker</p>
            <p>Phase convention: Orcina (phase lag from wave crest)</p>
        </footer>
    </div>

    <script>
        // Summary Pie Chart
        var summaryData = {json.dumps(summary_chart_data)};
        Plotly.newPlot('summaryChart', summaryData.data, summaryData.layout, {{responsive: true}});

        // Phase Check Chart
        var phaseData = {json.dumps(phase_chart_data)};
        Plotly.newPlot('phaseChart', phaseData.data, phaseData.layout, {{responsive: true}});

        // Peak Detection Chart
        var peakData = {json.dumps(peak_chart_data)};
        Plotly.newPlot('peakChart', peakData.data, peakData.layout, {{responsive: true}});
    </script>
</body>
</html>"""

        return html

    def _generate_vessel_characteristics_html(self, vessel_type: VesselType) -> str:
        """Generate HTML for vessel characteristics display."""
        if vessel_type not in VESSEL_CHARACTERISTICS:
            return "<p>No characteristics data available</p>"

        chars = VESSEL_CHARACTERISTICS[vessel_type]
        return f"""
        <div class="info-item">
            <span class="info-label">Heave Tn Range:</span>
            <span class="info-value">{chars.heave_tn_range[0]:.1f} - {chars.heave_tn_range[1]:.1f} s</span>
        </div>
        <div class="info-item">
            <span class="info-label">Pitch Tn Range:</span>
            <span class="info-value">{chars.pitch_tn_range[0]:.1f} - {chars.pitch_tn_range[1]:.1f} s</span>
        </div>
        <div class="info-item">
            <span class="info-label">Roll Tn Range:</span>
            <span class="info-value">{chars.roll_tn_range[0]:.1f} - {chars.roll_tn_range[1]:.1f} s</span>
        </div>
        """

    def _generate_phase_chart_data(self, phase_checks: List[PhaseCheckResult]) -> Dict:
        """Generate Plotly chart data for phase check results."""
        if not phase_checks:
            return {
                'data': [],
                'layout': {'title': 'No phase check data available'}
            }

        # Group by DOF
        dofs = ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']
        headings = sorted(set(c.heading for c in phase_checks))

        traces = []
        colors = {'PASS': '#28a745', 'WARNING': '#ffc107', 'FAIL': '#dc3545'}

        for dof in dofs:
            dof_checks = [c for c in phase_checks if c.dof == dof]
            if not dof_checks:
                continue

            x = [c.heading for c in dof_checks]
            y_amp = [c.actual_amplitude for c in dof_checks]
            y_phase = [c.actual_phase for c in dof_checks]
            marker_colors = [colors.get(c.status, '#6c757d') for c in dof_checks]
            text = [c.message for c in dof_checks]

            traces.append({
                'type': 'scatter',
                'x': x,
                'y': y_phase,
                'mode': 'markers+lines',
                'name': f'{dof.upper()} Phase',
                'text': text,
                'hovertemplate': '<b>%{text}</b><br>Heading: %{x}°<br>Phase: %{y:.1f}°<extra></extra>',
                'marker': {'size': 12, 'color': marker_colors},
                'line': {'dash': 'dot'}
            })

        return {
            'data': traces,
            'layout': {
                'title': 'Long Period Phase Angles by DOF and Heading',
                'xaxis': {'title': 'Wave Heading (degrees)', 'tickvals': headings},
                'yaxis': {'title': 'Phase Angle (degrees)', 'range': [-180, 180]},
                'hovermode': 'closest',
                'showlegend': True,
                'legend': {'orientation': 'h', 'y': -0.2}
            }
        }

    def _generate_peak_chart_data(self, peak_checks: List[PeakDetectionResult]) -> Dict:
        """Generate Plotly chart data for peak detection results."""
        if not peak_checks:
            return {
                'data': [],
                'layout': {'title': 'No peak detection data available'}
            }

        traces = []
        colors = {'PASS': '#28a745', 'WARNING': '#ffc107', 'FAIL': '#dc3545'}

        # Bar chart of peak periods with expected ranges
        dofs = list(set(c.dof for c in peak_checks))

        for dof in dofs:
            dof_checks = [c for c in peak_checks if c.dof == dof]

            x = [f"{dof.upper()} @ {c.heading:.0f}°" for c in dof_checks]
            y = [c.peak_period for c in dof_checks]
            marker_colors = [colors.get(c.status, '#6c757d') for c in dof_checks]
            text = [f"Peak: {c.peak_period:.1f}s<br>Expected: {c.expected_range[0]:.1f}-{c.expected_range[1]:.1f}s<br>Amp Factor: {c.amplification_factor:.1f}x" for c in dof_checks]

            traces.append({
                'type': 'bar',
                'x': x,
                'y': y,
                'name': dof.upper(),
                'text': text,
                'hovertemplate': '%{text}<extra></extra>',
                'marker': {'color': marker_colors}
            })

            # Add expected range as error bars
            for i, check in enumerate(dof_checks):
                mid = (check.expected_range[0] + check.expected_range[1]) / 2
                half_range = (check.expected_range[1] - check.expected_range[0]) / 2

                traces.append({
                    'type': 'scatter',
                    'x': [x[i]],
                    'y': [mid],
                    'mode': 'markers',
                    'marker': {'symbol': 'line-ew', 'size': 20, 'color': '#666'},
                    'error_y': {
                        'type': 'constant',
                        'value': half_range,
                        'visible': True,
                        'color': '#666'
                    },
                    'showlegend': False,
                    'hoverinfo': 'skip'
                })

        return {
            'data': traces,
            'layout': {
                'title': 'Peak Periods vs Expected Natural Period Ranges',
                'xaxis': {'title': 'DOF @ Heading'},
                'yaxis': {'title': 'Period (seconds)'},
                'barmode': 'group',
                'hovermode': 'closest',
                'showlegend': True
            }
        }

    def _generate_summary_chart_data(self, report: DisplacementRAOQualityReport) -> Dict:
        """Generate Plotly pie chart for results summary."""
        values = [report.passed_checks, report.warning_checks, report.failed_checks]
        labels = ['Passed', 'Warnings', 'Failed']
        colors = ['#28a745', '#ffc107', '#dc3545']

        # Filter out zero values
        filtered = [(l, v, c) for l, v, c in zip(labels, values, colors) if v > 0]
        if not filtered:
            return {
                'data': [],
                'layout': {'title': 'No checks performed'}
            }

        labels, values, colors = zip(*filtered)

        return {
            'data': [{
                'type': 'pie',
                'labels': labels,
                'values': values,
                'marker': {'colors': colors},
                'textinfo': 'label+value+percent',
                'hovertemplate': '<b>%{label}</b><br>Count: %{value}<br>Percentage: %{percent}<extra></extra>'
            }],
            'layout': {
                'title': 'Check Results Distribution',
                'showlegend': True
            }
        }

    def _generate_phase_table_html(self, phase_checks: List[PhaseCheckResult]) -> str:
        """Generate HTML table for phase check results."""
        if not phase_checks:
            return "<p>No phase checks performed</p>"

        rows = ""
        for check in sorted(phase_checks, key=lambda c: (c.dof, c.heading)):
            badge_class = f"badge-{check.status.lower()}"
            rows += f"""
            <tr>
                <td>{check.dof.upper()}</td>
                <td>{check.heading:.0f}°</td>
                <td>{check.period:.1f}</td>
                <td>{check.actual_amplitude:.4f}</td>
                <td>{check.expected_amplitude:.4f}</td>
                <td>{check.amplitude_error:.1f}%</td>
                <td>{check.actual_phase:.1f}°</td>
                <td>{check.expected_phase:.1f}°</td>
                <td>{check.phase_error:.1f}°</td>
                <td><span class="badge {badge_class}">{check.status}</span></td>
            </tr>
            """

        return f"""
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Heading</th>
                    <th>Period (s)</th>
                    <th>Actual Amp</th>
                    <th>Expected Amp</th>
                    <th>Amp Error</th>
                    <th>Actual Phase</th>
                    <th>Expected Phase</th>
                    <th>Phase Error</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
                {rows}
            </tbody>
        </table>
        """

    def _generate_peak_table_html(self, peak_checks: List[PeakDetectionResult]) -> str:
        """Generate HTML table for peak detection results."""
        if not peak_checks:
            return "<p>No peak detection performed</p>"

        rows = ""
        for check in sorted(peak_checks, key=lambda c: (c.dof, c.heading)):
            badge_class = f"badge-{check.status.lower()}"
            in_range = "Yes" if check.is_within_expected_range else "No"
            rows += f"""
            <tr>
                <td>{check.dof.upper()}</td>
                <td>{check.heading:.0f}°</td>
                <td>{check.peak_period:.2f}</td>
                <td>{check.peak_amplitude:.4f}</td>
                <td>({check.expected_range[0]:.1f}, {check.expected_range[1]:.1f})</td>
                <td>{in_range}</td>
                <td>{check.amplification_factor:.2f}x</td>
                <td><span class="badge {badge_class}">{check.status}</span></td>
            </tr>
            """

        return f"""
        <table>
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Heading</th>
                    <th>Peak Period (s)</th>
                    <th>Peak Amplitude</th>
                    <th>Expected Range</th>
                    <th>In Range</th>
                    <th>Amplification</th>
                    <th>Status</th>
                </tr>
            </thead>
            <tbody>
                {rows}
            </tbody>
        </table>
        """

    def _generate_expectations_table_html(self) -> str:
        """Generate reference table for expected long period values."""
        rows = ""

        for dof in ['surge', 'sway', 'heave', 'roll', 'pitch', 'yaw']:
            if dof not in LONG_PERIOD_EXPECTATIONS:
                continue

            for heading, exp in sorted(LONG_PERIOD_EXPECTATIONS[dof].items()):
                active = "Yes" if exp.is_active else "No"
                if exp.is_active:
                    amp_str = f"{exp.expected_amplitude:.2f}"
                    phase_str = f"{exp.expected_phase:.0f}°"
                else:
                    amp_str = "~0"
                    phase_str = "N/A"

                rows += f"""
                <tr>
                    <td>{dof.upper()}</td>
                    <td>{heading:.0f}°</td>
                    <td>{active}</td>
                    <td>{amp_str}</td>
                    <td>{phase_str}</td>
                </tr>
                """

        return f"""
        <table class="expectations-table">
            <thead>
                <tr>
                    <th>DOF</th>
                    <th>Heading</th>
                    <th>Active</th>
                    <th>Expected Amplitude</th>
                    <th>Expected Phase</th>
                </tr>
            </thead>
            <tbody>
                {rows}
            </tbody>
        </table>
        <p style="margin-top: 15px; font-size: 0.9rem; color: #666;">
            <strong>Phase Convention:</strong> Phase lag from wave crest to maximum positive excursion (Orcina convention).<br>
            <strong>Amplitude Units:</strong> m/m for translations, normalized to wave slope for rotations.
        </p>
        """
