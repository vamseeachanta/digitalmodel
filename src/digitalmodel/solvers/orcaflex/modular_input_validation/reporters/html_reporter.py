"""
ABOUTME: HTML reporter with interactive visualizations
ABOUTME: Generates HTML reports with Plotly charts and filterable tables
"""

from pathlib import Path
from typing import List
from datetime import datetime
import json

from ..models import ValidationResult, ValidationStatus, Severity


class HTMLReporter:
    """
    HTML reporter with interactive visualizations.

    Generates HTML files with:
    - Plotly interactive charts
    - Filterable result tables
    - Collapsible detail sections
    - Responsive layout
    """

    def __init__(self, output_path: Path):
        """
        Initialize HTML reporter.

        Args:
            output_path: Path to output HTML file
        """
        self.output_path = Path(output_path)
        self.output_path.parent.mkdir(parents=True, exist_ok=True)

    def report(self, results: List[ValidationResult]) -> None:
        """
        Generate HTML report.

        Args:
            results: List of validation results
        """
        html_content = self._generate_html(results)

        with open(self.output_path, 'w', encoding='utf-8') as f:
            f.write(html_content)

        print(f"HTML report saved to: {self.output_path}")

    def _generate_html(self, results: List[ValidationResult]) -> str:
        """Generate complete HTML document."""
        timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")

        # Generate chart data
        chart_data = self._generate_chart_data(results)

        return f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>CALM Buoy Validation Report</title>
    <script src="https://cdn.plot.ly/plotly-2.26.0.min.js"></script>
    <style>
        {self._get_css()}
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>CALM Buoy Modular Input Validation Report</h1>
            <p class="timestamp">Generated: {timestamp}</p>
        </header>

        <section class="summary">
            {self._generate_summary_html(results)}
        </section>

        <section class="charts">
            <h2>Validation Overview</h2>
            <div id="statusChart"></div>
            <div id="issuesChart"></div>
        </section>

        <section class="results">
            <h2>Detailed Results</h2>
            {self._generate_results_table(results)}
        </section>
    </div>

    <script>
        {self._generate_javascript(chart_data)}
    </script>
</body>
</html>
"""

    def _get_css(self) -> str:
        """Get CSS styles."""
        return """
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
            margin: 0;
            padding: 0;
            background-color: #f5f5f5;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            padding: 20px;
        }

        header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }

        header h1 {
            margin: 0 0 10px 0;
            font-size: 2em;
        }

        .timestamp {
            margin: 0;
            opacity: 0.9;
        }

        .summary {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 20px;
            margin-bottom: 30px;
        }

        .summary-card {
            background: white;
            padding: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
            text-align: center;
        }

        .summary-card h3 {
            margin: 0 0 10px 0;
            color: #666;
            font-size: 0.9em;
            text-transform: uppercase;
        }

        .summary-card .value {
            font-size: 2.5em;
            font-weight: bold;
            margin: 0;
        }

        .value.pass { color: #10b981; }
        .value.warn { color: #f59e0b; }
        .value.fail { color: #ef4444; }

        section {
            background: white;
            padding: 30px;
            border-radius: 8px;
            margin-bottom: 20px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }

        h2 {
            margin-top: 0;
            color: #333;
            border-bottom: 2px solid #667eea;
            padding-bottom: 10px;
        }

        table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 20px;
        }

        th, td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #eee;
        }

        th {
            background-color: #f8f9fa;
            font-weight: 600;
            color: #333;
        }

        tr:hover {
            background-color: #f8f9fa;
        }

        .status-badge {
            padding: 4px 12px;
            border-radius: 12px;
            font-size: 0.85em;
            font-weight: 500;
        }

        .status-pass {
            background-color: #d1fae5;
            color: #065f46;
        }

        .status-warn {
            background-color: #fed7aa;
            color: #92400e;
        }

        .status-fail {
            background-color: #fee2e2;
            color: #991b1b;
        }

        .status-skipped {
            background-color: #e5e7eb;
            color: #374151;
        }

        .charts {
            margin-bottom: 30px;
        }

        #statusChart, #issuesChart {
            margin-bottom: 20px;
        }
        """

    def _generate_summary_html(self, results: List[ValidationResult]) -> str:
        """Generate summary cards HTML."""
        total = len(results)
        passed = sum(1 for r in results if r.overall_status == ValidationStatus.PASS)
        warned = sum(1 for r in results if r.overall_status == ValidationStatus.WARN)
        failed = sum(1 for r in results if r.overall_status == ValidationStatus.FAIL)
        total_issues = sum(len(r.issues) for r in results)

        return f"""
        <div class="summary-card">
            <h3>Total Files</h3>
            <p class="value">{total}</p>
        </div>
        <div class="summary-card">
            <h3>Passed</h3>
            <p class="value pass">{passed}</p>
        </div>
        <div class="summary-card">
            <h3>Warnings</h3>
            <p class="value warn">{warned}</p>
        </div>
        <div class="summary-card">
            <h3>Failed</h3>
            <p class="value fail">{failed}</p>
        </div>
        <div class="summary-card">
            <h3>Total Issues</h3>
            <p class="value">{total_issues}</p>
        </div>
        """

    def _generate_results_table(self, results: List[ValidationResult]) -> str:
        """Generate results table HTML."""
        rows = ""
        for result in results:
            status_class = f"status-{result.overall_status.value}"
            status_badge = f'<span class="status-badge {status_class}">{result.overall_status.value.upper()}</span>'

            level_1_status = result.level_1.status.value if result.level_1 else 'N/A'
            level_2_status = result.level_2.status.value if result.level_2 else 'N/A'
            level_3_status = result.level_3.status.value if result.level_3 else 'N/A'

            issues_count = len(result.issues)
            critical_count = result.level_3.critical_issues if result.level_3 else 0

            rows += f"""
            <tr>
                <td>{result.file_path.name}</td>
                <td>{status_badge}</td>
                <td>{level_1_status}</td>
                <td>{level_2_status}</td>
                <td>{level_3_status}</td>
                <td>{issues_count}</td>
                <td>{critical_count}</td>
                <td>{result.validation_time_seconds:.2f}s</td>
            </tr>
            """

        return f"""
        <table>
            <thead>
                <tr>
                    <th>File</th>
                    <th>Status</th>
                    <th>Level 1</th>
                    <th>Level 2</th>
                    <th>Level 3</th>
                    <th>Issues</th>
                    <th>Critical</th>
                    <th>Time</th>
                </tr>
            </thead>
            <tbody>
                {rows}
            </tbody>
        </table>
        """

    def _generate_chart_data(self, results: List[ValidationResult]) -> dict:
        """Generate data for Plotly charts."""
        # Status distribution
        status_counts = {
            'Passed': sum(1 for r in results if r.overall_status == ValidationStatus.PASS),
            'Warnings': sum(1 for r in results if r.overall_status == ValidationStatus.WARN),
            'Failed': sum(1 for r in results if r.overall_status == ValidationStatus.FAIL),
            'Skipped': sum(1 for r in results if r.overall_status == ValidationStatus.SKIPPED)
        }

        # Issues by file
        files = [r.file_path.name for r in results]
        issues_counts = [len(r.issues) for r in results]
        critical_counts = [r.level_3.critical_issues if r.level_3 else 0 for r in results]

        return {
            'status': status_counts,
            'files': files,
            'issues': issues_counts,
            'critical': critical_counts
        }

    def _generate_javascript(self, chart_data: dict) -> str:
        """Generate JavaScript for interactive charts."""
        return f"""
        // Status pie chart
        var statusData = [{{
            values: [{chart_data['status']['Passed']}, {chart_data['status']['Warnings']}, {chart_data['status']['Failed']}, {chart_data['status']['Skipped']}],
            labels: ['Passed', 'Warnings', 'Failed', 'Skipped'],
            type: 'pie',
            marker: {{
                colors: ['#10b981', '#f59e0b', '#ef4444', '#9ca3af']
            }}
        }}];

        var statusLayout = {{
            title: 'Validation Status Distribution',
            height: 400
        }};

        Plotly.newPlot('statusChart', statusData, statusLayout);

        // Issues bar chart
        var issuesData = [
            {{
                x: {json.dumps(chart_data['files'])},
                y: {json.dumps(chart_data['issues'])},
                name: 'Total Issues',
                type: 'bar',
                marker: {{color: '#f59e0b'}}
            }},
            {{
                x: {json.dumps(chart_data['files'])},
                y: {json.dumps(chart_data['critical'])},
                name: 'Critical Issues',
                type: 'bar',
                marker: {{color: '#ef4444'}}
            }}
        ];

        var issuesLayout = {{
            title: 'Issues by File',
            barmode: 'group',
            height: 400,
            xaxis: {{title: 'File'}},
            yaxis: {{title: 'Count'}}
        }};

        Plotly.newPlot('issuesChart', issuesData, issuesLayout);
        """
