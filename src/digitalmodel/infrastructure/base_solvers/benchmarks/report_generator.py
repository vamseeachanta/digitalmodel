"""
ABOUTME: HTML report generation with interactive Plotly visualizations
ABOUTME: Creates performance reports with charts, comparisons, and baselines
"""

import logging
import json
from typing import Dict, List, Any, Optional
from pathlib import Path
from datetime import datetime

try:
    import plotly.graph_objects as go
    import plotly.express as px
    from plotly.subplots import make_subplots
    PLOTLY_AVAILABLE = True
except ImportError:
    PLOTLY_AVAILABLE = False
    logger = logging.getLogger(__name__)
    logger.warning("Plotly not available. HTML reports will use basic charts.")

logger = logging.getLogger(__name__)


class BenchmarkReportGenerator:
    """Generate interactive HTML reports from benchmark results."""

    def __init__(self, results_dir: Optional[Path] = None):
        """
        Initialize report generator.

        Args:
            results_dir: Directory for saving reports
        """
        self.results_dir = results_dir or Path("./benchmarks/reports")
        self.results_dir.mkdir(parents=True, exist_ok=True)

        if not PLOTLY_AVAILABLE:
            logger.warning("Plotly not installed. Install with: pip install plotly pandas kaleido")

    def generate_report(
        self,
        benchmark_data: Dict[str, Any],
        report_name: str = "benchmark_report"
    ) -> Path:
        """
        Generate complete HTML report from benchmark data.

        Args:
            benchmark_data: Benchmark suite report data
            report_name: Name for the report file

        Returns:
            Path to generated HTML file
        """
        if not PLOTLY_AVAILABLE:
            return self._generate_basic_html_report(benchmark_data, report_name)

        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = self.results_dir / f"{report_name}_{timestamp}.html"

        # Create figures
        figs = self._create_figures(benchmark_data)

        # Generate HTML
        html_content = self._create_html_template(benchmark_data, figs)

        with open(report_file, "w") as f:
            f.write(html_content)

        logger.info(f"Generated report: {report_file}")
        return report_file

    def _create_figures(self, benchmark_data: Dict[str, Any]) -> Dict[str, str]:
        """
        Create Plotly figures for the report.

        Args:
            benchmark_data: Benchmark data

        Returns:
            Dictionary with figure names and HTML representations
        """
        figs = {}

        # Summary statistics figure
        figs["summary_stats"] = self._create_summary_stats_figure(benchmark_data).to_html(
            include_plotlyjs="cdn"
        )

        # Performance by category
        figs["by_category"] = self._create_category_performance_figure(benchmark_data).to_html(
            include_plotlyjs=False
        )

        # Execution time comparison
        figs["execution_time"] = self._create_execution_time_figure(benchmark_data).to_html(
            include_plotlyjs=False
        )

        # Memory usage figure
        figs["memory_usage"] = self._create_memory_usage_figure(benchmark_data).to_html(
            include_plotlyjs=False
        )

        # Pass/Fail distribution
        figs["pass_fail"] = self._create_pass_fail_figure(benchmark_data).to_html(
            include_plotlyjs=False
        )

        # Performance comparison with baselines
        figs["baseline_comparison"] = self._create_baseline_comparison_figure(benchmark_data).to_html(
            include_plotlyjs=False
        )

        return figs

    def _create_summary_stats_figure(self, benchmark_data: Dict[str, Any]) -> go.Figure:
        """Create summary statistics display."""
        summary = benchmark_data.get("summary", {})

        stats_text = f"""
        <b>Benchmark Summary</b><br>
        Total Tests: {summary.get('total', 0)}<br>
        Passed: {summary.get('passed', 0)}<br>
        Failed: {summary.get('failed', 0)}<br>
        Pass Rate: {summary.get('pass_rate', 0):.1f}%<br>
        <br>
        <b>Performance Metrics</b><br>
        Avg Execution Time: {summary.get('avg_execution_time_ms', 0):.2f}ms<br>
        Max Execution Time: {summary.get('max_execution_time_ms', 0):.2f}ms<br>
        Avg Memory Peak: {summary.get('avg_memory_peak_mb', 0):.2f}MB<br>
        Max Memory Peak: {summary.get('max_memory_peak_mb', 0):.2f}MB
        """

        fig = go.Figure(data=[go.Table(
            cells=dict(
                values=[[stats_text]],
                align="left",
                font=dict(size=12),
                fill_color="rgba(200, 200, 200, 0.2)",
            )
        )])

        fig.update_layout(
            title="Summary Statistics",
            height=300,
            showlegend=False,
        )

        return fig

    def _create_category_performance_figure(self, benchmark_data: Dict[str, Any]) -> go.Figure:
        """Create performance by category figure."""
        categories = benchmark_data.get("categories", {})

        category_names = []
        avg_times = []
        test_counts = []

        for cat_name, results in categories.items():
            if results:
                category_names.append(cat_name)
                times = [r.get("metrics", {}).get("execution_time_ms", 0) for r in results]
                avg_times.append(sum(times) / len(times) if times else 0)
                test_counts.append(len(results))

        fig = go.Figure(data=[
            go.Bar(x=category_names, y=avg_times, name="Avg Execution Time (ms)",
                   hovertemplate="<b>%{x}</b><br>Avg Time: %{y:.2f}ms<extra></extra>")
        ])

        fig.update_layout(
            title="Average Execution Time by Category",
            xaxis_title="Category",
            yaxis_title="Time (ms)",
            hovermode="x unified",
            height=400,
        )

        return fig

    def _create_execution_time_figure(self, benchmark_data: Dict[str, Any]) -> go.Figure:
        """Create execution time comparison figure."""
        results = benchmark_data.get("results", [])

        if not results:
            return go.Figure().add_annotation(text="No data available")

        names = [r.get("name", "Unknown") for r in results]
        times = [r.get("metrics", {}).get("execution_time_ms", 0) for r in results]
        passed = [r.get("passed", False) for r in results]

        colors = ["green" if p else "red" for p in passed]

        fig = go.Figure(data=[
            go.Bar(
                x=names,
                y=times,
                marker_color=colors,
                hovertemplate="<b>%{x}</b><br>Time: %{y:.2f}ms<extra></extra>",
            )
        ])

        fig.update_layout(
            title="Execution Time by Test",
            xaxis_title="Test Name",
            yaxis_title="Time (ms)",
            hovermode="x unified",
            height=500,
            xaxis_tickangle=-45,
        )

        return fig

    def _create_memory_usage_figure(self, benchmark_data: Dict[str, Any]) -> go.Figure:
        """Create memory usage figure."""
        results = benchmark_data.get("results", [])

        if not results:
            return go.Figure().add_annotation(text="No data available")

        names = [r.get("name", "Unknown") for r in results]
        memory_peak = [r.get("metrics", {}).get("memory_peak_mb", 0) for r in results]
        memory_avg = [r.get("metrics", {}).get("memory_avg_mb", 0) for r in results]

        fig = go.Figure(data=[
            go.Bar(x=names, y=memory_peak, name="Peak Memory (MB)",
                   hovertemplate="<b>%{x}</b><br>Peak: %{y:.2f}MB<extra></extra>"),
            go.Bar(x=names, y=memory_avg, name="Avg Memory (MB)",
                   hovertemplate="<b>%{x}</b><br>Avg: %{y:.2f}MB<extra></extra>"),
        ])

        fig.update_layout(
            title="Memory Usage by Test",
            xaxis_title="Test Name",
            yaxis_title="Memory (MB)",
            barmode="group",
            hovermode="x unified",
            height=500,
            xaxis_tickangle=-45,
        )

        return fig

    def _create_pass_fail_figure(self, benchmark_data: Dict[str, Any]) -> go.Figure:
        """Create pass/fail distribution figure."""
        summary = benchmark_data.get("summary", {})

        passed = summary.get("passed", 0)
        failed = summary.get("failed", 0)

        fig = go.Figure(data=[
            go.Pie(
                labels=["Passed", "Failed"],
                values=[passed, failed],
                marker=dict(colors=["green", "red"]),
                hovertemplate="<b>%{label}</b><br>Count: %{value}<extra></extra>",
            )
        ])

        fig.update_layout(
            title="Test Results Distribution",
            height=400,
        )

        return fig

    def _create_baseline_comparison_figure(self, benchmark_data: Dict[str, Any]) -> go.Figure:
        """Create baseline comparison figure."""
        results = benchmark_data.get("results", [])

        # Filter results that have baseline comparison
        baseline_comparisons = [
            r for r in results
            if r.get("comparison_to_baseline") and r.get("baseline")
        ]

        if not baseline_comparisons:
            fig = go.Figure()
            fig.add_annotation(text="No baseline data available")
            return fig

        names = [r.get("name", "Unknown") for r in baseline_comparisons]
        time_deltas = [
            r.get("comparison_to_baseline", {}).get("time_delta_percent", 0)
            for r in baseline_comparisons
        ]
        memory_deltas = [
            r.get("comparison_to_baseline", {}).get("memory_delta_percent", 0)
            for r in baseline_comparisons
        ]

        fig = go.Figure(data=[
            go.Bar(x=names, y=time_deltas, name="Time Delta (%)",
                   hovertemplate="<b>%{x}</b><br>Time: %{y:.1f}%<extra></extra>"),
            go.Bar(x=names, y=memory_deltas, name="Memory Delta (%)",
                   hovertemplate="<b>%{x}</b><br>Memory: %{y:.1f}%<extra></extra>"),
        ])

        fig.update_layout(
            title="Performance Change vs Baseline",
            xaxis_title="Test Name",
            yaxis_title="Percent Change (%)",
            barmode="group",
            hovermode="x unified",
            height=500,
            xaxis_tickangle=-45,
        )

        return fig

    def _create_html_template(self, benchmark_data: Dict[str, Any], figs: Dict[str, str]) -> str:
        """Create HTML template with embedded figures."""
        timestamp = benchmark_data.get("timestamp", "Unknown")
        suite_name = benchmark_data.get("suite_name", "Benchmark Report")

        html = f"""
        <!DOCTYPE html>
        <html lang="en">
        <head>
            <meta charset="UTF-8">
            <meta name="viewport" content="width=device-width, initial-scale=1.0">
            <title>{suite_name}</title>
            <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
            <style>
                * {{
                    margin: 0;
                    padding: 0;
                    box-sizing: border-box;
                }}

                body {{
                    font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
                    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                    color: #333;
                    line-height: 1.6;
                }}

                .container {{
                    max-width: 1400px;
                    margin: 0 auto;
                    padding: 20px;
                }}

                .header {{
                    background: white;
                    padding: 30px;
                    border-radius: 10px;
                    margin-bottom: 30px;
                    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                }}

                .header h1 {{
                    color: #667eea;
                    margin-bottom: 10px;
                }}

                .header p {{
                    color: #666;
                    font-size: 14px;
                }}

                .grid {{
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(600px, 1fr));
                    gap: 20px;
                    margin-bottom: 30px;
                }}

                .chart-container {{
                    background: white;
                    border-radius: 10px;
                    padding: 20px;
                    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                }}

                .chart-container h2 {{
                    color: #667eea;
                    margin-bottom: 15px;
                    font-size: 18px;
                }}

                .full-width {{
                    grid-column: 1 / -1;
                }}

                .summary-grid {{
                    display: grid;
                    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
                    gap: 15px;
                    margin-bottom: 30px;
                }}

                .summary-card {{
                    background: white;
                    padding: 20px;
                    border-radius: 10px;
                    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                    text-align: center;
                }}

                .summary-card .value {{
                    font-size: 32px;
                    font-weight: bold;
                    color: #667eea;
                }}

                .summary-card .label {{
                    font-size: 12px;
                    color: #999;
                    text-transform: uppercase;
                    margin-top: 5px;
                }}

                .footer {{
                    background: white;
                    padding: 20px;
                    border-radius: 10px;
                    text-align: center;
                    color: #999;
                    font-size: 12px;
                    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
                }}

                .plotly {{
                    height: 100%;
                }}
            </style>
        </head>
        <body>
            <div class="container">
                <div class="header">
                    <h1>{suite_name}</h1>
                    <p>Generated: {timestamp}</p>
                </div>

                <div class="summary-grid">
                    <div class="summary-card">
                        <div class="value">{benchmark_data.get("summary", {}).get("total", 0)}</div>
                        <div class="label">Total Tests</div>
                    </div>
                    <div class="summary-card">
                        <div class="value">{benchmark_data.get("summary", {}).get("passed", 0)}</div>
                        <div class="label">Passed</div>
                    </div>
                    <div class="summary-card">
                        <div class="value">{benchmark_data.get("summary", {}).get("failed", 0)}</div>
                        <div class="label">Failed</div>
                    </div>
                    <div class="summary-card">
                        <div class="value">{benchmark_data.get("summary", {}).get("pass_rate", 0):.1f}%</div>
                        <div class="label">Pass Rate</div>
                    </div>
                </div>

                <div class="grid">
                    <div class="chart-container">
                        <h2>Summary Statistics</h2>
                        {figs.get("summary_stats", "")}
                    </div>

                    <div class="chart-container">
                        <h2>Test Results</h2>
                        {figs.get("pass_fail", "")}
                    </div>

                    <div class="chart-container full-width">
                        <h2>Execution Time by Test</h2>
                        {figs.get("execution_time", "")}
                    </div>

                    <div class="chart-container">
                        <h2>Performance by Category</h2>
                        {figs.get("by_category", "")}
                    </div>

                    <div class="chart-container">
                        <h2>Memory Usage</h2>
                        {figs.get("memory_usage", "")}
                    </div>

                    <div class="chart-container">
                        <h2>Baseline Comparison</h2>
                        {figs.get("baseline_comparison", "")}
                    </div>
                </div>

                <div class="footer">
                    <p>Benchmark Report Generated by DigitalModel Performance Framework</p>
                    <p>For questions, contact the development team</p>
                </div>
            </div>
        </body>
        </html>
        """

        return html

    def _generate_basic_html_report(
        self,
        benchmark_data: Dict[str, Any],
        report_name: str
    ) -> Path:
        """Generate basic HTML report without Plotly."""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = self.results_dir / f"{report_name}_{timestamp}.html"

        summary = benchmark_data.get("summary", {})
        results = benchmark_data.get("results", [])

        # Create results table
        results_table = "<tr><th>Name</th><th>Category</th><th>Time (ms)</th><th>Memory (MB)</th><th>Status</th></tr>"
        for result in results:
            status = "✓ Pass" if result.get("passed") else "✗ Fail"
            results_table += f"""
            <tr>
                <td>{result.get("name", "Unknown")}</td>
                <td>{result.get("category", "Unknown")}</td>
                <td>{result.get("metrics", {}).get("execution_time_ms", 0):.2f}</td>
                <td>{result.get("metrics", {}).get("memory_peak_mb", 0):.2f}</td>
                <td>{status}</td>
            </tr>
            """

        html = f"""
        <!DOCTYPE html>
        <html>
        <head>
            <title>{report_name}</title>
            <style>
                body {{ font-family: Arial, sans-serif; margin: 20px; }}
                h1 {{ color: #667eea; }}
                .summary {{ background: #f0f0f0; padding: 15px; border-radius: 5px; margin-bottom: 20px; }}
                table {{ width: 100%; border-collapse: collapse; }}
                th, td {{ padding: 10px; text-align: left; border-bottom: 1px solid #ddd; }}
                th {{ background-color: #667eea; color: white; }}
                tr:hover {{ background-color: #f5f5f5; }}
            </style>
        </head>
        <body>
            <h1>Benchmark Report</h1>
            <p>Generated: {benchmark_data.get("timestamp", "Unknown")}</p>

            <div class="summary">
                <h2>Summary</h2>
                <p>Total Tests: {summary.get("total", 0)}</p>
                <p>Passed: {summary.get("passed", 0)}</p>
                <p>Failed: {summary.get("failed", 0)}</p>
                <p>Pass Rate: {summary.get("pass_rate", 0):.1f}%</p>
                <p>Avg Time: {summary.get("avg_execution_time_ms", 0):.2f}ms</p>
                <p>Avg Memory: {summary.get("avg_memory_peak_mb", 0):.2f}MB</p>
            </div>

            <h2>Results</h2>
            <table>
                {results_table}
            </table>
        </body>
        </html>
        """

        with open(report_file, "w") as f:
            f.write(html)

        logger.info(f"Generated basic HTML report: {report_file}")
        return report_file
